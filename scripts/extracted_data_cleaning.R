## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Extracted Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Date: 03/10/2025
## Updated: 08/19/2025

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, stringr)

source(here("scripts/categorization_included_studies.R"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ---- (Already loaded in source code)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # Load the CSV file
# extracted_studies_raw <- read_csv(here("data/extracted_studies_aug_11.csv")) 
# # THIS DATA IS CONSENSUS

# # Create consensus column
# extracted_studies_raw <- extracted_studies_raw %>%
#   mutate(consensus = if_else(`Reviewer Name` == "Consensus", "Yes", "No"))
# 
# # Create dataset with consensus only
# studies_with_consensus <- extracted_studies_raw %>% 
#   filter(consensus == "Yes") %>% 
#   ungroup()
# 
# # Create dataset without consensus
# studies_without_consensus <- extracted_studies_raw %>% 
#   group_by(`Covidence #`) %>% 
#   filter(!any(consensus == "Yes")) %>% 
#   #Only keep the ones without consensus
#   filter(n() <= 2) %>% 
#   #Select first instance
#   slice(1) %>%
#   ungroup()
# 
# # Join both datasets
# extracted_studies_joined <- full_join(studies_with_consensus, studies_without_consensus)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean column names (pathologies)
categorized_studies_clean <- clean_names(categorized_studies) %>% 
  # Convert to lower for easier reading
  mutate(general_category_of_pathology = str_to_lower(general_category_of_pathology),
         specific_pathology = str_to_lower(specific_pathology),
         types_of_surgical_procedure_performed = str_to_lower(types_of_surgical_procedure_performed)
         ) %>% 
  # Clean surgery variable
  separate_rows(types_of_surgical_procedure_performed, sep = ",|;") %>%
  mutate(
    surgery = str_trim(str_to_lower(types_of_surgical_procedure_performed)),
    surgery = str_replace_all(surgery, "\\s+", " "),   # collapse multiple spaces
    surgery = str_replace_all(surgery, "grafts?", "graft"),  # unify plurals
    surgery = str_replace_all(surgery, "plasties", "plasty"), 
    surgery = str_replace_all(surgery, "debridements?", "debridement"),
    surgery = str_replace_all(surgery, "\\(.*?\\)", ""),      # remove text in parentheses
    surgery = str_trim(surgery)
  ) %>% 
  group_by(covidence_number) %>%
  summarise(
    surgery = if (all(is.na(surgery))) NA_character_ else paste(unique(na.omit(surgery)), collapse = "; "),
    .groups = "drop"
  )

categorized_studies_clean <- right_join(categorized_studies, categorized_studies_clean, by = "covidence_number")


# # View unique general pathologies values
# df <- categorized_studies_clean %>%
#   separate_rows(general_category_of_pathology, sep = ",|;") %>%
#   mutate(general_category_of_pathology = str_trim(general_category_of_pathology)) #%>%
#   #distinct(general_category_of_pathology)

# # View unique specific pathologies values
# df <- categorized_studies_clean %>% 
#   # Remove digits-only rows and trim whitespace
#   filter(!str_detect(specific_pathology, "^\\d+$")) %>%
#   # Remove "(e.g." and all content in parentheses
#   mutate(specific_pathology = str_remove_all(specific_pathology, "\\(.*?\\)")) %>%
#   separate_rows(specific_pathology, sep = ",|;|\\band\\/or\\b|\\band\\b|\\n|\\r\\n") %>% 
#   distinct(specific_pathology, .keep_all = TRUE) %>% 
#   mutate(specific_pathology = str_trim(specific_pathology)) %>% 
#   group_by(specific_pathology) %>% 
#   count()

# # View unique surgeries values
# df <- categorized_studies_clean %>%
#   separate_rows(surgery, sep = ",|;") %>%
#   distinct(surgery, .keep_all = TRUE) %>% # Ensure uniqueness before counting
#   count(surgery, name = "frequency")  # Count occurrences properly

# # Identify NAs
# pathology_missing <- categorized_studies_clean[is.na(categorized_studies_clean$general_category_of_pathology), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize general pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# UPDATED LIST (08/19/2025)
categories_gp <- list(
  "Burns" = c(
    "burns?", "burn complications"
  ),
  
  "Congenital Malformations" = c(
    "congenital malformations?", "congenital anomalies?", "congenital conditions?", 
    "congenital abnormalities?", "congenital deformities?", "acquired deformities?", 
    "craniofacial disorders?", "craniofacial anomalies?"
  ),
  
  "Trauma" = c(
    "trauma", "injur(y|ies)", "wounds?", "chronic wounds?", "scars?", 
    "ulcer(s)", "pressure ulcer(s)?", "venous ulcer(s)?", "arterial ulcer(s)?"
  ),
  
  "Infectious Conditions" = c(
    "noma", "infectious?", "infection(s)?", "infectious diseases?", 
    "neck infection", "buruli?", "bacteri?", "infected ulcer(s)?"
  ),
  
  "Neoplastic Conditions" = c(
    "tumor(s)?", "tumour(s)?", "neoplasms?", "cancer( \\(tumor\\))?", 
    "skin cancer", "neck cancer", "melanoma?", "squamous cell carcinoma"
  ),
  
  "Other Conditions" = c(
    "cyst?"
  )
)

# Function to categorize general pathology
categorize_gen_path <- function(general_pathology, specific_pathology, category_tags, 
                                category_tiab, categories) {
  
  assign_categories <- function(text) {
    if (is.na(text) || text == "") return(NA_character_)
    
    assigned <- names(categories)[
      sapply(categories, function(patterns) {
        any(sapply(patterns, function(pat) {
          # Add word boundaries if pattern doesn't already have them
          pattern <- if (grepl("^\\\\b.*\\\\b$", pat)) pat else paste0("\\b", pat, "\\b")
          str_detect(text, regex(pattern, ignore_case = TRUE))
        }))
      })
    ]
    
    if (length(assigned) > 0) {
      return(paste(assigned, collapse = "; "))
    }
    return(NA_character_)
  }
  
  # Try general_pathology first
  category <- assign_categories(general_pathology)
  if (!is.na(category)) return(category)
  
  # Then specific_pathology
  category_specific <- assign_categories(specific_pathology)
  if (!is.na(category_specific)) return(category_specific)
  
  # If none matched but some text exists, assign "Other"
  if (( !is.na(general_pathology) && general_pathology != "" ) ||
      ( !is.na(specific_pathology) && specific_pathology != "" )
      ) {
    return("Other")
  }
  
  # If all inputs missing or empty
  return(NA_character_)
}

# Apply function
categorized_general_pathology <- categorized_studies_clean %>%
  mutate(category_gen_pathology = mapply(categorize_gen_path, 
                                         general_category_of_pathology, 
                                         specific_pathology, 
                                         category_tags,
                                         category_tiab,
                                         MoreArgs = list(categories = categories_gp)))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize specific pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UPDATED LIST (08/19/2025)
categories_sp <- list(
  # Burns
  "Acute Burns" = c("acute burn(s| injuries?)?", "chemical burn(s)?", "contact burn(s)?",
                    "electrical burn(s)?", "flame burn(s)?", "open flame burn(s)?",
                    "scald burn(s)?", "inhalation burn(s)?", 
                    "burn(s)?", "scald(s)?", "thermal burn(s)?", "third-degree burn(s)?",
                    "severe burns?"),
  
  # Contractures & Scars
  "Post-Burn Contractures & Scars" = c("burn scar(s)?", "post-?burn contracture(s)?", "pbc", 
                                       "contracture(s)?"),
  "Scars" = c("scar revision", "hypertrophic scar(s)?", "keloid(s)?", 
              "cicatricial eyelid ectropion", "pathological scar(s)?", "scarring"),
  
  # Trauma & Injuries
  "Open Fractures" = c("open fracture(s)?", "open fractures of humerus", "open tibia fracture(s)?"),
  "Polytrauma" = c("polytrauma", "multiple injuries"),
  "Road Traffic Injuries" = c("road traffic accident(s)?", "road traffic crash(es)?", 
                              "road traf+ic injur(y|ies)", "rta"),
  "Severe Injuries" = c("amputation(s)?", "mutilation(s)?",
                        "gunshot", "trauma", "blunt"),
  "Soft Tissue Injuries" = c("wound(s)?", "soft tissue injur(y|ies)"),
  "Ulcers" = c("pressure ulcer(s)?", "pressure injur(y|ies)", "venous ulcer(s)?", "traumatic ulcer(s)?", 
               "posttraumatic ulcer(s)?", "chronic leg ulcers (CLU)", 
               "ulcerated"),
  "General Fractures" = c("maxillofacial fractures","mandibular fractures",
                          "zygomatic complex fractures", "fracture(s)"),
  "Other Trauma" = c("human bites", "internal injuries", "hand injur(y|ies)"),
  
  # Specific Infections
  "Buruli Ulcer" = c("buruli ulcer(s)?", "mycobacterium ulcerans"),
  "Noma" = c("noma", "cancrum oris"),
  "Necrotizing Fasciitis" = c("necrotizing fasciitis", "fasciitis?"),
  
  # Congenital & Craniofacial
  "Cleft Lip & Palate" = c("cleft lip(s)?", "cleft palate(s)?", "non-syndromic cleft lip", 
                           "orofacial cleft(s)?", "craniofacial clefts", "cleft lip/palate"),
  "Polydactyly & Syndactyly" = c("polydactyl(y)?", "preaxial polydactyly", "syndactyly"),
  "Clubfoot" = c("club ?foot", "talipes deformit(y|ies)", "rocker bottom foot", # Others
                 "congenital talipes equino varus"),
  
  # Neoplasms & Tumors
  "Cancer" = c("cancer(s)?", "carcinoma", "sarcoma", "squamous cell carcinoma", "skin cancer(s)?",
               "stromal tumor", "dermatofibrosarcoma", "scc in situ", "soft tissue sarcoma")
)

# Mapping from specific → general category
map_sp_to_gp <- list(
  # Burns
  "Acute Burns" = "Burns",
  "Post-Burn Contractures & Scars" = "Burns",
  
  # Trauma
  "Open Fractures" = "Trauma",
  "Polytrauma" = "Trauma",
  "Road Traffic Injuries" = "Trauma",
  "Severe Injuries" = "Trauma",
  "Scars" = "Trauma",
  "Soft Tissue Injuries" = "Trauma",
  "Ulcers" = "Trauma",
  "General Fractures" = "Trauma",
  
  # Infections
  "Buruli Ulcer" = "Infectious Conditions",
  "Noma" = "Infectious Conditions",
  "Necrotizing Fasciitis" = "Infectious Conditions",
  
  # Congenital
  "Cleft Lip & Palate" = "Congenital Malformations",
  "Polydactyly & Syndactyly" = "Congenital Malformations",
  "Clubfoot" = "Congenital Malformations",
  
  # Neoplasms
  "Cancer" = "Neoplastic Conditions"
)

# Function
categorize_specific_path <- function(specific_pathology, general_pathology, categories_sp, map_sp_to_gp) {
  
  assign_categories <- function(pathology) {
    if (is.na(pathology) || pathology == "") return(NA_character_)
    
    pathology <- str_to_lower(pathology)
    
    assigned <- names(categories_sp)[
      sapply(categories_sp, function(words) {
        pattern <- paste(words, collapse = "|")
        str_detect(pathology, regex(pattern, ignore_case = TRUE))
      })
    ]
    
    if (length(assigned) > 0) return(assigned)
    return(NA_character_)
  }
  
  # --- CASE 1: If no text at all → NA
  if (is.na(specific_pathology) || specific_pathology == "") {
    return(NA_character_)
  }
  
  # Try to match specific categories
  specific_cats <- assign_categories(specific_pathology)
  general_cats <- if (!is.na(general_pathology)) str_split(general_pathology, ";\\s*")[[1]] else character(0)
  
  # --- Helper: safe "Other ..." wrapper ---
  make_other <- function(gc) {
    if (gc == "Other" || str_starts(gc, "Other ")) {
      return(gc)  # already "Other …"
    } else {
      return(paste0("Other ", gc))
    }
  }
  
  # --- CASE 2a: text present but no specific match
  if (all(is.na(specific_cats))) {
    if (length(general_cats) > 0) {
      others <- sapply(general_cats, make_other)
      return(paste(unique(others), collapse = "; "))
    } else {
      return("Other")
    }
  }
  
  # --- CASE 3: Specific matches exist, filter by general
  valid_specifics <- specific_cats[
    sapply(specific_cats, function(sp) isTRUE(map_sp_to_gp[[sp]] %in% general_cats)) # ✅ safe check
  ]
  
  if (length(valid_specifics) > 0) {
    return(paste(valid_specifics, collapse = "; "))
  } else {
    if (length(general_cats) > 0) {
      others <- sapply(general_cats, make_other)
      return(paste(unique(others), collapse = "; "))
    } else {
      return("Other")
    }
  }
}

# Example application
categorized_combined_pathology <- categorized_general_pathology %>%
  mutate(category_specific_pathology = mapply(
    categorize_specific_path, 
    specific_pathology, 
    category_gen_pathology, 
    MoreArgs = list(categories_sp = categories_sp, map_sp_to_gp = map_sp_to_gp)
  ))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize surgeries performed ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define surgery categories
categories_surgery <- list(
  # 1. Cleft & Craniofacial
  "Cleft & Craniofacial Surgery" = c(
    "cleft", "lip", "palatoplasty", "palate repair", "lip repair", "rhinoplasty",
    "von langenbeck", "furlow", "millard", "tennison", "commissuroplasty", 
    "primary surgical repair of cl/p"
  ),
  # 2. Skin Grafting
  "Skin Grafting" = c(
    "skin graft", "split[- ]?thickness skin graft", "stsg",
    "full[- ]?thickness skin graft", "ftsg", "pinch graft",
    "grafting of cutaneous ulcers", "spongious graft", "grafting?"
  ),
  # 3. Flap Surgery
  "Flap Surgery" = c(
    "flap", "z[- ]?plasty", "y[- ]?v flap", "square flap", "advancement flap",
    "rotation flap", "transposition flap", "pedicled flap", "myocutaneous flap",
    "fasciocutaneous flap", "temporoparietal fascia", "nasolabial flap",
    "forehead flap", "sural flap", "estlander flap", "camille[- ]?bernard flap",
    "webster flap", "tubed pedicled abdominal flap", "thoracoabdominal flap",
    "free flap"
  ),
  # 4. Contracture Release & Scar Surgery
  "Contracture Release & Scar Surgery" = c(
    "contracture release", "scar revision", "burn contracture",
    "scar surgery", "release surgery", "excision with closure",
    "excision and grafting", "straight[- ]?line repair", "escharotomy", "burn excision",
    "primary skin closure; excision"
  ),
  # 5. Trauma & Reconstructive Procedures
  "Trauma & Reconstructive Procedures" = c(
    "fracture", "dislocation", "fixation", "orif", "open reduction",
    "closed reduction", "mmf", "maxillo[- ]?mandibular", "nerve repair",
    "tendon repair", "tendon transfer", "external fixator", "exploration",
    "debridement", "suturing", "wound care", "washout", "surgical toilet"
  ),
  # 6. Oncologic & Tumor Surgery
  "Oncologic & Tumor Surgery" = c(
    "tumou?r", "mastectomy", "lumpectomy", "breast lump excision",
    "nephrectomy", "ganglionectomy", 
    "cyst excision", #review
    #"biopsy",
    "lymph node", "tumor resection", "wide local excision"
  ),
  # 7. Amputation & Limb Salvage
  "Amputation & Limb Salvage" = c(
    "amputation", "amputations", "salvage amputation",
    "ankylosis release", #Review 
    "primary repair of cut achilles tendon",
    "sequestrectomy" #Any body part
  )
)

# Function to categorize surgery
categorize_surgeries <- function(surgery, categories) {
  # Return NA if input is NA or empty
  if (is.na(surgery) || str_trim(surgery) == "") return(NA_character_)
  
  # Lowercase for matching
  surgery <- str_to_lower(surgery)
  
  # Check categories
  matched <- names(categories)[sapply(categories, function(patterns) {
    regex <- paste0("(", paste(patterns, collapse = "|"), ")")
    str_detect(surgery, regex)
  })]
  
  if (length(matched) > 0) {
    return(paste(unique(matched), collapse = "; "))
  } else {
    return("Other")  # only for non-empty, non-NA inputs
  }
}


# Apply categorization to original dataset
categorized_surgeries <- categorized_studies_clean %>%
  mutate(category_surgery = sapply(surgery, categorize_surgeries, 
                                   categories = categories_surgery))

categorized_final <- categorized_combined_pathology %>%
  mutate(category_surgery = sapply(surgery, categorize_surgeries, 
                                   categories = categories_surgery))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Epi Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
incidence_subset <- categorized_final %>% 
  filter(!is.na(incidence_rate)) %>% 
  arrange(category_gen_pathology) %>% 
  mutate(output = paste0(title_3, ": ", incidence_rate)) %>% 
  pull(output)

prevalence_subset <- categorized_final %>% 
  filter(!is.na(prevalence_rate)) %>% 
  select(covidence_number, study_id, title_3, country, category_gen_pathology,
         category_specific_pathology, incidence_rate, prevalence_rate)

epi_subset <- categorized_final %>% 
  filter(!is.na(incidence_rate) | !is.na(prevalence_rate)) %>% 
  select(covidence_number, study_id, title_3, country, category_gen_pathology,
         category_specific_pathology, incidence_rate, prevalence_rate)

epi_summary <- categorized_final %>% 
  filter(!is.na(incidence_rate) | !is.na(prevalence_rate)) %>% 
  separate_rows(category_gen_pathology, sep = ";") %>% 
  mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>% 
  group_by(category_gen_pathology) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize DALYs Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

daly_subset <- categorized_final %>% 
  filter(!is.na(disability_adjusted_life_years_dal_ys)) #%>% 
  # select(covidence_number, study_id, title_3, country, general_category_of_pathology, 
  #        specific_pathology, disability_adjusted_life_years_dal_ys)

# Extract countries
country_daly <- daly_subset %>%  
  # Identify countries
  separate_rows(country, sep = ";|,") %>% 
  mutate(country = str_trim(country)) %>% 
  distinct(country) %>% 
  pull()

# Create summary table
summary_path_daly <- daly_subset %>%
  separate_rows(category_gen_pathology, sep = ";") %>%
  mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>%
  group_by(category_gen_pathology) %>%
  summarise(
    n = n_distinct(covidence_number),   # count unique studies per category
    .groups = "drop"
  ) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(n))

summary_sp_daly <- daly_subset %>%
  separate_rows(category_specific_pathology, sep = ";") %>%
  mutate(category_specific_pathology = str_trim(category_specific_pathology)) %>%
  group_by(category_specific_pathology) %>%
  summarise(
    n = n_distinct(covidence_number),   # count unique studies per category
    .groups = "drop"
  ) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(n))

### Orofacial clefts ----

# Find studies on congenital malformation
malformation_daly <- daly_subset %>%
  separate_rows(category_gen_pathology, sep = ";") %>%
  mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>%  # remove spaces
  filter(str_to_lower(category_gen_pathology) == "congenital malformations") %>%
  select(covidence_number, title_3, country, general_category_of_pathology, category_gen_pathology,
         specific_pathology, category_specific_pathology, types_of_surgical_procedure_performed, 
         category_surgery, disability_adjusted_life_years_dal_ys)

# Find studies on clefts
cleft_daly <- daly_subset %>% 
  separate_rows(category_specific_pathology, sep = ";") %>% 
  mutate(category_specific_pathology = str_trim(category_specific_pathology)) %>% 
  filter(str_to_lower(category_specific_pathology) == "cleft lip & palate") %>% 
  select(covidence_number, title_3, country, general_category_of_pathology, category_gen_pathology,
         specific_pathology, category_specific_pathology, types_of_surgical_procedure_performed, 
         category_surgery, disability_adjusted_life_years_dal_ys)

### Burns ----

# Find studies on burns
burn_daly <- daly_subset %>% 
  separate_rows(category_gen_pathology, sep = ";") %>% 
  mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>% 
  filter(str_to_lower(category_gen_pathology) == "burns") %>% 
  select(covidence_number, study_id, title_3, country, general_category_of_pathology, 
         specific_pathology, disability_adjusted_life_years_dal_ys)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Cost Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_1 <- categorized_final %>% 
  filter(!is.na(cost_to_individuals_cost_in_usd_or_other_currency))

cost_2 <- categorized_final %>% 
  filter(!is.na(cost_to_government_cost_in_usd_or_other_currency))

cost_subset <- full_join(cost_1, cost_2) %>% 
  select(covidence_number, study_id, title_3, country, general_category_of_pathology, 
         specific_pathology, cost_to_government_cost_in_usd_or_other_currency, 
         cost_to_individuals_cost_in_usd_or_other_currency)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combine Cost & DALYs Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Join both subsets
# cost_daly_subset <- left_join(cost_subset, burn_daly, by = c("study_id"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Surgical Capacity Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sc <- extracted_studies_clean %>% 
#   select(covidence_number, title_3, country, general_category_of_pathology, 
#          specific_pathology, other_findings, 33:83)

  