## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Extracted Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Date: 03/10/2025
## Updated: 09/16/2025

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, stringr, gt)

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
categorized_studies_clean <- clean_names(joined_studies) %>% 
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

categorized_studies_clean <- right_join(joined_studies, categorized_studies_clean, by = "covidence_number")


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
## Categorize PRS specific studies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



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

# Application
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

# UPDATED 09/03/2025
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
    "free flap", "free tissue transfer"
  ),
  # 4. Contracture Release & Scar Surgery
  "Contracture Release & Scar Surgery" = c(
    "contracture release", "scar revision", "burn contracture",
    "scar surgery", "release surgery", "excision with closure",
    "excision and grafting", "straight[- ]?line repair", "escharotomy", "burn excision",
    "primary skin closure; excision", "scar"
  ),
  # 5. Skeletal Reconstruction
  "Skeletal Reconstruction " = c(
    "fracture", "dislocation", "fixation", "orif", "open reduction",
    "closed reduction", "mmf", "maxillo[- ]?mandibular", "external fixator", 
    "codivilla", "ankylosis release"
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
    "primary repair of cut achilles tendon",
    "sequestrectomy" #Any body part
  ),
  # 8. Wound Management
  "Wound Management" = c(
    "debridement", "suturing", "wound care", "washout", "surgical toilet"
  ),
  # 9. Nerve & Tendon Repair
  "Soft Tissue Repair" =c(
    "nerve repair", "tendon repair", "tendon transfer", "soft tissue", "tendon"
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
# incidence_subset <- categorized_final %>% 
#   filter(!is.na(incidence_rate)) %>% 
#   arrange(category_gen_pathology) %>% 
#   mutate(output = paste0(title_3, ": ", incidence_rate)) %>% 
#   pull(output)
# 
# prevalence_subset <- categorized_final %>% 
#   filter(!is.na(prevalence_rate)) %>% 
#   select(covidence_number, study_id, title_3, country, category_gen_pathology,
#          category_specific_pathology, incidence_rate, prevalence_rate)
# 
# epi_subset <- categorized_final %>% 
#   filter(!is.na(incidence_rate) | !is.na(prevalence_rate)) %>% 
#   select(covidence_number, study_id, title_3, country, category_gen_pathology,
#          category_specific_pathology, incidence_rate, prevalence_rate)
# 
# epi_summary <- categorized_final %>% 
#   filter(!is.na(incidence_rate) | !is.na(prevalence_rate)) %>% 
#   separate_rows(category_gen_pathology, sep = ";") %>% 
#   mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>% 
#   group_by(category_gen_pathology) %>% 
#   summarise(n = n()) %>% 
#   arrange(desc(n))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize DALYs Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# daly_subset <- categorized_final %>% 
#   filter(!is.na(disability_adjusted_life_years_dal_ys)) %>% 
#   select(covidence_number, study_id, title_3, country,main_study_findings,
#          category_gen_pathology, category_specific_pathology,disability_adjusted_life_years_dal_ys) %>% 
#   arrange(category_gen_pathology)

#  # Extract countries
# country_daly <- daly_subset %>%  
#   # Identify countries
#   separate_rows(country, sep = ";|,") %>% 
#   mutate(country = str_trim(country)) %>% 
#   distinct(country) %>% 
#   pull()

# # Create summary table
# summary_path_daly <- daly_subset %>%
#   separate_rows(category_gen_pathology, sep = ";") %>%
#   mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>%
#   group_by(category_gen_pathology) %>%
#   summarise(
#     n = n_distinct(covidence_number),   # count unique studies per category
#     .groups = "drop"
#   ) %>%
#   mutate(percentage = n / sum(n) * 100) %>%
#   arrange(desc(n))

# summary_sp_daly <- daly_subset %>%
#   separate_rows(category_specific_pathology, sep = ";") %>%
#   mutate(category_specific_pathology = str_trim(category_specific_pathology)) %>%
#   group_by(category_specific_pathology) %>%
#   summarise(
#     n = n_distinct(covidence_number),   # count unique studies per category
#     .groups = "drop"
#   ) %>%
#   mutate(percentage = n / sum(n) * 100) %>%
#   arrange(desc(n))

### Orofacial clefts ----

# # Find studies on congenital malformation
# malformation_daly <- daly_subset %>%
#   separate_rows(category_gen_pathology, sep = ";") %>%
#   mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>%  # remove spaces
#   filter(str_to_lower(category_gen_pathology) == "congenital malformations") %>%
#   select(covidence_number, title_3, country, general_category_of_pathology, category_gen_pathology,
#          specific_pathology, category_specific_pathology, types_of_surgical_procedure_performed, 
#          category_surgery, disability_adjusted_life_years_dal_ys)
# 
# # Find studies on clefts
# cleft_daly <- daly_subset %>% 
#   separate_rows(category_specific_pathology, sep = ";") %>% 
#   mutate(category_specific_pathology = str_trim(category_specific_pathology)) %>% 
#   filter(str_to_lower(category_specific_pathology) == "cleft lip & palate") %>% 
#   select(covidence_number, title_3, country, general_category_of_pathology, category_gen_pathology,
#          specific_pathology, category_specific_pathology, types_of_surgical_procedure_performed, 
#          category_surgery, disability_adjusted_life_years_dal_ys)

### Burns ----

# # Find studies on burns
# burn_daly <- daly_subset %>% 
#   separate_rows(category_gen_pathology, sep = ";") %>% 
#   mutate(category_gen_pathology = str_trim(category_gen_pathology)) %>% 
#   filter(str_to_lower(category_gen_pathology) == "burns") %>% 
#   select(covidence_number, study_id, title_3, country, general_category_of_pathology, 
#          specific_pathology, disability_adjusted_life_years_dal_ys)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Cost Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cost_1 <- categorized_final %>% 
#   filter(!is.na(cost_to_individuals_cost_in_usd_or_other_currency))
# 
# cost_2 <- categorized_final %>% 
#   filter(!is.na(cost_to_government_cost_in_usd_or_other_currency))
# 
# cost_subset <- full_join(cost_1, cost_2) %>% 
#   select(covidence_number, study_id, title_3, country, main_study_findings, 
#          category_gen_pathology, category_specific_pathology, 
#          cost_to_government_cost_in_usd_or_other_currency, 
#          cost_to_individuals_cost_in_usd_or_other_currency)

# cost_subset <- cost_subset %>% 
#   select(covidence_number, study_id, title_3, country,main_study_findings, cost_to_government_cost_in_usd_or_other_currency,
#          cost_to_individuals_cost_in_usd_or_other_currency,
#          category_gen_pathology, category_specific_pathology) %>% 
#   arrange(category_gen_pathology) %>% 
#   filter(covidence_number == 212) %>% 
#   mutate(findings_cost = paste0(main_study_findings, "; ", cost_to_government_cost_in_usd_or_other_currency,
#                                  "; ", cost_to_individuals_cost_in_usd_or_other_currency)) %>% 
#   pull(findings_cost) %>% 
#   print()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Surgical Capacity Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sc <- categorized_final %>%
#   filter(if_any(34:83, ~ !is.na(.) & . != "")) %>%
#   select(covidence_number, study_id, title_3, country, main_study_findings, other_findings,
#          category_gen_pathology,category_specific_pathology, other_findings, 34:83) 

# # Filter for infrastructure variables only
# infrastructure <- sc %>% filter(if_any(28:51, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Bed capacity
# infrastructure %>% 
#   filter(if_any(28:30, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Operating rooms
# infrastructure %>% 
#   filter(if_any(31:33, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Burn Units
# infrastructure %>% 
#   filter(if_any(34:36, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Anaesthesia 
# infrastructure %>% 
#   filter(if_any(37:39, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Ventilators 
# infrastructure %>% 
#   filter(if_any(40:42, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Blood bank 
# infrastructure %>% 
#   filter(if_any(43:45, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Lab 
# infrastructure %>% 
#   filter(if_any(46:48, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Imaging
# infrastructure %>% 
#   filter(if_any(49:51, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Sterilization
# infrastructure %>% 
#   filter(if_any(52:54, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# # Filter for personnel variables only
# personnel <- sc %>% filter(if_any(15:22, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Surgeons
# personnel %>% 
#   filter(if_any(15:16, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Anasthesists
# personnel %>% 
#   filter(if_any(17:18, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Nurses
# personnel %>% 
#   filter(if_any(19:20, ~ !is.na(.) & . != "")) %>% 
#   nrow()
# 
# ## Other healthcare provider
# personnel %>% 
#   filter(if_any(21:22, ~ !is.na(.) & . != "")) %>% 
#   nrow()

# # Identify studies including Assessment Tools in abstract
# assessment_tools_used <- included_studies %>%
#   filter(str_detect(abstract, regex("sosas|who surgical assessment tool| who tool| pressco| pedipipes| pipes| assessment tool", 
#                                     ignore_case = TRUE)))
# 
# # Classify the studies by tool used
# categorized_assessment_tool <- assessment_tools_used %>%
#   mutate(tool_used = case_when(
#     str_detect(abstract, regex("SOSAS", ignore_case = TRUE)) ~ "SOSAS",
#     str_detect(abstract, regex("WHO Surgical Assessment Tool|WHO SAT| WHO tool | assessment tool", ignore_case = TRUE)) ~ "WHO SAT",
#     str_detect(abstract, regex("PediPIPES", ignore_case = TRUE)) ~ "PediPIPES",
#     str_detect(abstract, regex("PRESSCO", ignore_case = TRUE)) ~ "PRESSCO",
#     TRUE ~ NA_character_   # if none matched
#   ))

# Manual search categories
assessment_tool_studies <- list(
  # SOSAS studies (duplicates removed)
  sosas = c(9571, 8872, 1267, 1698, 2069, 2216, 2217, 2219, 3079, 8841),
  
  # PediPIPES studies
  pedipipes = c(3108),

  # WHO SAT studies
  who_sat = c(8555, 586),

  # WHO EESC (not one of your four, but included separately if you want)
  who_eesc = c(6965, 2689, 4656),
  
  # WHO Surgery Standards
  who_standard = c(8992)
)

categorized_assessment_tool <- categorized_final %>% 
  mutate(
    tool_used = case_when(
      # covidence_number %in% assessment_tool_studies$sosas     ~ "SOSAS", # Not really for capacity but more need
      covidence_number %in% assessment_tool_studies$pedipipes ~ "PediPIPES",
      covidence_number %in% assessment_tool_studies$who_sat   ~ "WHO SAT",
      covidence_number %in% assessment_tool_studies$who_eesc  ~ "WHO EESC",
      covidence_number %in% assessment_tool_studies$who_standard ~ "WHO Standard",
      TRUE                                                     ~ NA_character_  # or "Other"
    ),
    tool_used = factor(tool_used, levels = c("PediPIPES", "WHO SAT", "WHO EESC", "WHO Standard"))
  ) %>% 
  filter(!is.na(tool_used)) %>% 
  select(study_id, title_3, country, tool_used)%>% 
  arrange(tool_used)

# write_csv(df, here("outputs/assessment_tools_dataset.csv"))

# Clean dataset
categorized_assessment_tool <- categorized_assessment_tool %>%
  # Replace commas with semicolons across all character columns
  mutate(country = str_replace_all(country, ",", ";")) %>%
  # Reformat study_id
  mutate(study_id = str_replace(study_id, "(\\w+)\\s+(\\d{4})", "\\1, \\2"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create study characteristics ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Study Design ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create mapping list
categories_study_design <- list(
  
  # 1) Literature reviews (all review types, incl. generic "review(s)")
  "Literature review" = c(
    "systematic review",
    "meta[- ]?analysis",
    "scoping review",
    "narrative review",
    "nonsystematic literature review",
    "\\bliterature review\\b",
    "review article",
    "scientific review",
    "\\breviews?\\b",
    "study and review",
    "systematic analysis"
  ),
  
  # 2) Economic evaluation
  "Economic evaluation" = c(
    "cost",                         # cost-effectiveness / cost-of-illness / cost-DALY
    "\\bdaly\\b",
    "burden of disease",
    "economic model|economic modeling|macroeconomic",
    "step[- ]?down accounting",
    "economic evaluation|economic analysis"
  ),
  
  # 3) Case-control
  "Case control study" = c(
    "case[- ]?control"
  ),
  
  # 4) Mixed-methods (wins over cohort/cross-sectional/qualitative)
  "Mixed-methods" = c(
    "mixed[\\-\\s]*methods?",                 # mixed-methods / mixed methods / mixed- methods
    "mixed[\\-\\s]*method.*(survey|interview|cohort|observational|case study|needs assessment)"
  ),
  
  # 5) Cohort
  "Cohort study" = c(
    "\\bcohort\\b",
    "follow[- ]?up",
    "surveillance study|surveillance.*cohort",
    "registry[- ]?based.*cohort|registry[- ]?based surveillance",
    # observational with explicit direction
    "\\bprospective\\b.*observational",
    "\\bretrospective\\b.*observational",
    "observational.*\\bprospective\\b",
    "observational.*\\bretrospective\\b",
    "descriptive cohort",
    "before[- ]?and[- ]?after",
    "pre[- ]?test\\/?post[- ]?test",
    "pre[- ]?post",
    "intervention study"
  ),
  
  # 6) Cross-sectional (surveys, prevalence, tools)
  "Cross-sectional study" = c(
    "cross.?sectional",
    "point prevalence",
    "\\bsurvey\\b|questionnaire|assessment",
    "nation(wide|al).*survey|countrywide.*survey",
    "population[- ]?based.*survey|household survey|population[- ]?based cross[- ]?sectional household survey",
    "health facility survey|hospital[- ]?based.*survey|institution[- ]?based.*survey|in[- ]?hospital survey",
    "cluster.*(cross.?sectional|survey)",
    "serial cross.?sectional",
    "pipes|who[- ]?iatsic|who tool for situational analysis|surgeons overseas"
  ),
  
  # 7) Qualitative
  "Qualitative research" = c(
    "\\bqualitative\\b",
    "semi[- ]?structured interview|interviews?|focus group|thematic analysis"
  ),
  
  # 8) Case series (incl. chart/record/database reviews)
  "Case series" = c(
    "case‐series",
    "case[ -]?series",
    "retrospective chart study",
    "chart review",
    "record review",
    "database review",
    "medical (record|documentation) review",
    "case review",
    "clinical database"
  ),
  
  # 9) Descriptive (explicitly descriptive designs)
  "Descriptive" = c(
    "\\bdescriptive\\b",
    "descriptive analysis",
    "descriptive study",
    "descriptive observational",
    "descriptive report",
    "descriptive.*hospital[- ]?based",
    "descriptive.*multicenter|descriptive.*multi[- ]?center"
  ),
  
  # 10) Observational (unspecified / registry-/database-based / generic “study/analysis”)
  "Observational study" = c(
    "\\bobservational study\\b",
    # generic prospective/retrospective "study" without cohort/cross-sectional markers
    "^\\s*other:.*\\bprospective study\\b",
    "^\\s*other:.*\\bretrospective study\\b",
    # generic analyses
    "data analysis|analysis of|retrospective analysis|prospective analysis",
    # registry/database phrasing not otherwise captured
    "registry|trauma registry|prospectively collected .*registry data|clinical database",
    # gis/comparative phrasing
    "\\bgis\\b|geospatial|comparative study",
    "retrospective hospital based study",
    "retrospective and hospital-based study"
  ),
  
  # 11) Text & opinion
  "Text and opinion" = c(
    "commentary|perspective",
    "policy forum|expert consensus",
    "communication report",
    "text and opinion",
    "activity report"
  )
)

# Priority (first match wins)
priority_study_design <- c(
  "Economic evaluation",
  "Case control study",
  "Mixed-methods",
  "Case series",
  "Cohort study",
  "Cross-sectional study",
  "Qualitative research",
  "Literature review",
  "Descriptive",
  "Observational study",
  "Text and opinion"
  # "Other" is fallback
)

# Categorization function
categorize_study_design <- function(text, categories, priority_order = NULL) {
  if (is.na(text) || str_trim(text) == "") return(NA_character_)
  s <- str_to_lower(text)
  order_names <- if (is.null(priority_order)) names(categories) else priority_order
  
  for (nm in order_names) {
    pats <- categories[[nm]]
    re <- paste0("(", paste(pats, collapse = "|"), ")")
    if (str_detect(s, regex(re, ignore_case = TRUE))) return(nm)
  }
  "Other"
}

# Example usage
categorized_final <- categorized_final %>%
  mutate(category_study_design = sapply(
    study_design,
    categorize_study_design,
    categories = categories_study_design,
    priority_order = priority_study_design
   ))

dplyr::count(categorized_final, category_study_design, sort = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Source of Data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mapping Lists

## PRIMARY subtypes
primary_patterns <- list(
  "Survey/Questionnaire/Interview" = c(
    "sosas", "soas",                                   # Surgeons Overseas Assessment
    "household.*survey", "population[- ]?based.*(survey|interview)",
    "\\bsurvey\\b", "questionnaire", "responses? from .*specialists",
    "interview", "face[- ]?to[- ]?face", "self[- ]?administered", "web[- ]?based",
    "proforma(?!.*data)", "structured proforma", "data entered in proforma", "survey"
  ),
  "Clinical exam/testing" = c(
    "physical examination|clinical (evaluation|examination)|wound assessments?",
    "laboratory|microbiolog(ical|y)|radiograph|computed tomography|ct angiograph|sonograph|anaesthetic records"
  ),
  "Facility assessment (PIPES/IMEESC/GECT)" = c(
    "pipes(?!.*database)", "imeesc", "essential trauma care",
    "infrastructure evaluation", "direct inspection", "site assessment", "facility assessment",
    "structured interviews? with staff", "guidelines for essential trauma care; structured interviews; direct inspection"
  ),
  "Author-maintained log/form" = c(
    "surgical log recorded by the lead author", "data collected by author", "primary experience",
    "study questionnaire", "pro forma questionnaire", "flowchart sheet"
  ),
  "Primary medical-record abstraction" = c(
    # cases explicitly labeled primary but abstracted from records
    "primary data collection.*(medical records|chart|case[- ]?notes|patient files|register|operative report)"
  ),
  "Primary (unspecified)" = c("^\\s*primary data collection\\b|^\\s*primary data\\b")
)

## SECONDARY subtypes
secondary_patterns <- list(
  "Medical records" = c(
    "medical records?|patient files?|case[- ]?notes?|charts?|clinical notes?|folders?",
    "admission registers?|discharge (summary|summaries)",
    "operation (notes?|register)", "operating (room|theatre) (register|log ?book|logs?)",
    "records? department", "hospital records?", "consultation (logs?|registers?)",
    "nurses[’']? reports?|ward (round book|logs?)", "emergency unit records?", "rehabilitation team.*records?",
    "records of patients", "clinical records", "records of operations", "patient records", 
    "records and patient information", "case files"
  ),
  "Registry/Database/Logbook" = c(
    "registry|registries", "database|databases", "logbook|logbooks",
    "operative database|operating room case logs?|surgical case logbook",
    "burn (registry|database|surveillance)", "trauma registry", "pediatric surgery database",
    "smile train express|\\bstx\\b", "psr system", "ward[- ]?based clinical database",
    "hospital information management system|\\bhims\\b", "recorded pediatric surgery database",
    "kamuzu central hospital burn", "\\bkch\\b burn (registry|database|surveillance)", "logs of procedures for orofacial cleft"
  ),
  "Administrative/MOH/HDSS/WHO-SARA/SAT" = c(
    "ministry of health|\\bmoh\\b", "aggregate .*hospital statistics", "policy database",
    "health and demographic surveillance site|\\bhdss\\b",
    "who (sara|sat) (survey|database)|who sat database|who sara surveys"
  ),
  "Literature databases" = c(
    "pubmed|medline|embase|scopus|cochrane|google scholar|cinahl|lilacs|scielo|web of science|science direct",
    "african index medicus|ajol|pais international|global health|imme?mr|imsear|wholis|wprim|otseeker"
  ),
  "GBD/IHME/WHO datasets" = c(
    "global burden of disease|\\bgbd\\b|ihme",
    "who international clinical trials registry platform", "who data"
  ),
  "Program/NGO reports" = c(
    "msf|m[ée]decins sans fronti[èe]res|ocb",
    "usaid|amref|tropical health and education trust|\\bthet\\b|sentinelles foundation",
    "suppliers|product catalog|office of management and budget|ngo sources|program reports"
  ),
  "External digital/app data" = c(
    "ma3route app", "online learning platform"
  ),
  "Secondary (unspecified)" = c("^\\s*secondary data( collection)?\\b|existing data and literature|data sources included databases")
)

# Mapping Lists

## PRIMARY subtypes
primary_patterns <- list(
  "Survey/Questionnaire/Interview" = c(
    "sosas", "soas",                                   # Surgeons Overseas Assessment
    "household.*survey", "population[- ]?based.*(survey|interview)",
    "\\bsurvey\\b", "questionnaire", "responses? from .*specialists",
    "interview", "face[- ]?to[- ]?face", "self[- ]?administered", "web[- ]?based",
    "proforma(?!.*data)", "structured proforma", "data entered in proforma", "survey"
  ),
  "Clinical exam/testing" = c(
    "physical examination|clinical (evaluation|examination)|wound assessments?",
    "laboratory|microbiolog(ical|y)|radiograph|computed tomography|ct angiograph|sonograph|anaesthetic records"
  ),
  "Facility assessment (PIPES/IMEESC/GECT)" = c(
    "pipes(?!.*database)", "imeesc", "essential trauma care",
    "infrastructure evaluation", "direct inspection", "site assessment", "facility assessment",
    "structured interviews? with staff", "guidelines for essential trauma care; structured interviews; direct inspection"
  ),
  "Author-maintained log/form" = c(
    "surgical log recorded by the lead author", "data collected by author", "primary experience",
    "study questionnaire", "pro forma questionnaire", "flowchart sheet"
  ),
  "Primary medical-record abstraction" = c(
    # cases explicitly labeled primary but abstracted from records
    "primary data collection.*(medical records|chart|case[- ]?notes|patient files|register|operative report)"
  ),
  "Primary (unspecified)" = c("^\\s*primary data collection\\b|^\\s*primary data\\b")
)

## SECONDARY subtypes
secondary_patterns <- list(
  "Medical records" = c(
    "medical records?|patient files?|case[- ]?notes?|charts?|clinical notes?|folders?",
    "admission registers?|discharge (summary|summaries)",
    "operation (notes?|register)", "operating (room|theatre) (register|log ?book|logs?)",
    "records? department", "hospital records?", "consultation (logs?|registers?)",
    "nurses[’']? reports?|ward (round book|logs?)", "emergency unit records?", "rehabilitation team.*records?",
    "records of patients", "clinical records", "records of operations", "patient records", 
    "records and patient information", "case files"
  ),
  "Registry/Database/Logbook" = c(
    "registry|registries", "database|databases", "logbook|logbooks",
    "operative database|operating room case logs?|surgical case logbook",
    "burn (registry|database|surveillance)", "trauma registry", "pediatric surgery database",
    "smile train express|\\bstx\\b", "psr system", "ward[- ]?based clinical database",
    "hospital information management system|\\bhims\\b", "recorded pediatric surgery database",
    "kamuzu central hospital burn", "\\bkch\\b burn (registry|database|surveillance)", "logs of procedures for orofacial cleft"
  ),
  "Administrative/MOH/HDSS/WHO-SARA/SAT" = c(
    "ministry of health|\\bmoh\\b", "aggregate .*hospital statistics", "policy database",
    "health and demographic surveillance site|\\bhdss\\b",
    "who (sara|sat) (survey|database)|who sat database|who sara surveys"
  ),
  "Literature databases" = c(
    "pubmed|medline|embase|scopus|cochrane|google scholar|cinahl|lilacs|scielo|web of science|science direct",
    "african index medicus|ajol|pais international|global health|imme?mr|imsear|wholis|wprim|otseeker"
  ),
  "GBD/IHME/WHO datasets" = c(
    "global burden of disease|\\bgbd\\b|ihme",
    "who international clinical trials registry platform", "who data"
  ),
  "Program/NGO reports" = c(
    "msf|m[ée]decins sans fronti[èe]res|ocb",
    "usaid|amref|tropical health and education trust|\\bthet\\b|sentinelles foundation",
    "suppliers|product catalog|office of management and budget|ngo sources|program reports"
  ),
  "External digital/app data" = c(
    "ma3route app", "online learning platform"
  ),
  "Secondary (unspecified)" = c("^\\s*secondary data( collection)?\\b|existing data and literature|data sources included databases")
)

# Helper to test a text against a pattern vector
match_any <- function(s, pats) {
  if (is.na(s) || str_trim(s) == "") return(FALSE)
  any(str_detect(s, regex(paste(pats, collapse = "|"), ignore_case = TRUE)), na.rm = TRUE)
}

categorize_sources <- function(df, col = "sources_of_data") {
  s_raw <- df[[col]]                 # keep true NAs
  s <- tolower(s_raw)                # tolower() preserves NA
  
  n <- length(s)
  primary_hits   <- vector("list", n)
  secondary_hits <- vector("list", n)
  
  # Collect hits row-by-row (NA-safe)
  for (i in seq_len(n)) {
    x <- s[i]
    if (is.na(x) || str_trim(x) == "") {
      primary_hits[[i]]   <- character(0)
      secondary_hits[[i]] <- character(0)
      next
    }
    primary_hits[[i]] <- names(primary_patterns)[
      vapply(primary_patterns,  function(p) match_any(x, p), logical(1))
    ]
    secondary_hits[[i]] <- names(secondary_patterns)[
      vapply(secondary_patterns, function(p) match_any(x, p), logical(1))
    ]
  }
  
  # Decide type (preserve NA when input is NA/blank)
  source_type <- mapply(function(ph, sh, x) {
    if (is.na(x) || str_trim(x) == "") return(NA_character_)
    has_p <- length(ph) > 0
    has_s <- length(sh) > 0
    if (has_p && has_s) "Mixed"
    else if (has_p)     "Primary"
    else if (has_s)     "Secondary"
    else if (str_detect(x, "\\bna\\b|unspecified|no specific mention")) "Unclear"
    else "Unclear"
  }, primary_hits, secondary_hits, s, USE.NAMES = FALSE)
  
  # Details
  primary_detail    <- vapply(primary_hits,   function(v) if (length(v)) paste(unique(v), collapse = "; ") else NA_character_, character(1))
  secondary_detail  <- vapply(secondary_hits, function(v) if (length(v)) paste(unique(v), collapse = "; ") else NA_character_, character(1))
  
  source_detail <- mapply(function(t, p, q) {
    if (is.na(t)) return(NA_character_)
    if (t == "Primary")   p
    else if (t == "Secondary") q
    else if (t == "Mixed") paste0("Primary: ", p, "&", "Secondary: ", q)
    else NA_character_
  }, source_type, primary_detail, secondary_detail, USE.NAMES = FALSE)
  
  df %>%
    mutate(
      source_type      = source_type,       # NA inputs -> NA here
      source_detail    = source_detail,
      primary_source   = primary_detail,
      secondary_source = secondary_detail
    )
}

# Usage
categorized_final <- categorize_sources(categorized_final, col = "sources_of_data")

# # Inspect rows still unclear:
# test <- df %>% 
#   select(sources_of_data, source_type, source_detail, primary_source, secondary_source)
# dplyr::count(categorized_final, source_type, sort = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Study Setting ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Patterns (referral merged into tertiary; non-facility collapsed)
facility_category_patterns <- list(
  # Special contexts
  "Field/military hospital" = c(
    "level\\s*2\\s*medical\\s+treatment\\s+facility",
    "\\bminusma\\b", "\\bchn\\s*r2\\b", "chinese\\s+level\\s*2", "\\bcamp\\b"
  ),
  "NGO/faith/humanitarian hospital" = c(
    "msf|m[ée]decins\\s+sans\\s+fronti[èe]res|\\bocb\\b",
    "africa\\s+mercy|hospital\\s+ship|operation\\s+of\\s+hope", "Phebe Hospital",
    "partners\\s+in\\s+health|inshuti\\s+mu\\s+buzima",
    "smile\\s*train(?!.*partner\\s+hospitals)|smile\\s*train.*outreach|smile\\s*train\\s+partner\\s+hospitals",
    "short[- ]?term\\s+medical\\s+missions?|\\bstmm(s)?\\b", "faith-based",
    "foundation(?!.*university)",                              # e.g., CFDF
    "ngo(\\s|\\-)?(hospital|program)|not[- ]?for[- ]?profit",
    "Don Orione", "MyungSung Christian Medical", "Yung Sung Christian Medical Centre"
  ),
  
  # Mixed levels
  "Mixed facility levels" = c(
    "three\\s+levels.*community.*health\\s*centr(e|er).*district\\s+hospital",
    "primary\\s+health\\s+centres?.*secondary\\s+health\\s+care",
    "two\\s+tertiary.*four\\s+district",
    "clinics?,?\\s+small\\s+hospitals?,?\\s+and\\s+large\\s+hospitals?",
    "multiple\\s+health\\s+facilities",
    "district\\s+and\\s+regional\\s+hospitals",
    "nationally\\s+representative\\s+health\\s+facility\\s+surveys",
    "all\\s+hospitals",                                   # e.g., “All hospitals … in Zambia”
    "all\\s+\\d+\\s+surgically\\s+equipped\\s+hospitals",
    "primary.*secondary|secondary.*tertiary|primary.*tertiary",
    "two\\s+tertiary\\s+hospitals;?\\s+and\\s+four\\s+district\\s+hospitals",
    "\\b\\d+\\s+health\\s+facilities\\b",
    "urban\\s+and\\s+rural\\s+locations",
    "district\\s+and\\s+referral\\s+hospitals",
    "facilities\\s+performing\\s+surgery"
  ),
  
  # Tertiary / referral (merged)
  "Tertiary/referral hospital" = c(
    "\\btertiary\\b|teaching\\s+hospital",
    "University",
    "university(\\s+teaching)?\\s+hospital|university\\s+college\\s+hospital|university\\s+hospital\\s+centre|university\\s+clinics?",
    "central\\s+hospital|national\\s+hospital",
    "specialist(\\s+teaching)?\\s+hospital", "Addis Ababa Burn, Emergency, and Trauma (AaBET) hospital",
    "regional\\s+referral\\s+hospital", "Hospital Central Maputo",
    "speciali[sz]ed\\s+hospital|comprehensive\\s+speciali[sz]ed\\s+hospital",
    "children'?s\\s+hospital|p(ae)?diatric\\s+.*hospital",
    "trauma\\s+cent(re|er)", "orthopedic institute",
    "first-referral health facilities", "Harare Children’s Hospital",
    "plastic surgery centers", "Plastic surgery division",
    "pediatric surgery department", "National Orthopaedic Hospital", "Yekatit 12 Hospital",
    "Akonolinga Buruli Center", "Khartoum Teaching Dental Hospital", "princess marina hospital",
    # units/services that imply tertiary
    "burns?\\s+unit(s)?|burn\\s+care\\s+unit(s)?|burns?\\s+and\\s+plastic(s)?(\\s+surgery)?\\s+unit(s)?",
    "surgical\\s+oncology\\s+unit(s)?",
    "specialist\\s+cleft(\\s+lip\\s+and\\s+palate)?\\s+unit(s)?|cleft\\s+hospital(s)?",
    # referral merged here
    "\\breferral\\s+(hospital|centre|center)\\b|national\\s+referral|primary\\s+trauma\\s+centre|national\\s+trauma",
    # exemplar names
    "\\bchuk\\b|\\bchub\\b|korle\\s+bu|komfo\\s+anokye|bugando\\s+medical\\s+centre|\\bkcmc\\b|kijabe\\s+hospital|kenyatta\\s+national\\s+hospital|queen\\s+elizabeth\\s+central\\s+hospital|kamuzu\\s+central\\s+hospital|moi\\s+teaching\\s+and\\s+referral\\s+hospital",
    # departments within tertiary hospitals (if 'hospital' not repeated)
    "department\\s+of\\s+.*\\b(hospital)\\b"
  ),
  
  # Secondary hospitals
  "Secondary hospital" = c(
    "district[-\\s]*(level)?\\s+hospitals?",                   # district-level, district hospitals
    "provincial(\\s+general)?\\s+hospitals?|county\\s+hospitals?", "secondary level",
    "regional\\s+hospitals?(?!\\s*for)|\\blevel\\s*v\\b.*hospitals?",
    "\\bgeneral\\s+hospitals?\\b",
    "mission(ary)?\\s+hospitals?|faith[- ]?based\\s+hospitals?|adventist\\s+hospitals?",
    "private\\s+hospitals?",
    "government\\s+hospitals?(?!.*teaching)",
    "\\bsecondary\\s+(level\\s+)?hospitals?\\b",
    "public\\s+hospitals?",                                    # e.g., 8 public hospitals
    "rural\\s+hospitals?"
  ),
  
  # Primary care
  "Primary care facility" = c(
    "health\\s*centr(e|er)s?\\b|\\bhc(s)?\\b(?!\\s*(iv|iii|ii))",  # HCs, health centres
    "\\bhc\\s*(iv|iii|ii)\\b",                                     # keep HC levels too
    "\\bclinic(s)?\\b", "primary hospitals",
    "primary\\s+health\\s+(centr(e|er)|facility|care)",
    "health\\s+post",
    "level\\s*1\\s+hospital|first\\s+level\\s+hospital"             # LMIC nomenclature
  ),
  
  # Unspecified facility words (catch-all)
  "Unspecified facilities" = c(
    "hospitals?\\b",                                               # plural/singular
    "medical\\s+cent(re|er)s?",                                    # medical center/centre
    "\\b(institute|institut)\\b",                                  # e.g., orthopedic institute
    "\\bunit(s)?\\b(?!.*burn)",                                    # generic units if not matched above
    "healthcare\\s+facilit(y|ies)",
    "pediatric surgical OR"
  ),
  
  # Non-facility
  "Non-healthcare facility" = c(
    # population/household/areas/administrative
    "nationwide|across\\s+[a-z]+|population[- ]?based\\s+survey|household\\s+survey",
    "\\bdistricts?\\b|\\blga(s)?\\b|local\\s+government\\s+area",
    "health\\s+and\\s+demographic\\s+surveillance\\s+site|\\bhdss\\b|\\bdss\\b",
    "enumeration\\s+areas?",
    "\\bzone(s)?\\b|administrative\\s+zones?",
    "\\bvillage(s)?\\b|\\bidp\\b",
    # programs / events / regions / acronyms
    "programs?|preventive|preventative",
    "Nationally representative surveys",
    "rural Hararghe", "community", "world bank",
    "policy database",
    "congress|conference|symposium|meeting",
    "\\bssa\\b|sub-?saharan\\s+africa|low[-\\s]and[-\\s]lower\\s+middle[-\\s]income\\s+countries",
    "\\bglobal\\b|\\basia\\b|\\bafrica\\b(?!.*hospital)|multiple\\s+countries",
    # country/region-wide non-facility listings
    "\\b(\\d+\\s+)?district(s)?\\s+in\\b|\\b(\\d+\\s+)?district(s)?\\s+of\\b"
  )
)

# Priority
priority_facility_categories <- c(
  "Field/military hospital",
  "NGO/faith/humanitarian hospital",
  "Mixed facility levels",
  "Tertiary/referral hospital",
  "Secondary hospital",
  "Primary care facility",
  "Unspecified facilities",
  "Non-healthcare facility"
)

# Matcher function
match_any <- function(s, pats) {
  if (is.na(s) || str_trim(s) == "") return(FALSE)
  if (is.null(pats) || length(pats) == 0) return(FALSE)
  pat <- paste(pats, collapse = "|")
  if (identical(pat, "") || nchar(pat) == 0) return(FALSE)
  str_detect(s, regex(pat, ignore_case = TRUE))
}

# Function
categorize_setting_level <- function(df, col = "study_setting_location") {
  x_raw <- df[[col]]
  x <- tolower(x_raw)  # preserves NA
  
  internal_cat <- vapply(seq_along(x), function(i) {
    s <- x[i]
    if (is.na(s) || str_trim(s) == "") return(NA_character_)
    for (nm in priority_facility_categories) {
      if (match_any(s, facility_category_patterns[[nm]])) return(nm)
    }
    "Unclear"
  }, character(1))
  
  # setting_level <- ifelse(
  #   is.na(internal_cat), NA_character_,
  #   dplyr::case_when(
  #     internal_cat == "Primary care facility"                   ~ "Primary",
  #     internal_cat == "Secondary hospital"                      ~ "Secondary",
  #     internal_cat == "Tertiary/referral hospital"              ~ "Tertiary",
  #     internal_cat == "Non-facility (population/policy/global)" ~ "Non-facility",
  #     internal_cat == "Mixed facility levels"                   ~ "Mixed levels",
  #     internal_cat %in% c("Field/military hospital",
  #                         "NGO/faith/humanitarian hospital")  ~ "Special facility",
  #     internal_cat == "Unspecified facilities" ~ "Healthcare (unspecified)",
  #     TRUE                                                      ~ "Unclear"
  #   )
  # )
  
  df %>% mutate(setting_level = internal_cat)
}

# Usage
categorized_final <- categorize_setting_level(categorized_final, "study_setting_location")
# dplyr::count(categorized_final, setting_level, sort = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Population ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# View variables
# categorized_final %>% 
#   select(covidence_number, title_3, study_design, study_setting_location, 
#          author_affiliation, country, sources_of_data, age_group_demographics, 
#          category_gen_pathology, category_specific_pathology, category_surgery) %>% 
#   select(age_group_demographics) %>% 
#   unique()

# vectorized: val and unit can be vectors
to_years <- function(val, unit) {
  unit <- tolower(ifelse(is.na(unit) | unit == "", "years", unit))
  v <- as.numeric(val)
  v <- ifelse(str_detect(unit, "^day|^d"),    v / 365,
              ifelse(str_detect(unit, "^week|^w"),   v / 52,
                     ifelse(str_detect(unit, "^month|^mo|^m$"), v / 12, v)))
  v
}

# min/max years from any numbers in the string (handles days/weeks/months/years)
range_years <- function(x) {
  m <- str_match_all(x, "(>=|≤|<=|≥|>|<)?\\s*(\\d+(?:\\.\\d+)?)\\s*(day|days|d|week|weeks|w|month|months|mo|m|year|years|y)?")[[1]]
  if (nrow(m) == 0) return(c(NA_real_, NA_real_))
  yrs <- to_years(m[,3], m[,4])
  c(min(yrs, na.rm = TRUE), max(yrs, na.rm = TRUE))
}

# classify one string into Pediatric / Adult / All ages / Unspecified
classify_one_age <- function(s) {
  if (is.na(s) || str_trim(s) == "") return(NA_character_)
  x <- str_to_lower(s) |> str_squish() |> str_replace_all("[–—−]", "-")
  
  ped_hint   <- str_detect(x, "neonat|new\\s?born|p(ae)?diatr|child(ren)?|infant|
                           <=?\\s*18|≤\\s*18|under\\s*18|below\\s*18|<=?\\s*15|
                           ≤\\s*15|under\\s*15|pediatric|pediatrics")
  adult_hint <- str_detect(x, "\\badult(s)?\\b|elderly|geriatric|skeletal(ly)?\\s*mature|specialists?|surgeons?|>=?\\s*18|≥\\s*18|\\b18\\s*(and\\s*over|years?\\s*and\\s*above)|>=?\\s*50|≥\\s*50")
  all_hint   <- str_detect(x, "all\\s+ages?|all\\s+age\\s+(groups?|ranges?)|qll\\s+age")
  
  rng <- range_years(x); miny <- rng[1]; maxy <- rng[2]
  spans <- (!is.na(miny) && !is.na(maxy) && miny < 18 && maxy >= 18)
  
  if (all_hint || spans)                       return("All ages")
  if (ped_hint   || (!is.na(maxy) && maxy <= 18)) return("Pediatric")
  if (adult_hint || (!is.na(miny) && miny >= 18)) return("Adult")
  if (str_detect(x, "\\bunspecified\\b|^na$"))  return("Unspecified")
  "Unspecified"
}

# vectorized wrapper: adds one column 'age_group'
classify_age_group <- function(df, col = "age_group_demographics") {
  v <- df[[col]]
  out <- vapply(v, classify_one_age, character(1))
  df %>% mutate(age_group = factor(out, levels = c("Pediatric","Adult","All ages","Unspecified")))
}

# Usage
categorized_final <- classify_age_group(categorized_final, "age_group_demographics")

# dplyr::count(age_groups, age_group, sort = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Affiliation ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# View variables
# categorized_final %>% 
#   select(covidence_number, title_3, study_design, study_setting_location, 
#          author_affiliation, country, sources_of_data, age_group_demographics, 
#          category_gen_pathology, category_specific_pathology, category_surgery) %>% 
#   select(author_affiliation) %>% 
#   unique()

# Category List
categories_affiliation <- list(
  "African Country" = c(
    # Countries (+ common variants)
    "algeria","angola","benin","botswana","burkina\\s*faso","burundi",
    "cabo\\s*verde|cape\\s*verde","cameroon","central\\s+african\\s+republic|\\bcar\\b",
    "\\bchad\\b","comoros","democratic\\s+republic\\s+of\\s+the\\s+congo|\\bdrc\\b|congo\\s*\\(kinshasa\\)",
    "republic\\s+of\\s+the\\s+congo|congo\\s*\\(brazzaville\\)|\\bcongo\\b",
    "djibouti","egypt","equatorial\\s+guinea","eritrea","eswatini|swaziland",
    "ethiopia","gabon","gambia|the\\s+gambia","ghana","guinea\\-?bissau","\\bguinea\\b(?!-?bissau)",
    "c(o|ô|ô)te\\s*d[’']?ivoire|cote\\s*d[’']?ivoire|ivory\\s+coast",
    "kenya","lesotho","liberia","libya","madagascar","malawi","mali","mauritania","mauritius",
    "morocco","mozambique","namibia","niger(?!ia)","nigeria","rwanda",
    "s(ã|a)o\\s*tom(e|é)\\s*(and|&)\\s*pr(i|í)ncipe",
    "senegal","seychelles","sierra\\s*leone","somalia","south\\s+africa","south\\s+sudan",
    "sudan","tanzania","togo","tunisia","uganda","zambia","zimbabwe","western\\s+sahara",
    # Helpful African cities (when country not written)
    "lilongwe","blantyre","kigali","kampala","nairobi","dar\\s*es\\s*salaam",
    "mwanza","moshi","lusaka","harare","maputo","ouagadougou","bobo\\s*dioulasso",
    "accra","kumasi","abidjan","banjul","conakry","yaound[ée]","asmara",
    "addis\\s*ababa","luanda","ibadan","lagos","enugu","ado\\-?ekiti","jos",
    "makurdi","ile\\-?ife","sokoto","mbarara","kijabe", "rift valley", "kilimanjaro", 
    "State House Medical Centre", "osogbo", "Egerton University", "ebonyi", "alex ekwueme",
    "tamale teaching hospital", "korle-bu teaching hospital", "onabanjo", "komfo"
  ),
  
  "Western Country" = c(
    # US/Canada
    "united\\s*states(\\s*of\\s*america)?","\\busa\\b","\\bu\\.?s\\.?a?\\b",
    "canada",
    # UK & Ireland
    "united\\s*kingdom","\\buk\\b","england","scotland","wales","northern\\s*ireland",
    "great\\s*britain|\\bbritain\\b","ireland",
    # Western/Northern/Southern Europe
    "france","germany","netherlands|the\\s+netherlands|holland","belgium","switzerland","austria",
    "spain","portugal","italy","greece","denmark","norway","sweden","finland","iceland",
    "luxembourg","liechtenstein","andorra","monaco","san\\s*marino","vatican",
    # EU-CEE often grouped in 'Western' research contexts
    "poland","czech\\s*republic|czechia","slovakia","hungary","slovenia","croatia",
    "estonia","latvia","lithuania","malta","cyprus",
    # Oceania
    "australia","new\\s*zealand",
    # Helpful Western cities + States
    "boston","new\\s+york|nyc","philadelphia","chicago","seattle","san\\s+francisco","los\\s+angeles",
    "durham","chapel\\s+hill","charlottesville","vancouver","toronto","montr[eé]al",
    "london","birmingham","dundee","geneva","bergen","ume[aå]", "Minnesota", "North Carolina",
    "california", "Utah", "Iowa", "Virginia", "Harvard", "Sydney", "New Haven", "Dartmouth",
    "padova|padua","amsterdam","liverpool","oxford","cambridge","valencia","paris","clamart"
  ), 
  
  "Asian Country" = c(
    "china", "nepal", "pakistan", "aga khan", "seoul"
  )
)

categorize_affiliation <- function(affiliation_text, categories) {
  assign_side <- function(text) {
    if (is.na(text) || text == "") return(character(0))
    txt <- stringr::str_to_lower(text)
    
    names(categories)[sapply(categories, function(patterns) {
      any(sapply(patterns, function(pat) {
        pat_bounded <- if (grepl("^\\b.*\\b$", pat)) pat else paste0("\\b", pat, "\\b")
        stringr::str_detect(txt, stringr::regex(pat_bounded, ignore_case = TRUE))
      }))
    })]
  }
  
  hits <- assign_side(affiliation_text)
  
  if (length(hits) == 0) {
    if (is.na(affiliation_text) || affiliation_text == "") return(NA_character_)
    return("Other")
  }
  
  # consider only these 3 groups for combos, in a fixed order
  groups <- c("African Country", "Asian Country", "Western Country")
  present <- intersect(groups, hits)
  
  if (length(present) >= 2) {
    return(paste(present, collapse = " & "))
  }
  
  # only one group matched
  return(present %||% hits[1])
}

# With your object name:
categorized_final <- categorized_final %>%
  mutate(affiliation_region = sapply(author_affiliation, categorize_affiliation,
                                     categories = categories_affiliation))

# dplyr::count(affiliation, affiliation_region, sort = TRUE)
