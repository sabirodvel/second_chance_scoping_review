## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Extracted Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Date: 03/10/2025
## Updated: 08/11/2025

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
         )

# # View unique general pathologies values
# df <- categorized_studies_clean %>%
#   separate_rows(general_category_of_pathology, sep = ",|;") %>%
#   mutate(general_category_of_pathology = str_trim(general_category_of_pathology)) %>%
#   distinct(general_category_of_pathology)

# View unique specific pathologies values
df <- categorized_studies_clean %>% 
  # Remove digits-only rows and trim whitespace
  filter(!str_detect(specific_pathology, "^\\d+$")) %>%
  # Remove "(e.g." and all content in parentheses
  mutate(specific_pathology = str_remove_all(specific_pathology, "\\(.*?\\)")) %>%
  separate_rows(specific_pathology, sep = ",|;|\\band\\/or\\b|\\band\\b|\\n|\\r\\n") %>% 
  distinct(specific_pathology, .keep_all = TRUE) %>% 
  mutate(specific_pathology = str_trim(specific_pathology)) %>% 
  group_by(specific_pathology) %>% 
  count()

# # View unique cleaned pathologies
# unique_pathologies <- clean_pathologies$specific_pathology

# View unique surgeries values

#   # Specify rows to drop from surgical procedures (after distinct) EDIT AS NEEDED
# rows_to_drop <- c(29, 31:40, 43:124, 203, 215, 244, 257)

# df <- extracted_studies_clean %>% 
#   separate_rows(types_of_surgical_procedure_performed, sep = ",|;|&") %>%
#   mutate(sorted_surgeries = str_trim(types_of_surgical_procedure_performed)) %>% 
#   distinct(sorted_surgeries, .keep_all = TRUE) %>% # Ensure uniqueness before counting
#   select(sorted_surgeries) %>% 
#   filter(!row_number() %in% rows_to_drop) %>% 
#   mutate(sorted_surgeries = case_when(
#     row_number() == 1 ~ "modified millard; manchester repair; mullikens repair; furlows z plasty; tunrover flaps; unipedicle; mucoperiostal flaps",
#     row_number() == 32 ~ "flaps; thickness skin grafts",
#     row_number() == 95 ~ "cleft palate repair",
#     row_number() == 98 ~ "skin graft; debridement; amputation; muscle flap; local flap",
#     row_number() == 193 ~ "maxillo-mandibular fixation; debridement",
#     TRUE ~ sorted_surgeries)
#     ) %>% 
#   separate_rows(sorted_surgeries, sep = ",|;|&") %>% 
#   mutate(sorted_surgeries = str_trim(sorted_surgeries)) %>% 
#   distinct(sorted_surgeries) %>% 
#   count(sorted_surgeries, name = "frequency")  # Count occurrences properly
# 

# Identify NAs
pathology_missing <- categorized_studies_clean[is.na(categorized_studies_clean$general_category_of_pathology), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize general pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# UPDATED LIST (08/12/2025)
categories_gp <- list(
  "Burns" = c("burns?", "burn complications"),
  "Congenital Malformations" = c(
    "congenital malformations?", "congenital anomalies?", "congenital conditions?", 
    "congenital abnormalities?", "congenital deformities?", "acquired deformities?", 
    "craniofacial disorders?"
  ),
  "Trauma" = c("trauma", "injur(y|ies)", "wounds?", "chronic wounds?", "scars?"),
  "Infectious Conditions" = c(
    c("noma", "infectious?", "infection(s)?", "infectious diseases?", "neck infection", "buruli?", "bacteri?")
  ),
  "Neoplastic Conditions" = c(
    "tumor(s)?", "tumour(s)?", "cysts?", "neoplasms?", 
    "cancer( \\(tumor\\))?", "skin cancer", "neck cancer", "melanoma?"
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
  
  # # Then category_tags
  # category_tagss <- assign_categories(category_tags)
  # if (!is.na(category_tagss)) return(category_tagss)
  # 
  # # Then category_tiab
  # category_tiabb <- assign_categories(category_tiab)
  # if (!is.na(category_tiabb)) return(category_tiabb)
  
  # If none matched but some text exists, assign "Other"
  if (( !is.na(general_pathology) && general_pathology != "" ) ||
      ( !is.na(specific_pathology) && specific_pathology != "" ) #||
      # ( !is.na(category_tags) && category_tags != "" ) ||
      # ( !is.na(category_tiab) && category_tiab != "" )
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
                                         MoreArgs = list(categories = categories_gp))) #%>%
  # select(study_id, covidence_number, title_3, general_category_of_pathology, specific_pathology, category_gen_pathology, 
  #        category_tags, category_tiab, congenital_malformations, burns, trauma)

# Create columns for easy filtering
filtered_pathology <- categorized_general_pathology %>% 
  mutate(
    pathology_burns = case_when(
      str_detect(category_gen_pathology, "(?i)burns") ~ "Yes",
      TRUE ~ "No"
    ),
    pathology_trauma = case_when(
      str_detect(category_gen_pathology, "(?i)trauma") ~ "Yes",
      TRUE ~ "No"
    ),
    pathology_congenital_malformations = case_when(
      str_detect(category_gen_pathology, "(?i)congenital malformations") ~ "Yes",
      TRUE ~ "No"
    ),
    pathology_infection = case_when(
      str_detect(category_gen_pathology, "(?i)infectious") ~ "Yes",
      TRUE ~ "No"
    ),
    pathology_neoplasm = case_when(
      str_detect(category_gen_pathology, "(?i)neoplastic") ~ "Yes",
      TRUE ~ "No"
    ),
    pathology_other = case_when(
      str_detect(category_gen_pathology, "(?i)other") ~ "Yes",
      TRUE ~ "No"
    )
  )

# Save to csv to view 
write_csv(filtered_pathology, file = here("outputs/categorized_pathologies.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize specific pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# UPDATED LIST (08/12/2025)
categories_sp <- list(
  "Acute Burns" = c("acute burn(s| injuries?)?", "chemical burn(s)?", "contact burn(s)?", 
                    "electrical burn(s)?", "flame burn(s)?", "scald burn(s)?", "inhalation burn(s)?", "burn(s)?", "scald(s)?", "flame(s)?"),
  "Contractures" = c("contracture(s)?", "post-burn contracture(s)?"),
  "Scars" = c("burn scar(s)?", "scar revision", "hypertrophic scar(s)?", "keloid(s)?", "cicatricial eyelid ectropion", "scarring?"),
  "Severe Injuries" = c("amputation(s)?", "mutilation(s)?", "fracture(s)?", "road traffic injur(y|ies)", "gunshot", "trauma", "blunt"),
  "Soft Tissue Injuries" = c("wound(s)?", "lesion(s)?", "diabetic foot syndrome?", "soft tissue injuries?"),
  "Cleft & Craniofacial" = c("cleft lip", "cleft palate", "orofacial cleft(s)?", "craniofacial"),
  "Limb Deformities" = c("club ?foot", "syndactyly", "polydactyl(y)?", "limb anomal(y|ies)"),
  "Cancer" = c("carcinoma", "sarcoma", "tumou?r(s)?", "tumor(s)?", "cancer(s)?", "dermatofibrosarcoma"),
  "Other Neoplasms" = c("mass(es)?", "lump(s)?", "growth(s)?", "tumor-like", "nodules?", "adenoma", "fibroma"),
  "Noma" = c("noma", "cancrum oris"),
  "Buruli Ulcer" = c("buruli", "mycobacterium ulcerans"),
  "Other Infections" = c("infection(s)?", "anthrax", "pyomyositis", "fasciitis")
)


# Function to categorize specific pathologies
categorize_specific_path <- function(specific_pathology, categories) {
  
  assign_categories <- function(pathology) {
    if (is.na(pathology) || pathology == "") return(NA_character_)
    
    pathology <- str_to_lower(pathology)
    
    assigned <- names(categories)[
      sapply(categories, function(words) {
        pattern <- paste(words, collapse = "|")  # combine multiple patterns with OR
        full_pattern <- paste0("(?i)\\b(", pattern, ")\\b")  # case-insensitive with word boundaries
        str_detect(pathology, full_pattern)
      })
    ]
    
    if (length(assigned) > 0) {
      return(paste(assigned, collapse = "; "))
    }
    
    return(NA_character_)
  }
  
  category <- assign_categories(specific_pathology)
  
  if (!is.na(category)) return(category)
  
  if (!is.na(specific_pathology) && specific_pathology != "") {
    return("Other")
  }
  
  return(NA_character_)
}

# Apply function
categorized_specific_pathology <- categorized_general_pathology %>%
  mutate(category_specific_pathology = mapply(categorize_specific_path, 
                                              specific_pathology, 
                                              MoreArgs = list(categories = categories_sp))) %>%
  select(study_id, covidence_number, title_3, general_category_of_pathology, specific_pathology, 
         category_specific_pathology, category_gen_pathology, category_tags, category_tiab, 
         congenital_malformations, burns, trauma)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize surgeries performed ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define surgery categories
categories_surgery <- list(
  "Cleft & Craniofacial Surgeries" = c(
    "cleft lip repair", "cleft palate repair", "cleft lip", "cleft palate", 
    "cleft lip/palate", "cleft lip and/or palate", "combo \\(cleft lip and cleft palate\\)", 
    "commissuroplasty", "cleft lip revision", "cleft rhinoplasty", "cleft nose", 
    "cleft palate closure", "cleft fistula", "primary lip \\+ palate repair", 
    "primary repair of a bilateral lip deformity", "primary palate only repair", 
    "palatal fistula", "palatoplasty", "furlows z plasty", "furlow.s palatoplasty", 
    "mullikens repair", "millard", "millard variants", "millard forked flap", 
    "millard rotation advancement", "tennison randal", "modified millard"
  ),
  "Amputation & Limb Procedures" = c(
    "amputation", "amputations", "amputation and grafting", 
    "escharotomy fingers or limbs amputations", "primary repair of cut achilles tendon", 
    "ankylosis release", "tendon transfer", "plaster of paris"
  ),
  "Soft Tissue & Skin Procedures" = c(
    "skin graft", "skin grafting", "split skin graft", "split-thickness skin grafting", 
    "full thickness skin graft", "partial thickness skin graft", 
    "flap", "flaps", "flap cover", "local flap", "muscle flap", 
    "muscle/fasciocutaneous flap", "myocutaneous flap", "nasolabial flap", 
    "nasolabial and tongue flaps", "forehead flap", "parascapular flap", 
    "preputial skin flap", "glabella flaps", "camille-bernard flap", 
    "estlander flap", "webster flap", "composite flap", "direct closure repair", 
    "primary closure", "y-v flap", "z-plasty", "scar revision", "general procedures skin graft",
    "acute burn care", "acute burn management", "burn contracture", "burn contracture surgery", 
    "burn excision", "burn management", "burn surgeries", "burn scars",
    "contracture release", "contracture release and skin grafting", "contracture release/skin graft",
    "delayed excision with skin grafting", "delayed split skin grafting", 
    "early excision with skin grafting", "primary excision and skin grafting", 
    "excision \\+ graft", "excision with secondary intension", 
    "full thickness skin grafts", "partial thickness skin grafts", "skin grafts", 
    "grafting", "pinch grafting", "grafting of cutaneous ulcers", 
    "escharotomy"
  ),
  "Trauma & Fracture Management" = c(
    "fracture treatment", "open fracture management", "open reduction", 
    "closed reduction", "k-wire fixation", "orif", "open treatment of fracture", 
    "exploration and repair", "primary repair of nerve injury", 
    "maxillo-mandibular fixation"
  ),
  "Cancer & Tumor Surgery" = c(
    "mastectomy", "subcutaneous mastectomy", "breast lump excision", 
    "ganglionectomy", "nephrectomy", "tumor excision", 
    "exploratory laparotomy", "cyst excision", "biopsy", 
    "lymph node biopsy", "enucleation of cyst/tumor"
  ),
  "Other" = c(
    "cosmetic procedures", "fasciectomy", 
    "facial scar revision", "lip reconstruction", "nose reconstruction", 
    "primary closed rhinoplasty", "facial reconstruction", "gasps"
  )
)

# Function to categorize surgeries
categorize_surgeries <- function(surgery, categories) {
  if (is.na(surgery) || surgery == "") return(NA_character_)  # Handle missing values
  
  surgery <- str_to_lower(surgery)  # Convert to lowercase
  
  assigned <- names(categories)[sapply(categories, function(words) 
    any(str_detect(surgery, paste0("\\b(", paste(words, collapse = "|"), ")\\b", collapse = ""))))]  # Match categories
  
  if (length(assigned) > 0) {
    return(paste(assigned, collapse = "; "))  # Return multiple categories if applicable
  }
  
  return("Other")  # Default category
}

# Apply categorization to original dataset
categorized_surgeries <- extracted_studies_clean %>%
  mutate(category_surgery = mapply(categorize_surgeries, 
                                   types_of_surgical_procedure_performed, 
                                   MoreArgs = list(categories = categories_surgery))) %>% 
  select(study_id, title_3, general_category_of_pathology, specific_pathology, types_of_surgical_procedure_performed, category_surgery)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize DALYs Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

daly_subset <- extracted_studies_clean %>% 
  filter(!is.na(disability_adjusted_life_years_dal_ys)) 

# Extract countries
country_daly <- daly_subset %>%  
  # Identify countries
  separate_rows(country, sep = ";|,") %>% 
  mutate(country = str_trim(country)) %>% 
  distinct(country) %>% 
  pull()

# Extract pathologies
pathology_daly <- daly_subset %>% 
  separate_rows(general_category_of_pathology, sep = ";|,") %>% 
  mutate(
    general_category_of_pathology = str_trim(general_category_of_pathology),
    general_category_of_pathology = case_when(
      general_category_of_pathology == "congenital malformations and acquired conditions" ~ 
      "congenital malformations", 
      general_category_of_pathology == "pediatric surgical cases" ~ "other",
      general_category_of_pathology == "general pediatric surgery pathology" ~ "burns; trauma",
      TRUE ~ general_category_of_pathology
    )
  ) %>% 
  separate_rows(specific_pathology, sep = ";|,") %>% 
  mutate(
    specific_pathology = str_trim(specific_pathology),
    specific_pathology = case_when(
      specific_pathology %in% c("cleft lip", "cleft palate", "cleft lip and palate", "cleft care", "orofacial clefts") ~ "orofacial clefts",
      str_detect(specific_pathology, "burn") | specific_pathology == "contractures" ~ "burns",
      TRUE ~ specific_pathology
    )
  ) %>% 
  distinct(covidence_number, .keep_all = TRUE)


# Create summary table
summary_path_daly <- pathology_daly %>% 
  group_by(general_category_of_pathology) %>% 
  summarize(n = n()) %>% 
  mutate(percentage = n / sum(n) * 100) 

### Orofacial clefts ----

# Find studies on clefts
cleft_daly <- pathology_daly %>% 
  filter(specific_pathology == "orofacial clefts") %>% 
  select(covidence_number, title_3, general_category_of_pathology, 
         specific_pathology, disability_adjusted_life_years_dal_ys) %>% 
  distinct(covidence_number, .keep_all = TRUE)

# Extract countries and DALYs
burden_orofacialcleft <- daly_subset %>%
  mutate(extracted_info = str_extract_all(disability_adjusted_life_years_dal_ys, 
                                          "-\\s*([A-Za-z\\s]+)\\s*\\((\\d+\\.\\d+)\\s*DALYs")) %>%
  unnest(extracted_info) %>%
  mutate(
    country = str_trim(str_extract(extracted_info, "[A-Za-z\\s]+")),  # Extract country name
    dalys = as.numeric(str_extract(extracted_info, "\\d+\\.\\d+"))  # Extract DALY values
  ) %>%
  select(country, dalys)  # Remove temporary column

### Burns ----

# Find studies on burns
burn_daly <- pathology_daly %>% 
  filter(general_category_of_pathology == "burns") %>% 
  select(covidence_number, study_id, title_3, country, general_category_of_pathology, 
         specific_pathology, disability_adjusted_life_years_dal_ys) %>% 
  distinct(covidence_number, .keep_all = TRUE)

# # Create dataset based on extraction
# burn_daly <- data.frame(
#   score = c("disability", "disability", "disability", "disability", "disability", "quality of life", "quality of life", "quality of life", "quality of life", "quality of life"),
#   time = c(0, 1, 3, 6, 12, 0, 1, 3, 6, 12),
#   value = c(0.22, 0.13, 0.06, 0.05, 0.03, 0.69, 0.79, 0.86, 0.89, 0.93)
# )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Cost Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost_1 <- extracted_studies_clean %>% 
  filter(!is.na(cost_to_individuals_cost_in_usd_or_other_currency))

cost_2 <- extracted_studies_clean %>% 
  filter(!is.na(cost_to_government_cost_in_usd_or_other_currency))

cost_subset <- full_join(cost_1, cost_2) %>% 
  select(covidence_number, study_id, title_3, country, general_category_of_pathology, 
         specific_pathology, cost_to_government_cost_in_usd_or_other_currency, 
         cost_to_individuals_cost_in_usd_or_other_currency)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combine Cost & DALYs Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Join both subsets
cost_daly_subset <- left_join(cost_subset, burn_daly, by = c("study_id"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Organize Surgical Capacity Info ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sc <- extracted_studies_clean %>% 
  select(covidence_number, title_3, country, general_category_of_pathology, 
         specific_pathology, other_findings, 33:83)

  