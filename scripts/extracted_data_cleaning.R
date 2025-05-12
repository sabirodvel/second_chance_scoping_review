## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Extracted Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Date: 03/10/2025
## Updated: 05/07/2025

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, stringr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the CSV file
extracted_studies_raw <- read_csv(here("data/extracted_studies_may_07.csv")) 
# THIS DATA IS NOT CONSENSUS. INCLUDES ALL EXTRACTION NEED TO REMOVE DUPLICATES?!

# Create consensus column
extracted_studies_raw <- extracted_studies_raw %>%
  mutate(consensus = if_else(`Reviewer Name` == "Consensus", "Yes", "No"))

# Create dataset with consensus only
studies_with_consensus <- extracted_studies_raw %>% 
  filter(consensus == "Yes") %>% 
  ungroup()

# Create dataset without consensus
studies_without_consensus <- extracted_studies_raw %>% 
  group_by(`Covidence #`) %>% 
  filter(!any(consensus == "Yes")) %>% 
  #Only keep the ones without consensus
  filter(n() <= 2) %>% 
  #Select first instance
  slice(1) %>%
  ungroup()

# Join both datasets
extracted_studies_joined <- full_join(studies_with_consensus, studies_without_consensus)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean column names (pathologies)
extracted_studies_clean <- clean_names(studies_with_consensus) %>% 
  # Convert to lower for easier reading
  mutate(general_category_of_pathology = str_to_lower(general_category_of_pathology),
         specific_pathology = str_to_lower(specific_pathology),
         types_of_surgical_procedure_performed = str_to_lower(types_of_surgical_procedure_performed)
         )

# View unique general pathologies values
df <- extracted_studies_clean %>% 
  separate_rows(general_category_of_pathology, sep = ",|;|and") %>%
  distinct(general_category_of_pathology) %>% 
  mutate(general_category_of_pathology = str_trim(general_category_of_pathology))

# View unique specific pathologies values
df <- extracted_studies_clean %>% 
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

  # Specify rows to drop from surgical procedures (after distinct) EDIT AS NEEDED
rows_to_drop <- c(29, 31:40, 43:124, 203, 215, 244, 257)

df <- extracted_studies_clean %>% 
  separate_rows(types_of_surgical_procedure_performed, sep = ",|;|&") %>%
  mutate(sorted_surgeries = str_trim(types_of_surgical_procedure_performed)) %>% 
  distinct(sorted_surgeries, .keep_all = TRUE) %>% # Ensure uniqueness before counting
  select(sorted_surgeries) %>% 
  filter(!row_number() %in% rows_to_drop) %>% 
  mutate(sorted_surgeries = case_when(
    row_number() == 1 ~ "modified millard; manchester repair; mullikens repair; furlows z plasty; tunrover flaps; unipedicle; mucoperiostal flaps",
    row_number() == 32 ~ "flaps; thickness skin grafts",
    row_number() == 95 ~ "cleft palate repair",
    row_number() == 98 ~ "skin graft; debridement; amputation; muscle flap; local flap",
    row_number() == 193 ~ "maxillo-mandibular fixation; debridement",
    TRUE ~ sorted_surgeries)
    ) %>% 
  separate_rows(sorted_surgeries, sep = ",|;|&") %>% 
  mutate(sorted_surgeries = str_trim(sorted_surgeries)) %>% 
  distinct(sorted_surgeries) %>% 
  count(sorted_surgeries, name = "frequency")  # Count occurrences properly


  # Identify NAs
pathology_missing <- extracted_studies_clean[is.na(extracted_studies_clean$general_category_of_pathology), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize general pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# UPDATED LIST (05/07/2025)
categories_gp <- list(
  "Burns" = c("burn", "burns", "burn complications"),
  "Congenital Malformations" = c(
    "congenital malformations", "congenital malformation", "congenital anomalies",
    "congenital conditions", "congenital abnormalities", "congenital deformities",
    "acquired deformities", "craniofacial disorders"
  ),
  "Trauma" = c("trauma", "injury", "injuries", "ulcers", "wounds", "chronic wounds", "scars"),
  "Infectious Conditions" = c("noma", "infections", "infectious diseases", "neck infection"),
  "Neoplastic Conditions" = c("tumor", "cyst", "neoplasm", "neoplasms", "cancer (tumor)", "skin cancer", "cancer", "neck cancer"),
  "Other" = c(
    "aesthetics", "pediatric surgical cases", "acquired conditions",
    "pediatric surgical emergencies", "NA", "head", "cosmetic",
    "general pediatric surgery pathology", "emergency", "essential surgical care",
    "head", "musculoskeletal impairment (tmsi)", "pediatric surgical procedure",
    "pediatric general surgery", "paediatric surgical conditions", 
    "paediatric surgical cases", "oral/maxillofacial surgical cases", 
    "broad (rural surgical outreach program)"
  )
)


# Function to categorize based on categories
categorize_gen_path <- function(general_pathology, specific_pathology, categories) {
  
  # Helper function to assign categories based on words
  assign_categories <- function(pathology) {
    if (is.na(pathology) || pathology == "") return(NA_character_)  # Ensure NA is explicitly character
    
    assigned <- names(categories)[sapply(categories, function(words) 
      any(str_detect(pathology, paste0("\\b", words, "\\b"))))]
    
    if (length(assigned) > 0) {
      return(paste(assigned, collapse = "; "))  # Return categories as semicolon-separated string
    }
    
    return(NA_character_)  # Return NA if no match is found
  }
  
  # Try to categorize general_pathology first
  category <- assign_categories(general_pathology)
  
  # If general_pathology is categorized, return it
  if (!is.na(category)) return(category)
  
  # Otherwise, try categorizing specific_pathology
  category_specific <- assign_categories(specific_pathology)
  
  # If specific_pathology is categorized, return it
  if (!is.na(category_specific)) return(category_specific)
  
  # If neither category is found but text exists in either column, return "Other"
  if (!is.na(general_pathology) && general_pathology != "" || 
      !is.na(specific_pathology) && specific_pathology != "") {
    return("Other")
  }
  
  return(NA_character_)  # If both are NA or empty, return NA
}


# Apply categorization to each row
categorized_general_pathology <- extracted_studies_clean %>%
  mutate(category_gen_pathology = mapply(categorize_gen_path, 
                                         general_category_of_pathology, 
                                         specific_pathology, 
                                         MoreArgs = list(categories = categories_gp))) %>%
  select(study_id, title_3, general_category_of_pathology, specific_pathology, category_gen_pathology)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize specific pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Updated list of categories with expanded keywords for specific pathologies
categories_sp <- list(
  "Acute Burns" = c(
    "acute burns", "acute burn", "chemical burns", "contact burns", "electrical burns", "flame burns", "scald burn", 
    "scald burns", "inhalation burns", "mixed or full thickness accidental burns", "scalds, fire, contact", "burn management", "burns and contractures"
  ),
  
  "Contractures" = c(
    "burn contracture", "contractures", "contractures after burn injury", "contractures of various etiologies", 
    "post-burn contractures", "burn scar contractures"
  ),
  
  "Burn Scars" = c(
    "burn scars", "scar revision", "scar secondary to burn", "facial scar revision", "hypertrophic scars", 
    "keloid", "keloid surgery", "keloids", "scarring", "burn scar contractures"
  ),
  
  "Severe Injuries" = c(
    "amputation", "thumb amputation", "fracture hand bones", "fractures", "open fractures", 
    "road traffic injuries", "gunshot injury", "ring avulsion injury", "fingertip injury", 
    "crush hand", "cut extensor", "cut wrist", "cuts over the face", "neglected hand injury", 
    "trauma", "trauma-related injuries", "trauma-related conditions", "traumatic injuries", "injury/orthopaedic"
  ),
  
  "Soft Tissue Injuries" = c(
    "wounds", "granulating wound", "granulating wound secondary to burn", "chronic ulcer secondary to burn", 
    "diabetic foot ulcers", "lipohypertrophy", "human bites", "osteomyelitis", "mycetoma foot", 
    "mycetoma hand", "pressure injuries", "discharging sinus", "necrotizing fasciitis", 
    "soft tissue injuries", "cystic hygroma", "dermoid cysts", "lipoma", "lipomas", "fibromas", "ulcers"
  ),
  
  "Cleft & Craniofacial" = c(
    "cleft care", "cleft lip", "cleft lip/palate", "cleft lips", "cleft palate", "cleft palates", 
    "facial clefts", "orofacial clefts", "craniofacial miscellaneous"
  ),
  
  "Limb Deformities" = c(
    "club foot", "clubfoot", "congenital talipes equinovarus", "syndactyly", "polydactyl", 
    "polydactyly", "congenital anomalies", "congenital deformities", "limb miscellaneous", 
    "other limb anomalies"
  ),
  
  "Cancer" = c(
    "malignant melanoma", "squamous cell carcinoma", "basal cell carcinoma", "marjolin’s ulcer", 
    "soft tissue sarcoma", "stromal tumor", "neoplasms", "tumors", "tumor"
  ),
  
  "Other Neoplasms" = c(
    "neurofibroma", "lymphangioma", "hemangioma", "lipoma", "fibromas", "cystic hygroma", 
    "ganglia", "masses"
  ),
  
  "Noma" = c(
    "noma", "cancrum oris"
  ),
  
  "Other Infections" = c(
    "fournier’s gangrene", "infections", "infectious conditions", "anthrax", "appendicitis", 
    "human bites", "intraabdominal masses", "mycetoma foot", "mycetoma hand", "pyomyositis", 
    "necrotizing fasciitis", "soft tissue infections"
  )
)

# Function to categorize 
categorize_specific_path <- function(specific_pathology, categories) {
  
  # Helper function to assign categories based on words
  assign_categories <- function(pathology) {
    if (is.na(pathology) || pathology == "") return(NA_character_)  # Ensure NA is explicitly character
    
    # Convert pathology to lowercase for case-insensitive matching
    pathology <- str_to_lower(pathology)
    
    assigned <- names(categories)[sapply(categories, function(words) 
      any(str_detect(pathology, paste0("(?i)\\b", words, "\\b"))))]  # Case-insensitive boundary match
    
    if (length(assigned) > 0) {
      return(paste(assigned, collapse = "; "))  # Return categories as semicolon-separated string
    }
    
    return(NA_character_)  # Return NA if no match is found
  }
  
  # Try to categorize specific_pathology first
  category <- assign_categories(specific_pathology)
  
  # If specific_pathology is categorized, return it
  if (!is.na(category)) return(category)
  
  # If no category is found but text exists, return "Other"
  if (!is.na(specific_pathology) && specific_pathology != "") {
    return("Other")
  }
  
  return(NA_character_)  # If empty, return NA
}

# Apply categorization to each row
categorized_specific_pathology <- extracted_studies_clean %>%
  mutate(category_specific_pathology = mapply(categorize_specific_path, 
                                              specific_pathology, 
                                              MoreArgs = list(categories = categories_sp))) %>%
  select(study_id, title_3, general_category_of_pathology, specific_pathology, category_specific_pathology)

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
    general_category_of_pathology = if_else(
      general_category_of_pathology == "congenital malformations and acquired conditions", 
      "congenital malformations", 
      general_category_of_pathology
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

  