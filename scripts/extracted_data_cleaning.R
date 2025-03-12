## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Extracted Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Date: 03/10/2025
## Updated: 03/11/2025

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, stringr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the CSV file
extracted_studies_raw <- read_csv(here("data/extracted_studies_march_10.csv")) 
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
extracted_studies_clean <- clean_names(extracted_studies_joined) %>% 
  # Convert to lower for easier reading
  mutate(general_category_of_pathology = str_to_lower(general_category_of_pathology),
         specific_pathology = str_to_lower(specific_pathology))

# View unique general pathologies values
df <- extracted_studies_clean %>% 
  separate_rows(general_category_of_pathology, sep = ",|;| and") %>%
  distinct(general_category_of_pathology) %>% 
  mutate(general_category_of_pathology = str_trim(general_category_of_pathology))

# View unique specific pathologies values
df <- extracted_studies_clean %>% 
  separate_rows(specific_pathology, sep = ",|;| and") %>%
  distinct(specific_pathology) %>% 
  mutate(specific_pathology = str_trim(specific_pathology)) %>% 
  group_by(specific_pathology) %>% 
  count()

# Identify NAs
pathology_missing <- extracted_studies_clean[is.na(extracted_studies_clean$general_category_of_pathology), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize general pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create groups/categories for general pathology
categories_gp <- list(
  "Burns" = c("burn", "burns", "burn contractures", "burn and burn complications", "burn management"),
  "Congenital Malformations" = c("congenital malformation", "congenital malformations", 
                                 "congenital anomalies", "congenital conditions", "cleft lip", 
                                 "cleft palate", "orofacial clefts", "craniofacial disorders"),
  "Trauma" = c("trauma", "injury", "ulcers", "wounds", "wound", "scars", "trauma-related", "fracture", "fractures"),
  "Infectious Conditions" = c("noma"),
  "Neoplastic Conditions" = c("skin cancer", "cyst", "neoplasm")
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

# Create groups/categories for specific pathology
categories_sp <- list(
  "Acute Burns" = c("burns", "acute burn injuries caused by hot liquids", "acute burns", "chemical burns", "scald burns", 
                    "flame burns", "electrical burns", "contact burns", "burn injuries", 
                    "burn management"),
  
  "Contractures" = c("burn contracture", "burn contractures", "contractures", "contractures after burn injury", 
                     "post-burn contractures (pbc)", "dupytren contracture"),
  
  "Burn Scars" = c("burn scars", "burn scar contractures", "scar revision", "scar secondary to burn"),
  
  "Severe Injuries" = c("amputation", "amputation secondary to burn", "gunshot injury", "trauma", "trauma-related conditions", 
                        "traumatic injuries", "fracture hand bones", "fractures", "crush hand", "thumb amputation", 
                        "ring avulsion injury", "cut achilles tendon", "cut extensor", "cut wrist", "cuts over the face", 
                        "fingertip injury", "open fractures", "lower limb trauma", "nerve injury"),
  
  "Soft Tissue Injuries" = c("wounds", "necrotizing fasciitis", "granulating wound", "granulating wound secondary to burn", 
                             "diabetic foot ulcers", "lipohypertrophy", "human bites", "keloids", 
                             "lipoma", "osteomyelitis", "mycetoma foot", "mycetoma hand", "hypertrophic scars", "keloid surgery", "complications to soft tissues"),
  
  "Cleft & Craniofacial" = c("cleft lip", "cleft palate", "orofacial clefts", "craniofacial miscellaneous (e.g.)", 
                             "facial clefts", "cleft care", "microtia", "spina bifida", "syndactyly", 
                             "other craniofacial anomalies (e.g.)", "nasal defect", "ectopic eyelid", "tongue deformities"),
  
  "Limb Deformities" = c("club foot", "polydactyl", "polydactyly", "bifid digits", "hypoplastic hand", 
                         "congenital talipes equinovarus", "arthrogryposis", "camptodactyly"),
  
  "Cancer" = c("basal cell carcinoma", "breast cancer", "malignant melanoma", "marjolin’s ulcer", "soft tissue sarcoma", 
               "squamous cell carcinoma"),
  
  "Other Neoplasms" = c("stromal tumor", "neurofibroma", "lymphangioma", "hemangioma"),
  
  "Noma" = c("noma", "cancrum oris"),
  
  "Other Infections" = c("anthrax", "appendicitis", "fournier’s gangrene", "infections (e.g.)", "human bites (lip)", 
                         "steven johnson’s syndrome")
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


