## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Extracted Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Date: 03/10/2025
## Updated: 03/10/2025

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean column names
extracted_studies_clean <- clean_names(extracted_studies_raw) %>% 
  # Convert to lower for easier reading
  mutate(general_category_of_pathology = str_to_lower(general_category_of_pathology),
         specific_pathology = str_to_lower(specific_pathology))

# View unique values
df <- extracted_studies_clean %>% 
  separate_rows(general_category_of_pathology, sep = ",|;| and") %>% 
  distinct(general_category_of_pathology) %>% 
  mutate(general_category_of_pathology = str_trim(general_category_of_pathology))

# Delete input based on vague description
extracted_studies_clean <- extracted_studies_clean %>% 
  mutate(general_category_of_pathology = replace(general_category_of_pathology,
                                                 c(43, 46, 47, 55, 76, 77),
                                                 NA))

# Identify NAs to go back and correct!!!!
pathology_missing <- extracted_studies_clean[is.na(extracted_studies_clean$general_category_of_pathology), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize general pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create groups/categories for general pathology
categories_gp <- list(
  "Burns" = c("burn", "burns", "burn contractures", "burn and burn complications"),
  "Congenital Malformations" = c("congenital malformation", "congenital malformations", 
                                 "congenital anomalies", "congenital conditions", "cleft lip", 
                                 "cleft palate", "orofacial clefts", "craniofacial disorders"),
  "Trauma" = c("trauma", "injury", "ulcers", "wounds", "wound", "scars", "trauma-related", "fracture", "fractures"),
  "Infectious Conditions" = c("noma"),
  "Cyst, Tumor & Cancer" = c("skin cancer", "cyst", "neoplasm")
  )

# Function to categorize based on Tags
categorize_gen_path <- function(general_pathology, specific_pathology, categories) {
  
  # Helper function to assign categories based on words
  assign_categories <- function(pathology) {
    if (is.na(pathology) || pathology == "") return(NA)  # Return NA if empty or NA
    
    assigned <- names(categories)[sapply(categories, function(words) 
      any(str_detect(pathology, paste0("\\b", words, "\\b"))))]
    
    if (length(assigned) == 0) return("Other")  # If no category found, return "Other"
    
    return(paste(assigned, collapse = "; "))  # Return categories as comma-separated string
  }
  
  # First, try to categorize general_pathology
  category <- assign_categories(general_pathology)
  
  # If general_pathology had a category, return it
  if (!is.null(category)) return(category)
  
  # If general_pathology was NA or empty, try specific_pathology
  return(assign_categories(specific_pathology))  # Return category for specific_pathology or "Other"
}

# Apply categorization to each row
categorized_general_pathology <- extracted_studies_clean %>%
  mutate(category_gen_pathology = mapply(categorize_gen_path, 
                                         general_category_of_pathology, 
                                         specific_pathology, 
                                         MoreArgs = list(categories = categories_gp))) %>%
  select(general_category_of_pathology, specific_pathology, category_gen_pathology)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize specific pathologies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



