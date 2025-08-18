## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Included studies categorization
## 
## Author: Sabina Rodriguez
##
## Date: 02/13/2025
## Updated: 08/18/2025

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, here, stringr, readr, openxlsx)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the CSV file
included_studies <- read_csv(here("data/included_studies_aug_11.csv")) # References of studies
extracted_studies <- read_csv(here("data/extracted_studies_aug_11.csv")) # Consensu of extracted studies

# Clean dataset
included_studies <- clean_names(included_studies) %>% 
  mutate(covidence_number = as.numeric(str_extract(covidence_number, "\\d+")))

extracted_studies <- clean_names(extracted_studies)

# Join two datasets
joined_studies <- left_join(extracted_studies, included_studies, by = "covidence_number")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize studies based on Tags ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identify studies without tags (Need to go back and give tag)
included_studies_na <- joined_studies[is.na(included_studies$tags), ] 

# Define categories based on Tags
tag_categories <- list(
  "Access & Barriers" = c("Access to Healthcare", "Barriers to Care", "Barriers to Reconstructive Surgeries", "Health Disparities", "Cost", "cost effectiveness", "Surgical Capacity"),
  "Burden & Outcome" = c("Burden", "Burden of Reconstructive Surgeries", "DALYs", "QALYs", "Surgical Complications"),
  "Trauma" = c("Fractures", "Post-Traumatic Injuries", "Mutilating Pathologies", "Deforming Pathologies", "Trauma", "Ulcers", "Road Traffic Accidents", "Post-War", "Natural Disasters"),
  "Burns" = c("Burns"),
  "Congenital Malformations" = c("Cleft Palate", "Clubfoot", "Congenital Malformations", "Maxillo-facial surgery"),
  "Infectious & Chronic Conditions" = c("Noma", "Hemagioma", "Skin Conditions", "Tumors", "Cancers"),
  "Population" = c("Children / Pediatric Population", "Elderly / Geriatric Population"),
  "Other" = c("Awaiting classification", "Grey Literature", "Ongoing study")
)

# Function to categorize based on Tags
categorize_by_tags <- function(tags, tag_categories) {
  assigned_categories <- names(tag_categories)[sapply(tag_categories, function(words) any(str_detect(tags, paste0("\\b", words, "\\b"))))]
  if (is.na(tags)) {
    return(NA)
  }
  
  if (length(assigned_categories) == 0) {
    return("Uncategorized")
  }
  return(paste(assigned_categories, collapse = ", "))
}

# Apply categorization to each row
cat_included <- joined_studies %>%
  mutate(
    category_tags = sapply(tags, categorize_by_tags, tag_categories)
  )

# ## Define the categories of interest
# categories_of_interest <- c("Access & Barriers", "Burden & Outcome")
# 
# # Create a summary table for two main categories
# cat_included %>%
#   separate_rows(category_tags, sep = ", ") %>%
#   # filter(category_tags %in% categories_of_interest) %>%
#   group_by(category_tags) %>%
#   summarize(count = n(), .groups = "drop") %>%
#   arrange(desc(count))

# # Save categorized results
# write_csv(df, "categorized_studies_with_tags.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize studies based on TiAB ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define categories and keywords for automatic classification (Title & Abstract)
categories <- list(
  "Burden & Outcome" = c(
    "incidence", "prevalence", "mortality", "morbidity", "dalys", "cases", 
    "burden", "disability", "outcome", "quality of life", "qalys", "health impact", 
    "epidemiology", "risk factor", "long-term effects", "disease progression"
  ),
  "Access & Barriers" = c(
    "cost", "access", "infrastructure", "workforce", "policy", "insurance", 
    "barrier", "healthcare disparity", "equity", "affordability", "availability", 
    "health system", "capacity", "resources", "financial burden", "socioeconomic status", 
    "transportation", "referral system", "specialist availability", "rural access"
  ),
  "Congenital Malformations" = c(
    "cleft", "flap", "craniofacial", "deformity", "malformation", 
    "syndrome", "genetic disorder", "birth defect", "lip", "palate", "microtia", 
    "clubfoot", "congenital", "maxillofacial", "dysmorphology"
  ),
  "Burns" = c(
    "burn", "burns", "contracture", "contractures", "burn scars"
  ),
  "Trauma" = c(
    "fracture", "fractures", "post-traumatic injury", "post-traumatic injuries",  
    "mutilating pathology", "mutilating pathologies", "deforming pathology", "deforming pathologies",  
    "trauma", "ulcer", "ulcers", "road traffic accident", "road traffic accidents",  
    "post-war", "natural disaster", "natural disasters", "injury", "injuries",  
    "surgical complication", "surgical complications", "maxillo-facial surgery",  
    "wound", "wounds", "wound healing", "blunt trauma", "penetrating injury", 
    "soft tissue injury", "crush injury", "traumatic amputation", "polytrauma"
  ),
  "Systematic Review" = c(
    "systematic", "systematic review", "meta-analysis", 
    "literature review", "scoping review", "narrative review"
  )
)

# Function to categorize based on key words
categorize_by_tiab <- function(text, categories) {
  text <- tolower(text)
  assigned_categories <- names(categories)[sapply(categories, function(words) any(str_detect(text, paste0("\\b", words, "\\b"))))]
  if (length(assigned_categories) == 0) {
    return("Uncategorized")
  }
  return(paste(assigned_categories, collapse = ", "))
}

# Apply categorization to each row
cat_included <- cat_included %>%
  mutate(
    category_tiab = mapply(categorize_by_tiab, paste(title, abstract), MoreArgs = list(categories)))

categorized_studies <- cat_included %>% 
  mutate(
    burden = case_when(
      str_detect(category_tags, "(?i)burden & outcome") |
        str_detect(category_tiab, "(?i)burden & outcome") ~ "Yes",
      TRUE ~ "No"
    ),
    access_barrier = case_when(
      str_detect(category_tags, "(?i)access & barriers") |
        str_detect(category_tiab, "(?i)access & barriers") ~ "Yes",
      TRUE ~ "No"
    ),
    burns = case_when(
      str_detect(category_tags, "(?i)burns") |
        str_detect(category_tiab, "(?i)burns") ~ "Yes",
      TRUE ~ "No"
    ),
    trauma = case_when(
      str_detect(category_tags, "(?i)trauma") |
        str_detect(category_tiab, "(?i)trauma") ~ "Yes",
      TRUE ~ "No"
    ),
    congenital_malformations = case_when(
      str_detect(category_tags, "(?i)congenital malformations") |
        str_detect(category_tiab, "(?i)congenital malformations") ~ "Yes",
      TRUE ~ "No"
    ),
    systematic_review = case_when(
      str_detect(category_tiab, "(?i)systematic review") |
        str_detect(study_design, "(?i)review") ~ "Yes",
      TRUE ~ "No"
    )
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Identify categories ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Extract studies on burden & outcomes
# burden_outcome <- categorized_studies %>%
#   filter(burden == "Yes")
# 
# # Extract studies on barriers to care
# access_barriers <- categorized_studies %>%
#   filter(access_barrier == "Yes")
# 
# #Extract studies on trauma
# trauma_outcome <- categorized_studies
# 
# # Extract systematic reviews
# systematic_reviews <- categorized_studies %>%
#   filter(systematic_review == "Yes")
