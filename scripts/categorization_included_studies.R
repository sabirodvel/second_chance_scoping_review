## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Included studies categorization
## 
## Author: Sabina Rodriguez
##
## Date: 02/13/2025
## Updated: 08/11/2025

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
extracted_studies <- read_csv(here("data/extracted_studies_aug_11.csv"))

# Clean dataset
included_studies <- clean_names(included_studies)
extracted_studies <- clean_names(extracted_studies)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Categorize studies based on Tags ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identify studies without tags (Need to go back and give tag)
included_studies_na <- included_studies[is.na(included_studies$tags), ] 

# Define categories based on Tags
tag_categories <- list(
  "Access & Barriers" = c("Access to Healthcare", "Barriers to Care", "Barriers to Reconstructive Surgeries", "Health Disparities", "Cost", "cost effectiveness", "Surgical Capacity"),
  "Burden & Outcome" = c("Burden", "Burden of Reconstructive Surgeries", "DALYs", "QALYs", "Surgical Complications"),
  "Trauma" = c("Burns", "Fractures", "Post-Traumatic Injuries", "Mutilating Pathologies", "Deforming Pathologies", "Trauma", "Ulcers", "Road Traffic Accidents", "Post-War", "Natural Disasters"),
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
cat_included <- included_studies %>%
  mutate(
    category_tags = sapply(tags, categorize_by_tags, tag_categories)
  )

## Define the categories of interest
categories_of_interest <- c("Access & Barriers", "Burden & Outcome")

# Create a summary table for two main categories
summary_table <- cat_included %>%
  separate_rows(category_tags, sep = ", ") %>%
  filter(category_tags %in% categories_of_interest) %>%
  group_by(category_tags) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# Save categorized results
write_csv(df, "categorized_studies_with_tags.csv")

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
  "Trauma" = c(
    "burn", "burns", "fracture", "fractures", "post-traumatic injury", "post-traumatic injuries",  
    "mutilating pathology", "mutilating pathologies", "deforming pathology", "deforming pathologies",  
    "trauma", "ulcer", "ulcers", "road traffic accident", "road traffic accidents",  
    "post-war", "natural disaster", "natural disasters", "injury", "injuries",  
    "surgical complication", "surgical complications", "maxillo-facial surgery",  
    "wound", "wounds", "wound healing", "blunt trauma", "penetrating injury", 
    "soft tissue injury", "crush injury", "traumatic amputation", "polytrauma"
  ),
  "Systematic Review" = c(
    "systematic", "systematic review", "meta-analysis", 
    "literature review", "scoping review"
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

cat_included <- cat_included %>% 
  separate_rows(category_tags, sep = ", ") %>%
  separate_rows(category_tiab, sep = ", ") %>% 
  mutate(
    burden = case_when(
      category_tags == "Burden & Outcome" | category_tiab == "Burden & Outcome"
      ~ "Yes",
      TRUE ~ "No"),
    access_barrier = case_when(
      category_tags == "Access & Barriers" | category_tiab == "Access & Barriers"
      ~ "Yes",
      TRUE ~ "No")) %>% 
  distinct(title, .keep_all = TRUE) # Removes duplicate rows based on Title

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Identify categories ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Extract studies on burden & outcomes
# burden_outcome <- cat_included %>% 
#   filter(burden == "Yes")
# 
# # Extract studies on barriers to care
# access_barriers <- cat_included %>% 
#   filter(access_barrier == "Yes")

# # Extract systematic reviews
# systematic_reviews <- df %>%
#   mutate(systematic_review = ifelse(str_detect(category_tiab, "Systematic Review"), "Yes", "No")) %>% 
#   filter(systematic_review == "Yes")

# Create a summary table for two main categories
summary_cat_table <- cat_included %>%
  separate_rows(category_tags, sep = ", ") %>%
  group_by(burden, access_barrier) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Save categorized results
# write_csv(df, "outputs/categorized_studies_feb_26.csv")
# 
# # Save systematic reviews list
# write_csv(systematic_reviews, here("outputs/systematic_reviews_feb_26.csv"))
# 
# # Save access / barriers to care
# write_csv(access_barriers, here("outputs/access_barriers_feb_26.csv"))
# 
# # Save burden & outcomes 
# write_csv(burden_outcome, here("outputs/burden_outcome_feb_26.csv"))

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Save all datasets into excel sheet

# mapping the data frames onto the list
data_frames <- list("included_studies" = df, 
                    "burden_outcomes" = burden_outcome,
                    "access_barriers" = access_barriers,
                    "systematic_reviews" = systematic_reviews)

# writing the list of data frames onto the xlsx file
write.xlsx(data_frames,
           file = "outputs/included_studies_cat_feb_26.xlsx")
