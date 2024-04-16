library(tidyverse)
library(responsePatterns)


# Read in and combine all files (could be written as function)
paths <- list.files("data/original", pattern = "[.]xlsx$", full.names = TRUE)

df <- paths %>%
  
  set_names(basename) %>%
  
  map(readxl::read_excel) %>%

  list_rbind(names_to = "source_file") %>%
  
  # Keep only columns that are not empty
  keep(~ any(!is.na(.))) %>%
  
  # Add new continuous id
  cbind(newID = 1:nrow(.), .) %>% 
  
  # Extract information about survey wave from source file name
  mutate(Survey = str_extract(source_file, "(?<=\\d{4}_)(.*)(?=-Results)")) %>%
  mutate(Year = str_extract(source_file, "\\d{4}")) %>%
  
  # Add column for survey duration (to be calculated later)
  mutate(Duration = 0) %>% 
  
  # Save combined dataset
  write_csv(., "data/processed/combined_data.csv")



# Produce codebook for all variables
create_var_overview <- function(path) {
  df <- read_csv(path, col_names = FALSE, n_max = 1, col_types = "c")
  
  out <- df %>%
    pivot_longer(everything(), names_to = "var_id", values_to = "var_full") %>% 
    
    # Add information about instructions to codebook
    mutate(instruction = case_when(
      as.integer(sub("X", "", var_id)) >= 15 & 
        as.integer(sub("X", "", var_id))  <= 23 ~ "agreement",
      as.integer(sub("X", "", var_id)) >= 24 & 
        as.integer(sub("X", "", var_id))  <= 37 ~ "institutional occurrence",
      as.integer(sub("X", "", var_id)) >= 38 & 
        as.integer(sub("X", "", var_id))  <= 53 ~ "agreement"
    )) 
  
  write_csv(out, "data/processed/var_overview.csv")
  
  out
}

create_var_overview("data/processed/combined_data.csv")



# Process combined data (could be written as function)
df <- read_csv("data/processed/combined_data.csv", col_names = FALSE, skip = 1) %>% 
  
  # Add institution type and location info to institutional surveys
  mutate(X54 = case_when(
    X63 == "SumDU" ~ "classical",
    X63 == "DonNU" ~ "classical",
    X63 == "LPU" ~ "technical",
    X63 == "LutskNTU" ~ "technical",
    TRUE ~ as.character(X54)
  )) %>%
  mutate(X55 = case_when(
    X63 == "SumDU" ~ "Sumy Oblast",
    X63 == "DonNU" ~ "Vinnytsia Oblast",
    X63 == "LPU" ~ "Lviv Oblast",
    X63 == "LutskNTU" ~ "Volyn Oblast",
    # In 2022 question targeted location before displacement in 2022
    (X63 == "National" & X64 == "2022") ~ X60,
    TRUE ~ as.character(X55)
  )) %>% 
  
  # Calculate survey duration (total response time in minutes)
  mutate(X65 = difftime(X5, X4, units = "mins")) %>% 
  
  # Save preprocessed dataset
  write_csv(., "data/processed/preprocessed_data.csv")



# Recode likert scale answers to numeric
likert_to_numeric <- function(df) {
  
  # Read in dataset with recoding information
  likert <- read_csv("data/additional/likert_codes.csv")
  
  # Recode all likert answers across dataset
  df <- df %>% 
    mutate(across(everything(),
                  ~ ifelse(.x %in% likert$likert,
                           likert$numeric[match(.x, likert$likert)], .x)))
}

df_recoded <- likert_to_numeric(df)



# Investigate response patterns within all agreement questions

# Extract all columns that use an agreement scale
items <- read_csv("data/processed/var_overview.csv")

agreement_items <- items$var_id[!is.na(items$instruction)
                                & items$instruction == "agreement"]


# Check for response patterns within all agreement questions
response_patterns <- df_recoded[c("X1", agreement_items)] %>% 
  
  # Replace "don't know" answers to NA and set to integer
  mutate(across(everything(), ~ ifelse(. == "don't know", NA, .))) %>% 
  mutate(across(everything(), as.integer)) %>% 
  
  rp.patterns(., max.length = NULL,
              min.length = 3,
              id.var = "X1",
              na.rm = TRUE,
              std.patterns = FALSE,
              na.top = TRUE,
              store.data = TRUE)


# Extract relevant data from ResponsePatterns object
id <- slot(response_patterns, "id")
indices <- slot(response_patterns, "indices")
score <- indices$score
percentile <- indices$percentile

# Add extracted data to the dataset
df_checked <- data.frame(id = id, score = score, percentile = percentile) %>% 
  merge(df, ., by.x = "X1", by.y = "id")


# Check correlation between response time and repetitiveness percentile
cor(as.numeric(df_checked$percentile), as.numeric(df_checked$X65))