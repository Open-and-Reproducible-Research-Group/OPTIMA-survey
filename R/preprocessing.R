library(tidyverse)


# Read in and combine all files (could be written as function)
paths <- list.files("data/original", pattern = "[.]xlsx$", full.names = TRUE)

df <- paths %>%
  
  set_names(basename) %>%
  
  map(readxl::read_excel) %>%

  list_rbind(names_to = "source_file") %>%
  
  # Keep only columns that are not empty
  keep(~ any(!is.na(.))) %>%
  
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
    pivot_longer(everything(), names_to = "var_id", values_to = "var_full")
  
  write_csv(out, "data/processed/var_overview.csv")
  
  out
}

create_var_overview("data/processed/combined_data.csv")



# Process combined data
df <- read_csv("data/processed/combined_data.csv", col_names = FALSE, skip = 1) %>% 
  
  # Add institution type and location info to institutional surveys
  mutate(X53 = case_when(
    X62 == "SumDU" ~ "classical",
    X62 == "DonNU" ~ "classical",
    X62 == "LPU" ~ "technical",
    X62 == "LutskNTU" ~ "technical",
    TRUE ~ as.character(X53)
  )) %>%
  mutate(X54 = case_when(
    X62 == "SumDU" ~ "Sumy Oblast",
    X62 == "DonNU" ~ "Vinnytsia Oblast",
    X62 == "LPU" ~ "Lviv Oblast",
    X62 == "LutskNTU" ~ "Volyn Oblast",
    # In 2022 question targeted location before displacement in 2022
    (X62 == "National" & X63 == "2022") ~ X59,
    TRUE ~ as.character(X54)
  )) %>% 
  
  # Calculate survey duration (total response time in minutes)
  mutate(X64 = difftime(X4, X3, units = "mins"))
  