library(tidyverse)

# Shorten filenames
# Might finally not be necessary, but useful at the moment
shorten_filename <- function(filename){
  short_name <- paste0("df", tolower(substr(filename, start = 17, stop = 22)))
  if (exists(short_name)) {
    short_name <- paste(short_name, "2", sep = "_")
  }
  return(short_name)
}


# Read in all data files
# This could also be written as a function
# Would need some adaptions to path handling
datafiles <- list.files("data/original")
datasets <- list()
for (file in datafiles) {
  df_name <- shorten_filename(file)
  assign(df_name, readxl::read_excel(paste0("data/original/", file)))
  datasets <- append(datasets, df_name)
}


# Check for empty columns and delete them
remove_empty_cols <- function(dataset){
  dataset %>%
    keep(~ any(!is.na(.)))
}


# There might be an easier way to loop through datasets
for (dataset in datasets) {
  assign(dataset, remove_empty_cols(get(dataset)))
}


# Add columns specifying year of dataset (wave) and subset
# Could maybe separated out into a function
for (dataset in datasets) {
  df <- get(dataset)
  df$Wave <- paste0("20", substr(dataset, start = 3, stop = 4))
  df$Source <- substr(dataset, start = 6, stop = 8)
  assign(dataset, df)
}


# Combine subsets into one dataframe
full_data <- dplyr::bind_rows(mget(unlist(datasets)))

# Save full data as csv
write_csv(full_data, "data/processed/OPTIMA-Survey-full.csv")


# Create overview over variables for codebook
# Function taken from RPT survey
create_var_overview <- function(path) {
  df <- read_csv(path, col_names = FALSE, n_max = 1, col_types = "c")
  
  out <- df %>%
    pivot_longer(everything(), names_to = "var_id", values_to = "var_full")
  
  write_csv(out, "data/processed/var_overview.csv")
  
  out
}

create_var_overview("data/processed/OPTIMA-Survey-full.csv")


df <- read_csv("data/processed/OPTIMA-Survey-full.csv", col_names = FALSE, skip = 1)


# Add institution type and location info to institutional surveys
df <- df %>%
  mutate(X55 = case_when(
    X54 == "sum" ~ "classical",
    X54 == "don" ~ "classical",
    X54 == "lpu" ~ "technical",
    X54 == "lut" ~ "technical",
    TRUE ~ as.character(X55)
  )) %>%
  mutate(X56 = case_when(
    X54 == "sum" ~ "Sumy Oblast",
    X54 == "don" ~ "Donetsk Oblast",
    X54 == "lpu" ~ "Lviv Oblast",
    X54 == "lut" ~ "Volyn Oblast",
    (X54 == "nat" & X53 == "2022") ~ X61,
    TRUE ~ as.character(X56)
  ))