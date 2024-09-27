# Read in and combine all files
combine_data <- function(path_to_folder) {
  
  paths <- list.files(path_to_folder, pattern = "[.]xlsx$", full.names = TRUE)
  
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
    mutate(Year = str_extract(source_file, "\\d{4}"))
  
  # check that dir exists and if not, create it
  if (!dir.exists("data/processed")) dir.create("data/processed")
  
  save_path <- "data/processed/combined_data.csv"
  write_csv(df, save_path)
  
  save_path
}


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
        as.integer(sub("X", "", var_id))  <= 37 ~ "frequency",
      as.integer(sub("X", "", var_id)) >= 38 & 
        as.integer(sub("X", "", var_id))  <= 53 ~ "agreement"
    )) 
  
  # check that dir exists and if not, create it
  if (!dir.exists("data/processed")) dir.create("data/processed")
  
  write_csv(out, "data/processed/var_overview.csv")
  
  out
}


process_combined_data <- function(df){
  
  # Add institution type and location info to institutional surveys
  df <- df %>%
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
    mutate(duration = as.numeric(difftime(X5, X4, units = "mins")))
  
  df
}

# Recode all likert scales as factors
likert_to_factor <- function(df, var_overview){
  
  agreement <- var_overview$var_id[
    var_overview$instruction == 'agreement'] %>% 
    na.omit(.)
  agreement_levels = c("strongly agree", "rather agree", "rather disagree",
                       "strongly disagree", "don't know", "NA")
  
  frequency <- var_overview$var_id[
    var_overview$instruction == 'frequency'] %>% 
    na.omit(.)
  
  frequency_levels = c("very often", "frequently", "sometimes", "rarely",
                       "never", "don't know", "NA")
  
  df[agreement] <- lapply(df[agreement],
                                function(x) {
                                  x[is.na(x)] <- "NA"
                                  factor(x,levels = agreement_levels)
                                  })
  
  df[frequency] <- lapply(df[frequency],
                          function(x) {
                            x[is.na(x)] <- "NA"
                            factor(x,levels = frequency_levels)
                          })
  
  
  df
}