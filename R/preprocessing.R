library(readxl)
library(dplyr)

# Shorten filenames
shorten_filename <- function(filename){
  short_name <- paste0("df", tolower(substr(filename, start = 17, stop = 22)))
  if (exists(short_name)){
    short_name <- paste(short_name, "2", sep = "_")
  }
  return(short_name)
}


# Read in all data files
datafiles <- list.files("data")
datasets <- list()
for (file in datafiles){
  df_name <- shorten_filename(file)
  assign(df_name, read_excel(paste0("data/", file)))
  datasets <- append(datasets, df_name)
}


# Check for empty columns and delete them
remove_empty_cols <- function(dataset){
  all_columns <- colnames(dataset)
  empty_columns <- list()
  for (column in all_columns){
    if(all(is.na(dataset[[column]]))){
      empty_columns <- append(empty_columns, column)
    }
  }
  reduced_dataset <- dataset[, -which(names(dataset) %in% empty_columns)]
  return(reduced_dataset)
}


for (dataset in datasets){
  df_name <- dataset
  assign(df_name, remove_empty_cols(get(dataset)))
}