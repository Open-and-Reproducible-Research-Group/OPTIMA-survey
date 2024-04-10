library(readxl)
library(dplyr)

# Shorten filenames
# Might finally not be necessary, but useful at the moment
shorten_filename <- function(filename){
  short_name <- paste0("df", tolower(substr(filename, start = 17, stop = 22)))
  if (exists(short_name)){
    short_name <- paste(short_name, "2", sep = "_")
  }
  return(short_name)
}


# Read in all data files
# This could also be written as a function
# Would need some adaptions to path handling
datafiles <- list.files("data/original")
datasets <- list()
for (file in datafiles){
  df_name <- shorten_filename(file)
  assign(df_name, read_excel(paste0("data/original/", file)))
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


# There might be an easier way to loop through datasets
for (dataset in datasets){
  assign(dataset, remove_empty_cols(get(dataset)))
}


# Add columns specifying year of dataset (wave) and subset
# Could maybe separated out into a function
for (dataset in datasets){
  df <- get(dataset)
  df$Wave <- paste0("20", substr(dataset, start = 3, stop = 4))
  df$Source <- substr(dataset, start = 6, stop = 8)
  assign(dataset, df)
}


# Combine subsets into one dataframe
full_data <- dplyr::bind_rows(mget(unlist(datasets)))

# Save full data as csv
write.csv(full_data, "data/processed/OPTIMA-Survey-full.csv")

