library(targets)
library(tarchetypes)
library(quarto)

source("R/functions.R")

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse"))

list(
  tar_target(
    combined_data_path,
    combine_data("data/original")
  ),
  tar_target(
    var_overview,
    create_var_overview(combined_data_path)
  ),
  tar_target(
    combined_data,
    read_csv(combined_data_path, col_names = FALSE, skip = 1)
  ),
  tar_target(
    processed_data,
    process_combined_data(combined_data)
  ),
  tar_target(
    save_processed_data,
    write_csv(processed_data, "data/processed/preprocessed_data.csv")
  ),
  tar_quarto(sample_characteristics,
             "analysis_notebooks/sample_characteristics.qmd",
             quiet = TRUE)
)