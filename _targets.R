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
    recoded_data,
    likert_to_factor(processed_data, var_overview)
  ),
  tar_target(
    save_data,
    write_csv(recoded_data, "data/processed/preprocessed_data.csv")
  ),
  tar_quarto(sample_characteristics,
             "analysis_notebooks/sample_characteristics.qmd",
             quiet = FALSE),
  tar_quarto(survey_analysis,
             "analysis_notebooks/survey_analysis.qmd",
             quiet = FALSE),
  tar_quarto(inferential_analysis,
             "analysis_notebooks/inferential_analysis.qmd",
             quiet = FALSE),
  tar_quarto(hei_statistics_comparison,
             "analysis_notebooks/hei_statistics_comparison.qmd",
             quiet = FALSE)
)