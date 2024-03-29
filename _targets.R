# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# Load the R scripts with your custom functions:
source("2-code/0-packages.R")
#source("2-code/0b-initial_processing.R")
source("2-code/1-functions_processing.R")
source("2-code/2-functions_analysis.R")
source("2-code/3-functions_field_data.R")
source("2-code/4-functions_sir.R")

# list of targets
list(
  tar_target(sample_key_file, "1-data/sample_key.csv", format = "file"),
  tar_target(sample_key, read.csv(sample_key_file)),
  tar_target(sample_weights_file, "1-data/sample_weights.csv", format = "file"),
  tar_target(sample_weights, read.csv(sample_weights_file)),
  tar_target(analysis_key, read.csv("1-data/analysis_key.csv")),
  tar_target(moisture, read.csv("1-data/moisture.csv")),
  tar_target(dry_weight, compute_weights(sample_weights, moisture)),
  
  # optode
  tar_target(optode_data, import_optode_data("1-data/optodes")),
  tar_target(optode_data_2wk, import_optode_data_2wk("1-data/optodes/results_2022-09-15_2wk.csv")),
  tar_target(optode_map, read_sheet("1Pumt5ZA2Ojc8Ow-Ex-gH63ChtTtZs1gPKc50UM098rY") %>% 
               mutate_all(as.character)),
  tar_target(optode_data_processed, process_optode_data(optode_data, optode_data_2wk, optode_map, sample_key)),
  tar_target(gg_optode_all, plot_optode_data_all_samples(optode_data_processed)),
  tar_target(gg_optode, plot_optode_data(optode_data_processed)),
  
  # weoc
  tar_target(weoc_data, import_weoc_data(FILEPATH = "1-data/npoc", PATTERN = "Summary")),
  tar_target(weoc_processed, process_weoc(weoc_data, analysis_key, dry_weight)),
  tar_target(gg_weoc, plot_weoc(weoc_processed, sample_key)),
  
  # SIR
  tar_target(sir_data, read_sheet("1aKSpp3FVN90XfWjUt-P2Zwtcokhos1g3KzMpsnjfPls", sheet = "egm") %>% 
               mutate_all(as.character)),
  tar_target(sir_data_processed, process_sir(sir_data)),
  tar_target(gg_sir_calcurve, plot_sir_cal_curve(sir_data_processed)),
  tar_target(sir_biomass, compute_sir_biomass(sir_data_processed)),
  
  
  # ions
  tar_target(ions_raw, import_ions_data(FILEPATH = "1-data/ions/")),
  tar_target(ions_processed, process_ions_data(ions_raw, dry_weight,
                                           IONS = c("Lithium", "Sodium", "Ammonia", 
                                                    "Potassium", "Magnesium", "Calcium", 
                                                    "Nitrite", "Nitrate", "Chloride", 
                                                    "Bromide", "Sulfate", "Phosphate", 
                                                    "Fluoride"))$samples),
  tar_target(gg_ions, plot_ions(ions_processed, sample_key)),
  
  # pH
  tar_target(pH_data, read_sheet("1zMu8HR3Wak4Ru64BQkc1GQBWg8CdKrw-u1LZRQhR2rA") %>% 
               mutate_all(as.character)),
  tar_target(pH_processed, process_pH(pH_data)),
  tar_target(gg_pH, plot_pH(pH_processed, sample_key)),
  
  # field
  tar_target(troll_data_file, "1-data/field_data/230106_troll_oxygen_peak_data.csv", format = "file"),
  tar_target(troll_data, read.csv(troll_data_file)),
  tar_target(troll_processed, process_troll(troll_data)),
  tar_target(gg_troll, plot_troll(troll_processed)),
  
  # export
  tar_target(exports, {
    write.csv(optode_data_processed, "1-data/processed/optode_data_processed.csv", row.names = FALSE)
    write.csv(ions_processed, "1-data/processed/ions_processed.csv", row.names = FALSE)
    write.csv(pH_processed, "1-data/processed/pH_processed.csv", row.names = FALSE)
  }, format = "file"),
  
  # report
  tar_render(report, path = "3-reports/anoxia_report.Rmd")
  

  
  )

