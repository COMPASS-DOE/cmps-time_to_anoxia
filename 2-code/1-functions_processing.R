# TIME TO ANOXIA
# COMPASS-FME

## 1-functions_processing.R
## Use this script to process optode and chemistry data.

## KFP, August 2022

######################## ####
######################## ####



# prelim functions --------------------------------------------------------
## reorder factors
recode_levels = function(dat){
  dat %>% 
    mutate(location = factor(location, 
                             levels = c("upland-A", "upland-B", "transition-A", "wetland-A")),
           timepoint = factor(timepoint, 
                              levels = c("time-zero", "12-hour", "24-hour", "2-week")),
           transect = factor(transect,
                             levels = c("upland", "transition", "wetland")))
}

#
## compute dry weights
compute_weights = function(sample_weights, moisture){
  
  dry_weights = 
    sample_weights %>% 
   # dplyr::select(sample_name, location, weight_g) %>% 
    left_join(moisture %>% mutate(moisture_percent = as.numeric(moisture_percent))) %>% 
    mutate(weight_g = as.numeric(weight_g)) %>% 
    rename(fm_g = weight_g) %>% 
    mutate(od_g = fm_g/((moisture_percent/100)+1),
           soilwater_g = fm_g - od_g) %>% 
   # filter(!is.na(sample_name)) %>% 
    force()
  
  dry_weights
}

#
# process data - optodes --------------------------------------------------
import_optode_data = function(FILEPATH){
  filePaths_spectra <- list.files(path = FILEPATH,pattern = "*hr.csv", full.names = TRUE)
  spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
    df <- read.csv(path, header=TRUE, skip = 4, check.names = F)
    df %<>%
      rownames_to_column("timestep") %>% 
      pivot_longer(-timestep, names_to = "optode_disc_number", values_to = "do_mg_L") %>% 
      mutate(time_minutes = as.numeric(timestep) * 5)
    df[["source"]] <- rep(path, nrow(df))
    df}))
}

import_optode_data_2wk = function(PATH){
  
  dat = read.csv(PATH, check.names = F)
  dat %>% 
    pivot_longer(-date_time, names_to = "optode_disc_number", values_to = "do_mg_L") %>% 
    #mutate(timepoint = "2-wk") %>% 
    force()
  
}

process_optode_data = function(optode_data, optode_data_2wk, optode_map, sample_key){
  
 # sample_key= read.csv( "1-data/sample_key.csv")
  
 optode_24hr = 
    optode_data %>% 
    mutate(start_date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           start_date = lubridate::ymd(start_date)) %>% 
    dplyr::select(-source) %>% 
    left_join(optode_map %>% dplyr::select(start_date, optode_disc_number, sample_name) %>% 
                mutate(start_date = lubridate::ymd(start_date))) 
 
 optode_2wk = 
   optode_data_2wk %>% 
   mutate(date_time = lubridate::mdy_hm(date_time)) %>% 
   separate(optode_disc_number, sep = "_", into = c("optode_disc_number", "sample_name"), extra = "merge") %>% 
   mutate(time_minutes = difftime(date_time, min(date_time), units = "mins"),
          time_minutes = as.numeric(time_minutes))
 
 optode_combined = 
   optode_24hr %>% 
   bind_rows(optode_2wk) %>% 
   left_join(sample_key) %>% 
   filter(!notes %in% "skip optode") %>% 
   mutate(location = factor(location, levels = c("upland-A", "upland-B", "transition-A", "wetland-A", "water"))) %>% 
   group_by(sample_name) %>% 
   mutate(minimum_do = min(do_mg_L),
          difference_do = minimum_do - 0,
          DO_corrected_mgL = case_when(grepl("-B", location) ~ do_mg_L,
                                        TRUE ~ do_mg_L - difference_do),
          # calculate rolling means
          DO_rolling_mgL = zoo::rollmean(DO_corrected_mgL, k = 7, fill = NA),
          DO_corrected_mgL = round(DO_corrected_mgL, 2),
          DO_rolling_mgL = round(DO_rolling_mgL,2)) %>% 
   dplyr::select(timestep, time_minutes, start_date, sample_name, site, location, timepoint, DO_corrected_mgL, DO_rolling_mgL, notes) %>% 
   arrange(desc(DO_rolling_mgL)) %>% 
   force()
   
 optode_combined

}

#
# process data - WEOC -----------------------------------------------------
import_weoc_data = function(FILEPATH, PATTERN){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))
  
}
process_weoc = function(weoc_data, analysis_key, dry_weight){
  
  npoc_processed = 
    weoc_data %>% 
    # remove skipped samples
    filter(!`Sample ID` %in% "skip") %>% 
    # keep only relevant columns and rename them
    dplyr::select(`Sample Name`, `Result(NPOC)`) %>% 
    rename(analysis_ID = `Sample Name`,
           npoc_mgL = `Result(NPOC)`) %>% 
    # keep only sampple rows 
    filter(grepl("DOC_", analysis_ID)) %>% 
    # join the analysis key to get the sample_label
    left_join(analysis_key) %>%
    # do blank/dilution correction
    mutate(blank_mgL = case_when(sample_name == "blank-filter" ~ npoc_mgL)) %>% 
    fill(blank_mgL, .direction = c("up")) %>% 
    mutate(NPOC_dilution = as.numeric(NPOC_dilution),
           npoc_corr_mgL = (npoc_mgL-blank_mgL) * NPOC_dilution) %>% 
    # join gwc and subsampling weights to normalize data to soil weight
        left_join(dry_weight) %>% 
        mutate(npoc_ug_g = npoc_corr_mgL * ((water_g + soilwater_g)/od_g),
               npoc_ug_g = round(npoc_ug_g, 2)) %>% 
        dplyr::select(sample_name, npoc_corr_mgL, npoc_ug_g) %>% 
    force()
  
  npoc_samples = 
    npoc_processed %>% 
    filter(grepl("anoxia", sample_name))
  
  npoc_samples
}

#
# process data - ions -----------------------------------------------------
## import 
import_ions_data = function(FILEPATH){
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  
  filePaths <- list.files(path = FILEPATH, pattern = ".csv", full.names = TRUE)
  
  ions_raw = do.call(bind_rows, lapply(filePaths, function(path){
    # then add a new column `source` to denote the file name
    df <- read.csv(path, skip = 2)
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
}

## process

# `process_data`: this function will assign ions and tidy the dataframe
# input parameters are (a) the dataframe being cleaned and (b) the ions in question.

process_ions_data = function(ions_raw, IONS, dry_weight){
  
  # The input data are in shitty, non-tidy format, 
  # with multi-line headers and multiple chunks of data per dataframe.  
  # This function assigns the ions and turns it into tidy format, 
  # then cleans/processes the dataframe
  
  # a. assign ions ----
  
  # identify the rows that contain ions names
  label_rows = which(grepl(paste(IONS, collapse = "|"), ions_raw$Time))
  
  # make this a dataframe/tibble
  label_rows_df = 
    label_rows %>% 
    as_tibble() %>%
    rename(Row_number = value) %>% 
    mutate(label = TRUE, 
           Row_number = as.character(Row_number))
  
  # now join this to the dataframe
  data_new = 
    ions_raw %>% 
    rownames_to_column("Row_number") %>% 
    left_join(label_rows_df) %>% 
    mutate(Ion = case_when(label ~ Amount)) %>% 
    # ^ this pulls the Ion name only for certain rows
    # use fill() to down-fill the values
    # it will down-fill until it hits the next non-empty cell
    # therefore, make sure to include ALL ion names in the IONS parameter
    fill(Ion) %>% 
    dplyr::select(-Row_number, -label)
  
  # the dataframe now has all the ions assigned to each row
  # but it is still horribly untidy
  
  # b. clean the dataframe -----
  # preliminary processing to make it tidy
  data_new_processed = 
    data_new %>% 
    janitor::clean_names() %>% 
    filter(!notes %in% "skip") %>% 
    mutate_all(na_if,"") %>% 
    filter(!is.na(no)) %>% 
    dplyr::select(name, ion, amount) %>% 
    rename(amount_ppm = amount) %>% 
    mutate(amount_ppm = as.numeric(amount_ppm)) %>% 
    mutate(ion = str_remove_all(ion, " UV")) %>% 
    filter(!is.na(name)) %>% 
    filter(!grepl("Name", name)) %>% 
    mutate(type = case_when(grepl("SB", name) ~ "blank",
                            grepl("Blank", name) ~ "blank",
                            grepl("Std", name) ~ "standard",
                            grepl("water", name, ignore.case = T) ~ "water",
                            str_detect(name, "[0-9]x") ~ "calibration",
                            TRUE ~ "sample")) %>% 
    filter(ion %in% c("Nitrate", "Chloride", "Sulfate", "Phosphate")) %>% 
    force()
  
  # c. finishing touches ----
  samples = 
    data_new_processed %>% 
    filter(type == "sample") %>% 
    mutate(sample_name = paste0("anoxia_", str_pad(name, 3, pad = "0"))) %>% 
    dplyr::select(sample_name, ion, amount_ppm) %>% 
    left_join(dry_weight) %>% 
    mutate(amount_ug_g = amount_ppm * ((water_g + soilwater_g)/od_g),
           amount_ug_g = round(amount_ug_g, 2)) %>% 
    dplyr::select(sample_name, ion, amount_ppm, amount_ug_g) %>% 
    force()
  # filter blanks are all below detect, so no need to blank-correct
  # samples were not diluted, so no need to dilution-correct
  
  calibration = 
    data_new_processed %>% 
    filter(type == "calibration")
  
  # d. output ----
  list(samples = samples)
  
}


#

# process data - pH -------------------------------------------------------
process_pH = function(pH_data){
  #pH_processed = 
  pH_data %>% 
    dplyr::select(sample_name, pH) %>% 
    mutate(pH = as.numeric(pH)) %>% 
    filter(grepl("anoxia", sample_name))
  
}
  