

library(tidyverse)
library(googlesheets4)

process_sir = function(sir_data){
  #sir_data_processed = 
  sir_data %>% 
    filter(!notes %in% "skip") %>% 
    filter(!is.na(type)) %>% 
    mutate_at(vars(contains("egm")), as.numeric) %>% 
    mutate_at(vars(contains("weight")), as.numeric) %>% 
    mutate(glucose_ug = as.numeric(glucose_ug),
           respiration = egm_CO2_ppm - egm_ambient_ppm,
           weight_dry_g = weight_vial_and_dry_soil_g - weight_empty_vial_g,
           respiration_g = respiration/weight_dry_g,
           glucose_ug_g = glucose_ug/weight_dry_g)
}

plot_sir_cal_curve = function(sir_data_processed){
  sir_data_processed %>% 
    ggplot(aes(x = glucose_ug_g/1000, y = respiration_g, color = type, fill = type))+
    #  geom_point(size = 3)+ 
    geom_line(size = 1)+
    geom_point(size = 5, shape = 21, stroke = 1, color = "white")+
    scale_color_manual(values = soilpalettes::soil_palette("podzol", 5))+
    scale_fill_manual(values = soilpalettes::soil_palette("podzol", 5))+
    annotate("text", label = "Transition, A horizon", x = 16, y = 850, size = 5)+
    annotate("text", label = "Upland, A horizon", x = 8, y = 550, size = 5)+
    annotate("text", label = "Upland, B horizon", x = 5, y = 150, size = 5)+
    labs(title = "SIR calibration curve \n",
         x = "Glucose added, mg/g soil",
         y = expression(bold("CO"[2] * " produced, ppm/g-hr")))+
    theme_kp()+
    theme(legend.position = "none")
}

compute_sir_biomass = function(sir_data_processed){
  # calculate microbial biomass from calibration curves
  # PV=nRT
  mmol_air = ((1*40)/(82.05*(25+273)))*1000
  
  
  sir_data_biomass = 
    sir_data_processed %>% 
    group_by(type) %>% 
    dplyr::summarise(CO2_ppm = max(respiration_g)) %>% 
    ungroup() %>% 
    mutate(CO2_percent = CO2_ppm / 10000,
           CO2_mL = CO2_percent * 40,
           CO2_mL_100g = CO2_mL * 100,
           #CO2_mL = (CO2_ppm_g/1000000) * 40,
           biomass_mg_100g = (40.04 * CO2_mL_100g) + 0.37,
           biomass_ug_100g = biomass_mg_100g/1000) %>% 
    dplyr::select(type, biomass_ug_100g)
  # gives: transition A: 14.0, upland-A: 9.36, upland-B: 3.95 ug/100g
}