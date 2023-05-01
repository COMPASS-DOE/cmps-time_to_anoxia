

library(tidyverse)
library(googlesheets4)


sir_data = read_sheet("1aKSpp3FVN90XfWjUt-P2Zwtcokhos1g3KzMpsnjfPls", sheet = "egm") %>% mutate_all(as.character)
sir_data_processed = 
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



sir_data %>% 
  ggplot(aes(y = CO2, x = glucose_conc_uM))+
  geom_point()+
  facet_wrap(~type)

sir_data_processed %>% 
  ggplot(aes(y = CO2_diff, x = glucose_conc_uM))+
  geom_point()+
  facet_wrap(~type, scales = "free")

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
         biomass_ug_100g = biomass_mg_100g/1000)
# gives: transition 
