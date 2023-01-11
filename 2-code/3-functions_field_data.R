

troll_data = read.csv("1-data/field_data/230106_troll_oxygen_peak_data.csv")

troll_processed = 
  troll_data %>% 
  mutate(datetime = lubridate::ymd_hms(datetime))

troll_rates = 
  troll_processed %>% 
  group_by(location, peak) %>% 
  dplyr::mutate(MAX_DO_TIME = case_when(do_mgl == max(do_mgl) ~ datetime),
                KEEP = datetime >= max(MAX_DO_TIME, na.rm = TRUE)) %>% 
  filter(KEEP) %>% 
 # dplyr::mutate(MIN_DO_TIME = case_when(do_mgl == min(do_mgl) ~ datetime)) %>% 
  dplyr::summarise(max_do = max(do_mgl),
                   min_do = min(do_mgl),
                   max_do_time = min(hours),
                   min_do_time = max(hours)) %>% 
  #mutate(d_time_hr = as.numeric(difftime(min_do_time, max_do_time))) %>% 
  force()


troll_rates_labels = 
  troll_rates %>% 
  mutate(y = case_when(location == "TR" ~ 7,
                       location == "WC" ~ 6),
         label = paste(min_do_time - max_do_time, "hr"))

troll_processed %>% 
  ggplot(aes(x = hours, y = do_mgl,
             color = as.character(peak)))+
  geom_line(size = 1)+
  facet_wrap(~location, scales = "free")




troll_processed %>% 
  ggplot(aes(x = hours, y = do_mgl,
             color = location))+
  geom_line(size = 1)+
  scale_color_manual(breaks = c("TR", "WC"),
                     labels = c("transition", "wetland"),
                     values = c("red", "blue"))+
  labs(x = "Hours",
       y = "Dissolved oxygen, mg/L",
       color = "")+
  facet_wrap(~peak, scales = "free_x")



troll_processed %>% 
  ggplot(aes(x = hours, y = p_h_orp,
             color = location))+
  geom_line(size = 1)+
  labs(title = "Redox potential")+
  facet_wrap(~peak, scales = "free")


troll_processed %>% 
  ggplot(aes(x = hours, y = do_mgl, color = location))+
  geom_line(size = 1)+
  
  geom_segment(data = troll_rates_labels, 
               aes(x = max_do_time, xend = min_do_time,
                   y = y, yend = y))+
  geom_text(data = troll_rates_labels, hjust = 0,
             aes(x = max_do_time, y = y + 0.35, label = label))+
  facet_wrap(~peak, scales = "free_x")





# comparing rates
troll_rates2 = 
  troll_rates %>% 
  group_by(location, peak) %>% 
  dplyr::mutate(slope_mg_L_hr = (max_do - min_do)/(min_do_time - max_do_time)) %>% 
  mutate(time_to_anoxia_hr = (min_do_time - max_do_time)) %>% 
  rename(
         transect = location,
         replicate = peak) %>% 
  mutate(replicate = as.character(replicate)) %>% 
  mutate(experiment = "FIELD")




optode2 = 
  optode_do_summary = 
  optode_combined2 %>% 
  filter(!is.na(location)) %>% 
  filter(location != "upland-B") %>% 
  filter(timepoint == "24-hour") %>% 
  group_by(sample_name) %>% 
  filter(slope == 0 & slope2 == 0) %>% 
  group_by(sample_name) %>% 
  slice(which.min(time_hr)) %>% 
  dplyr::select(sample_name, location, timepoint, transect, horizon, time_hr) %>% 
  mutate(time_hr = case_when(sample_name == "anoxia_047" ~ 24, TRUE ~ time_hr)) %>% 
  rename(replicate = sample_name) %>% 
  mutate(replicate = as.character(replicate)) %>% 
  mutate(slope_mg_L_hr = (8.5-0)/time_hr) %>% 
  rename(time_to_anoxia_hr = time_hr) %>% 
  mutate(experiment = "LAB")

all = 
  troll_rates2 %>% 
  bind_rows(optode2) %>% 
  mutate(transect = recode(transect, "TR" = "transition", "WC" = "wetland")) %>% 
  mutate(transect = factor(transect, levels = c("upland", "transition", "wetland")))


all %>% 
  ggplot(aes(x = transect, y = slope_mg_L_hr))+
  geom_jitter(width = 0.1, size = 3)+
  facet_wrap(~experiment)

all %>% 
  ggplot(aes(x = transect, y = time_to_anoxia_hr))+
  geom_jitter(width = 0.1, size = 3)+
  facet_wrap(~experiment)
