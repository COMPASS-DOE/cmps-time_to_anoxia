library(tidyverse)
library(googlesheets4)
library(FME)


# -------------------------------------------------------------------------

compute_rate_ratio = function(){
  
  # set the function
  solveCOM <- function(t, state, pars){
    with(as.list(c(state, pars)),{
      r1<- 0.5* Oxy/(Oxy+10)*CHO/(CHO+10)  #denitrification     
      dOxy <--r1*BM1-alpha*0.5*Oxy  #stoichiometry from van den Berg
      dCHO <--r1*BM1
      dBM1 <- r1*BM1
      return(list(c(dOxy, dCHO,dBM1)))
    })
  }
  
  times=seq(0,30, by=0.05)
  state<-c(Oxy=8,CHO=50, BM1=1)
  
  # apply this to different scenarios
  pars1=list (alpha=0)
  pars2=list (alpha=0.1)
  pars3=list (alpha=0.2)
  pars4=list (alpha=0.5)
  pars5=list (alpha=1)
  pars6=list (alpha=2)
  pars7=list (alpha=5)
  
  ox1<-ode(y=state, times=times, func=solveCOM, parms=pars1)
  output1<-data.frame(ox1[,1:4])
  
  ox2<-ode(y=state, times=times, func=solveCOM, parms=pars2)
  output2<-data.frame(ox2[,1:4])
  
  ox3<-ode(y=state, times=times, func=solveCOM, parms=pars3)
  output3<-data.frame(ox3[,1:4])
  
  ox4<-ode(y=state, times=times, func=solveCOM, parms=pars4)
  output4<-data.frame(ox4[,1:4])
  
  ox5<-ode(y=state, times=times, func=solveCOM, parms=pars5)
  output5<-data.frame(ox5[,1:4])
  
  ox6<-ode(y=state, times=times, func=solveCOM, parms=pars6)
  output6<-data.frame(ox6[,1:4])
  
  ox7<-ode(y=state, times=times, func=solveCOM, parms=pars7)
  output7<-data.frame(ox7[,1:4])
  
  combined = bind_rows(
    output1 %>% mutate(ratio = 0),
    output2 %>% mutate(ratio = 0.1),
    output3 %>% mutate(ratio = 0.2),
    output4 %>% mutate(ratio = 0.5),
    output5 %>% mutate(ratio = 1),
    output6 %>% mutate(ratio = 2),
    output7 %>% mutate(ratio = 5)
  ) %>% 
    mutate(ratio = as.character(ratio))
  
  # plot
  
  ratio_palette = c("#7f3b08", "#b35806", "#e08214", "#fdb863","#b2abd2", "#8073ac", "#542788")
  
  combined %>% 
    ggplot(aes(x = time, y = Oxy, color = ratio))+
    geom_line(linewidth = 1.5)+
    scale_color_manual(values = ratio_palette)+
    labs(x = "Time (hr)",
         y = "DO (mg/L)",
         color = expression("k" [L] * "/V"[max]))+
    theme_bw(base_size = 22)+
    theme(legend.position = c(0.7, 0.7),
          panel.grid = element_blank())
  
  
  combined %>% 
    ggplot(aes(x = time, y = Oxy, color = ratio))+
    geom_line(linewidth = 1.5)+
    scale_color_manual(values = ratio_palette)+
    labs(x = "Time (hr)",
         y = "DO (mg/L)",
         color = expression("k" [L] * "/V"[max]))+
    theme_bw(base_size = 22)+
    theme(legend.position = "none",
          panel.grid = element_blank())
  
  # microbial end
  combined %>% 
    filter(ratio == 0) %>% 
    ggplot(aes(x = time, y = Oxy))+
    geom_line(linewidth = 1.5, color = "#7f3b08")+
    labs(x = "Time (hr)",
         y = "DO (mg/L)",
         color = expression("k" [L] * "/V"[max]))+
    theme_bw(base_size = 22)+
    theme(legend.position = c(0.7, 0.7),
          panel.grid = element_blank())
  
  
  combined %>% 
    filter(ratio == 0) %>% 
    ggplot(aes(x = time, y = Oxy))+
    geom_line(linewidth = 1.5, color = "#7f3b08")+
    labs(x = "Time (hr)",
         y = "DO (mg/L)",
         color = expression("k" [L] * "/V"[max]))+
    theme_void()+
    theme(panel.border = element_rect(fill = NA, color = "black", linewidth = 1))

    theme(legend.position = "none",
          panel.grid = element_blank())
  
  
  
  # abiotic end
  combined %>% 
    filter(ratio == 5) %>% 
    ggplot(aes(x = time, y = Oxy))+
    geom_line(linewidth = 1.5, color = "#542788")+
    labs(x = "Time (hr)",
         y = "DO (mg/L)",
         color = expression("k" [L] * "/V"[max]))+
    theme_bw(base_size = 22)+
    theme(legend.position = c(0.7, 0.7),
          panel.grid = element_blank())
  
  
  combined %>% 
    filter(ratio == 5) %>% 
    ggplot(aes(x = time, y = Oxy))+
    geom_line(linewidth = 1.5, color = "#542788")+
    labs(x = "Time (hr)",
         y = "DO (mg/L)",
         color = expression("k" [L] * "/V"[max]))+
    theme_void()+
    theme(panel.border = element_rect(fill = NA, color = "black", linewidth = 1))
  
  theme(legend.position = c(0.7, 0.7),
          panel.grid = element_blank())
  
  # export 600 x 500
  
  
}


