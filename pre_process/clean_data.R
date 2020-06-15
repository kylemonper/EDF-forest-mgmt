#########################
## function for cleaning/ formating data 
## in preparation for discounting

library(tidyverse)
options(scipen = 999)


clean_data <- function(data) {
  # filter down to just one plot
  plot_all <- all_data %>% 
    mutate(time = rxcycle) 
  
  #### match cycle to year ####

    plot_all$time = ifelse(plot_all$time==1 & plot_all$section == "pre", 0, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==2 & plot_all$section == "pre", 10, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==3 & plot_all$section == "pre", 20, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==4 & plot_all$section == "pre", 30, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==1 & plot_all$section == "post", 1, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==2 & plot_all$section == "post", 11, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==3 & plot_all$section == "post", 21, plot_all$time) 
    plot_all$time = ifelse(plot_all$time==4 & plot_all$section == "post", 31, plot_all$time) 
    

  #make sure time is numeric
  plot_all$time <- as.numeric(plot_all$time)
  
  ### clean harvest data ####
  ## remove na's, and duplicates from pre/post
  
  clean_harvest_data <- function(column) {
    
    plot_all[,column] <-  if_else(is.na(plot_all$complete_cpa), 0, unlist(plot_all[,column]))
    plot_all[,column] <- if_else(unlist(plot_all[column]) > 0 & plot_all$section == "post" ,0, unlist(plot_all[,column]))
    
    return(unlist(plot_all[,column]))
    
  }
  
  
  plot_all$Merch_Carbon_Removed <- clean_harvest_data("Merch_Carbon_Removed")
  plot_all$chip_yield_gt <- clean_harvest_data("chip_yield_gt")
  plot_all$merch_yield_gt <-  clean_harvest_data("merch_yield_gt")
  plot_all$haul_chip_cpa <- clean_harvest_data("haul_chip_cpa")
  plot_all$haul_merch_cpa <- clean_harvest_data("haul_merch_cpa")
  plot_all$harvest_onsite_cpa <- clean_harvest_data("harvest_onsite_cpa")
  plot_all$chip_val_dpa <- clean_harvest_data("chip_val_dpa")
  plot_all$merch_val_dpa <- clean_harvest_data("merch_val_dpa")
  plot_all$complete_cpa <- clean_harvest_data("complete_cpa") # keep complete_cpa last
  
  return(plot_all)
  
}
