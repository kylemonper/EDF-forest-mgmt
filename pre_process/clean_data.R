#########################
## function for cleaning/ formating data 
## in preparation for discounting

library(tidyverse)
options(scipen = 999)


clean_data <- function(data) {

  
  plot_all <- data
  ### clean harvest data ####
  ## remove na's, and duplicates from pre/post
  
  clean_harvest_data <- function(column) {
    
    plot_all[,column] <-  if_else(is.na(plot_all$complete_cpa), 0, unlist(plot_all[,column]))
    plot_all[,column] <- if_else(unlist(plot_all[column]) > 0 & plot_all$section == "post" ,0, unlist(plot_all[,column]))
    
    return(unlist(plot_all[,column]))
    
  }
  
  
  plot_all$Merch_Carbon_Removed <- clean_harvest_data("Merch_Carbon_Removed")
  plot_all$Carbon_Released_From_Fire <- clean_harvest_data("Carbon_Released_From_Fire")
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
