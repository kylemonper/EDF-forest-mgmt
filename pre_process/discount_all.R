###########################
## loop through all sites/treatments to add decay/discounting
library(tidyverse)
source("pre_process/add_discounting.R")

## create function that applies the discount function  ^ to each plot+package 
discount_all <- function(df) {
  
  # pull out unique biosum ids
  uniq_biosum_ids <- unique(df$ID)
  
  # loop through everything
  final_total <- NULL
  final_plot <- NULL
  
  number_completed <- 0
  total_to_complete <- length(uniq_biosum_ids)
  
  for (i in 1:length(uniq_biosum_ids)){
    
    # select plot
    id <- uniq_biosum_ids[i]
    
    plot <- plot_all %>% 
      filter(ID == paste(id))
    
    # get all packages that pertain to this plot, loop through these, applying the discounting function to each
    uniq_packages <- unique(plot$rxpackage)
    
    for (j in 1:length(uniq_packages)){
      
      pkg <- uniq_packages[j]
      
      plot_package <- plot %>% 
        filter(rxpackage == pkg) 
      
      discounted <- add_discounting(plot_package) 
      
      final_plot <- rbind(final_plot, discounted)
      
      
    }
    
    final_total <- rbind(final_total, final_plot)
    final_plot <- NULL
    
    
    ### progress tracker
    number_completed = number_completed + 1
    if (number_completed %% 200 == 0 || number_completed == total_to_complete) {
      print(sprintf("Percentage completion: %.2f%%", (number_completed / length(uniq_biosum_ids)) * 100))
    }
    
  }
  
  return(final_total)
  
}