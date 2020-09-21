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
  
  for (i in 1:total_to_complete){
    
    # select plot
    id <- uniq_biosum_ids[i]
    
    plot <- plot_all %>% 
      filter(ID == paste(id))
    
    # get all packages groups that pertain to this plot, loop through these, applying the discounting function to each
    ## this will need to be manually updated if new core packages are added (right now this includes 010, 020, 030, 040, 70, 99)
    package_groups <- list(c("010", "011", "012", "013", "014"), 
                           c("020", "021", "022", "023", "024"), 
                           c("030", "031", "032", "033", "034"),
                           c("040", "041", "042", "043", "044"),
                           c("700", "701", "702", "703", "704"),
                           c("999", "991", "992", "993", "994"))
    
    for (j in 1:length(package_groups)){
      
      
      plot_package <- plot %>% 
        filter(rxpackage %in% package_groups[[j]]) 
      
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
