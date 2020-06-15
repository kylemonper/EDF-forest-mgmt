### function to add decay of harvested wood
library(tidyverse)
options(scipen = 999)

merch_decay <- read_csv("input/softwood_lumber_decay.csv")


## product decay
add_harvest_decay <- function(df) {
  
  ### if a harvest occured, start a time count of 1, if not == NA
  merch_time <- df %>% 
    mutate(cycle1_merch = if_else(merch_carbon > 0 & rxcycle == 1 & section == "pre", 1, NA_real_),
           cycle2_merch = if_else(merch_carbon > 0 & rxcycle == 2 & section == "pre", 1, NA_real_),
           cycle3_merch = if_else(merch_carbon > 0 & rxcycle == 3 & section == "pre", 1, NA_real_),
           cycle4_merch = if_else(merch_carbon > 0 & rxcycle == 4 & section == "pre", 1, NA_real_))
  
  ## next step: how to add +1 to each row to have "time since harvest"
  cum.na <- function(x){
    # set NA's to 0
    x[which(is.na(x))] <- 0
    # set cumulative sum twice to increase each element n+1 
    x <- cumsum(cumsum(x))
    # reset the 0 values to NA again
    x[which(x < 1)] <- NA_real_
    return(x)
  }
  
  
  merch <- merch_time %>% 
    mutate(cycle1_time = cum.na(cycle1_merch),
           cycle2_time = cum.na(cycle2_merch),
           cycle3_time = cum.na(cycle3_merch),
           cycle4_time = cum.na(cycle4_merch))
  
  
  ### add in decay for each cycle
  merch <- left_join(merch, merch_decay, by = c("cycle1_time" = "year")) %>% 
    rename("cycle1_rate" = "decay")
  merch <- left_join(merch, merch_decay, by = c("cycle2_time" = "year")) %>% 
    rename("cycle2_rate" = "decay")
  merch <- left_join(merch, merch_decay, by = c("cycle3_time" = "year")) %>% 
    rename("cycle3_rate" = "decay")
  merch <- left_join(merch, merch_decay, by = c("cycle4_time" = "year")) %>% 
    rename("cycle4_rate" = "decay")
  
  ## calculate decay based on decay rate * harvested for each cycle
  decay <- function(time, rate) {
    
    harvested <- merch$merch_carbon[which(merch[,time] == 1)]
    decay <- harvested * merch[,rate]
    if(nrow(decay) == 0) {
      decay <- as.vector(rep(NA_real_, nrow(merch)))
    }
    return(as_vector(decay))
  }
  
  
  merch_loss <- merch %>% 
    mutate(decay1 = decay("cycle1_time", "cycle1_rate"),
           decay2 = decay("cycle2_time", "cycle2_rate"),
           decay3 = decay("cycle3_time", "cycle3_rate"),
           decay4 = decay("cycle4_time", "cycle4_rate"))
  
  
  summed <- rowSums(merch_loss[,c("decay1","decay2","decay3","decay4")], na.rm = T)
  
  return(summed)
  
}