###########################
## Function to add decay for {chipped product pathways}
## right now only looking at decay for chips, not products
library(tidyverse)
options(scipen = 999)


#### decay rates for merch and non-merch (from Bodie Cabiyo)
non_merch <- read_delim("input/chip_pathways.txt", delim = ",")


#### wood chip pathways + decay
add_chip_decay <- function(df, pathway, decay_pct){
  
  # fraction that goes to this pathway
  fraction <- df %>% 
    mutate(chip_carbon = chip_carbon * decay_pct)
  
  ### if a harvest occured, start a time count of 1, if not == NA
  chip_time <- fraction %>% 
    mutate(cycle1_chip = if_else(chip_carbon > 0 & rxcycle == 1 & section == "pre", 1, NA_real_),
           cycle2_chip = if_else(chip_carbon > 0 & rxcycle == 2 & section == "pre", 1, NA_real_),
           cycle3_chip = if_else(chip_carbon > 0 & rxcycle == 3 & section == "pre", 1, NA_real_),
           cycle4_chip = if_else(chip_carbon > 0 & rxcycle == 4 & section == "pre", 1, NA_real_))
  
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
  
  
  chip <- chip_time %>% 
    mutate(cycle1_time = cum.na(cycle1_chip),
           cycle2_time = cum.na(cycle2_chip),
           cycle3_time = cum.na(cycle3_chip),
           cycle4_time = cum.na(cycle4_chip))
  
  ## before joining in decay rate, select the path we want to include
  chip_decay <- non_merch %>% 
    filter(path == pathway) %>% 
    mutate(Year = Year + 1) %>% ### start at year 1 not 0
    select(Year, embodied)
  
  
  ### add in decay for each cycle
  chip <- left_join(chip, chip_decay, by = c("cycle1_time" = "Year")) %>% 
    rename("cycle1_rate" = "embodied")
  chip <- left_join(chip, chip_decay, by = c("cycle2_time" = "Year")) %>% 
    rename("cycle2_rate" = "embodied")
  chip <- left_join(chip, chip_decay, by = c("cycle3_time" = "Year")) %>% 
    rename("cycle3_rate" = "embodied")
  chip <- left_join(chip, chip_decay, by = c("cycle4_time" = "Year")) %>% 
    rename("cycle4_rate" = "embodied")
  
  ## calculate decay based on decay rate * harvested for each cycle
  decay <- function(time, rate) {
    
    harvested <- chip$chip_carbon[which(chip[,time] == 1)]
    decay <- harvested * chip[,rate]
    if(nrow(decay) == 0) {
      decay <- as.vector(rep(NA_real_, nrow(chip)))
    }
    return(as_vector(decay))
  }
  
  
  chip_loss <- chip %>% 
    mutate(decay1 = decay("cycle1_time", "cycle1_rate"),
           decay2 = decay("cycle2_time", "cycle2_rate"),
           decay3 = decay("cycle3_time", "cycle3_rate"),
           decay4 = decay("cycle4_time", "cycle4_rate"))
  
  
  summed <- rowSums(chip_loss[,c("decay1","decay2","decay3","decay4")], na.rm = T)
  
  return(summed)
}