#################################################
###       Final Proccessing Script
###
### This code uses function reads in relevent pre and post
### tables along with the optimization selections
### for calculating change in carbon sequestration 
### over the course of the 32 years of treatments
# 

###            ::::::::::::::::::::::::::::::::::::                ###
# the major steps in this workflow have been turned into 
# seperate functions/scripts to help clarify the general workflow.
# see sourced scripts for more detail into the workings of each step
###            ::::::::::::::::::::::::::::::::::::                ###


### library ####

library(tidyverse)
library(beepr)
library(RODBC)
source("pre_process/clean_data.R")
source("pre_process/add_chip_decay.R")
source("pre_process/add_harvest_decay.R")
source("pre_process/infer_annual_smoke.R")
source("pre_process/add_discounting.R")
source("pre_process/discount_all.R")
source("pre_process/get_relative.R")
source("pre_process/calc_fire_emissions.R")


options(scipen = 999)



#### read in subsetted data
##~ see pre_process/subset_data.R to see/ alter 
##~ which Access files are read in and which variables are kept from them
all_data <- read_csv("output_data/all_data.csv")


### reformat/ clean in preperation for discounting
cleaned_data <- clean_data(all_data)

#### this step: 
#~ 1) adds annual mean fire emissions
#~~~~ adds in fire probabilites (from CALFIRE fire probability maps) and 
#~~~~ converts the pot_smoke variable from pm2.5 emissions to C emissions
#~~~~ multiply these together to get average annual c emissions from fire
#~ 2) to account for regrowth after fire by:
#~~~~ calculating annual growth rate after a cc (similar to fire)
plot_all <- add_fire_emissions(cleaned_data)


### discount

#~ set allocation of chips % to decay (non-proccessed) vs biochar
#### might be worth looking into using the carbon_hrv data instead.....
decay_pct <- 1
char_pct <- 0

#~ set discount rate
dc_rate <- 0.05


# test functions:
# test <- plot_all %>%
#   filter(ID == 148 & rxpackage == "003")
# 
# tmp_disc <- discount_all(test)
# tmp <- get_relative(tmp_disc)


### discount all packages
## this will take ~2 hours
Sys.time()
all_discounted_wFire <- discount_all(plot_all)
Sys.time()
write_csv(all_discounted_wFire, "output_data/all_discounted_wFire.csv")
beep(3)

all_discounted_wFire <- read_csv("output_data/all_discounted_wFire.csv")
relative_carb <- get_relative(all_discounted_wFire)
write_csv(relative_carb, "output_data/relative_carb_fire.csv")


##### old runs ####
# 
# #### repeat but with no discounting
# dc_rate <- 0
# Sys.time()
# all_discounted_og_00 <- discount_all(plot_all)
# Sys.time()
# write_csv(all_discounted_og_00, "all_discounted_og_00.csv")
# 
# all_discounted_og_00 <- read_csv("all_discounted_og_00.csv")
# all_discounted_og_05 <- read_csv("all_discounted_og_05.csv")
# 
# #### get relative cost & carbon
# relative_carb_og_00 <- get_relative(all_discounted_og_00)
# relative_carb_og_05 <- get_relative(all_discounted_og_05)
# 
# ### last write: 2/25/2020
# write_csv(relative_carb_og_00, "relative_carb_og_00.csv")
# write_csv(relative_carb_og_05, "relative_carb_og_05.csv")
######







