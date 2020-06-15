#################################################
###       Final Proccessing Script
###
### This code uses function reads in relevent pre and post
### tables along with the optimization selections
### for calculating change in carbon sequestration 
### over the course of the 32 years of treatments
# 

### library ####

library(tidyverse)
library(RODBC)
source("pre_process/clean_data.R")
source("pre_process/discount_all.R")
source("pre_process/get_relative.R")


options(scipen = 999)



#### read in subsetted data
##~ see pre_process/subset_data.R to see/ alter 
##~ which Access files are read in and which variables are kept from them

all_data <- read_csv("output_data/all_data.csv")

### reformat/ clean in preperation for discounting
plot_all <- clean_data(all_data)


### discount

#~ set allocation of chips % to decay (non-proccessed) vs biocahr
decay_pct <- 1
char_pct <- 0

#~ set discount rate
dc_rate <- 0.05


## test functions:
# test <- plot_all %>%
#   filter(ID == 1011)
# 
# tmp_disc <- discount_all(test)
# tmp <- get_relative(tmp_disc)


### discount all packages
## this will take ~2 hours
# Sys.time()
# all_discounted_og_05 <- discount_all(plot_all)
# Sys.time()
# write_csv(all_discounted_FullDecay, "all_discounted_og_05.csv")
# 
# 
# #### repeat but with no discounting
# dc_rate <- 0
# Sys.time()
# all_discounted_og_00 <- discount_all(plot_all)
# Sys.time()
# write_csv(all_discounted_og_00, "all_discounted_og_00.csv")


all_discounted_og_00 <- read_csv("all_discounted_og_00.csv")
all_discounted_og_05 <- read_csv("all_discounted_og_05.csv")


#### get relative cost & carbon
relative_carb_og_00 <- get_relative(all_discounted_og_00)
relative_carb_og_05 <- get_relative(all_discounted_og_05)


### last write: 2/25/2020
write_csv(relative_carb_og_00, "relative_carb_og_00.csv")
write_csv(relative_carb_og_05, "relative_carb_og_05.csv")







