#########################################
###  Subset data from Access files
###

library(tidyverse)
library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)


##################
## read in data ##
##################

#get harvest cost/ relevant acres
conn <- odbcConnectAccess2007("input/optimizer_results_cycle_1_MaxMerch_Carbon_Stored_2019-12-02_09-43-16.accdb")
acres <- sqlFetch(conn, "stand_costs_revenue_volume_sum_by_rxpackage", as.is = T)
cost <- sqlFetch(conn, "product_yields_net_rev_costs_summary_by_rx", as.is = T)
odbcCloseAll()



#carbon data
conn <- odbcConnectAccess2007("input/PREPOST_FVS_CARBON.ACCDB")
pre_carb <- sqlFetch(conn, "PRE_FVS_CARBON", as.is = T)
post_carb <- sqlFetch(conn, "POST_FVS_CARBON", as.is = T)
odbcCloseAll()


# harvested carbon data
conn <- odbcConnectAccess2007("input/PREPOST_FVS_HRV_CARBON.ACCDB")
pre_carb_hrv <- sqlFetch(conn, "PRE_FVS_HRV_CARBON", as.is = T)
post_carb_hrv <- sqlFetch(conn, "POST_FVS_HRV_CARBON", as.is = T)
odbcCloseAll()

#### get lat long data
conn <- odbcConnectAccess2007("input/master.mdb")
plot_m <- sqlFetch(conn, "plot", as.is = T)
cond_m <- sqlFetch(conn, "cond", as.is = T)
ftype <- sqlFetch(conn, "CEC_ftype", as.is = T)
odbcCloseAll()

names(ftype)[1] <- "fortypcd"

type <- ftype %>% 
  select(fortypcd, MEANING)

plot_sel <- plot_m %>%
  select(biosum_plot_id, lat, lon, elev)

cond_sel <- cond_m %>%
  select(biosum_cond_id, biosum_plot_id, fortypcd) %>% 
  left_join(type)



cond_lat_lon <- left_join(cond_sel, plot_sel)



##############################
#### select columns/join ####
############################

## harvest cost per acre
cost_sel <- cost %>%
  mutate(complete_cpa = harvest_onsite_cpa + haul_chip_cpa + haul_merch_cpa) %>%
  select(biosum_cond_id, rxpackage, rxcycle, complete_cpa, haul_chip_cpa, haul_merch_cpa, harvest_onsite_cpa, chip_yield_gt, merch_yield_cf, merch_yield_gt, merch_val_dpa, chip_val_dpa) %>%
  arrange(biosum_cond_id, rxpackage, rxcycle)


## stand carbon
pre_carb_sel <- pre_carb %>%
  select(biosum_cond_id, fvs_variant, rxpackage, rxcycle, Total_Stand_Carbon, Aboveground_Total_Live)

post_carb_sel <- post_carb %>%
  select(biosum_cond_id, fvs_variant, rxpackage, rxcycle, Total_Stand_Carbon, Aboveground_Total_Live)



## harvest carbon
pre_hrv_sel <- pre_carb_hrv %>%
  select(biosum_cond_id, rxpackage, rxcycle, Merch_Carbon_Removed)

post_hrv_sel <- post_carb_hrv %>%
  select(biosum_cond_id, rxpackage, rxcycle, Merch_Carbon_Removed)



## join carbon tables
pre_carbon_tot <- left_join(pre_carb_sel, pre_hrv_sel)
post_carbon_tot <- left_join(post_carb_sel, post_hrv_sel)



# join in acreage
pre_carbon_tot <- left_join(acres[,c("biosum_cond_id", "acres", "owngrpcd")], pre_carbon_tot)
post_carbon_tot <- left_join(acres[,c("biosum_cond_id", "acres", "owngrpcd")], post_carbon_tot)

### join to econ
pre_full <- left_join(pre_carbon_tot, cost_sel)  %>%
  distinct()
post_full <- left_join(post_carbon_tot, cost_sel)  %>%
  distinct()



#### give cond_id new numbers that will make them easier to join together after reimporting data
plots <- unique(post_full$biosum_cond_id)
new_id <- data.frame(biosum_cond_id = plots, ID = 1:length(plots))


plots_loc <- left_join(new_id, cond_lat_lon)


write_csv(plots_loc, "plot_loc.csv")
#test <- read_csv("plot_loc.csv")


# name pre and post for future reference
pre_full$section <- "pre"
post_full$section <- "post"


## full df 
all_data <- bind_rows(pre_full,post_full)

all_data <- left_join(all_data, new_id) 

## clean environment of everything except the new full dataset
rm(list = ls())

write_csv(all_data, "output_data/all_data.csv")
#all_data <- read_csv("all_data.csv")