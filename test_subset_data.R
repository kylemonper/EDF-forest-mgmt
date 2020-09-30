#### testing issue that not all cycles present for each ID


library(tidyverse)
library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)


##################
## read in data ##
##################


#get harvest cost/ relevant acres
conn <- odbcConnectAccess2007("c:/FIA_BioSum/edf_072020/optimizer/scenario2/db/optimizer_results.accdb")
cost <- sqlFetch(conn, "econ_by_rx_cycle", as.is = T)
odbcCloseAll()





cost_sel <- cost %>%
  select(biosum_cond_id, rxpackage, rxcycle, owngrpcd, "haul_chip_cpa" = chip_haul_cost_dpa, "haul_merch_cpa" = merch_haul_cost_dpa, "harvest_onsite_cpa" = harvest_onsite_cost_dpa, "chip_yield_gt" = chip_wt_gt, "merch_yield_cf" = merch_vol_cf, "merch_yield_gt" = merch_wt_gt, "merch_val_dpa" = merch_val_dpa, "chip_val_dpa" = chip_val_dpa) %>% 
  mutate(complete_cpa = harvest_onsite_cpa + haul_chip_cpa + haul_merch_cpa) %>% 
  filter(owngrpcd != 30)

#help w/ memory
rm(cost)



#carbon data
conn <- odbcConnectAccess2007("c:/FIA_BioSum/edf_072020/fvs/db/PREPOST_FVS_CARBON.ACCDB")
pre_carb <- sqlFetch(conn, "PRE_FVS_CARBON", as.is = T)
post_carb <- sqlFetch(conn, "POST_FVS_CARBON", as.is = T)
odbcCloseAll()



pre_carb_sel <- pre_carb %>%
  select(biosum_cond_id, fvs_variant, rxpackage, rxcycle, Year, Total_Stand_Carbon, Aboveground_Total_Live, Carbon_Released_From_Fire)

post_carb_sel <- post_carb %>%
  select(biosum_cond_id, fvs_variant, rxpackage, rxcycle, Year, Total_Stand_Carbon, Aboveground_Total_Live, Carbon_Released_From_Fire)

rm(post_carb, pre_carb)




# harvested carbon data
conn <- odbcConnectAccess2007("c:/FIA_BioSum/edf_072020/fvs/db/PREPOST_FVS_HRV_CARBON.ACCDB")
pre_carb_hrv <- sqlFetch(conn, "PRE_FVS_HRV_CARBON", as.is = T)
post_carb_hrv <- sqlFetch(conn, "POST_FVS_HRV_CARBON", as.is = T)
odbcCloseAll()

## harvest carbon
pre_hrv_sel <- pre_carb_hrv %>%
  select(biosum_cond_id, rxpackage, rxcycle, Merch_Carbon_Removed)

post_hrv_sel <- post_carb_hrv %>%
  select(biosum_cond_id, rxpackage, rxcycle, Merch_Carbon_Removed)

rm(pre_carb_hrv, post_carb_hrv)


## get fire data
conn <- odbcConnectAccess2007("c:/FIA_BioSum/edf_072020/fvs/db/PREPOST_FVS_POTFIRE.ACCDB")
pre_fire <- sqlFetch(conn, "PRE_FVS_POTFIRE", as.is = T)
post_fire <- sqlFetch(conn, "POST_FVS_POTFIRE", as.is = T)
odbcCloseAll()

pre_fire_sel <- pre_fire %>% 
  select(biosum_cond_id, rxpackage, rxcycle, Pot_Smoke_Mod, Pot_Smoke_Sev)

post_fire_sel <- post_fire %>% 
  select(biosum_cond_id, rxpackage, rxcycle, Pot_Smoke_Mod, Pot_Smoke_Sev)

rm(pre_fire, post_fire)


#### get lat long data
conn <- odbcConnectAccess2007("c:/FIA_BioSum/edf_072020/db/master.mdb")
plot_m <- sqlFetch(conn, "plot", as.is = T)
cond_m <- sqlFetch(conn, "cond", as.is = T)
#ftype <- sqlFetch(conn, "CEC_ftype", as.is = T)
odbcCloseAll()

# names(ftype)[1] <- "fortypcd"

# type <- ftype %>% 
#   select(fortypcd, MEANING, CEC_type)

plot_sel <- plot_m %>%
  select(biosum_plot_id, lat, lon, elev)

cond_sel <- cond_m %>%
  select(biosum_cond_id, biosum_plot_id, fortypcd, acres) # %>% 
#left_join(type)



cond_lat_lon <- left_join(cond_sel, plot_sel)



##############################
####       join          ####
############################

## join carbon tables
pre_carbon_tot <- left_join(pre_carb_sel, pre_hrv_sel)
post_carbon_tot <- left_join(post_carb_sel, post_hrv_sel)

## join to fire tables
pre_full <- left_join(pre_carbon_tot, pre_fire_sel)
post_full <- left_join(post_carbon_tot, post_fire_sel)


## clean env't to reduce memory load
# rm(list=setdiff(ls(), c("pre_full", "post_full", "cost_sel", "cond_lat_lon")))

cost_ids <- unique(cost_sel$biosum_cond_id)

pre_full_filt <- pre_full %>% filter(biosum_cond_id %in% cost_ids)
post_full_filt <- post_full %>% filter(biosum_cond_id %in% cost_ids)

### join to econ
pre_full <- left_join(pre_full_filt, cost_sel)  %>%
  distinct()
post_full <- left_join(post_full_filt, cost_sel)  %>%
  distinct()


ids <- unique(pre_full$biosum_cond_id)

df <- pre_full %>% 
  filter(biosum_cond_id == ids[1]) %>% 
  arrange(rxpackage, rxcycle)

#### give cond_id new numbers that will make them easier to join together after reimporting data
plots <- unique(post_full$biosum_cond_id)
new_id <- data.frame(biosum_cond_id = plots, ID = 1:length(plots))


plots_loc <- left_join(new_id, cond_lat_lon)

IDs <- plots_loc %>% 
  select(biosum_cond_id, ID, acres)

write_csv(plots_loc, "output_data/plot_loc.csv")
#test <- read_csv("output_data/plot_loc.csv")


# name pre and post for future reference
pre_full$section <- "pre"
post_full$section <- "post"


## full df 
all_data <- bind_rows(pre_full,post_full)
### add ID + forest type
all_data <- left_join(all_data, IDs) 

## clean environment of everything except the new full dataset
# rm(list=setdiff(ls(), "all_data"))

write_csv(all_data, "output_data/all_data.csv")
#all_data <- read_csv("output_data/all_data.csv")
