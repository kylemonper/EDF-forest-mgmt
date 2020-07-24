##### estimating avoided emissions

#### so for each plot, we want to know 
#~ 1) what is the the weighted average emissions from fire 
#~~~~~~(based on amount emitted * % probability of fire each year)
#~ 2) what is the average regrowth rate
#~~~~~~ this we will estimate based on the average annual growth rate for each plot following a clear cut.
#~~~~~~ in the case that a clear cut was never initiated for a given plot, we will use the average growth rate (followwing a cc) for the specific variant/ forest type combo

add_fire_emissions <- function(all_data) {
  
  ### fire probabilites for each  plot
  pfire <- read_csv("input/plot_locs_pfire.csv")
  
  # all_data <- read_csv("output_data/all_data.csv")
  
  
  
  ### starting with getting the regrowth rates: ####
  ##~~ firs step is going to be identifying when a cc occurs and dividing change in total_stand_carbon/ change in time after the cc
  ##~~ have to be careful not to include data for when 2 cc occur for a single plot
  
  
  ## select all clear cuts
  cc <- all_data %>% filter(rxpackage == "032")
  
  
  
  ## select IDs where a cc occured (based on the presence of merchantable volume)
  cc_occur <- cc %>% 
    group_by(ID) %>% 
    filter(Merch_Carbon_Removed > 0) %>% 
    count() %>% 
    ungroup()
  
  
  ##~ by doing the group by count we can see which ones had 1,2 or 0 (by omission) harvests
  one <- cc_occur %>%  filter(n == 1) 
  two <- cc_occur %>%  filter(n == 2) 
  
  ## this elimates the cases where the harvest occured in the last cycle i.e cases that wont give us good trends
  one_sub <- cc %>% 
    filter(ID %in% one$ID) %>% 
    group_by(ID) %>% 
    filter(time == 30 & Merch_Carbon_Removed == 0) ## only want plots where nothing removed in year 30
  
  ## separate out the plots that had 1,2, and 0 harvests to calculate the rates seperately
  one_harv <- cc %>% filter(ID %in% one_sub$ID)
  two_harv <- cc %>% filter(ID %in% two$ID)
  no_harv <- cc %>% filter(!ID %in% one_harv$ID & !ID %in% two_harv$ID)
  
  
  
  ######## doing the single harvest first ####
  ## get unique ids
  single_ids <- unique(one_harv$ID)
  # create empty df
  single <- data.frame(matrix(NA, nrow = length(single_ids), ncol = 4))
  names(single) <- c("ID", "post_cut_gwth_rt","fvs_variant", "ftype")
  
  for(i in 1:length(single_ids)) {
    # focus on single plot
    test <- one_harv %>% filter(ID == single_ids[i])
    # find year in which harvest occured
    year <- test$time[test$Merch_Carbon_Removed > 0]
    #only look at years after the harvest, then get the change in stand carbon between then and the last year
    post_cc <- test %>%  filter(time >= year)
    growth <- test$Total_Stand_Carbon[test$time == max(test$time)] - test$Total_Stand_Carbon[test$time == year]
    
    # get time difference and then use to calculate annual growth rate
    time_diff <- max(test$time) - year
    growth_rate <- growth/time_diff
    
    single[i,1] <- single_ids[i]
    single[i,2] <- growth_rate
    single[i,3] <- test[1, "fvs_variant"]
    single[i,4] <- test[1,"ftype"]
  }
  
  
  # #### explore 
  # test <- one_harv %>% filter(ID == 25)
  # 
  # 
  # 
  # 
  # mean_growth <- single %>% 
  #   group_by(fvs_variant, ftype) %>% 
  #   summarise(
  #     mean_rt = mean(post_cut_gwth_rt)
  #   )
  # 
  # 
  
  #### repreat similar process for the two harvest cases #####
  
  double_ids <- unique(two_harv$ID)
  double <- data.frame(matrix(NA, nrow = length(double_ids), ncol = 4))
  names(double) <- c("ID", "post_cut_gwth_rt","fvs_variant", "ftype")
  
  for(i in 1:length(double_ids)) {
    # get single plot
    filt <- two_harv %>% filter(ID == double_ids[i])
    #id the year where the second harvest occurs
    last_harv <- max(filt$time[filt$Merch_Carbon_Removed > 0])
    first_harv <- min(filt$time[filt$Merch_Carbon_Removed > 0])
    
    # only look at the first growth period before the second harves
    first_growth_per <- filt %>% filter(time < last_harv & time >= first_harv)
    
    growth <- first_growth_per$Total_Stand_Carbon[first_growth_per$time == max(first_growth_per$time)] - first_growth_per$Total_Stand_Carbon[first_growth_per$time == min(first_growth_per$time)]
    
    
    time_diff <-  max(first_growth_per$time) - min(first_growth_per$time)
    growth_rate <- growth/time_diff
    
    double[i,1] <- double_ids[i]
    double[i,2] <- growth_rate
    double[i,3] <- filt[1, "fvs_variant"]
    double[i,4] <- filt[1,"ftype"]
    
  }
  
  
  known_growth_rts <- bind_rows(single, double)
  
  mean_growth_rts <- known_growth_rts %>% 
    group_by(fvs_variant, ftype) %>% 
    summarise(
      mean_posthrvst_rt = mean(post_cut_gwth_rt)
    )
  
  ## use this mean growth rate to update the rate for the plots we dont have post cc data for
  
  
  infered_rt <- no_harv %>% 
    select(ID, ftype, fvs_variant) %>% 
    left_join(mean_growth_rts) %>% 
    rename(post_cut_gwth_rt = mean_posthrvst_rt) %>% 
    distinct()
  
  
  all_growth_rts <- bind_rows(known_growth_rts, infered_rt)
  
  ### looks like "nonstocked" forests dont have any data for them
  # test <- all_growth_rts %>% filter(is.na(post_cut_gwth_rt))
  
  ## use the mean of all growth rates to update these
  all_growth_rts$post_cut_gwth_rt[is.na(all_growth_rts$post_cut_gwth_rt)] <- mean(all_growth_rts$post_cut_gwth_rt, na.rm = T)
  
  
  #### next, adding the fire emissions ####
  
  ### convert pm 2.5 emissions (from pot_smoke variable) to C emissions.  ####
  ## to do this we used data from:
  #"National Research Council: Committee on Air Quality Management in the United States, Board on Environmental Studies and Toxicology, Board on Atmospheric Sciences and Climate, Division on Earth and Life Studies (2004).
  # which provides % allocation of chemical emissions from wildfire smoke. see: https://en.wikipedia.org/wiki/Wildland_fire_emission
  
  
  perc_pm25 <- .0047
  ### get elemental carbon from each emitted chemical ###
  # % of CO2 that is C
  pct_c <- .2729
  pct_co2 <- .7144 * pct_c ### % of co2 in fire * # of C in co

  ## first convert other chems into CO2 equivalent (multiplying % content by CO2e)
  methane_eq <- .0027 * 25
  n20_eq <- .0039 * 298
  co_eq <- .0552 * 1.57
  h20_eq <- .2097 * .03
  ## now take CO2e values and multiply by %C in co2
  pct_methane <- methane_eq * pct_c
  pct_n20 <- n20_eq * pct_c
  pct_co <- co_eq * pct_c
  pct_h20 <- h20_eq * pct_c
  
  ## get total smoke based on knowing the % of pm2.5 
  total_smoke_mod <- all_data$Pot_Smoke_Mod / perc_pm25
  total_smoke_sev <- all_data$Pot_Smoke_Sev / perc_pm25
  
    ## use total smoke to get % of carbon from other primary chemicals
  all_data$smoke_c_mod <- total_smoke_mod * (pct_co + pct_co2 + pct_methane + pct_n20 + pct_h20)
  all_data$smoke_c_sev <- total_smoke_sev * (pct_co + pct_co2 + pct_methane + pct_n20 + pct_h20)
  
  
  ##### add in probabilities to each plot #####
  pfire_sub <- pfire %>% 
    select(ID, prob)
  
  joined <- left_join(all_data, pfire_sub)
  
  all_data_update <- left_join(joined, all_growth_rts)
  
  return(all_data_update)

}


