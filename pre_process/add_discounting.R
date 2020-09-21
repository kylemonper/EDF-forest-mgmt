############# refactoring the add_discount.R code

library(tidyverse)
library(stringr)



add_discounting <- function(df) {
  
  
  packages <- str_sort(unique(df$rxpackage))
  ## the above rearranges the packages, we want "999" to be first
  if(any(str_detect(packages, "9"))) packages <- c("999", "991", "992", "993", "994")
  
  no_na <- list()
  for(j in 1:length(packages)) {
    
    carbon <- df %>% 
      # convert green tons to C
      mutate(merch_carbon = merch_yield_gt * .325) %>% 
      mutate(chip_carbon = chip_yield_gt * .325)
    
    pre_post <- carbon %>% 
      filter(rxpackage == packages[j])
    
    
    
    new_time <- data.frame(Year = c(1,2,3,11,12,13,21,22,23,31,32,33)) %>% 
      left_join(pre_post, by = "Year")
    
    
    
    new_time <- replace_na(new_time, list(merch_carbon = 0, chip_carbon = 0, Total_Stand_Carbon = 0))
    
    #### the total stand carbon in the previous year is equal the to the amount that was removed plut the current amount
    for (i in c(1,4,7,10)) {
      
      new_time$Total_Stand_Carbon[i] <- new_time$merch_carbon[i+1] + new_time$chip_carbon[i+1] + new_time$Total_Stand_Carbon[i+1]
      
    }
    
    
    pre_post <- new_time %>% 
      arrange(Year) %>% 
      # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
      mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon)))
    
    ########
    
    for (i in 2:(nrow(pre_post)-1)) {
      
      difference <- pre_post$Total_Stand_Carbon[i] - pre_post$Total_Stand_Carbon[i-1]
      time_since <- pre_post$Year[i] - pre_post$Year[i-1]
      
      pre_post$each_year[i] <- difference/time_since
      
    }
    ## do first and last year manually
    pre_post$each_year[1] <- pre_post$diff[1]
    pre_post$each_year[nrow(pre_post)] <- pre_post$diff[nrow(pre_post)]
    
    
    # now create a new dataframe with all of the times and make sure time is an integer
    tmp <- tibble(Year = 1:33)
    
    # now merge together the dataframes
    suppressMessages(
      plot_time <- left_join(tmp, pre_post, by = "Year") 
    )
    
    
    # now need to fill in the empty total stand carbon columns 
    
    plot_time$each_year = ifelse(plot_time$Year <=11 & plot_time$Year >=4, 
                                 plot_time$each_year[11],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$Year <=20 & plot_time$Year >=14, 
                                 plot_time$each_year[21],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$Year <=30 & plot_time$Year >=24, 
                                 plot_time$each_year[31],
                                 plot_time$each_year)
    
    
    
    ## repeat similar process for fire smoke emissions (i.e extrapolating annual numbers)
    
    #plot_time <- infer_annual_smoke(plot_time)
    
    
    
    ## add in decay of merchantable wood
    plot_time$merch_decay <- add_harvest_decay(plot_time)
    ## add in decay of chip paths (decay/ biochar)
    plot_time$decay <- add_chip_decay(plot_time, "decay", decay_pct)
    plot_time$biochar <- add_chip_decay(plot_time, "biochar", char_pct)
    
    ## merch_decay is for merchantable, decay is for woodchips
    ## get yearly difference for merch and chips
    plot_merch_diff <- plot_time %>% 
      mutate(merch_diff = merch_decay - lag(merch_decay),
             merch_diff = replace_na(merch_diff, 0)) %>% 
      mutate(decay_diff = decay - lag(decay),
             decay_diff = replace_na(decay_diff, 0)) %>% 
      mutate(biochar_diff = biochar - lag(biochar),
             biochar_diff = replace_na(biochar_diff, 0)) 
    
    
    ## add a discounted column that is discounted by 0.05 
    # that is the cumulative sum for each year of discounted carbon
    
    #### set value of chips to zero
    plot_merch_diff$chip_val_dpa <- 0
    
    no_na[[j]] <- replace_na(plot_merch_diff, list(haul_merch_cpa = 0, haul_chip_cpa = 0, harvest_onsite_cpa = 0, each_year = 0))
    
  }
  
  
  
  
  burned_1 <- all_burned %>% 
    filter(ID == df$ID[1]) %>% 
    mutate(pct_burned = burned/acres)
  
  ## list of columns that will be updated based on % burned
  
  update_cols <- c("each_year", "merch_decay", "decay", "biochar", "merch_diff", "decay_diff", "biochar_diff", "haul_chip_cpa", "haul_merch_cpa", "harvest_onsite_cpa", "merch_val_dpa", "chip_val_dpa")
  
  
  
  if(nrow(burned_1 < 1)) {
    compiled <- no_na[[1]]
    compiled[3:33,paste(update_cols)] <-  compiled[3:33,paste(update_cols)] * (1-(burned_1$pct_burned[1])) + no_na[[2]][3:33,paste(update_cols)] * burned_1$pct_burned[1]
    compiled[13:33,paste(update_cols)] <-  compiled[13:33,paste(update_cols)] * (1-(burned_1$pct_burned[2])) + no_na[[3]][13:33,paste(update_cols)] * burned_1$pct_burned[2]
    compiled[23:33,paste(update_cols)] <-  compiled[23:33,paste(update_cols)] * (1-(burned_1$pct_burned[3])) + no_na[[4]][23:33,paste(update_cols)] * burned_1$pct_burned[3]
    compiled[33,paste(update_cols)] <-  compiled[33,paste(update_cols)] * (1-(burned_1$pct_burned[4])) + no_na[[5]][33,paste(update_cols)] * burned_1$pct_burned[4]
    
    
    compiled$smoke_c_mod_sim <- NA
    compiled$smoke_c_sev_sim <- NA
    
    compiled$smoke_c_mod_sim[2] <- no_na[[1]]$smoke_c_mod[2] * burned_1$pct_burned[1]
    compiled$smoke_c_mod_sim[12] <- no_na[[2]]$smoke_c_mod[12] * burned_1$pct_burned[2]
    compiled$smoke_c_mod_sim[22] <- no_na[[3]]$smoke_c_mod[22] * burned_1$pct_burned[3]
    compiled$smoke_c_mod_sim[32] <- no_na[[4]]$smoke_c_mod[32] * burned_1$pct_burned[4]
    
    compiled$smoke_c_sev_sim[2] <- no_na[[1]]$smoke_c_sev[2] * burned_1$pct_burned[1]
    compiled$smoke_c_sev_sim[12] <- no_na[[2]]$smoke_c_sev[12] * burned_1$pct_burned[2]
    compiled$smoke_c_sev_sim[22] <- no_na[[3]]$smoke_c_sev[22] * burned_1$pct_burned[3]
    compiled$smoke_c_sev_sim[32] <- no_na[[4]]$smoke_c_sev[32] * burned_1$pct_burned[4]
    
    compiled <- compiled %>% replace_na(list(smoke_c_sev_sim = 0, smoke_c_mod_sim = 0))
  } else {
    compiled <- no_na[[1]]
    
    compiled$smoke_c_mod_sim <- 0
    compiled$smoke_c_sev_sim <- 0
  }
  
  
  
  
  plot_time_discounts <- compiled %>% 
    # discounted carbon for each year
    # mutate(discount_carb = each_year/((1+dc_rate)^Year)) %>% 
    # cumulative discounted carbon
    mutate(cum_discount_carb = cumsum(each_year/((1+dc_rate)^Year))) %>% 
    mutate(cum_discount_merch = cumsum(merch_diff/((1+dc_rate)^Year))) %>% 
    mutate(cum_discount_decay = cumsum(decay_diff/((1+dc_rate)^Year))) %>% 
    mutate(cum_discount_biochar = cumsum(biochar_diff/((1+dc_rate)^Year))) %>% 
    mutate(total_discount_carb = cum_discount_carb + cum_discount_merch + cum_discount_biochar + cum_discount_decay) %>% 
    ## discounted fire 
    mutate(cum_disc_fire_mod = cumsum(smoke_c_mod_sim/((1+dc_rate)^Year)),
           cum_disc_fire_sev = cumsum(smoke_c_sev_sim/((1+dc_rate)^Year))) %>% 
    # discounted cost
    mutate(discount_haul_merch = haul_merch_cpa/((1+dc_rate)^Year)) %>% 
    mutate(discount_haul_chip = haul_chip_cpa/((1+dc_rate)^Year)) %>% 
    mutate(discount_harvest = harvest_onsite_cpa/((1+dc_rate)^Year)) %>% 
    mutate(discount_cost = discount_haul_chip + discount_haul_merch + discount_harvest) %>% 
    mutate(discount_cost = replace_na(discount_cost,0)) %>% 
    mutate(cum_discount_cost = sum(discount_cost),
           cum_disc_haul_merch = sum(discount_haul_merch),
           cum_disc_haul_chip = sum(discount_haul_chip),
           cum_disc_harvest = sum(discount_harvest))%>% 
    #discouneted revenue
    mutate(discount_merch_dpa = merch_val_dpa/((1+dc_rate)^Year)) %>% 
    mutate(discount_chip_dpa = chip_val_dpa/((1+dc_rate)^Year)) %>% 
    mutate(discount_val = discount_merch_dpa + discount_chip_dpa) %>% 
    mutate(discount_val = replace_na(discount_val, 0)) %>% 
    mutate(cum_discount_val = cumsum(discount_val))
  
  
  ## the information we want to end up with for each distinct plot and package
  final_cumulative <- plot_time_discounts %>% 
    filter(Year == 33) %>% 
    select(biosum_cond_id, ID, owngrpcd, acres, rxpackage, cum_discount_carb, cum_discount_merch, cum_discount_cost, cum_disc_haul_chip , cum_disc_haul_merch, cum_disc_harvest, cum_disc_fire_mod, cum_disc_fire_sev, total_discount_carb, cum_discount_decay, cum_discount_biochar, cum_discount_val)
  
}


  
  
  
  
  
  
### scratch

  
  
  
