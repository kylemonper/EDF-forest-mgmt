#########################
## using the add_chip_decay and add_harvest_decay function
## discount carbon fluxes and costs

library(tidyverse)
source("pre_process/add_chip_decay.R")
source("pre_process/add_harvest_decay.R")
options(scipen = 999)


add_discounting <- function(df) {
  carbon <- df %>% 
    # convert green tons to C
    mutate(merch_carbon = merch_yield_gt * .325) %>% 
    mutate(chip_carbon = chip_yield_gt * .325)
  
  
  
  new_time <- data.frame(time = c(-1,0,1,9,10,11,19,20,21,29,30,31)) %>% 
    left_join(carbon, by = "time")
  
  new_time <- replace_na(new_time, list(merch_carbon = 0, chip_carbon = 0, Total_Stand_Carbon = 0))
  
  
  for (i in c(1,4,7,10)) {
    
    new_time$Total_Stand_Carbon[i] <- new_time$merch_carbon[i+1] + new_time$chip_carbon[i+1] + new_time$Total_Stand_Carbon[i+1]
    
  }
  
  
  
  pre_post <- new_time %>% 
    arrange(time) %>% 
    # find the difference between the the stand carbon in cycle 1 and 2, 2 and 3 etc
    mutate(diff = Total_Stand_Carbon - lag(Total_Stand_Carbon, default = first(Total_Stand_Carbon)))
  
  
  ###### calculate yearly difference
  
  
  for (i in 2:(nrow(pre_post)-1)) {
    
    difference <- pre_post$Total_Stand_Carbon[i] - pre_post$Total_Stand_Carbon[i-1]
    time_since <- pre_post$time[i] - pre_post$time[i-1]
    
    pre_post$each_year[i] <- difference/time_since
    
  }
  ## do last year manually
  pre_post$each_year[12] <- pre_post$diff[12]
  
  
  # now create a new dataframe with all of the times and make sure time is an integer
  tmp <- tibble(time = -1:31)
  
  # now merge together the dataframes
  plot_time <- left_join(tmp, pre_post, by = "time") 
  
  # now need to fill in the empty total stand carbon columns 
  for (i in 1:nrow(plot_time)){
    plot_time$each_year = ifelse(plot_time$time <=8 & plot_time$time >=2, 
                                 plot_time$each_year[11],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$time <=18 & plot_time$time >=12, 
                                 plot_time$each_year[21],
                                 plot_time$each_year)
    plot_time$each_year = ifelse(plot_time$time <=28 & plot_time$time >=22, 
                                 plot_time$each_year[31],
                                 plot_time$each_year)
  }
  
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
  
  no_na <- replace_na(plot_merch_diff, list(haul_merch_cpa = 0, haul_chip_cpa = 0, harvest_onsite_cpa = 0, each_year = 0))
  
  plot_time_discounts <- no_na %>% 
    # discounted carbon for each year
    # mutate(discount_carb = each_year/((1+dc_rate)^time)) %>% 
    # cumulative discounted carbon
    mutate(cum_discount_carb = cumsum(each_year/((1+dc_rate)^time))) %>% 
    mutate(cum_discount_merch = cumsum(merch_diff/((1+dc_rate)^time))) %>% 
    mutate(cum_discount_decay = cumsum(decay_diff/((1+dc_rate)^time))) %>% 
    mutate(cum_discount_biochar = cumsum(biochar_diff/((1+dc_rate)^time))) %>% 
    mutate(total_discount_carb = cum_discount_carb + cum_discount_merch + cum_discount_biochar + cum_discount_decay) %>% 
    # discounted cost
    mutate(discount_haul_merch = haul_merch_cpa/((1+dc_rate)^time)) %>% 
    mutate(discount_haul_chip = haul_chip_cpa/((1+dc_rate)^time)) %>% 
    mutate(discount_harvest = harvest_onsite_cpa/((1+dc_rate)^time)) %>% 
    mutate(discount_cost = discount_haul_chip + discount_haul_merch + discount_harvest) %>% 
    mutate(discount_cost = replace_na(discount_cost,0)) %>% 
    mutate(cum_discount_cost = cumsum(discount_cost),
           cum_disc_haul_merch = cumsum(discount_haul_merch),
           cum_disc_haul_chip = cumsum(discount_haul_chip),
           cum_disc_harvest = cumsum(discount_harvest))%>% 
    #discouneted revenue
    mutate(discount_merch_dpa = merch_val_dpa/((1+dc_rate)^time)) %>% 
    mutate(discount_chip_dpa = chip_val_dpa/((1+dc_rate)^time)) %>% 
    mutate(discount_val = discount_merch_dpa + discount_chip_dpa) %>% 
    mutate(discount_val = replace_na(discount_val, 0)) %>% 
    mutate(cum_discount_val = cumsum(discount_val))
  
  ## the information we want to end up with for each distinct plot and package
  final_cumulative <- plot_time_discounts %>% 
    filter(time == 31) %>% 
    select(biosum_cond_id, ID, owngrpcd, acres, rxpackage, cum_discount_carb, cum_discount_merch, cum_discount_cost, cum_disc_haul_chip , cum_disc_haul_merch, cum_disc_harvest , total_discount_carb, cum_discount_decay, cum_discount_biochar, cum_discount_val)
  
  
  
}
