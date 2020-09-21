###############################
### get relative costs & carbon
### by subtracting the baseline


get_relative <- function(data, inc_tmp = TRUE) {
  
 
  if (inc_tmp) {
    #### first, add in the cost of the managment plans
    ###~ this probably desrves its own function, but oh well...
    
    ## calc costs for management plans
    #~ see calc_plan_cost.R for alternative calculation methods
    thp_simple_average <- (40000/250 + 80000/1250 + 120000/2000)/3
    
    ntmp_simple_average <- thp_simple_average*1.2 # 20% more than thp
    
    ##  agin for the simple
    thp_simple_df <- data.frame(time = seq(1,31,6), thp_cpa = rep(thp_simple_average,6))
    
    thp_cost_simp <- thp_simple_df %>% 
      mutate(disc_thp = thp_simple_average/((1+0.05)^time))
    
    total_thp_simp <- sum(thp_cost_simp$disc_thp)
    
    
    ### if private and cc, add cost of thp, if private and thin, add ntmp
    #~ then add this to the cum_discount_cost
    df <- data %>% 
      mutate(plan_cost = if_else(owngrpcd == 40 & rxpackage == "700", total_thp_simp*acres, 
                                 if_else(owngrpcd == 40 & rxpackage != "700", ntmp_simple_average*acres, 0)),
             cum_discount_cost = cum_discount_cost + plan_cost)
  } else {
    df <- data
  }

  
  
  ##########################
  ### subtract baseline ####
  ##########################
  
  ### next steps:
  #~ incorperate baseline
  #     baseline for non-selected plots == grow only
  #     baseline for selected plots == (acres_assign)*carbon[for base package] + (acres-acres_assign)*carbon[for baseline]
  selected_sites <- read_csv("output_data/baseline_sites.csv")
  
  ### first, wrangle data:
  ## get grow only for each plot
  
  grow_only <- df %>% 
    filter(rxpackage == "999") %>% 
    select(ID, acres, rxpackage, cum_discount_carb, cum_disc_fire_mod, cum_disc_fire_sev) %>% 
    rename("grow_only_carb" = "cum_discount_carb") %>% 
    rename("GO_fire_mod" = "cum_disc_fire_mod") %>% 
    rename("GO_fire_sev" = "cum_disc_fire_sev")
  
  
  ## get relevent data from selected_sites
  selected_data <- selected_sites %>% 
    select(ID, acres, rxpackage, random_harvest_assign)
  
  
  ## get discounted carbon values for each of these selected sites
  
  selected_disc <- left_join(selected_data, df, by = c("ID", "rxpackage")) %>% 
    rename("rxpackage_sel" = "rxpackage") %>% # rename columns to distinguish them before joining
    rename("discount_carb_sel" = "cum_discount_carb") %>% 
    rename("cost_baseline_rx" = "cum_discount_cost") %>% 
    rename("discount_merch_sel" = "cum_discount_merch") %>% 
    rename("total_carb_sel" = "total_discount_carb") %>% 
    rename("fire_mod_sel" = "cum_disc_fire_mod") %>% 
    rename("fire_sev_sel" = "cum_disc_fire_sev") %>% 
    distinct()
  
  
  all_base <- left_join(grow_only, selected_disc, by = c("ID")) %>% 
    distinct()
  
  
  
  ## calculate baseline
  #~ first calculate the % of plot that is grow only vs radmonly selected acres
  #~ then multiply this by the calculated discounted carbon value for each package
  
  baseline_total <- all_base %>% 
    mutate(random_harvest_assign = replace_na(random_harvest_assign, 0),
           total_carb_sel = replace_na(total_carb_sel,0),
           cost_baseline_rx = replace_na(cost_baseline_rx,0),
           cum_discount_val = replace_na(cum_discount_val,0),
           fire_mod_sel = replace_na(fire_mod_sel, 0),
           fire_sev_sel = replace_na(fire_sev_sel, 0),
          ## calc relative percents
           pct_grow_only = ((acres-random_harvest_assign)/acres),
           pct_select = (random_harvest_assign/acres),
          # calc base carb, fire, cost and val
           base_disc_carb = (pct_grow_only*grow_only_carb)+(pct_select*total_carb_sel), 
           base_fire_mod = (pct_grow_only*GO_fire_mod)+(pct_select*fire_mod_sel), 
           base_fire_sev = (pct_grow_only*GO_fire_sev)+(pct_select*fire_sev_sel), 
           base_disc_cost = (pct_select*cost_baseline_rx),
           base_disc_val = (pct_select*cum_discount_val),
          # update base carb by subtracting possible emissions
           base_carb_mod = base_disc_carb - base_fire_mod,
           base_carb_sev = base_disc_carb - base_fire_sev) %>% 
    dplyr::select(ID,base_disc_carb, base_carb_sev, base_carb_mod, base_disc_cost, base_disc_val)
  
  ## joing togeter and calculate relative carbon
  incorp_base <- left_join(df, baseline_total) %>% 
    distinct()
  
  
  relative_carb <- incorp_base %>% 
    ### update total_disc_carb based on emissions
    mutate(tot_disc_carb_mod = total_discount_carb - cum_disc_fire_mod,
           tot_disc_carb_sev = total_discount_carb - cum_disc_fire_sev) %>% 
    mutate(relative_carb_grow = total_discount_carb - base_disc_carb,
           relative_carb_min = tot_disc_carb_sev - base_carb_mod,
           relative_carb_max = tot_disc_carb_mod - base_carb_sev,
           relative_cost = cum_discount_cost-base_disc_cost,
           relative_val = cum_discount_val-base_disc_val) %>% 
    mutate(total_carbon_grow = relative_carb_grow * acres,
           total_carbon_max = relative_carb_max * acres,
           total_carbon_min = relative_carb_min * acres,
           total_cost = relative_cost* acres,
           total_val = relative_val*acres,
           cpu_grow = total_cost/total_carbon_grow,
           cpu_max = total_cost/total_carbon_max,
           cpu_min = total_cost/total_carbon_min,
           cpu_rev_max = (total_cost-total_val)/total_carbon_max,
           cpu_rev_min = (total_cost-total_val)/total_carbon_min)
  
  return(relative_carb)
  
  
}
