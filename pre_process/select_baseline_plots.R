###############################
#### Select Baseline plots ####
###############################


##############################
####### data wrangling #######
##############################
# read in spatial joined plot w/ counties
# read in harvest data from 2012
# join, convert to mmb, and select appropriate data for private and federal


counties <- readxl::read_xls("input/plot_counties.xls") %>% 
  select(ID, NAME)
harvest_2012 <- read_csv("input/CA_harvest_2012.csv")

### joing with plot_all (from carbon_2.R after running through line 144)
plot_county <- left_join(all_data, counties)
plot_area <- left_join(plot_county, harvest_2012, by = c("NAME" = "county"))

### remove rows where area == NA (this means harvesting did not occur in this area within 2012)
plot_actual <- filter(plot_area, !is.na(area)) 

## convert merch_yield_cf to mmbf 
plot_bf <- mutate(plot_actual, merch_yield_mmbf = round(merch_yield_cf * 12 / 1000000,8))

#### select only "baseline packages" (clearcut for private, and !!!!!!002!!!!! for federal), and first cycle
# is 002 the right package to use?
plot_sel <- plot_bf %>% 
  filter(rxpackage == "700" & owngrpcd == 40 |
           rxpackage == "010" & owngrpcd == 10) %>% 
  select(biosum_cond_id, ID, acres, owngrpcd, rxcycle, rxpackage, NAME, mmbf_county, merch_yield_mmbf) %>% 
  rename("county" = "NAME")
### get rid of pre and post
plot_sel <- distinct(plot_sel)


###############################
####### run selection  ########
###############################
### STEPS:
#~ 1) set targets for private and federal w/in each county
#~ 2) randomly select plots within each county that meet those targets.

## first: define percent allocation of harvest to federal and private (based on 2012 harvest report)
#~ !!!! this is a major assumption that this same ration applies accross all counties/ areas

private_pct <- .837
federal_pct <- 1 - private_pct

## write function that sets the target harvest for each owner type within each county. 
#~ set target for total harvest based on each ratio^^


set_target <- function(county_sel) {
  ## filter county
  selected_county <- filter(plot_sel, county == county_sel)
  
  ## set target based on defined ratio
  ## multiple how much is harvested in county by the statewide harvest percentage 
  #~ (if privatemultiple by private pct, otherwise multiply by federal pct
  result <- selected_county %>% 
    mutate(target = if_else(owngrpcd == 40, mmbf_county * private_pct, mmbf_county * federal_pct))
  
  
}

unique_counties <- unique(plot_sel$county)
target_list <- lapply(unique_counties, set_target)
plot_targets <- bind_rows(target_list)


#### function for randomizing sites within each area based on some specifed harvest amount (detirmed by the error around the target harvest value for each area)
## FUNCTION WORKFLOW:
#~ select sites within specific county & ownder group & are harvested in year 0
#~ generate random numbers to simulate the harvested area of each plot (turning mmbf/acre into mmbf)
#~ randomly arrange [select] sites
#~ take the cumulative cumulative sum of their harvest
#~ if somehwhere that cumulative sum is within 5% of the target harvest for that county, select that arrangment of random plots/harvested area
#~ if after 10,000 random simulation, nothing is chosen, begin increasing acceptable error 



randomize_sites <- function(location, ownrcd){
  final_results <- NULL
  ## track status when running lapply()
  print(location)
  
  # loop through each cycle
  for(i in 1:4){
    
    print(i)
    #filter data for sites that are in area and are harvested during rxcycle 1
    data_filt <- plot_targets %>% 
      filter(county == location & owngrpcd == ownrcd & rxcycle == i & merch_yield_mmbf > 0)
    
    #if no plots meet these conditions, stop function
    if(nrow(data_filt) == 0){
      results <- NULL
    } else {
      
      #define target mmbf for this area (for use in error calculation)
      target <- data_filt$target[1]
      
      # this is to help define the exponential decay function for selecting random numbers
      ratio <- target/sum(data_filt$merch_yield_mmbf)
      
      #### for some counties, too small a ratio (i.e expnential decay function) doesn't give large enough harvest restults, so set minimum ratio to 50
      if(ratio < 50){
        ratio <- 50
      } else {
        ratio <- ratio
      }
      
      ### if a site was previously selected, use the same assigned harvested acres as the first time around
      if(!is.null(final_results)) {
        joined <- left_join(data_filt, final_results[,c("ID", "random_harvest_assign")], by = "ID")
      } else {
        joined <- data_filt 
      }
      
      
      
      ### randomly select sites until at selected sites are within some error of the target value
      results <- NULL
      j <- 1
      while(is.null(results)) {
        
        ## randomly assign the number of acres to be harvest
        #~ first step: create vector (of same length as filtered data) filled with random numbers
        
        random_harvest_acres <- data.frame(random_harvest_assign = rexp(nrow(joined),1/ratio))
        
        ## bind this column to data and get total random harvest
        suppressMessages(
          acres_assigned <- bind_cols(joined, random_harvest_acres)
          
        )
        
        
        if(length(acres_assigned) == 12) {
          names(acres_assigned)[11] <- "random_harvest_assign"
          names(acres_assigned)[12] <- "random_harvest_assign1"
        }
          
        
        ### if a site was previously selected, use the same assigned harvested acres as the first time around
        if(any(is.na(acres_assigned$random_harvest_assign))) {
          
          
          total_acres <- acres_assigned %>% 
            mutate(random_harvest_assign = if_else(is.na(random_harvest_assign), random_harvest_assign1, random_harvest_assign))
        } else {
          total_acres <- acres_assigned
        }
        
        
        ## ensure that we cant harvest more area that the plot has 
        total_harvest <- total_acres %>% 
          mutate(random_harvest_assign = if_else(random_harvest_assign > acres, acres, random_harvest_assign), 
                 total_yield = random_harvest_assign*merch_yield_mmbf)
        
        
        ## randomly arrange sites
        rand_data <- total_harvest[sample(1:nrow(total_harvest)),]
        
        
        
        #calculate cummulative sum 
        cum_sum <- rand_data %>% 
          mutate(cum_sum = cumsum(total_yield))
        
        #### assign error value of 1%; if we've gone through >2,000 while loops, increase the error by 1%
        ## this is to ensure that the loop will eventually finish if for some reason the numbers arn't working well
        if(j < 2000){
          error <- .01
        } else if(j %% 2000 == 0) {
          error <- .01 + (j/2000 * .01)
        }
        
        ## select at least 2 sites that are within 20% of target
        selected <- cum_sum %>% 
          filter(cum_sum <= target * (1 + error) & cum_sum >= target * (1 - error)) ### filter for total harvest values w/in some error of the target
        
        ## if at least two sites were successfully selected, get list of sites, otherwise repeat loop
        if(nrow(selected) >= 1){
          results <- cum_sum %>% 
            filter(cum_sum <= target * (1 + error))
          results$error <- error
        } else {
          results <- NULL
        }
        
        #count number of loops
        j <- j+1
      }
    }
    final_results <- bind_rows(final_results, results)
    if(nrow(final_results) == 0){
      final_results <- NULL
    }
    
  }
  
  return(final_results)
  
}



random_site_public <- lapply(unique_counties, randomize_sites, ownrcd = 10)
random_site_private <- lapply(unique_counties, randomize_sites, ownrcd = 40)

random_public <- bind_rows(random_site_public)
random_private <- bind_rows(random_site_private)

## check rough accuracy towards targets:
paste("total harvest accuracy:",(sum(random_private$total_yield) + sum(random_public$total_yield))/1425/4)
paste("private harvest accuracy:", sum(random_private$total_yield)/1197/4)
paste("public harvest accuracy:",sum(random_public$total_yield)/203/4)

selected_sites <- bind_rows(random_private, random_public)

write_csv(selected_sites, "baseline_sites.csv")