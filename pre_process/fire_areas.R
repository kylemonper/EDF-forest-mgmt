##### fire areas ########

library(tidyverse)

# areas <- read_delim("C:/FIA_BioSum/GIS/fire_areas.txt", delim = ",")
# 
# years <- sort(unique(areas$YEAR_))
# 
# zeroed <- data.frame(year_zeroed = 1:length(years), YEAR_ = years)
# 
# areas <- left_join(areas, zeroed) %>% 
#   filter(!is.na(REPORT_AC))
# 
# 
# 
# ggplot(areas, aes(YEAR_, REPORT_AC)) +
#   geom_point() +
#   geom_smooth(method = lm, formula = y ~ log(x))
# 
# 
# 
# 
# yearly <- areas %>% 
#   group_by(year_zeroed) %>% 
#   summarise(
#     year_total = sum(REPORT_AC, na.rm = T)
#   )
# 
# ggplot(yearly, aes(year_zeroed, year_total)) +
#   geom_line()
# +
#   geom_smooth(method = lm, formula = log(y) ~ x)
# 
# 
# 
# size_lm <- glm(REPORT_AC ~ year_zeroed, data = areas)
# summary(size_lm)
# 
# yearly_lm <- glm(year_total ~ year_zeroed, data = yearly)
# summary(yearly_lm)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## workflow for Monte_carlo  ##
###############################

#~ get year
#~ use model to predict +/- SD number of acres that year
#~~~~ this will be the target/cap
# sample (with replacement) plots based on their probability
## "apply fire" using model to predict size +/- SD
all_data <- read_csv("output_data/all_data.csv") 

#~ Year will also be used to predict size +/- SD
#~~ continiously predict fires of random size
Sys.time()
acres <- read_csv("output_data/all_data.csv") %>% 
  select(ID, acres) %>% 
  filter(!is.na(acres)) %>% 
  distinct()
fire_probs <- readxl::read_excel("input/plot_fire_prob.xls") %>% 
  select(ID, prob)

fire_probs$prob[is.na(fire_probs$prob)] <- 0.0000001



## all we need is probablity and acres
acre_prob <- left_join(fire_probs, acres) 


sim_fires <- function(acre_prob, avg_annl) {
  # create 'new acres' field that will diminish as area is "burned"
  new_acres <- acre_prob %>% 
    mutate(new_acres = acres,
           burned = 0)
  
  
  ### number of simluations to perform
  num_sims <- 10
  
  cycle_year <- c(2,12,22,32)
  full_sim <- list()
  
  ## loop through each cycle
  for(j in 1:4) {
    # keep track of loop
    print(paste( "cycle:", j))
    
    ## create new df
    if(j == 1) {
      sim_acres <- new_acres
    } else {
      sim_acres <- full_sim[[j-1]] %>% 
        group_by(ID) %>% 
        dplyr::summarise(
          new_acres = mean(new_acres), 
          burned = mean(burned)
        )
      sim_acres <- left_join(new_acres[c("ID", "acres", "prob")], sim_acres, by = "ID")
    }
    
    
    
    for(i in 1:num_sims) {
      
      sim_acres_new <- sim_acres
      
      
      ## set target arcres to be burned
      #~~ eventually this will hopefully be determined based on time series model +/- randomness
      target <- rnorm(1, avg_annl/3, 100000)
      if (target*4 > sum(acres$acres)-10000) target <- sum(acres$acres)/4-10000
      total_burned <- 0
      
      
      count <- 0
      while(total_burned < target) {
        count <- count +1
        # select plot to burn
        plot <- sim_acres_new %>% 
          # remove any plots that have been completely burned 
          filter(new_acres > 0) 
        
        if (nrow(plot) > 0) {
          plot <- plot %>% 
            sample_n(1, weight = prob) 
          
          
          # similuate size of fire
          #~~ update to use predict
          size <- rnorm(1,100,100)
          if(size < 0) size <- 100
          
          ## make sure that the size can exceed the remaining acres
          remaining <- plot$new_acres
          if (size > remaining) size <- remaining 
          
          ## update remaining acres and amount burned thus far
          sim_acres_new$new_acres[sim_acres_new$ID == plot$ID] <- sim_acres_new$new_acres[sim_acres_new$ID == plot$ID] - size
          sim_acres_new$burned[sim_acres_new$ID == plot$ID] <- sim_acres_new$burned[sim_acres_new$ID == plot$ID] + size
          
          # track total burned
          total_burned <- total_burned + size
          
        
        } else {
          break
        }
      }
      
      #keep track of loop
      if (i %% 5 == 0) print(paste("sim:",i, Sys.time()))
      
      ## id simulation
      sim_acres_new$sim <- i
      
      if(i == 1){
        full_sim[[j]] <- sim_acres_new
      } else {
        full_sim[[j]] <- bind_rows(full_sim[[j]], sim_acres_new)
      }
      
      
      
    }
    
    
  }
  Sys.time()
  
  full_sim_rows <- bind_rows(full_sim, .id = "cycle")
  
  avg_burned <- full_sim_rows %>% 
    group_by(ID, cycle) %>% 
    summarise(
      avg_newacre = mean(new_acres))
  
  
  w_acres <- left_join(avg_burned, acres)
  
  ids <- unique(w_acres$ID)
  
  burned_each_year <- list()
  
  for (i in 1:length(ids)) {
    
    filt <- w_acres %>% 
      filter(ID == ids[i]) %>% 
      mutate(tot_burned = acres - avg_newacre)
    
    
    suppressWarnings(
      filt$burned[1] <- filt$tot_burned[1]
    )
    
    filt$burned[2:4] <- diff(filt$tot_burned)
    
    burned_each_year[[i]] <- filt
    
    
  }
  
  all_burned <- bind_rows(burned_each_year)
  
  
  return(all_burned)
}

all_burned_500k <- sim_fires(acre_prob, 500000)
all_burned_1MM <- sim_fires(acre_prob, 1000000)
all_burned_5MM <- sim_fires(acre_prob, 5000000)
all_burned_10MM <- sim_fires(acre_prob, 10000000)

write_csv(all_burned_5MM, "output_data/all_burned_5MM.csv")
write_csv(all_burned_10MM, "output_data/all_burned_10MM.csv")
write_csv(all_burned_500k, "output_data/all_burned_500k.csv")
write_csv(all_burned_1MM, "output_data/all_burned_1MM.csv")



