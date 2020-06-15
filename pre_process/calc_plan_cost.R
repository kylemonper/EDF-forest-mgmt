############################
# add in cost of THP and NTMP
############################
###
###       THP
###
############################
library(tidyverse)


thp <- read_csv("input/CALFIRE_THPS.csv")
ntmp <- read_csv("input/CALFIRE_NTMPS.csv")

#### we need to rethink the weighted sum: this is waaaaaaay over estimating the cost

##THP
thp_grouped <- thp %>% 
  mutate(size = if_else(ACRES < 500, "small",
                        if_else(ACRES > 500 & ACRES < 2500, "medium", "large")))

thp_total <- thp_grouped %>% 
  group_by(size) %>% 
  summarise(total = sum(ACRES),
            avg_size = median(ACRES)) %>% 
  ungroup() %>% 
  mutate(cost = if_else(size == "small", 40000,
                        if_else(size == "medium", 80000, 120000)),
         avg_cpa = cost/avg_size)

thp_avg_cpa <- weighted.mean(thp_total$avg_cpa, thp_total$total)

### NTMP
ntmp_grouped <- ntmp %>% 
  mutate(size = if_else(ACRES < 500, "small", "large"))

ntmp_total <- ntmp_grouped %>% 
  group_by(size) %>% 
  summarise(total = sum(ACRES),
            avg_size = mean(ACRES)) %>% 
  ungroup() %>% 
  mutate(cost = if_else(size == "small", 48000, 124000),
         avg_cpa = cost/avg_size)

ntmp_avg_cpa <- weighted.mean(ntmp_total$avg_cpa, ntmp_total$total)


#####
## try more simple average for both:
thp_simple_average <- (40000/250 + 80000/1250 + 120000/2000)/3

ntmp_simple_average <- thp_simple_average*1.2 # 20% more than thp

##  agin for the simple
thp_simple_df <- data.frame(time = seq(1,31,6), thp_cpa = rep(thp_simple_average,6))

thp_cost_simp <- thp_simple_df %>% 
  mutate(disc_thp = thp_simple_average/((1+0.05)^time))

total_thp_simp <- sum(thp_cost_simp$disc_thp)



