#####################
##### Optimize #####
###################
library(tidyverse)

### final step:
## we now have final discounted values for each package for this plot, now select the package with the lowest CPU

relative_carb <- read_csv("output_data/relative_carb_og_05.csv")

price <- 200

## new method for selecting optimal (based on value of carbon)
optimal <- relative_carb %>% 
  filter(total_carbon > 0 & rxpackage != "031") %>% 
  mutate(value = (price * total_carbon) - total_cost) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))

opt_tie_break <- optimal %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()



optimal_noCC <- relative_carb %>% 
  filter(total_carbon > 0 ) %>%     
  filter(!rxpackage %in% c("032", "033")) %>% 
  mutate(value = (price * total_carbon) - total_cost) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))

opt_tie_break_nocc <- optimal_noCC %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()


###################################
############ RESULTS ##############
###################################

###### MCC #######
## get cumsum
cumsum <- opt_tie_break %>% 
  arrange(cpu) %>% 
  filter(cpu > -100 & cpu < 200) %>% 
  mutate(cumsum_carb = cumsum(total_carbon))


cumsum_noCC <- opt_tie_break_nocc %>% 
  arrange(cpu) %>% 
  filter(cpu > -100 & cpu < 200) %>% 
  mutate(cumsum_carb = cumsum(total_carbon))


library(scales) # for comma in x axis

ggplot(cumsum, aes(cumsum_carb, cpu)) +
  geom_point(aes(color = cpu)) +
  scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 60000000),label=comma) +
  scale_y_continuous(limits = c(-150,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon",
    title = "MCC (w/ CC)"
  )


ggplot(cumsum_noCC, aes(cumsum_carb, cpu)) +
  geom_point(aes(color = cpu)) +
  scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red", midpoint = 50) +
  scale_x_continuous(limits = c(0, 60000000),label=comma) +
  scale_y_continuous(limits = c(-150,220), expand = c(0,0)) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon",
    title = "MCC (w/o CC)"
  )


### repeat for using rev
optimal_rev <- relative_carb  %>% 
  filter(total_carbon > 0 & !rxpackage %in% c("031", "032", "033")) %>% 
  mutate(value = (price * total_carbon) - (total_cost - total_val)) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))

opt_tie_break_rev <- optimal_rev %>% 
  group_by(ID) %>% 
  sample_n(1) %>% 
  ungroup()

cumsum_rev <- opt_tie_break_rev %>% 
  arrange(cpu_rev) %>% 
  mutate(cumsum_rev = cumsum(total_carbon))

test <- opt_tie_break_rev %>% 
  filter(owngrpcd == 40)

ggplot(cumsum_rev, aes(cumsum_rev, cpu_rev)) +
  geom_point() +
  #scale_colour_gradient2(low = "forestgreen", mid = "yellow", high = "red") +
  scale_x_continuous(limits = c(-1000, 95000000), expand = c(0,0),label=comma) +
  scale_y_continuous(limits = c(-1200,400), expand = c(0,0)) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  labs(
    x = "Tons of Carbon",
    y = "$/Ton of Carbon"
  )