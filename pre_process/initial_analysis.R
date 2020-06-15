###########################
## initial analysis



##### Package counts ####
package_count <- optimal %>% 
  group_by(rxpackage) %>% 
  tally()

package_count$rxpackage <- factor(package_count$rxpackage, levels = package_count$rxpackage[order(-package_count$n)])

ggplot(package_count, aes(as.factor(rxpackage), n)) +
  geom_col() +
  labs(
    title = "counts of optimal packages",
    x = "treatment package", 
    y = "count"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



##### get plot locations for mapping #####
counties <- read_csv("plot_county.csv") %>% 
  select(ID, lat, lon, NAME)
cpu_locs <- left_join(opt_tie_break, counties)
#write_csv(cpu_locs, "cpu_locs.csv")
# calculate the most common package per county and plot this



#### calculate average cpu for each (not including optimal) treatments

tmp <- relative_carb %>% 
  filter(cpu > -400 & cpu < 400)

ggplot(tmp, aes(x = reorder(rxpackage,-cpu,mean), y = cpu)) +
  geom_boxplot(outlier.shape = NA)


##### how much would be abated at the given price of 15

carb_15 <- cumsum %>% 
  filter(cpu <= 15)

abate_25 <- cumsum %>% 
  filter(cumsum_carb <= 25000000) 

### this function approximates taking the integral of points w/in an x-y coordinate system
total_cost_25mt <- pracma::trapz(abate_25$cumsum_carb, abate_25$cpu)

cpu_locs <- left_join(optimal, counties)


#### Summary table #####
summary_cc <- cpu_locs %>% 
  group_by(NAME, owngrpcd) %>% 
  summarise(
    average_cpa = mean(relative_cost),
    sd_cpa = sd(relative_cost),
    avg_carbon_pa = mean(relative_carb),
    sd_carbon = sd(relative_carb)
  )

rx_count <- cpu_locs %>% 
  group_by(NAME, owngrpcd, rxpackage) %>% 
  tally() %>% 
  top_n(3, n)

count_wide <- rx_count %>% 
  spread(rxpackage, n)


final_sum <- left_join(count_wide, summary_cc)

#### again for no CC
optimal_noCC <- relative_carb %>% 
  filter(total_carbon > 0 ) %>%     
  filter(!rxpackage %in% c("032", "033")) %>% 
  mutate(value = (price * total_carbon) - total_cost) %>% 
  group_by(ID) %>% 
  filter(value > 0 &
           value == max(value))


cpu_locs_nocc <- left_join(optimal_noCC, counties)


summary_nocc <- cpu_locs_nocc %>% 
  group_by(NAME, owngrpcd) %>% 
  summarise(
    average_cpa = mean(relative_cost),
    sd_cpa = sd(relative_cost),
    avg_carbon_pa = mean(relative_carb),
    sd_carbon = sd(relative_carb)
  )

rx_count_nocc <- cpu_locs_nocc %>% 
  group_by(NAME, owngrpcd, rxpackage) %>% 
  tally() %>% 
  top_n(3, n)


count_wide <- rx_count_nocc %>% 
  spread(rxpackage, n)

final_summary_nocc <- left_join(count_wide, summary_nocc)



all_data <- read_csv("all_data.csv")
source("discount_all_new.R")
source("old_discount_method.R") 

decay_pct <- 1
char_pct <- 0

#### decay rates for merch and non-merch
merch_decay <- read_csv("softwood_lumber_decay.csv")
non_merch <- read_delim("chip_pathways.txt", delim = ",")



test <- add_discounting_old(all_data, 34, "018")
base <- add_discounting_old(all_data, 34, "031")


### LEGEND:
# grow only is blue
# red/brown : merch and chip carbon
# green is total stand
# dashed: total overall carbon
ggplot(test, aes(x = time, y = cum_discount_carb)) +
  geom_line(color = "green") +
  geom_line(aes(x = time, y = cum_discount_merch), color = "red") +
  geom_line(aes(x = time, y = cum_discount_decay), color = "brown") +
  geom_line(aes(x = time, y = total_discount_carb), linetype = "dashed") +
  geom_line(data = base, aes(time, total_discount_carb), color = "blue") +
  labs(title = "old method")


## now repeat with same plot/ package using the new method:
df_18 <- add_discounting_new_long(all_data, 34, "018")
df_go <- add_discounting_new_long(all_data, 34, "031")



# same legend
ggplot(df_18, aes(x = time, y = cum_discount_carb)) +
  geom_line(color = "green") +
  geom_line(aes(x = time, y = cum_discount_merch), color = "red") +
  geom_line(aes(x = time, y = cum_discount_decay), color = "brown") +
  geom_line(aes(x = time, y = total_discount_carb), linetype = "dashed") +
  geom_line(data = df_go, aes(x = time, y = total_discount_carb), color = "blue") +
  labs(title = "new method")

write_csv(final_sum, "summary_table_cc.csv")
write_csv(final_summary_nocc, "summary_table_noCC.csv")



plot_count_nocc <- cpu_locs_nocc %>% 
  select(NAME, owngrpcd, ID) %>% 
  distinct() %>% 
  group_by(NAME, owngrpcd) %>% 
  tally()
write_csv(plot_count_nocc, "plot_count_nocc.csv")


plot_count_nocc <- cpu_locs %>% 
  select(NAME, owngrpcd, ID) %>% 
  distinct() %>% 
  group_by(NAME, owngrpcd) %>% 
  tally()
write_csv(plot_count_nocc, "plot_count_cc.csv")





###### if revenue compare lack of net revenue as cost