#############################
## adding OWNCD to FvsIN standInit table


library(tidyverse)
library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)


##################
## read in data ##
##################

#get harvest cost/ relevant acres
conn <- odbcConnectAccess2007("input/FVSIn.accdb")
plot <- sqlFetch(conn, "PLOT", as.is = T)
cond <- sqlFetch(conn, "COND", as.is = T)
stand_init <- sqlFetch(conn, "FVS_StandInit", as.is = T)


##### join OWNC from cond to plot based on plot cn
plot_cond <- cond %>% 
  left_join(plot, by = c("PLT_CN" = "CN")) %>% 
  select(Latitude = LAT, Longitude = LON, Inv_Year = INVYR.x , OWNCD) %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

test <- left_join(stand_init, plot_cond) %>% 
  distinct()




