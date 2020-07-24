#############################
## adding OWNCD to FvsIN standInit table 
## -- allows for setting up Upper diameter limits by ownership type + not treat any besides federal and private
## This loops through each FVSin.accdb file generated via biosum
## and adds a FVS_StandInit_new table that has the ownercodes for each stand
## after running this you need to replace the old standinit table with the new
## and recreate the table conncection in access
##~~ this script/ process only needs to be done once before running FVS

library(tidyverse)
library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)


##################
## read in data ##
##################

## list of directories holding the FVSIn for each variant
dirs <- c("CA", "SO", "NC", "WS")



#get harvest cost/ relevant acres
conn <- odbcConnectAccess2007("input/CA_FIADB.accdb")
cond <- sqlFetch(conn, "COND", as.is = T)
odbcCloseAll()


for (i in 1:4) {
  conn <-  odbcConnectAccess2007(file.path("input/FVS_in", dirs[i], "FVSIn.accdb"))
  stand_init <- sqlFetch(conn, "FVS_StandInit", as.is = T)

  cond_new <- cond %>% 
    select(biosum_cond_id, OWNCD)
  
  ###########
  stand_init_new <- stand_init %>% 
    mutate(County_Old = County)


  ##### join OWNC from cond to plot based on plot cn
  plot_cond <- cond %>%
    left_join(plot, by = c("PLT_CN" = "CN")) %>%
    select(Latitude = LAT, Longitude = LON, OWNCD, Inv_Year = MEASYEAR, ElevFt = ELEV) %>%
    mutate(Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude))

  test <- left_join(stand_init, plot_cond) %>%
    distinct()


  ids <- unique(stand_init$Stand_ID)

  all_ids <- data.frame(ids, 1:length(ids))
  names(all_ids) <- c("Stand_ID", "ID")
  ### exclude sites with duplicates
  ##this is not necessarily accurate, however it it is conservative as we cont know for sure which OWNcodes are the right ones this should ultimately be updated....
  joined <- left_join(test, all_ids)

  dups <- joined %>%
    group_by(ID) %>%
    count() %>%
    filter(n == 1)

  final <- joined %>%
    filter(ID %in% dups$ID) %>%
    select(-County) %>%
    mutate(County = OWNCD)

  sqlSave(conn, dat = final, tablename = "FVS_StandInit_new", rownames = F)
  odbcCloseAll()

}


##########################
###### CEC method ########
##########################
dirs <- c("CA", "SO", "NC", "WS")


conn <- odbcConnectAccess2007("input/master_edf.mdb")
cond <- sqlFetch(conn, "COND", as.is = T)
odbcCloseAll()

for (i in 1:length(dirs)) {
  conn <-  odbcConnectAccess2007(file.path("input/FVS_in", dirs[i], "FVSIn.accdb"))
  standinit <- sqlFetch(conn, "FVS_StandInit", as.is = TRUE)
  treeinit <- sqlFetch(conn, "FVS_TreeInit", as.is = TRUE)
  
  sqlSave(conn, dat = cond, tablename = "cond", rownames = FALSE)
  
  #Set County to County_Old and set County to cond.owncd
  sqlQuery(conn, 'ALTER TABLE FVS_StandInit ADD COLUMN County_Old NUMERIC')
  sqlQuery(conn, 'UPDATE FVS_StandInit SET FVS_StandInit.County_Old = [FVS_StandInit].[County];')
  sqlQuery(conn, 'UPDATE cond INNER JOIN FVS_StandInit ON cond.biosum_cond_id = FVS_StandInit.Stand_ID SET FVS_StandInit.County = [cond].[owncd];')
  
  #Set inventory year to 2007
  sqlQuery(conn, 'ALTER TABLE FVS_StandInit ADD COLUMN Inv_Year_Old NUMERIC')
  sqlQuery(conn, 'UPDATE FVS_StandInit SET FVS_StandInit.Inv_Year_Old = [FVS_StandInit].[Inv_Year];')
  sqlQuery(conn, 'UPDATE FVS_StandInit SET FVS_StandInit.Inv_Year = 2007;')
  
  odbcCloseAll()
  
  print(i)
}
beepr::beep(3)

