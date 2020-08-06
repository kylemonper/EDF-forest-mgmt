#############################
## adding OWNCD to FvsIN standInit table 
## -- allows for setting up Upper diameter limits by ownership type + not treat any besides federal and private
## This loops through each FVSin.accdb file generated via biosum
## and adds a FVS_StandInit_new table that has the ownercodes for each stand
## after running this you need to replace the old standinit table with the new
## and recreate the table conncection in access
##~~ this script/ process only needs to be done once before running FVS

library(RODBC)

## ensure that plot ids are read in as entire number (vs being auto-converted to scientific notation)
options(scipen = 999)



##########################
###### CEC method ########
##########################
dirs <- c("CA", "SO", "NC", "WS")


conn <- odbcConnectAccess2007("C:/FIA_BioSum/edf_072020/db/master.mdb")
cond <- sqlFetch(conn, "COND", as.is = T)
odbcCloseAll()

for (i in 1:length(dirs)) {
  conn <-  odbcConnectAccess2007(file.path("C:/FIA_BioSum/edf_072020/fvs/data/", dirs[i], "FVSIn.accdb"))
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


