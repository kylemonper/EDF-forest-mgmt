#### remove protected plots
### this was copied directly from the CEC project_setup.R code
library(RODBC)


master <- "C:/FIA_BioSum/edf_072020/db/master.mdb"
conn <- odbcConnectAccess2007(master)
cond <- sqlFetch(conn, "cond", as.is = TRUE)


sqlQuery(conn, 'SELECT * INTO cond_backup FROM cond')

conditions.to.remove <- data.frame("biosum_cond_id" = cond$biosum_cond_id[cond$reservcd == 1 | cond$landclcd != 1]) #get condition ids where reservcd is equal to 1 or landclcd is not equal to 1


sqlSave(conn, dat = conditions.to.remove, tablename = "conditions_to_remove", rownames = FALSE)


sqlQuery(conn, 'DELETE cond.*, cond.biosum_cond_id FROM cond WHERE (((cond.biosum_cond_id) In (Select conditions_to_remove.[biosum_cond_id] from conditions_to_remove)));')



odbcCloseAll()
