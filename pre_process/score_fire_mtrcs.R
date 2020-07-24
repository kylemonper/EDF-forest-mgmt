
score_it <- function(x) {
  #Calculate Fire Resistance Score by scoring each ofthe above 4 variable from 0-3 and then summing the scores. 
  #Canopy Bulk Density: 
  ## > 0.15           | score = 0
  ## > 0.11 & <= 0.15 | score = 1
  ## > 0.05 & <= 0.11 | score = 2
  ##  <= 0.05         | score = 3
  
  #Canopy Base Height: 
  ## <= 7         | score = 0
  ## > 7 & <= 20  | score = 1
  ## > 20 & <= 30 | score = 2
  ## > 30         | score = 3
  
  #Percent Resistant Basal Area: 
  ## <= .25         | score = 0
  ## > .25 & <= .50 | score = 1
  ## > .50 & <= .75 | score = 2
  ## > .75          | score = 3
  
  #Survival Volume Ratio:
  ## <= .02         | score = 0
  ## > .02 & <= .30 | score = 1
  ## > .30 & <= .75 | score = 2
  ## > .75          | score = 3
  
  
  x$CBD_Score <- cut(x$Canopy_Density, breaks = c(-0.1, 0.05, 0.11, 0.15, max(x$Canopy_Density, na.rm = TRUE)), labels = c(3,2,1,0), right = TRUE)
  x$CBH_Score <- cut(x$CBH, breaks = c(-0.1, 7, 20, 30, max(x$CBH, na.rm = TRUE)), labels = c(0,1,2,3), right = TRUE)
  x$PERRESBA_score <- cut(x$PERRESBA, breaks = c(-0.1, 0.25, 0.50, 0.75 ,max(x$PERRESBA, na.rm = TRUE)), labels = c(0,1,2,3), right = TRUE)
  x$SurvVolRatio_score <- cut(x$SurvVolRatio, breaks = c(-0.1,0.02,0.30,0.75,max(x$SurvVolRatio, na.rm = TRUE)), labels = c(0,1,2,3), right = TRUE)
  
  
  x$CBD_Score <- as.numeric(as.character(x$CBD_Score))
  x$CBH_Score <- as.numeric(as.character(x$CBH_Score))
  x$PERRESBA_score <- as.numeric(as.character(x$PERRESBA_score))
  x$SurvVolRatio_score <- as.numeric(as.character(x$SurvVolRatio_score))
  
  
  columns <- c(which(names(x) == "CBD_Score"), which(names(x) == "CBH_Score"), which(names(x) == "PERRESBA_score"), which(names(x) == "SurvVolRatio_score"))
  
  x$FRS <- rowSums(x[,columns], na.rm = TRUE) 
  
  #Calculate Hazard Score by scoring each ofthe above 4 variable as 0 or 1 and then summing the scores. 
  #This function also calculates MortVolPct2 by taking the Mortality_VOL_Sev and dividing by non-zero TCUft values. It also does this
  #for Mortality_VOL_Mod. This produces to Hazard Scores: one for Severe and one for Moderate. It also adjusts the Torch Index
  #values that are too high or too low. 
  
  #Torch Index: 
  ## < 20  | score = 1
  ## >= 20 | score = 0
  
  #PTorch_Sev: 
  ## > 0.20  | score = 1
  ## <= 0.20 | score = 0
  
  #MortVolPct_Sev: 
  ## > 0.30  | score = 1
  ## <= 0.30 | score = 0
  
  #MortVolPct_Mod: 
  ## > 0.30  | score = 1
  ## <= 0.30 | score = 0
  
  #Surf_Flame_Sev: 
  ## > 4  | score = 1
  ## <= 4 | score = 0
  
  
  
  x$MortVolPct_FOFEM <- x$MortVol_FOFEM/x$VolSum
  x$MortVolPct_FOFEM[x$VolSum == 0] <- 0
  
  x$MortVolPct2_Sev <- x$Mortality_VOL_Sev/x$TCuFt
  x$MortVolPct2_Sev[x$TCuFt == 0] <- 0
  
  x$MortVolPct2_Mod <- x$Mortality_VOL_Mod/x$TCuFt
  x$MortVolPct2_Mod[x$TCuFt == 0] <- 0
  
  x$Torch_Index2 <- x$Torch_Index
  x$Torch_Index2[x$Torch_Index < 0] <- 102
  x$Torch_Index2[x$Torch_Index > 100] <- 101
  

  
  x$Torch_Index_score <- cut(x$Torch_Index2, breaks = c(-0.1, 20, max(x$Torch_Index2, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$PTorch_Sev_Score <- cut(x$PTorch_Sev, breaks = c(-0.1, 0.2, max(x$PTorch_Sev, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$Surf_Flame_Sev_score <- cut(x$Surf_Flame_Sev, breaks = c(-0.1, 4, max(x$Surf_Flame_Sev, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$MortVolPct2_Sev_Score <- cut(x$MortVolPct2_Sev, breaks = c(-0.1, 0.3, max(x$MortVolPct2_Sev, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  x$MortVolPct2_Mod_Score <- cut(x$MortVolPct2_Mod, breaks = c(-0.1, 0.3, max(x$MortVolPct2_Mod, na.rm = TRUE)), labels = c(1,0), right = TRUE)
  
  
  x$Torch_Index_score<- as.numeric(as.character(x$Torch_Index_score))
  x$PTorch_Sev_Score <- as.numeric(as.character(x$PTorch_Sev_Score))
  x$Surf_Flame_Sev_score<- as.numeric(as.character(x$Surf_Flame_Sev_score))
  x$MortVolPct2_Sev_Score <- as.numeric(as.character(x$MortVolPct2_Sev_Score))
  x$MortVolPct2_Mod_Score <- as.numeric(as.character(x$MortVolPct2_Mod_Score))
  
  columns_sev <- c(which(names(x) == "Torch_Index_score"), which(names(x) == "PTorch_Sev_Score"), which(names(x) == "Surf_Flame_Sev_score"), which(names(x) == "MortVolPct2_Sev_Score"))
  columns_mod <- c(which(names(x) == "Torch_Index_score"), which(names(x) == "PTorch_Sev_Score"), which(names(x) == "Surf_Flame_Sev_score"), which(names(x) == "MortVolPct2_Mod_Score"))
  
  x$HS_Sev <- rowSums(x[,columns_sev], na.rm = TRUE)
  x$HS_Mod <- rowSums(x[,columns_mod], na.rm = TRUE) 
  
  return(x)
}