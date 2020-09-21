################
### .BAT files

#~ make sure all .bat files exist
#~ create new .bat that will automate running all other .bats
#~ this will be done for each variant and all 4 canbe run simultanously (assuming 4-core compute power)
library(stringr)
library(dplyr)

master_dir <- "C:/FIA_BioSum/edf_072020/fvs/data"


all_var <- c("CA", "NC", "SO", "WS")


for (i in 1:length(all_var)) {
  ### get list of all .kcps as reference
  kcps <- list.files(file.path(master_dir, all_var[i]), pattern = glob2rx("FVSOUT_*.KCP"))
  ## rm baseyear
  kcp_filt <- kcps[str_detect(kcps, "BaseYr", negate = T)]
  # get list of .bats
  bats <- list.files(file.path(master_dir, all_var[i]), pattern = glob2rx("*.bat"))
  
  # make sure that all bats and kcps match
  if(length(setdiff(substr(bats,1,14), substr(kcp_filt,1,14))) != 0) {
    
    stop(paste("difference between bats and kcps. check file: \n", setdiff(substr(bats,1,14), substr(kcp_filt,1,14))))
    
  } 
  # just to be sure
  if(length(unique(bats)) != length(unique(kcp_filt))) {
    stop(paste("different length of unique bat vs kcp files in variant folder:", all_var[i]))
    
  } 
  
  ### create initial file dirrecting to variant directory
  cat(paste("cd", all_var[i]), file = file.path(master_dir, paste("runall_", all_var[i], ".bat", sep = "")), sep = "\n")
  
  ## for each variant create a new .bat that will call each .bat in that variant successivley 
  for (j in 1:length(bats)) {
    
    
    cat(paste("call", bats[j]), file = file.path(master_dir, paste("runall_", all_var[i], ".bat", sep = "")), sep = "\n", append = TRUE)
    
  }
  
}
















