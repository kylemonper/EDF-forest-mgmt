##########################
## this code takes master base kcp files (for standard cuts w/o fires) from one directory
## fills out the fire scenarios by copy these files and appending the appropriate fire cmd
## then copies this copmplete set of kcps into other variants, updating names as appropriate

## to set this up you need to make 4 txt files with fire cmds (one for each cycle)
## and make sure that the base kcp (one w/o fire) is filled out within the "master folder"
## all other .kcp should have been created as templates by biosum

library(tidyverse)
library(stringr)


master_dir <- "C:/FIA_BioSum/edf_072020/fvs/data/CA"

other_var <- c("NC", "SO", "WS")
all_var <- c("CA", "NC", "SO", "WS")

## first remove the .template file endings created by biosum
for(i in 1:length(all_var)) {
  # back out from master dir and into other_var dir
  path <- file.path(master_dir, "..", all_var[i])
  
  #look for templates
  templates <- list.files(path = path, pattern = "*.template")
  
  #if no templates then skip loop
  if(length(templates) == 0) next
  
  for(j in 1:length(templates)) {
    new_name <- tools::file_path_sans_ext(templates[j]) # rm .template extension
    file.remove(file.path(path, templates[j])) # del old file
    file.create(file.path(path, new_name)) # create new file
  }
  
}

## list of unique base packages
## !!!! update based on number of "unique" packages !!!!
#~ the KCP that ends in 0 following these base characters should have the txt containing
#~ the standard .KCP to be copied into each other pkg that will have the fire seq. appended to it
uniq_pkg <- c("P01", "P70", "P99", "P02", "P03", "P04")




### get txt files that will be appended to standard cycles to add fires during certain cycles
fire1 <- readLines(file.path(master_dir, "fire_cyc1.txt"))
fire2 <- readLines(file.path(master_dir, "fire_cyc2.txt"))
fire3 <- readLines(file.path(master_dir, "fire_cyc3.txt"))
fire4 <- readLines(file.path(master_dir, "fire_cyc4.txt"))




##########################
####### workflow: ########
#~ list all .kcp files in master directory
#~ filter for each base package (ie uniq_pkg)
#~~~ copy base package (eg PO10, P700, etc) into other files and append with the appropriate fire
#~~~~~~ex: anything that ends in 1 (eg P**1) will be appended with fire1
# then copy this complete master list into the other variants
#~~ need to update file anme and .MDB outpute directory with new variant name


#### list all master kcp files
files <- list.files(master_dir, pattern = glob2rx("FVSOUT_*.KCP"))
#####

for (i in 1:length(uniq_pkg)) {
  
  pkg <- files[str_detect(files, uniq_pkg[i])]

  # base pkg is one where _P*** doesnt end in 1,2,3 or 4
  base_pkg <- pkg[str_detect(substr(pkg, 1, 15), c("1-|2-|3-|4-"), negate = T)]
  other_pkg <- pkg[str_detect(substr(pkg, 1, 15), c("1-|2-|3-|4-"), negate = F)]
  
  base_kcp <- readLines(file.path(master_dir, base_pkg))
  
  ## need to replace output .MDB with proper file name and add fire commands
  #~ done for each package
  for (j in 1:length(other_pkg)) {
    
    # remove .KCP to get file name
    base_name_old <- substr(base_pkg, 1, nchar(base_pkg)-4)
    base_name_new <- substr(other_pkg[j], 1, nchar(other_pkg[j])-4)
    
    ## update FVSOUT_*.MDB
    new_kcp <- str_replace(base_kcp, base_name_old, base_name_new)
    
    ## replace old template file with updated kcp
    cat(new_kcp, file = file.path(master_dir, other_pkg[j]), sep = "\n")

    
    ## detect the ending of P*** and append with appropriate fire file
    if(str_detect(substr(other_pkg[j], 1, 15), "1-")) {
      cat(fire1, file = file.path(master_dir, other_pkg[j]), sep = "\n", append = T)
    } else if(str_detect(substr(other_pkg[j], 1, 15), "2-")) {
      cat(fire2, file = file.path(master_dir, other_pkg[j]), sep = "\n", append = T)
    } else if(str_detect(substr(other_pkg[j], 1, 15), "3-")) {
      cat(fire3, file = file.path(master_dir, other_pkg[j]), sep = "\n", append = T)
    } else if(str_detect(substr(other_pkg[j], 1, 15), "4-")) {
      cat(fire4, file = file.path(master_dir, other_pkg[j]), sep = "\n", append = T)
    }
    
    
  }
  
}


### at this point we have a finished set of packages for 1 variant. 
##~~ need to copy these over to the other variants, making appropriate updates to files/names
##~~ going to do this file by file and copy each into the other variant directories

for(i in 1:length(files)) {
  # read in .kcp
  master_kcp <- readLines(file.path(master_dir, files[i]))
  
  # apply this ^ into each other variant
  for(j in 1:length(other_var)) {
    # update output .MDB chaning only the variant name
    new_kcp <- str_replace(master_kcp, "FVSOUT_CA", paste("FVSOUT_", other_var[j] , sep = ""))
    
    # get new name for file 
    new_file <- str_replace(files[i], "CA", other_var[j])
    
    
    
    cat(new_kcp, file = file.path(master_dir, "..", other_var[j], new_file), sep = "\n")
    
  }
  
}
















