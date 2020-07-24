### function that goes within the add_discounting.R workflow
### this extrapolates smoke emissions data for the gap years 
### which we do not have data for 



infer_annual_smoke <- function(plot_time) {
  
  #### scratch work for dealing w/ the fire data
  ## fill in NA is appropriate constants
  plot_time$prob <- plot_time$prob[2]
  plot_time$post_cut_gwth_rt <- plot_time$post_cut_gwth_rt[2]
  
  
  # assuming smoke in year -1 is equal to the smoke in year two, minus the diff btwn yr 0 and 1
  plot_time$smoke_c_mod[1] <- plot_time$smoke_c_mod[2] - (plot_time$smoke_c_mod[3] - plot_time$smoke_c_mod[2])
  # assuming smoke in year 2:9 changes at linear rate btwn year 1:10
  ## get diff:
  smoke_diff <- plot_time$smoke_c_mod[12] - plot_time$smoke_c_mod[3]
  time_diff <- plot_time$time[12] - plot_time$time[3]
  annl_diff <- smoke_diff/time_diff 
  ## apply diff to fill in blanks
  for (i in 4:11) {
    
    plot_time$smoke_c_mod[i] <- plot_time$smoke_c_mod[i-1] + annl_diff
    
  }
  ## repeat above for next gap
  smoke_diff <- plot_time$smoke_c_mod[22] - plot_time$smoke_c_mod[13]
  time_diff <- plot_time$time[22] - plot_time$time[13]
  annl_diff <- smoke_diff/time_diff 
  ## apply diff to fill in blanks
  for (i in 14:21) {
    
    plot_time$smoke_c_mod[i] <- plot_time$smoke_c_mod[i-1] + annl_diff
    
  }
  ## repeat above for next gap
  smoke_diff <- plot_time$smoke_c_mod[32] - plot_time$smoke_c_mod[23]
  time_diff <- plot_time$time[32] - plot_time$time[23]
  annl_diff <- smoke_diff/time_diff 
  ## apply diff to fill in blanks
  for (i in 24:31) {
    
    plot_time$smoke_c_mod[i] <- plot_time$smoke_c_mod[i-1] + annl_diff
    
  }
  
  
  
  ### repeat for smoke_c_sev
  
  plot_time$smoke_c_sev[1] <- plot_time$smoke_c_sev[2] - (plot_time$smoke_c_sev[3] - plot_time$smoke_c_sev[2])
  
  smoke_diff <- plot_time$smoke_c_sev[12] - plot_time$smoke_c_sev[3]
  time_diff <- plot_time$time[12] - plot_time$time[3]
  annl_diff <- smoke_diff/time_diff 
  
  for (i in 4:11) {
    
    plot_time$smoke_c_sev[i] <- plot_time$smoke_c_sev[i-1] + annl_diff
    
  }
  
  smoke_diff <- plot_time$smoke_c_sev[22] - plot_time$smoke_c_sev[13]
  time_diff <- plot_time$time[22] - plot_time$time[13]
  annl_diff <- smoke_diff/time_diff 
  
  for (i in 14:21) {
    
    plot_time$smoke_c_sev[i] <- plot_time$smoke_c_sev[i-1] + annl_diff
    
  }
  
  smoke_diff <- plot_time$smoke_c_sev[32] - plot_time$smoke_c_sev[23]
  time_diff <- plot_time$time[32] - plot_time$time[23]
  annl_diff <- smoke_diff/time_diff 
  
  for (i in 24:31) {
    
    plot_time$smoke_c_sev[i] <- plot_time$smoke_c_sev[i-1] + annl_diff
    
  }
  
  annual_smoke <- plot_time %>% 
    mutate(annl_smoke_mod = smoke_c_mod * (prob/100),
           annl_smoke_sev = smoke_c_sev * (prob/100),
           annl_regwth_rt = post_cut_gwth_rt * (prob/100))
  
  return(annual_smoke)
  
}  
