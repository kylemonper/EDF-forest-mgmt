#### thinking about arrays
library(tidyverse)


gen_data <- function(x) {
  set.seed(x)
  df <- data.frame(id = 1:6, value = rnorm(6))
  return(df)
}

runs <- lapply(1:1000, gen_data)
df <- dplyr::bind_rows(runs)
means <- df %>% 
  group_by(id) %>% 
  summarise(mean = mean(value),
            sd = sd(value))
