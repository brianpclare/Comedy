library(tidyverse)

swears <- c("fuck", "fucking", "shit", "asshole", "motherfucker", "fucked",
                      "motherfuckers", "fuckin", "cunt")
swear_matrix <- word_matrix %>% select(Name, swears)
swear_matrix <- swear_matrix %>% mutate(total_freq = rowSums(swear_matrix[2:10])) %>% 
  select(Comedian = Name, total_freq)
