library(tidyverse)
library(readxl)

ranks <- read_xlsx("rankings.xlsx")
ranks <- ranks %>% mutate(Avg = rowMeans(ranks[2:4], na.rm = TRUE))

ranks$adjustment <- rowSums(is.na(ranks))
ranks$adj_avg <- ranks$Avg / ( 1- (ranks$adjustment / 3))

write_csv(ranks, "Comedy_Interactive//ranks.csv")
