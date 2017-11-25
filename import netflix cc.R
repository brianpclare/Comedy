library(tidyverse)
library(stringr)
library(tidytext)

JG_Cinco <- readLines("netflix\\jim gaffigan cinco.txt")

JG_Cinco <- str_extract_all(JG_Cinco, pattern = '>[^>]*<') 

JG_Cinco <- data.frame(matrix(unlist(JG_Cinco)))
colnames(JG_Cinco)[1] <- 'X1'

JG_Cinco <- JG_Cinco %>% unnest_tokens(word, X1)
