library(tidyverse)
library(tidytext)
library(readr)
library(stringr)
library(ggvis)


wordsearch <- function(term){
  ggplot(data = filter(master, word == term) %>% 
           top_n(10, rf), mapping = aes(x = reorder(name, -rf), y = rf, fill = name)) + 
    geom_col() + theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
    labs(x = "Comedian", y = "Frequency", title = str_c("Comedians who say '", term, "' most frequently"))
}

wordsearch("dave")

wordsearch2 <- function(term){
  master %>% filter(word == term) %>% top_n(10, rf) %>% 
  ggvis(x = ~name, y = ~rf) %>%  layer_bars()
  #+ theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
   # labs(x = "Comedian", y = "Frequency", title = str_c("Comedians who say '", term, "' most frequently"))
}

wordsearch2("redneck")
