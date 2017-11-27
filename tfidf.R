library(tidyverse)
library(readr)
library(tidytext)

#Run import transcripts first

idf <- master %>% bind_tf_idf(word, name, n)

distinctive_words <- idf %>% group_by(name) %>% summarize(distinctive = max(tf_idf))

individuality <- idf %>% group_by(name) %>% summarize(score = mean(tf_idf))
