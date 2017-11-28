library(tidyverse)
library(readr)
library(tidytext)
library(wordcloud)

#Run import transcripts first

idf <- master %>% bind_tf_idf(word, name, n)

distinctiveness <- idf %>% group_by(name) %>% summarize(mean = mean(tf_idf),
                                        median = median(tf_idf), wordcount = n())

individuality <- idf %>% group_by(name) %>% filter(word != "heh" & word != "ha" & 
                  word != "don" & word != "ve" & word != "ain" & word != "merrily" &
                    word != "hannibal" & word != "chappelle" & word != "ll") %>%
  filter(tf_idf == max(tf_idf)) %>% select(name, word, n, tf_idf)

AS_tfidf <- idf %>% filter(name == "Amy Schumer")
DC_tfidf <- idf %>% filter(name == "Dave Chappelle")

wordcloud(AS_tfidf$word, AS_tfidf$tf_idf, max.words = 30)
wordcloud(DC_tfidf$word, DC_tfidf$tf_idf, max.words = 30)
