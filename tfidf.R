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
                    word != "hannibal" & word != "chappelle" & word != "ll" & word != "tm" &
                  word != "masseuse" & word != "chuckles") %>%
  filter(tf_idf == max(tf_idf)) %>% select(name, word, n, tf_idf)
# 
# AS_tfidf <- idf %>% filter(name == "Amy Schumer")
# DC_tfidf <- idf %>% filter(name == "Dave Chappelle")
# 
# wordcloud(AS_tfidf$word, AS_tfidf$tf_idf, max.words = 30)
# wordcloud(DC_tfidf$word, DC_tfidf$tf_idf, max.words = 30)

#clustering

word_matrix <- as.tibble(rbind(t(left_join(as.tibble(top500_words), AS_norm) %>% select(rf)), 
                     t(left_join(as.tibble(top500_words), AA_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), AW_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), Burr_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), BH_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), BB_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), BP_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), CR_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), DC_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), DM_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), DG_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), DT_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), EM_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), FB_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), GC_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), IS_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), JF_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), JC_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), JG_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), JJ_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), JM_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), CK_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), LB_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), MB_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), NB_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), PO_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), RP_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), RW_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), RG_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), SH_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), TS_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), SW_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), RF_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), JS_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), HB_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), LC_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), MH_norm) %>% select(rf)),
                     t(left_join(as.tibble(top500_words), BE_norm) %>% select(rf))
                     )
)

colnames(word_matrix) <- t(top500_words)

word_matrix <- cbind(comedians_list, word_matrix)

colnames(word_matrix)[1] <- "Name"

clusters <- kmeans(word_matrix[2:501], 10, nstart = 20)
word_matrix$cluster <- as.factor(clusters$cluster)

comedy_clusters <- word_matrix %>% select(Name, cluster)

#Looking at each cluster in detail

cluster1_words <- cbind( top500_words, as.tibble(clusters$centers[1,]) ) %>% arrange(desc(value))
cluster2_words <- cbind( top500_words, as.tibble(clusters$centers[2,]) ) %>% arrange(desc(value))
cluster3_words <- cbind( top500_words, as.tibble(clusters$centers[3,]) ) %>% arrange(desc(value))
cluster4_words <- cbind( top500_words, as.tibble(clusters$centers[4,]) ) %>% arrange(desc(value))
cluster5_words <- cbind( top500_words, as.tibble(clusters$centers[5,]) ) %>% arrange(desc(value))
cluster6_words <- cbind( top500_words, as.tibble(clusters$centers[6,]) ) %>% arrange(desc(value))
cluster7_words <- cbind( top500_words, as.tibble(clusters$centers[7,]) ) %>% arrange(desc(value))
cluster8_words <- cbind( top500_words, as.tibble(clusters$centers[8,]) ) %>% arrange(desc(value))
cluster9_words <- cbind( top500_words, as.tibble(clusters$centers[9,]) ) %>% arrange(desc(value))
cluster10_words <- cbind( top500_words, as.tibble(clusters$centers[10,]) ) %>% arrange(desc(value))

 
zoom_matrix <- as.tibble(rbind(t(left_join(as.tibble(top500_words), AA_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), DM_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), DT_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), IS_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), JF_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), JC_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), PO_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), RW_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), RG_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), SW_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), JS_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), HB_norm) %>% select(rf)),
                               t(left_join(as.tibble(top500_words), MH_norm) %>% select(rf))
                               )
)

colnames(zoom_matrix) <- t(top500_words)

zoom_names <- c("Aziz Ansari", "Demetri Martin", "Daniel Tosh", "Iliza Schlesinger", "Jeff Foxworthy",
                "Jimmy Carr", "Patton Oswalt", "Ron White", "Ricky Gervais",
                "Steven Wright", "Jerry Seinfeld", "Hannibal Burress", "Mitch Hedberg")

zoom_matrix <- cbind(zoom_names, zoom_matrix)

colnames(zoom_matrix)[1] <- "Name"

zclusters <- kmeans(zoom_matrix[2:501], 4, nstart = 20)
zoom_matrix$cluster <- as.factor(zclusters$cluster)

zcomedy_clusters <- zoom_matrix %>% select(Name, cluster)

tree_names <- word_matrix %>% select(-Name)
rownames(tree_names) <- comedians_list
tree <- hclust(dist(tree_names))
plot(tree)
