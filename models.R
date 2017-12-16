#run this after rankings and swears

ranks_min <- ranks %>% select(Comedian, Ranking = adj_avg) %>% filter(Ranking < 50)

model_swears <- inner_join(ranks_min, swear_matrix)
model_swears <- model_swears %>% select(Comedian, Swears = total_freq, Ranking) %>% 
  mutate(Swears = 100*Swears)


lm_swears <- lm(data = model_swears, Ranking ~ Swears)
summary(lm_swears)

ggplot(data = model_swears, mapping = aes(x = Swears, y = Ranking)) + geom_point()


top10_word_matrix <- word_matrix %>% select(Comedian = Name, top10_words$word)

model_words <- inner_join(ranks_min, top10_word_matrix)
model_words <- model_words %>% select(-Comedian)

lm_words <- lm(data = model_words, Ranking ~ .)
summary(lm_words)
