#run this after rankings and swears

ranks_min <- ranks %>% select(Comedian, Ranking = adj_avg) %>% filter(Ranking < 50)

model_swears <- inner_join(ranks_min, swear_matrix)
model_swears <- model_swears %>% select(Comedian, Swears = total_freq, Ranking) %>% 
  mutate(Swears = 100*Swears)

model_swears$decade <- c("70s", "70s", "90s", "00s", "00s", "80s", "80s", "10s", "60s", "90s", 
                         "00s", "60s", "90s", "00s", "00s", "10s")

lm_swears <- lm(data = model_swears, Ranking ~ Swears)
summary(lm_swears)

factor(model_swears$decade, levels = c("60s", "70s", "80s", "90s", "00s", "10s"))

ggplot(data = model_swears, mapping = aes(x = Swears, y = Ranking, label = Comedian, color = decade)) +
  geom_point() + geom_text(aes(label = Comedian), hjust = 0, vjust = 0) +
  scale_x_continuous(limits = c(0, 12)) + scale_color_discrete(name = "Decade")


top10_word_matrix <- word_matrix %>% select(Comedian = Name, top10_words$word)

model_words <- inner_join(ranks_min, top10_word_matrix)
model_words <- model_words %>% select(-Comedian)

lm_words <- lm(data = model_words, Ranking ~ .)
summary(lm_words)
