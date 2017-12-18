library(tidyverse)
library(maps)
library(mapdata)

all_states <- map_data("state")
state_for_map$region <- sapply(state_for_map$region, tolower)
tours_by_state <- full_join(state_for_map, all_states)
tours_by_state$region <- as.factor(tours_by_state$region)
tours_by_state <- tours_by_state %>% filter(is.na(long) == FALSE)

tours_by_state[is.na(tours_by_state)] <- 0


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  legend.title = element_blank()
)

combined_state_map <- full_join(combined_state_freqs, all_states)
combined_state_map$region <- as.factor(combined_state_map$region)
combined_state_map$Total <- as.numeric(combined_state_map$Total)

mid <- mean(combined_state_map$Total, na.rm = TRUE)
lmid <- log(mid)

ggplot(data = combined_state_map) + geom_polygon(aes(x = long, y = lat, group = group, fill = Total),
                                                 color = "black") +
  scale_fill_gradient2(midpoint = lmid, low = "red", high = "blue", mid = "white",
                       space = "Lab", trans = "log") + coord_fixed(1.3) + ditch_the_axes + 
  labs(title = "Frequency of Comedian Visits to US States")


ggplot() + geom_polygon(data = tours_by_state,
                        mapping = aes(x = long, y = lat, group = group, fill = `Jeff Foxworthy`),
                        color = "black") +
  coord_fixed(1.3) + ditch_the_axes + 
  scale_fill_gradient2(midpoint = log(0.05), low = "red", high = "blue", mid = "white",
                       space = "Lab", trans = "log") +
  labs(title = "Jeff Foxworthy")

ggplot() + geom_polygon(data = tours_by_state,
                        mapping = aes(x = long, y = lat, group = group, fill = `Ron White`),
                        color = "black") +
  coord_fixed(1.3) + ditch_the_axes + 
  scale_fill_gradient2(midpoint = log(0.05), low = "red", high = "blue", mid = "white",
                       space = "Lab", trans = "log") +
  labs(title = "Ron White")

ggplot() + geom_polygon(data = tours_by_state,
                        mapping = aes(x = long, y = lat, group = group, fill = `Larry the Cable Guy`),
                        color = "black") +
  coord_fixed(1.3) + ditch_the_axes + 
  scale_fill_gradient2(midpoint = log(0.05), low = "red", high = "blue", mid = "white",
                       space = "Lab", trans = "log") +
  labs(title = "Larry the Cable Guy")

ggplot() + geom_polygon(data = tours_by_state,
                        mapping = aes(x = long, y = lat, group = group, fill = `Bill Engvall`),
                        color = "black") +
  coord_fixed(1.3) + ditch_the_axes + 
  scale_fill_gradient2(midpoint = log(0.05), low = "red", high = "blue", mid = "white",
                       space = "Lab", trans = "log") +
  labs(title = "Bill Engvall")

ggplot() + geom_polygon(data = tours_by_state,
                        mapping = aes(x = long, y = lat, group = group, fill = `Amy Schumer`),
                        color = "black") +
  coord_fixed(1.3) + ditch_the_axes + 
  scale_fill_gradient2(midpoint = log(0.05), low = "red", high = "blue", mid = "white",
                       space = "Lab", trans = "log") +
  labs(title = "Amy Schumer")
