library(tidyverse)

# Importing John Mulaney's tour history. This one is easy

mulaney_tour <- read_tsv("shows//mulaney.txt") %>% unique() %>% mutate(comedian = "John Mulaney")

#Jim Gaffigan. This one is annoying. Here's an empty dataframe we'll throw some stuff into 

gaffigan_tour <- as.tibble(matrix( nrow = 37, ncol = 3))
colnames(gaffigan_tour) <- c("venue", "date", "citystate")

gaffigan_tour$comedian <- "Jim Gaffigan"

# Okay so we read in the txt copy-pasted from his website and filter out some comments
# There's also a couple dates filtered out because they didn't copy-paste right
# And I'm okay with having two missing tour dates


gaffigan_tour_raw <- read_table("shows//gaffigan.txt", col_names = FALSE) %>% 
  mutate(comedian = "Jim Gaffigan") %>% filter(X1 != "TICKETS" & X1 != "Sold Out!" & 
       X1 != "Early Show" & X1 != "Late Show")

# Everything is in one column, which sucks
# So here are some for loops to split them up into different columns
# There's probably a way to do this in tidyr or dplyr but I couldn't figure it out
# If I go back and learn I'll change this

i <- 3
k <- 1
while(i <= length(gaffigan_tour_raw$X1)){
  gaffigan_tour$citystate[k] <- gaffigan_tour_raw$X1[i]
  i <- i+3
  k <- k+1
}

i <- 1
k <- 1
while(i <= length(gaffigan_tour_raw$X1)){

  gaffigan_tour$date[k] <- gaffigan_tour_raw$X1[i]
  i <- i+3
  k <- k+1
}

i <- 2
k <- 1
while(i <= length(gaffigan_tour_raw$X1)){
  gaffigan_tour$venue[k] <- gaffigan_tour_raw$X1[i]
  i <- i+3
  k <- k+1
}

# Okay let's move on to Louis CK

ck_tour_raw <- read_table("shows//ck.txt", col_names = FALSE) %>%  
  mutate(comedian = "Louis CK") %>% filter(X1 != "TICKETS" & X1 != "Sold Out!" & 
        X1 != "Early Show" & X1 != "Late Show" & X1 != "Early show" & X1 != "Late show" &
          X1 != "Benefit- All proceeds go to \"Equality NC\"" & X1 != "Late show - RECENTLY ADDED")

# Same deal

ck_tour <- as.tibble(matrix( nrow = 207, ncol = 3))
colnames(ck_tour) <- c("venue", "date", "citystate")

ck_tour$comedian <- "Louis CK"

# Same deal

i <- 3
k <- 1
while(i <= length(ck_tour_raw$X1)){
  ck_tour$citystate[k] <- ck_tour_raw$X1[i]
  i <- i+3
  k <- k+1
}

i <- 1
k <- 1
while(i <= length(ck_tour_raw$X1)){
  ck_tour$date[k] <- ck_tour_raw$X1[i]
  i <- i+3
  k <- k+1
}

i <- 2
k <- 1
while(i <= length(ck_tour_raw$X1)){
  ck_tour$venue[k] <- ck_tour_raw$X1[i]
  i <- i+3
  k <- k+1
}