library(tidyverse)
library(ggmap)

#A function to read in tour history data
#From bandsintown.com

read_tour <- function(file, comedian){
  doc <- read_tsv(file, col_names = TRUE) %>% unique() %>% select(-Ignore) %>% 
    mutate(name = comedian) %>%
    separate(col = Location, into = c("City", "State"), sep = ", ")
  return(doc)
}

#Function to add the year to recent dates

fix_dates <- function(table){
  for(i in 1:length(table$Date)){
    if(!grepl(",", table$Date[i], fixed = TRUE)){
      table$Date[i] <- str_c(table$Date[i], ", 2017", sep = "")
    }
  }
  
  table <- table %>% separate(col = Date, into = c("MD", "Year"), sep = ",")
  table <- table %>% separate(col = MD, into = c("Month", "Day"), sep = " ")
  return(table)
}

#Who is in the US the most?
US_ratio <- function(tour, US){
  ratio <- length(US$Venue) / length(tour$Venue)
  return(ratio)
}

data(state)
state_list <- as.tibble(matrix(state.abb), bycol = FALSE)
colnames(state_list) <- c("State")

by_state <- function(comedian){
  df <- comedian %>% group_by(State) %>% summarize(count = n()) %>% right_join(state_list) %>% 
    mutate(rel_freq = count / sum(count, na.rm = TRUE))
  return(df)
}

tour_names <- c("Amy Schumer", "Aziz Ansari", "Bill Burr", "Bill Engvall", "Bo Burnham", "Brian Posehn",
                "Chris Rock", "Daniel Tosh", "Dave Chappelle", "Demetri Martin", "Frankie Boyle", "Hannibal Burress", 
                "Iliza Schlesinger", "Jeff Foxworthy", "Jim Gaffigan", "Jim Jefferies", "Jimmy Carr",
                "John Mulaney", "Louis CK", "Neal Brennan", "Patton Oswalt", "Ron White", "Tom Segura",
                "Larry the Cable Guy", "Maria Bamford")

Ratios <- as.tibble(matrix(c(0), nrow = length(tour_names), ncol = 3))
colnames(Ratios) <- c("Comedian", "Ratio of US Shows", "Number of Shows")
Ratios$Comedian <- tour_names

#Amy Schumer
schumer_tour <- read_tour("shows//amy schumer.txt", "Amy Schumer")
schumer_tour <- fix_dates(schumer_tour)
schumer_US <- schumer_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[1] <- US_ratio(schumer_tour, schumer_US)
Ratios$`Number of Shows`[1] <- length(schumer_tour$Venue)

#Aziz Ansari
ansari_tour <- read_tour("shows//aziz ansari.txt", "Aziz Ansari")
ansari_tour <- fix_dates(ansari_tour)
ansari_US <- ansari_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[2] <- US_ratio(ansari_tour, ansari_US)
Ratios$`Number of Shows`[2] <- length(ansari_tour$Venue)

#Bill Burr
burr_tour <- read_tour("shows//bill burr.txt", "Bill Burr")
burr_tour <- fix_dates(burr_tour)
burr_US <- burr_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[3] <- US_ratio(burr_tour, burr_US)
Ratios$`Number of Shows`[3] <- length(burr_tour$Venue)

#Bill Engvall
engvall_tour <- read_tour("shows//bill engvall.txt", "Bill Engvall")
engvall_tour <- fix_dates(engvall_tour)
engvall_US <- engvall_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[4] <- US_ratio(engvall_tour, engvall_US)
Ratios$`Number of Shows`[4] <- length(engvall_tour$Venue)

#Bo Burnham
burnham_tour <- read_tour("shows//bo burnham.txt", "Bo Burnham")
burnham_tour <- fix_dates(burnham_tour)
burnham_US <- burnham_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[5] <- US_ratio(burnham_tour, burnham_US)
Ratios$`Number of Shows`[5] <- length(burnham_tour$Venue)

#Brian Posehn
posehn_tour <- read_tour("shows//brian posehn.txt", "Brian Posehn")
posehn_tour <- fix_dates(posehn_tour)
posehn_US <- posehn_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[6] <- US_ratio(posehn_tour, posehn_US)
Ratios$`Number of Shows`[6] <- length(posehn_tour$Venue)

#Chris Rock
rock_tour <- read_tour("shows//chris rock.txt", "Chris Rock")
rock_tour <- fix_dates(rock_tour)
rock_US <- rock_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[7] <- US_ratio(rock_tour, rock_US)
Ratios$`Number of Shows`[7] <- length(rock_tour$Venue)

#Daniel Tosh
tosh_tour <- read_tour("shows//daniel tosh.txt", "Daniel Tosh")
tosh_tour <- fix_dates(tosh_tour)
tosh_US <- tosh_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[8] <- US_ratio(tosh_tour, tosh_US)
Ratios$`Number of Shows`[8] <- length(tosh_tour$Venue)

#Dave Chappelle
chappelle_tour <- read_tour("shows//dave chappelle.txt", "Dave Chappelle")
chappelle_tour <- fix_dates(chappelle_tour)
chappelle_US <- chappelle_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[9] <- US_ratio(chappelle_tour, chappelle_US)
Ratios$`Number of Shows`[9] <- length(chappelle_tour$Venue)

#Demetri Martin
martin_tour <- read_tour("shows//demetri martin.txt", "Demetri Martin")
martin_tour <- fix_dates(martin_tour)
martin_US <- martin_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[10] <- US_ratio(martin_tour, martin_US)
Ratios$`Number of Shows`[10] <- length(martin_tour$Venue)

#Frankie Boyle
boyle_tour <- read_tour("shows//frankie boyle.txt", "Frankie Boyle")
boyle_tour <- fix_dates(boyle_tour)
boyle_US <- boyle_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[11] <- US_ratio(boyle_tour, boyle_US)
Ratios$`Number of Shows`[11] <- length(boyle_tour$Venue)

#Hannibal Burress
burress_tour <- read_tour("shows//hannibal burress.txt", "Hannibal Burress")
burress_tour <- fix_dates(burress_tour)
burress_US <- burress_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[12] <- US_ratio(burress_tour, burress_US)
Ratios$`Number of Shows`[12] <- length(burress_tour$Venue)

#Iliza Shlesinger
iliza_tour <- read_tour("shows//iliza.txt", "Iliza Shlesinger")
iliza_tour <- fix_dates(iliza_tour)
iliza_US <- iliza_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[13] <- US_ratio(iliza_tour, iliza_US)
Ratios$`Number of Shows`[13] <- length(iliza_tour$Venue)

#Jeff Foxworthy
fw_tour <- read_tour("shows//jeff foxworthy.txt", "Jeff Foxworthy")
fw_tour <- fix_dates(fw_tour)
fw_US <- fw_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[14] <- US_ratio(fw_tour, fw_US)
Ratios$`Number of Shows`[14] <- length(fw_tour$Venue)

#Jim Gaffigan
gaffigan_tour <- read_tour("shows//gaffigan.txt", "Jim Gaffigan")
gaffigan_tour <- fix_dates(gaffigan_tour)  
gaffigan_US <- gaffigan_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[15] <- US_ratio(gaffigan_tour, gaffigan_US)
Ratios$`Number of Shows`[15] <- length(gaffigan_tour$Venue)

#Jim Jefferies
jefferies_tour <- read_tour("shows//jim jefferies.txt", "Jim Jefferies")
jefferies_tour <- fix_dates(jefferies_tour)
jefferies_US <- jefferies_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[16] <- US_ratio(jefferies_tour, jefferies_US)
Ratios$`Number of Shows`[16] <- length(jefferies_tour$Venue)

#Jimmy Carr
carr_tour <- read_tour("shows//jimmy carr.txt", "Jimmy Carr")
carr_tour <- fix_dates(carr_tour)
carr_US <- carr_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[17] <- US_ratio(carr_tour, carr_US)
Ratios$`Number of Shows`[17] <- length(carr_tour$Venue)

#John Mulaney
mulaney_tour <- read_tour("shows//mulaney.txt", "John Mulaney")
mulaney_tour <- fix_dates(mulaney_tour)
mulaney_US <- mulaney_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[18] <- US_ratio(mulaney_tour, mulaney_US)
Ratios$`Number of Shows`[18] <- length(mulaney_tour$Venue)

#Louis CK
ck_tour <- read_tour("shows//ck.txt", "Louis CK")
ck_tour <- fix_dates(ck_tour)
ck_US <- ck_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[19] <- US_ratio(ck_tour, ck_US)
Ratios$`Number of Shows`[19] <- length(ck_tour$Venue)

#Neal Brennan
brennan_tour <- read_tour("shows//neal brennan.txt", "Neal Brennan")
brennan_tour <- fix_dates(brennan_tour)
brennan_US <- brennan_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[20] <- US_ratio(brennan_tour, brennan_US)
Ratios$`Number of Shows`[20] <- length(brennan_tour$Venue)

#Patton Oswalt
oswalt_tour <- read_tour("shows//patton oswalt.txt", "Patton Oswalt")
oswalt_tour <- fix_dates(oswalt_tour)
oswalt_US <- oswalt_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[21] <- US_ratio(oswalt_tour, oswalt_US)
Ratios$`Number of Shows`[21] <- length(oswalt_tour$Venue)

#Ron White
white_tour <- read_tour("shows//ron white.txt", "Ron White")
white_tour <- fix_dates(white_tour)
white_US <- white_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[22] <- US_ratio(white_tour, white_US)
Ratios$`Number of Shows`[22] <- length(white_tour$Venue)

#Tom Segura
segura_tour <- read_tour("shows//tom segura.txt", "Tom Segura")
segura_tour <- fix_dates(segura_tour)
segura_US <- segura_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[23] <- US_ratio(segura_tour, segura_US)
Ratios$`Number of Shows`[23] <- length(segura_tour$Venue)

#Larry the Cable Guy
larry_tour <- read_tour("shows//larry the cable guy.txt", "Larry the Cable Guy")
larry_tour <- fix_dates(larry_tour)
larry_US <- larry_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[24] <- US_ratio(larry_tour, larry_US)
Ratios$`Number of Shows`[24] <- length(larry_tour$Venue)

#Maria Bamford
bamford_tour <- read_tour("shows//maria bamford.txt", "Maria Bamford")
bamford_tour <- fix_dates(bamford_tour)
bamford_US <- bamford_tour %>% filter(State %in% state.abb)
Ratios$`Ratio of US Shows`[25] <- US_ratio(bamford_tour, bamford_US)
Ratios$`Number of Shows`[25] <- length(bamford_tour$Venue)

state_freqs <- as.tibble(t(cbind(by_state(schumer_US)$count, by_state(ansari_US)$count, by_state(burr_US)$count,
                     by_state(engvall_US)$count, by_state(burnham_US)$count, by_state(posehn_US)$count, by_state(rock_US)$count,
                     by_state(tosh_US)$count, by_state(chappelle_US)$count, by_state(martin_US)$count,
                     by_state(boyle_US)$count, by_state(burress_US)$count, by_state(iliza_US)$count,
                     by_state(fw_US)$count, by_state(gaffigan_US)$count, by_state(jefferies_US)$count,
                     by_state(carr_US)$count, by_state(mulaney_US)$count, by_state(ck_US)$count, by_state(brennan_US)$count, 
                     by_state(oswalt_US)$count, by_state(white_US)$count, by_state(segura_US)$count,
                     by_state(larry_US)$count, by_state(bamford_US)$count)) )

state_freqs[is.na(state_freqs)] <- 0
colnames(state_freqs) <- state.name

state_freqs <- as.tibble(cbind(tour_names, state_freqs))
colnames(state_freqs)[1] <- "Comedian"

combined_state_freqs <- as.tibble(colSums(state_freqs[2:51]))
combined_state_freqs <- as.tibble(cbind(tolower(state.name), combined_state_freqs$value))
colnames(combined_state_freqs) <- c("region", "Total")

state_rel_freqs <- as.tibble(t(cbind(by_state(schumer_US)$rel_freq, by_state(ansari_US)$rel_freq, by_state(burr_US)$rel_freq,
                                 by_state(engvall_US)$rel_freq, by_state(burnham_US)$rel_freq, by_state(posehn_US)$rel_freq, by_state(rock_US)$rel_freq,
                                 by_state(tosh_US)$rel_freq, by_state(chappelle_US)$rel_freq, by_state(martin_US)$rel_freq,
                                 by_state(boyle_US)$rel_freq, by_state(burress_US)$rel_freq, by_state(iliza_US)$rel_freq,
                                 by_state(fw_US)$rel_freq, by_state(gaffigan_US)$rel_freq, by_state(jefferies_US)$rel_freq,
                                 by_state(carr_US)$rel_freq, by_state(mulaney_US)$rel_freq, by_state(ck_US)$rel_freq, by_state(brennan_US)$rel_freq, 
                                 by_state(oswalt_US)$rel_freq, by_state(white_US)$rel_freq, by_state(segura_US)$rel_freq,
                                 by_state(larry_US)$rel_freq, by_state(bamford_US)$rel_freq)) )

state_rel_freqs[is.na(state_rel_freqs)] <- 0
colnames(state_rel_freqs) <- state.name

state_rel_freqs <- as.tibble(cbind(tour_names, state_rel_freqs))
colnames(state_rel_freqs)[1] <- "Comedian"

state_for_map <- as.tibble(t(select(state_rel_freqs, -Comedian)))
colnames(state_for_map) <- tour_names
state_for_map$region <- state.name


most_visits <- as.tibble(cbind(state.abb, 0))

colnames(most_visits) <- c("State", "Top Comedian Visitor")

for(i in 1:length(state.abb)){
  df <- state_freqs %>% arrange(desc(.[[i+1]]))
  most_visits$`Top Comedian Visitor`[i] <- as.character(df$Comedian[1])
}
    

# state_freqs$total <- apply(state_freqs[-1], 1, sum)


# tree_tour_table <- state_freqs %>% select(-Comedian)
# rownames(tree_tour_table) <- tour_names
# tour_tree <- hclust(dist(tree_tour_table))
# plot(tour_tree)
# 
# tour_clust <- kmeans(state_freqs[2:50], 8, nstart = 20)
# state_freqs$cluster <- as.factor(tour_clust$cluster)
# tour_clusters <- state_freqs %>% select(Comedian, cluster)

write_csv(Ratios, "Comedy_Interactive//Ratios.csv")

write_csv(tours_by_state, "Comedy_Interactive//tours_by_state.csv")
