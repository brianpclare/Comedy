library(tidyverse)

#A function to read in tour history data
#From bandsintown.com

read_tour <- function(file, comedian){
  doc <- read_tsv(file, col_names = TRUE) %>% unique() %>% select(-Ignore) %>% 
    mutate(name = comedian) %>%
    separate(col = Location, into = c("City", "State"), sep = ",")
  return(doc)
}

fix_dates <- function(table){
  for(i in 1:length(table$Date)){
    if(!grepl(",", table$Date[i], fixed = TRUE)){
      table$Date[i] <- str_c(table$Date[i], ", 2017", sep = "")
    }
  }
  return(table)
}

#John Mulaney
mulaney_tour <- read_tour("shows//mulaney.txt", "John Mulaney")
 
grepl("," , mulaney_tour$Date[1], fixed = TRUE)
for(i in 1:length(mulaney_tour$Date)){
      if(!grepl(",", mulaney_tour$Date[i], fixed = TRUE)){
        mulaney_tour$Date[i] <- str_c(mulaney_tour$Date[i], ", 2017", sep = "")
      }
    }

                                                                        
#Louis CK
ck_tour <- read_tour("shows//ck.txt", "Louis CK")
ck_tour <- fix_dates(ck_tour)

#Jimmy Carr
carr_tour <- read_tour("shows//jimmy carr.txt", "Jimmy Carr")
carr_tour <- fix_dates(carr_tour)

#Bill Burr
burr_tour <- read_tour("shows//bill burr.txt", "Bill Burr")
burr_tour <- fix_dates(burr_tour)

#Hannibal Burress
burress_tour <- read_tour("shows//hannibal burress.txt", "Hannibal Burress")
burress_tour <- fix_dates(burress_tour)

#Tom Segura
segura_tour <- read_tour("shows//tom segura.txt", "Tom Segura")
segura_tour <- fix_dates(segura_tour)

#Amy Schumer
schumer_tour <- read_tour("shows//amy schumer.txt", "Amy Schumer")
schumer_tour <- fix_dates(schumer_tour)

#Patton Oswalt
oswalt_tour <- read_tour("shows//patton oswalt.txt", "Patton Oswalt")
oswalt_tour <- fix_dates(oswalt_tour)

#Demetri Martin
martin_tour <- read_tsv("shows//demetri martin.txt", "Demetri Martin")
martin_tour <- fix_dates(martin_tour)

#Ron White
white_tour <- read_tour("shows//ron white.txt", "Ron White")
white_tour <- fix_dates(white_tour)

#Jeff Foxworthy
fw_tour <- read_tour("shows//jeff foxworthy.txt", "Jeff Foxworthy")
fw_tour <- fix_dates(fw_tour)

#Dave Chappelle
chappelle_tour <- read_tour("shows//dave chappelle.txt", "Dave Chappelle")
chappelle_tour <- fix_dates(chappelle_tour)

#Chris Rock
rock_tour <- read_tour("shows//chris rock.txt", "Chris Rock")
rock_tour <- fix_dates(rock_tour)

#Jim Gaffigan
gaffigan_tour <- read_tour("shows//gaffigan.txt", "Jim Gaffigan")
gaffigan_tour <- fix_dates(gaffigan_tour)  

#Jim Jefferies
jefferies_tour <- read_tour("shows//jim jefferies.txt", "Jim Jefferies")
jefferies_tour <- fix_dates(jefferies_tour)

#Brian Posehn
posehn_tour <- read_tour("shows//brian posehn.txt", "Brian Posehn")
posehn_tour <- fix_dates(posehn_tour)

#Daniel Tosh
tosh_tour <- read_tour("shows//daniel tosh.txt", "Daniel Tosh")
tosh_tour <- fix_dates(tosh_tour)

#Aziz Ansari
ansari_tour <- read_tour("shows//aziz ansari.txt", "Aziz Ansari")
ansari_tour <- fix_dates(ansari_tour)

#Bo Burnham
burnham_tour <- read_tour("shows//bo burnham.txt", "Bo Burnham")
burnham_tour <- fix_dates(burnham_tour)

#Iliza Schlesinger
iliza_tour <- read_tour("shows//iliza.txt", "Iliza Schlesinger")
iliza_tour <- fix_dates(iliza_tour)

#Neal Brennan
brennan_tour <- read_tour("shows//neal brennan.txt", "Neal Brennan")
brennan_tour <- fix_dates(brennan_tour)
