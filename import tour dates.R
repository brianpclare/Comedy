library(tidyverse)

#A function to read in tour history data
#From bandsintown.com

read_tour <- function(file, comedian){
  doc <- read_tsv(file, col_names = TRUE) %>% unique() %>% select(-Ignore) %>% 
    mutate(name = comedian) %>%
    separate(col = Location, into = c("City", "State"), sep = ",")
  return(doc)
}

#John Mulaney
mulaney_tour <- read_tour("shows//mulaney.txt", "John Mulaney")
                                                                         
#Louis CK
ck_tour <- read_tour("shows//ck.txt", "Louis CK")

#Jimmy Carr
carr_tour <- read_tour("shows//jimmy carr.txt", "Jimmy Carr")

#Bill Burr
burr_tour <- read_tour("shows//bill burr.txt", "Bill Burr")

#Hannibal Burress
burress_tour <- read_tour("shows//hannibal burress.txt", "Hannibal Burress")

#Tom Segura
segura_tour <- read_tour("shows//tom segura.txt", "Tom Segura")

#Amy Schumer
schumer_tour <- read_tour("shows//amy schumer.txt", "Amy Schumer")

#Patton Oswalt
oswalt_tour <- read_tour("shows//patton oswalt.txt", "Patton Oswalt")

#Demetri Martin
martin_tour <- read_tsv("shows//demetri martin.txt", "Demetri Martin")

#Ron Whit
white_tour <- read_tour("shows//ron white.txt", "Ron White")

#Jeff Foxworthy
foxworthy_tour <- read_tour("shows//jeff foxworthy.txt", "Jeff Foxworthy")

#Dave Chappelle
chappelle_tour <- read_tour("shows//dave chappelle.txt", "Dave Chappelle")

#Chris Rock
rock_tour <- read_tour("shows//chris rock.txt", "Chris Rock")

#Jim Gaffigan
gaffigan_tour <- read_tour("shows//gaffigan.txt", "Jim Gaffigan")
  
#Jim Jefferies
jefferies_tour <- read_tour("shows//jim jefferies.txt", "Jim Jefferies")

#Brian Posehn
posehn_tour <- read_tour("shows//brian posehn.txt", "Brian Posehn")

#Daniel Tosh
tosh_tour <- read_tour("shows//daniel tosh.txt", "Daniel Tosh")

#Aziz Ansari
ansari_tour <- read_tour("shows//aziz ansari.txt", "Aziz Ansari")

#Bo Burnham
burnham_tour <- read_tour("shows//bo burnham.txt", "Bo Burnham")

#Iliza Schlesinger
iliza_tour <- read_tour("shows//iliza.txt", "Iliza Schlesinger")

#Neal Brennan
brennan_tour <- read_tour("shows//neal brennan.txt", "Neal Brennan")

