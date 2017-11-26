library(tidyverse)


#John Mulaney
mulaney_tour <- read_tsv("shows//mulaney.txt") %>% unique() %>% select(-Ignore) %>%
  mutate(comedian = "John Mulaney")

#Louis CK
ck_tour <- read_tsv("shows//ck.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(Comedian = "Louis CK")

#Jimmy Carr
carr_tour <- read_tsv("shows//jimmy carr.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(Comedian = "Jimmy Carr")

#Bill Burr from bandsintown.com
burr_tour <- read_tsv("shows//bill burr.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Bill Burr")

#Hannibal Burress also from bandsintown.com
burress_tour <- read_tsv("shows//hannibal burress.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Hannibal Burress")

#Tom Segura from bandsintown.com
segura_tour <- read_tsv("shows//tom segura.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Tom Segura")

#Amy Schumer from bandsintown.com
schumer_tour <- read_tsv("shows//amy schumer.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Amy Schumer")

#Patton Oswalt
oswalt_tour <- read_tsv("shows//patton oswalt.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Patton Oswalt")

#Demetri Martin
martin_tour <- read_tsv("shows//demetri martin.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Demetri Martin")

#Ron White
white_tour <- read_tsv("shows//ron white.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Ron White")

#Jeff Foxworthy
foxworthy_tour <- read_tsv("shows//jeff foxworthy.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Jeff Foxworthy")

#Dave Chappelle
chappelle_tour <- read_tsv("shows//dave chappelle.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Dave Chappelle")

#Chris Rock
rock_tour <- read_tsv("shows//chris rock.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Chris Rock")

#Jim Gaffigan
gaffigan_tour <- read_tsv("shows//gaffigan.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Jim Gaffigan")
  
#Jim Jefferies
jefferies_tour <- read_tsv("shows//jim jefferies.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Jim Jefferies")

#Brian Posehn
posehn_tour <- read_tsv("shows//brian posehn.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Brian Posehn")

#Daniel Tosh
tosh_tour <- read_tsv("shows//daniel tosh.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Daniel Tosh")

#Aziz Ansari
ansari_tour <- read_tsv("shows//aziz ansari.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Aziz Ansari")

#Bo Burnham
burnham_tour <- read_tsv("shows//bo burnham.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Bo Burnham")

#Iliza Schlesinger
iliza_tour <- read_tsv("shows//iliza.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Iliza Schlesinger")

#Neal Brennan
brennan_tour <- read_tsv("shows//neal brennan.txt") %>% select(-Ignore) %>%
  unique() %>% mutate(comedian = "Neal Brennan")
