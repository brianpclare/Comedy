library(tidyverse)
library(readr)
library(stringr)
library(tidytext)

clean_cc <- function(source, filename){
  df <- read_table(source, col_names = FALSE)
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "<(.*?)>", " "))
  colnames(df) <- c("string")
  df <- as.tibble(str_replace(df$string, "\\[(.*)\\]", " "))
  colnames(df) <- c("string")
  df <- df[34:length(df$string), 1]
  write_csv(df, filename)
}


## Aziz Ansari
clean_cc("netflix//aziz live msg.txt", "netflix//aziz msg fixed.txt")

## Bill Burr
clean_cc("netflix//bill burr sorry.txt", "netflix//burr sorry fixed.txt")

## Bill Hicks
clean_cc("netflix//bill hicks sane man.txt", "netflix//bh sane fixed.txt")

## Brian Posehn
clean_cc("netflix//posehn fartist.txt", "netflix//posehn fartist fixed.txt")

## Hannibal Burress
clean_cc("netflix//hannibal burress camisado.txt", "netflix//hb cc.txt")
clean_cc("netflix//hannibal burress chicago.txt", "netflix//hb chicago.txt")
clean_cc("netflix//hannibal burress edinburgh.txt", "netflix//hb edinburgh.txt")

## Iliza Shlesinger
clean_cc("netflix//iliza war paint.txt", "netflix//iliza war paint fixed.txt")

## Jerry Seinfeld
clean_cc("netflix//jerry before seinfeld.txt", "netflix//jerry fixed.txt")

## Jim Gaffigan
clean_cc("netflix//jim gaffigan cinco.txt", "netflix//gaffigan cinco fixed.txt")
clean_cc("netflix//jim gaffigan mr universe.txt", "netflix//gaffigan universe fixed.txt")
clean_cc("netflix//jim gaffigan king baby.txt", "netflix//gaffigan king baby fixed.txt")
clean_cc("netflix//jim gaffigan obsessed.txt", "netflix//gaffigan obsessed fixed.txt")
clean_cc("netflix//jim gaffigan beyond the pale.txt", "netflix//gaffigan beyond the pale.txt")

## Mike Birbiglia
clean_cc("netflix//birbiglia jokes.txt", "netflix//birbiglia jokes fixed.txt")
clean_cc("netflix//birbiglia gf bf.txt", "netflix//birbiglia gf bf fixed.txt")
clean_cc("netflix//birbiglia nothing.txt", "netflix//birbiglia nothing fixed.txt")

