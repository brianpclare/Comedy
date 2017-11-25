library(tidyverse)
library(readr)
library(tidytext)

data(stop_words)

read_transcript <- function(file, comedian, title){
  df <- read_table(file, col_names = FALSE)
  
  df <- df %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
    filter(word != 'applause') %>% filter(word != 'music') %>%   anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>% mutate(comedian = comedian, title = title)
  
  return(df)
}

#Ali Wong
AW_Baby <- read_transcript("scraps//ali wong baby cobra.txt", 
                           "Ali Wong", "Baby Cobra")

#Amy Schumer
AS_Leather <- read_transcript("youtube//amy schumer the leather special.txt",
                              "Amy Schumer", "The Leather Special")
AS_Sex <- read_transcript("scraps//amy schumer mostly sex stuff.txt",
                          "Amy Schumer", "Mostly Sex Stuff")
AS_Live <- read_transcript("scraps//amy schumer live at the apollo.txt", 
                           "Amy Schumer", "Live at the Apollo")
Amy_Schumer <- rbind(AS_Leather, AS_Sex, AS_Live) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Aziz Ansari
AA_Delicious <- read_transcript("youtube//aziz ansari dangerously delicious.txt", 
                                "Aziz Ansari", "Dangerously Delicious")
AA_Buried <- read_transcript("youtube//aziz ansari buried alive.txt", 
                        "Aziz Ansari", "Buried Alive")
AA_Intimate <- read_transcript("scraps//aziz ansari intimate moments.txt",
                               "Aziz Ansari", "Intimate Moments for a Sensual Evening")
Aziz_Ansari <- rbind(AA_Buried, AA_Delicious, AA_Intimate) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Bill Burr
Burr_You <- read_transcript("youtube//bill burr you people are all the same.txt", 
                       "Bill Burr", "You People Are All The Same")
Burr_Why <- read_transcript("youtube//bill burr why do i do this.txt", 
                       "Bill Burr", "Why Do I Do This")
Burr_Let <- read_transcript("scraps//bill burr let it go.txt", "Bill Burr", "Let It Go")
#Burr_Sorry <- read_transcript("scraps//bill burr im sorry you feel that way.txt",
#                              "Bill Burr", "Im Sorry You Feel That Way")
Bill_Burr <- rbind(Burr_Why, Burr_You, Burr_Let) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Bo Burnham
BB_Happy <- read_transcript('scraps//bo burnham make happy.txt', 'Bo Burnham', 'Make Happy')
BB_What <- read_transcript("youtube//bo burnham what.txt", "Bo Burnham", "What")
Bo_Burnham <- rbind(BB_Happy, BB_What)%>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Chris Rock
CR_Pain <- read_transcript("youtube//chris rock bring the pain.txt", "Chris Rock", "Bring the Pain")
CR_BB <- read_transcript("youtube//chris rock bigger and blacker.txt", "Chris Rock", "Bigger and Blacker")
CR_Never <- read_transcript("youtube//chris rock never scared.txt", "Chris Rock", "Never Scared")
Chris_Rock <- rbind(CR_BB, CR_Never, CR_Pain) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Daniel Tosh
DT_Serious <- read_transcript("youtube//daniel tosh completely serious.txt", 
                         "Daniel Tosh", "Completely Serious")
DT_People <- read_transcript("scraps//daniel tosh people please.txt", 
                             "Daniel Tosh", "People Pleaser")
Daniel_Tosh <- rbind(DT_People, DT_Serious)%>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Dave Chappelle
DC_Killing <- read_transcript("youtube//dave chappelle killing them softly.txt",
                         "Dave Chappelle", "Killing Them Softly")
DC_Fwiw <- read_transcript("youtube//dave chappelle for what its worth.txt", 
                           "Dave Chappelle", "For What It's Worth")
Dave_Chappelle <- rbind(DC_Fwiw, DC_Killing) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Demetri Martin
DM_If <- read_transcript("youtube//demetri martin if i.txt", "Demetri Martin", "If I")

#Donald Glover
DG_Weirdo <- read_transcript("youtube//donald glover weirdo.txt", "Donald Glover", "Weirdo")

#Eddie Murphy
EM_Raw <- read_transcript("youtube//eddie murphy raw.txt", "Eddie Murphy", "Raw")
EM_Del <- read_transcript("scraps//eddie murphy delirious.txt", "Eddie Murphy", "Delirious")
Eddie_Murphy <- rbind(EM_Raw, EM_Del)%>% group_by(word) %>% 
  summarize(n = sum(n)) %>% arrange(desc(n))

#Hannibal Burress
HB_Animal <- read_transript("youtube//hannibal burress animal furnace.txt", 
                            "Hannibal Burress", "Animal Furnace")
HB_Comedy <- read_transcript("scraps//hannibal burress comedy camisado.txt", 
                             "Hannibal Burress, Comedy Camisado")
Hannibal_Burress <- rbind(HB_Comedy, HB_Animal) %>% group_by(word) %>% 
  summarize(n = sum(n)) %>% arrange(desc(n))

#Iliza Schlesinger
IS_Hot <- read_transcript("youtube//iliza schlesinger freezing hot.txt",
                          "Iliza Schlesinger", "Freezing Hot")
IS_Kills <- read_transcript("youtube//iliza schlesinger confirmed kills.txt",
                       "Iliza Schlesinger", "Confirmed Kills")
Iliza_Sch <- rbind(IS_Hot, IS_Kills) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Jeff Foxworthy
JF_Totally <- read_transcript("youtube//jeff foxworthy totally committed.txt", 
                         "Jeff Foxworthy", "Totally Committed")
JF_Georgia <- read_transcript("youtube//jeff foxworthy in georgia.txt",
                         "Jeff Foxworthy", "Live in Georgia")
JF_Thinking <- read_transcript("youtube//jeff foxworthy weve been thinking.txt",
                               "Jeff Foxworthy", "We've Been Thinking")
Jeff_Foxworthy <- rbind(JF_Georgia, JF_Totally, JF_Thinking) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Jerry Seinfeld
JS_Telling <- read_transcript("youtube//jerry seinfeld telling you.txt", 
                         "Jerry Seinfeld", "I'm Telling You For The Last Time")

#Jim Gaffigan
JG_Universe <- read_transcript("youtube//jim gaffigan mr universe.txt", "Jim Gaffigan", "Mr Universe")
JG_Cinco <- read_transcript("scraps//jim gaffigan cinco.txt", "Jim Gaffigan", "Cinco")
Jim_Gaffigan <- rbind(JG_Cinco, JG_Universe)%>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Jim Jefferies
JJ_Dumb <- read_transcript("youtube//jim jefferies freedumb.txt", "Jim Jefferies", "Freedumb")
JJ_Contra <- read_transcript("youtube//jim jefferies contraband live.txt", "Jim Jefferies", "Contraband Live")
JJ_Alco <- read_transcript("youtube//jim jefferies alcoholocaust.txt", "Jim Jefferies", "Alcoholocaust")
JJ_God <- read_transcript("scraps//jim jefferies i swear to god.txt", "Jim Jefferies", "I Swear to God")
Jim_Jefferies <- rbind(JJ_Dumb, JJ_Contra, JJ_Alco, JJ_God) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Jimmy Carr
JC_Funny <- read_transcript("youtube//jimmy carr funny business.txt", "Jimmy Carr", "Funny Business")
JC_Concert <- read_transcript("youtube//jimmy carr in concert.txt", "Jimmy Carr", "In Concert")
JC_Laugh <- read_transcript("youtube//jimmy carr making people laugh.txt", "Jimmy Carr", "Making People Laugh")
JC_Comedian <- read_transcript("youtube//jimmy carr comedian.txt", "Jimmy Carr", "Comedian")
Jimmy_Carr <- rbind(JC_Comedian, JC_Concert, JC_Funny, JC_Laugh) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#John Mulaney
JM_Comeback <- read_transcript("youtube//john mulaney comeback kid.txt", "John Mulaney", "The Comeback Kid")
JM_New <- read_transcript("youtube//john mulaney new in town.txt", "John Mulaney", "New in Town")
John_Mulaney <- rbind(JM_Comeback, JM_New) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Larry the Cable Guy
LC_Thinking <- read_transcript("youtube//larry the cable guy weve been thinking.txt",
                               "Larry the Cable Guy", "We've Been Thinking")

#Louis CK
CK_Chewed <- read_transcript("youtube//louis ck chewed up.txt", "Louis CK", "Chewed Up")
CK_ONS <- read_transcript("youtube//louis ck one night stand.txt", "Louis CK", "One Night Stand")
CK_Beacon <- read_transcript("youtube//louis ck live at the beacon.txt", "Louis CK", "Live at the Beacon")
CK_Hilarious <- read_transcript("scraps//louis ck hilarious.txt", "Louis CK", "Hilarious")
Louis_CK <- rbind(CK_Chewed, CK_ONS, CK_Beacon, CK_Hilarious) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Mitch Hedberg
MH_Special <- read_transcript("scraps//mitch hedberg special.txt", "Mitch Hedberg", "Comedy Central Special")

#Patton Oswalt
PO_Finest <- read_transcript("youtube//patton oswalt finest hour.txt", "Patton Oswalt", "Finest Hour")
PO_Talking <- read_transcript("youtube//patton oswalt talking for clapping.txt",
                              "Patton Oswalt", "Talking for Clapping")
PO_CCP <- read_transcript("netflix//patton oswalt comedy central presents.txt", 
                          "Patton Oswalt", "Comedy Central Presents")
Patton_Oswalt <- rbind(PO_CCP, PO_Finest, PO_Talking) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Redd Foxx
RF_HBO <- read_transcript("youtube//redd foxx hbo special.txt", "Redd Fox", "HBO Special")

#Richard Pryor
RP_Here <- read_transcript("scraps//richard pryor here and now.txt",
                           "Richard Pryor", "Here and Now")
RP_Sunset <- read_transcript("scraps//richard pryor live on the sunset strip.txt",
                           "Richard Pryor", "Live on the Sunset Strip")
RP_Smoke <- read_transcript("scraps//richard pryor live and smokin.txt",
                           "Richard Pryor", "Live and Smokin")
Richard_Pryor <- rbind(RP_Here, RP_Smoke, RP_Sunset) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Ricky Gervais
RG_Out <- read_transcript("youtube//ricky gervais out of england.txt", 
                     "Ricky Gervais", "Out of England")

#Ron White
RW_Unprof <- read_transcript("youtube//ron white a little unprofessional.txt", 
                             "Ron White", "A Little Unprofessional")
RW_Behavior <- read_transcript("youtube//ron white behavioral problems.txt", 
                               "Ron White", "Behavioral Problems")
RW_Tater <- read_transcript("youtube//ron white they call me tater salad.txt", 
                            "Ron White", "They Call Me Tater Salad")
Ron_White <- rbind(RW_Behavior, RW_Tater, RW_Unprof) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Steve Harvey
SH_Trip <- read_transcript("youtube//steve harvey dont trip.txt", "Steve Harvey", "Don't Trip")
SH_HBO <- read_transcript("youtube//steve harvey hbo special.txt", "Steve Harvey", "HBO Special")
Steve_Harvey <- rbind(SH_HBO, SH_Trip) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

#Tom Segura
TS_Normal <- read_transcript("youtube//tom segura completely normal 2014.txt", 
                        "Tom Segura", "Completely Normal")
TS_Mostly <- read_transcript("scraps//tom segura mostly stories.txt", 
                             "Tom Segura", "Mostly Stories")
Tom_Segura <- rbind(TS_Mostly, TS_Normal)%>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))


