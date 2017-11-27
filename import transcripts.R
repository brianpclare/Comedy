library(tidyverse)
library(readr)
library(tidytext)

data(stop_words)

read_transcript <- function(file, comedian, title){
  df <- read_table(file, col_names = FALSE)
  
  df <- df %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
    filter(word != 'applause') %>% filter(word != 'music') %>%   anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>% mutate(comedian = comedian, title = title) %>%
    mutate(RF = n / sum(n))
  
  return(df)
}

#Ali Wong
AW_Baby <- read_transcript("scraps//ali wong baby cobra.txt", 
                           "Ali Wong", "Baby Cobra")
Ali_Wong <- rbind(AW_Baby)%>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Amy Schumer
AS_Leather <- read_transcript("youtube//amy schumer the leather special.txt",
                              "Amy Schumer", "The Leather Special")
AS_Sex <- read_transcript("scraps//amy schumer mostly sex stuff.txt",
                          "Amy Schumer", "Mostly Sex Stuff")
AS_Live <- read_transcript("scraps//amy schumer live at the apollo.txt", 
                           "Amy Schumer", "Live at the Apollo")
Amy_Schumer <- rbind(AS_Leather, AS_Sex, AS_Live) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Aziz Ansari
AA_Delicious <- read_transcript("youtube//aziz ansari dangerously delicious.txt", 
                                "Aziz Ansari", "Dangerously Delicious")
AA_Buried <- read_transcript("youtube//aziz ansari buried alive.txt", 
                        "Aziz Ansari", "Buried Alive")
AA_Intimate <- read_transcript("scraps//aziz ansari intimate moments.txt",
                               "Aziz Ansari", "Intimate Moments for a Sensual Evening")
Aziz_Ansari <- rbind(AA_Buried, AA_Delicious, AA_Intimate) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Bill Burr
Burr_You <- read_transcript("youtube//bill burr you people are all the same.txt", 
                       "Bill Burr", "You People Are All The Same")
Burr_Why <- read_transcript("youtube//bill burr why do i do this.txt", 
                       "Bill Burr", "Why Do I Do This")
Burr_Let <- read_transcript("scraps//bill burr let it go.txt", "Bill Burr", "Let It Go")
Bill_Burr <- rbind(Burr_Why, Burr_You, Burr_Let) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Bill Engvall
BE_Sign <- read_transcript("youtube//bill engvall heres your sign.txt",
                           "Bill Engvall", "Here's Your Sign")
Bill_Engvall <- rbind(BE_Sign)%>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Bill Hicks
BH_Rev <- read_transcript("scraps//bill hicks revelations.txt", "Bill Hicks", "Revelations")
BH_Relentless <- read_transcript("scraps//bill hicks relentless.txt", "Bill Hicks", "Relentless")
Bill_Hicks <- rbind(BH_Relentless, BH_Rev) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Bo Burnham
BB_Happy <- read_transcript('scraps//bo burnham make happy.txt', 'Bo Burnham', 'Make Happy')
BB_What <- read_transcript("youtube//bo burnham what.txt", "Bo Burnham", "What")
Bo_Burnham <- rbind(BB_Happy, BB_What) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Brian Posehn
BP_Crim <- read_transcript("youtube//brian posehn criminally posehn.txt", 
                           "Brian Posehn", "Criminally Posehn")
BP_JFL <- read_transcript("youtube//brian posehn just for laughs.txt", 
                          "Brian Posehn", "Just for Laughs")
Brian_Posehn <- rbind(BP_JFL, BP_Crim) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Chris Rock
CR_Pain <- read_transcript("youtube//chris rock bring the pain.txt", "Chris Rock", "Bring the Pain")
CR_BB <- read_transcript("youtube//chris rock bigger and blacker.txt", "Chris Rock", "Bigger and Blacker")
CR_Never <- read_transcript("youtube//chris rock never scared.txt", "Chris Rock", "Never Scared")
Chris_Rock <- rbind(CR_BB, CR_Never, CR_Pain) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Daniel Tosh
DT_Serious <- read_transcript("youtube//daniel tosh completely serious.txt", 
                         "Daniel Tosh", "Completely Serious")
DT_People <- read_transcript("scraps//daniel tosh people pleaser.txt", 
                             "Daniel Tosh", "People Pleaser")
Daniel_Tosh <- rbind(DT_People, DT_Serious)%>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Dave Chappelle
DC_Killing <- read_transcript("youtube//dave chappelle killing them softly.txt",
                         "Dave Chappelle", "Killing Them Softly")
DC_Fwiw <- read_transcript("youtube//dave chappelle for what its worth.txt", 
                           "Dave Chappelle", "For What It's Worth")
DC_Texas <- read_transcript("scraps//dave chappelle texas.txt",
                            "Dave Chappelle", "Deep in the Heart of Texas")
DC_Spin <- read_transcript("scraps//dave chappelle age of spin.txt",
                            "Dave Chappelle", "The Age of Spin")
Dave_Chappelle <- rbind(DC_Fwiw, DC_Killing, DC_Texas, DC_Spin) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Demetri Martin
DM_If <- read_transcript("youtube//demetri martin if i.txt", "Demetri Martin", "If I")
DM_Live <- read_transcript("youtube//demetri martin live at the time.txt", 
                           "Demetri Martin", "Live, at the time")
DM_Benefit <- read_transcript("youtube//demetri martin benefit.txt", 
                              "Demetri Martin", "Benefit clip")
DM_Standup <- read_transcript("youtube//demetri martin standup comedian.txt", 
                              "Demetri Martin", "Standup Comedian")
Demetri_Martin <- rbind(DM_Benefit, DM_If, DM_Live, DM_Standup) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Donald Glover
DG_Weirdo <- read_transcript("youtube//donald glover weirdo.txt", "Donald Glover", "Weirdo")
Donald_Glover <- rbind(DG_Weirdo) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Eddie Murphy
EM_Raw <- read_transcript("youtube//eddie murphy raw.txt", "Eddie Murphy", "Raw")
EM_Del <- read_transcript("scraps//eddie murphy delirious.txt", "Eddie Murphy", "Delirious")
Eddie_Murphy <- rbind(EM_Raw, EM_Del) %>% group_by(word) %>% 
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Frankie Boyle
FB_Sodom <- read_transcript("scraps//frankie boyle the last days of sodom.txt", 
                            "Frankie Boyle", "The Last Days of Sodom")
FB_Live2 <- read_transcript("scraps//frankie boyle live 2.txt", "Frankie Boyle", "Live 2")
FB_Hurt <- read_transcript("scraps//frankie boyle hurt like youve never been loved.txt",
                           "Frankie Boyle", "Hurt Like You've Never Been Loved")
Frankie_Boyle <- rbind(FB_Hurt, FB_Live2, FB_Sodom) %>% group_by(word) %>% 
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Gabriel Iglesias
#GI_Haf <- read_transcript("scraps//gabriel iglesias hot and fluffy.txt", 
#                          "Gabriel Iglesias", "Hot and Fluffy")
#GI_Not <- read_transcript("scraps//gabriel iglesias not fat.txt", 
 #                         "Gabriel Iglesias", "I'm Not Fat I'm Fluffy")


#George Carlin
GC_Again <- read_transcript("scraps//george carlin again.txt", "George Carlin", "Again")
GC_USC <- read_transcript("scraps//george carlin at usc.txt", "George Carlin", "Live at USC")
GC_Life <- read_transcript("scraps//george carlin life is worth losing.txt", 
                           "George Carlin", "Life is Worth Losing")
George_Carlin <- rbind(GC_Again, GC_Life, GC_USC)%>% group_by(word) %>% 
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Hannibal Burress
HB_Animal <- read_transcript("youtube//hannibal burress animal furnace.txt", 
                            "Hannibal Burress", "Animal Furnace")
#HB_Comedy <- read_transcript("scraps//hannibal burress cc.txt", 
#                             "Hannibal Burress, Comedy Camisado")
Hannibal_Burress <- rbind(HB_Animal) %>% group_by(word) %>% 
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Iliza Schlesinger
IS_Hot <- read_transcript("youtube//iliza schlesinger freezing hot.txt",
                          "Iliza Schlesinger", "Freezing Hot")
IS_Kills <- read_transcript("youtube//iliza schlesinger confirmed kills.txt",
                       "Iliza Schlesinger", "Confirmed Kills")
Iliza_Sch <- rbind(IS_Hot, IS_Kills) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Jeff Foxworthy
JF_Totally <- read_transcript("youtube//jeff foxworthy totally committed.txt", 
                         "Jeff Foxworthy", "Totally Committed")
JF_Georgia <- read_transcript("youtube//jeff foxworthy in georgia.txt",
                         "Jeff Foxworthy", "Live in Georgia")
JF_Thinking <- read_transcript("youtube//jeff foxworthy weve been thinking.txt",
                               "Jeff Foxworthy", "We've Been Thinking")
Jeff_Foxworthy <- rbind(JF_Georgia, JF_Totally, JF_Thinking) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Jerry Seinfeld
JS_Telling <- read_transcript("youtube//jerry seinfeld telling you.txt", 
                         "Jerry Seinfeld", "I'm Telling You For The Last Time")
Jerry_Seinfeld <- rbind(JS_Telling)%>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Jim Gaffigan
JG_Universe <- read_transcript("youtube//jim gaffigan mr universe.txt", "Jim Gaffigan", "Mr Universe")
JG_Cinco <- read_transcript("scraps//jim gaffigan cinco.txt", "Jim Gaffigan", "Cinco")
Jim_Gaffigan <- rbind(JG_Cinco, JG_Universe)%>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Jim Jefferies
JJ_Dumb <- read_transcript("youtube//jim jefferies freedumb.txt", "Jim Jefferies", "Freedumb")
JJ_Contra <- read_transcript("youtube//jim jefferies contraband live.txt", "Jim Jefferies", "Contraband Live")
JJ_Alco <- read_transcript("youtube//jim jefferies alcoholocaust.txt", "Jim Jefferies", "Alcoholocaust")
JJ_God <- read_transcript("scraps//jim jefferies i swear to god.txt", "Jim Jefferies", "I Swear to God")
Jim_Jefferies <- rbind(JJ_Dumb, JJ_Contra, JJ_Alco, JJ_God) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Jimmy Carr
JC_Funny <- read_transcript("youtube//jimmy carr funny business.txt", "Jimmy Carr", "Funny Business")
JC_Concert <- read_transcript("youtube//jimmy carr in concert.txt", "Jimmy Carr", "In Concert")
JC_Laugh <- read_transcript("youtube//jimmy carr making people laugh.txt", "Jimmy Carr", "Making People Laugh")
JC_Comedian <- read_transcript("youtube//jimmy carr comedian.txt", "Jimmy Carr", "Comedian")
Jimmy_Carr <- rbind(JC_Comedian, JC_Concert, JC_Funny, JC_Laugh) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#John Mulaney
JM_Comeback <- read_transcript("youtube//john mulaney comeback kid.txt", "John Mulaney", "The Comeback Kid")
JM_New <- read_transcript("youtube//john mulaney new in town.txt", "John Mulaney", "New in Town")
John_Mulaney <- rbind(JM_Comeback, JM_New) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Larry the Cable Guy
LC_Thinking <- read_transcript("youtube//larry the cable guy weve been thinking.txt",
                               "Larry the Cable Guy", "We've Been Thinking")
Larry_Cableguy <- rbind(LC_Thinking) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Lenny Bruce
LB_Berk <- read_transcript("scraps//lenny bruce berkeley.txt", "Lenny Bruce", "Berkeley")
Lenny_Bruce <- rbind(LB_Berk) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Louis CK
CK_Chewed <- read_transcript("youtube//louis ck chewed up.txt", "Louis CK", "Chewed Up")
CK_ONS <- read_transcript("youtube//louis ck one night stand.txt", "Louis CK", "One Night Stand")
CK_Beacon <- read_transcript("youtube//louis ck live at the beacon.txt", "Louis CK", "Live at the Beacon")
CK_Hilarious <- read_transcript("scraps//louis ck hilarious.txt", "Louis CK", "Hilarious")
Louis_CK <- rbind(CK_Chewed, CK_ONS, CK_Beacon, CK_Hilarious) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Maria Bamford
MB_Baby <- read_transcript("scraps//maria bamford old baby.txt", 
                           "Maria Bamford", "Old Baby")
Maria_Bamford <- rbind(MB_Baby) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Mitch Hedberg
MH_Special <- read_transcript("scraps//mitch hedberg special.txt", "Mitch Hedberg", "Comedy Central Special")
MH_Grill <- read_transcript("albums//mitch hedberg strategic grill locations.txt",
                            "Mitch Hedberg", "Strategic Grill Locations")
Mitch_Hedberg <- rbind(MH_Special, MH_Grill) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Neal Brennan
NB_WBD <- read_transcript("scraps//neal brennan women and black dudes.txt",
                          "Neal Brennan", "Women and Black Dudes")
NB_3 <- read_transcript("scraps//neal brennan 3 mics.txt", "Neal Brennan", "3 Mics")
Neal_Brennan <- rbind(NB_3, NB_WBD) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Patton Oswalt
PO_Finest <- read_transcript("youtube//patton oswalt finest hour.txt", "Patton Oswalt", "Finest Hour")
PO_Talking <- read_transcript("youtube//patton oswalt talking for clapping.txt",
                              "Patton Oswalt", "Talking for Clapping")
PO_CCP <- read_transcript("netflix//patton oswalt comedy central presents.txt", 
                          "Patton Oswalt", "Comedy Central Presents")
Patton_Oswalt <- rbind(PO_CCP, PO_Finest, PO_Talking) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Redd Foxx
RF_HBO <- read_transcript("youtube//redd foxx hbo special.txt", "Redd Fox", "HBO Special")
Redd_Foxx <- rbind(RF_HBO) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Richard Pryor
RP_Here <- read_transcript("scraps//richard pryor here and now.txt",
                           "Richard Pryor", "Here and Now")
RP_Sunset <- read_transcript("scraps//richard pryor live on the sunset strip.txt",
                           "Richard Pryor", "Live on the Sunset Strip")
RP_Smoke <- read_transcript("scraps//richard pryor live and smokin.txt",
                           "Richard Pryor", "Live and Smokin")
RP_Live <- read_transcript("scraps//richard pryor live 79.txt",
                            "Richard Pryor", "Live In Concert")
Richard_Pryor <- rbind(RP_Here, RP_Smoke, RP_Sunset, RP_Live) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Ricky Gervais
RG_Out <- read_transcript("youtube//ricky gervais out of england.txt", 
                     "Ricky Gervais", "Out of England")
Ricky_Gervais <- rbind(RG_Out) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Ron White
RW_Unprof <- read_transcript("youtube//ron white a little unprofessional.txt", 
                             "Ron White", "A Little Unprofessional")
RW_Behavior <- read_transcript("youtube//ron white behavioral problems.txt", 
                               "Ron White", "Behavioral Problems")
RW_Tater <- read_transcript("youtube//ron white they call me tater salad.txt", 
                            "Ron White", "They Call Me Tater Salad")
Ron_White <- rbind(RW_Behavior, RW_Tater, RW_Unprof) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Steven Wright
SW_Special <- read_transcript("youtube//steven wright special.txt", "Steven Wright", "Special")
Steven_Wright <- rbind(SW_Special) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Steve Harvey
SH_Trip <- read_transcript("youtube//steve harvey dont trip.txt", "Steve Harvey", "Don't Trip")
SH_HBO <- read_transcript("youtube//steve harvey hbo special.txt", "Steve Harvey", "HBO Special")
Steve_Harvey <- rbind(SH_HBO, SH_Trip) %>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

#Tom Segura
TS_Normal <- read_transcript("youtube//tom segura completely normal 2014.txt", 
                        "Tom Segura", "Completely Normal")
TS_Mostly <- read_transcript("scraps//tom segura mostly stories.txt", 
                             "Tom Segura", "Mostly Stories")
Tom_Segura <- rbind(TS_Mostly, TS_Normal)%>% group_by(word) %>%
  summarize(n = sum(n), name = max(comedian)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))


all_words <- rbind(Amy_Schumer, Aziz_Ansari, Ali_Wong, Bill_Burr, Bill_Hicks, Bo_Burnham, Brian_Posehn,
      Chris_Rock, Dave_Chappelle, Demetri_Martin, Donald_Glover, Daniel_Tosh, Eddie_Murphy, Frankie_Boyle,
      George_Carlin, Iliza_Sch, Jeff_Foxworthy, Jimmy_Carr, Jim_Gaffigan, Jim_Jefferies,
      John_Mulaney, Louis_CK, Lenny_Bruce, Maria_Bamford, Neal_Brennan, Patton_Oswalt, Richard_Pryor,
      Ron_White, Ricky_Gervais, Steve_Harvey, Tom_Segura, Steven_Wright, Redd_Foxx, Jerry_Seinfeld,
      Hannibal_Burress) %>% group_by(word) %>% 
  summarize(n = sum(n))%>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

words_only <- all_words %>% select(word)

text_norm <- function(comedian){
  normalized <- right_join(comedian, words_only)%>%
    mutate(n = ifelse(is.na(n), 0, n), rf = n / sum(n))
  return(normalized)
}

AA_norm <- text_norm(Aziz_Ansari)
AW_norm <- text_norm(Ali_Wong)
AS_norm <- text_norm(Amy_Schumer)
BB_norm <- text_norm(Bo_Burnham)
Burr_norm <- text_norm(Bill_Burr)
BH_norm <- text_norm(Bill_Hicks)
BP_norm <- text_norm(Brian_Posehn)
CR_norm <- text_norm(Chris_Rock)
CK_norm <- text_norm(Louis_CK)
DC_norm <- text_norm(Dave_Chappelle)
DT_norm <- text_norm(Daniel_Tosh)
DM_norm <- text_norm(Demetri_Martin)
DG_norm <- text_norm(Donald_Glover)
EM_norm <- text_norm(Eddie_Murphy)
FB_norm <- text_norm(Frankie_Boyle)
GC_norm <- text_norm(George_Carlin)
HB_norm <- text_norm(Hannibal_Burress)
IS_norm <- text_norm(Iliza_Sch)
JC_norm <- text_norm(Jimmy_Carr)
JG_norm <- text_norm(Jim_Gaffigan)
JJ_norm <- text_norm(Jim_Jefferies)
JF_norm <- text_norm(Jeff_Foxworthy)
JS_norm <- text_norm(Jerry_Seinfeld)
JM_norm <- text_norm(John_Mulaney)
LC_norm <- text_norm(Larry_Cableguy)
LB_norm <- text_norm(Lenny_Bruce)
MB_norm <- text_norm(Maria_Bamford)
MH_norm <- text_norm(Mitch_Hedberg)
NB_norm <- text_norm(Neal_Brennan)
PO_norm <- text_norm(Patton_Oswalt)
RF_norm <- text_norm(Redd_Foxx)
RP_norm <- text_norm(Richard_Pryor)
RW_norm <- text_norm(Ron_White)
RG_norm <- text_norm(Ricky_Gervais)
SH_norm <- text_norm(Steve_Harvey)
SW_norm <- text_norm(Steven_Wright)
TS_norm <- text_norm(Tom_Segura)
BE_norm <- text_norm(Bill_Engvall)

vocab_similarity <- function(c1, c2){
  x <- norm(c2$rf, type = "2")
  y <- norm(c1$rf, type = "2") 
  z <- c1$rf %*% c2$rf
  w <- x*y
  answer <- z / w
  return(answer)
}

vocab_similarity(AW_norm, JC_norm)

comedians_list <- c("Amy Schumer", "Aziz Ansari", "Ali Wong", "Bill Burr", "Bill Hicks",
  "Bo Burnham", "Brian Posehn", "Chris Rock", "Dave Chappelle", "Demetri Martin",
  "Donald Glover", "Daniel Tosh", "Eddie Murphy", "Frankie Boyle", "George Carlin",
  "Iliza Schlesinger", "Jeff Foxworthy", "Jimmy Carr", "Jim Gaffigan", "Jim Jefferies",
  "John Mulaney", "Louis CK", "Lenny Bruce", "Maria Bamford", "Neal Brennan", "Patton Oswalt",
  "Richard Pryor", "Ron White", "Ricky Gervais", "Steve Harvey", "Tom Segura", "Steven Wright",
  "Redd Foxx", "Jerry Seinfeld", "Hannibal Burress", "Larry the Cable Guy", "Mitch Hedberg",
  "Bill Engvall")

all_comparisons <- as.tibble(matrix(c(0), nrow = 38, ncol = 38))
colnames(all_comparisons) <- comedians_list
all_comparisons$name <- comedians_list

do_comps <- function(comedian){
  x1 <- vocab_similarity(comedian, AS_norm)
  x2 <- vocab_similarity(comedian, AA_norm)
  x3 <- vocab_similarity(comedian, AW_norm)
  x4 <- vocab_similarity(comedian, Burr_norm)
  x5 <- vocab_similarity(comedian, BH_norm)
  x6 <- vocab_similarity(comedian, BB_norm)
  x7 <- vocab_similarity(comedian, BP_norm)
  x8 <- vocab_similarity(comedian, CR_norm)
  x9 <- vocab_similarity(comedian, DC_norm)
  x10 <- vocab_similarity(comedian, DM_norm)
  x11 <- vocab_similarity(comedian, DG_norm)
  x12 <- vocab_similarity(comedian, DT_norm)
  x13 <- vocab_similarity(comedian, EM_norm)
  x14 <- vocab_similarity(comedian, FB_norm)
  x15 <- vocab_similarity(comedian, GC_norm)
  x16 <- vocab_similarity(comedian, IS_norm)
  x17 <- vocab_similarity(comedian, JF_norm)
  x18 <- vocab_similarity(comedian, JC_norm)
  x19 <- vocab_similarity(comedian, JG_norm)
  x20 <- vocab_similarity(comedian, JJ_norm)
  x21 <- vocab_similarity(comedian, JM_norm)
  x22 <- vocab_similarity(comedian, CK_norm)
  x23 <- vocab_similarity(comedian, LB_norm)
  x24 <- vocab_similarity(comedian, MB_norm)
  x25 <- vocab_similarity(comedian, NB_norm)
  x26 <- vocab_similarity(comedian, PO_norm)
  x27 <- vocab_similarity(comedian, RP_norm)
  x28 <- vocab_similarity(comedian, RW_norm)
  x29 <- vocab_similarity(comedian, RG_norm)
  x30 <- vocab_similarity(comedian, SH_norm)
  x31 <- vocab_similarity(comedian, TS_norm)
  x32 <- vocab_similarity(comedian, SW_norm)
  x33 <- vocab_similarity(comedian, RF_norm)
  x34 <- vocab_similarity(comedian, JS_norm)
  x35 <- vocab_similarity(comedian, HB_norm)
  x36 <- vocab_similarity(comedian, LC_norm)
  x37 <- vocab_similarity(comedian, MH_norm)
  x38 <- vocab_similarity(comedian, BE_norm)
  return(c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18,
          x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34,
          x35, x36, x37, x38))
}

all_comparisons$`Amy Schumer` <- do_comps(AS_norm)
all_comparisons$`Aziz Ansari` <- do_comps(AA_norm)
all_comparisons$`Ali Wong` <- do_comps(AW_norm)
all_comparisons$`Bill Burr` <- do_comps(Burr_norm)
all_comparisons$`Bill Hicks` <- do_comps(BH_norm)
all_comparisons$`Bo Burnham` <- do_comps(BB_norm)
all_comparisons$`Brian Posehn` <- do_comps(BP_norm)
all_comparisons$`Chris Rock` <- do_comps(CR_norm)
all_comparisons$`Dave Chappelle` <- do_comps(DC_norm)
all_comparisons$`Demetri Martin` <- do_comps(DM_norm)
all_comparisons$`Donald Glover` <- do_comps(DG_norm)
all_comparisons$`Daniel Tosh` <- do_comps(DT_norm)
all_comparisons$`Eddie Murphy` <- do_comps(EM_norm)
all_comparisons$`Frankie Boyle` <- do_comps(FB_norm)
all_comparisons$`George Carlin` <- do_comps(GC_norm)
all_comparisons$`Iliza Schlesinger` <- do_comps(IS_norm)
all_comparisons$`Jeff Foxworthy` <- do_comps(JF_norm)
all_comparisons$`Jimmy Carr` <- do_comps(JC_norm)
all_comparisons$`Jim Gaffigan` <- do_comps(JG_norm)
all_comparisons$`Jim Jefferies` <- do_comps(JJ_norm)
all_comparisons$`John Mulaney` <- do_comps(JM_norm)
all_comparisons$`Louis CK` <- do_comps(CK_norm)
all_comparisons$`Lenny Bruce` <- do_comps(LB_norm)
all_comparisons$`Maria Bamford` <- do_comps(MB_norm)
all_comparisons$`Neal Brennan` <- do_comps(NB_norm)
all_comparisons$`Patton Oswalt` <- do_comps(PO_norm)
all_comparisons$`Richard Pryor` <- do_comps(RP_norm)
all_comparisons$`Ron White` <- do_comps(RW_norm)
all_comparisons$`Ricky Gervais` <- do_comps(RG_norm)
all_comparisons$`Steve Harvey` <- do_comps(SH_norm)
all_comparisons$`Tom Segura` <- do_comps(TS_norm)
all_comparisons$`Steven Wright` <- do_comps(SW_norm)
all_comparisons$`Redd Foxx` <- do_comps(RF_norm)
all_comparisons$`Jerry Seinfeld` <- do_comps(JS_norm)
all_comparisons$`Hannibal Burress` <- do_comps(HB_norm)
all_comparisons$`Larry the Cable Guy` <- do_comps(LC_norm)
all_comparisons$`Mitch Hedberg` <- do_comps(MH_norm)
all_comparisons$`Bill Engvall` <- do_comps(BE_norm)

num_comps <- all_comparisons %>% select(-name)
all_comparisons$mean <- colMeans(num_comps)

top5 <- function(comedian){
  source <- all_comparisons[comedian]
  source <- cbind(source, all_comparisons$name)
  colnames(source) <- c("Similarity", "Comedian")
  source <- source %>% arrange(desc(Similarity)) %>% top_n(6, Similarity)
  # Number one in similarity is self, so let's ignore that
  return(source[2:6,])
}

schumer5 <- top5("Amy Schumer")

#Some grouping of comedians

Blue_Collar <- rbind(JF_Georgia, JF_Thinking, JF_Totally, LC_Thinking, BE_Sign, 
                     RW_Behavior, RW_Tater, RW_Unprof) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))

Blue_Collar_norm <- text_norm(Blue_Collar)

UK <- rbind(JC_Comedian, JC_Concert, JC_Funny, JC_Laugh, RG_Out, FB_Hurt,
            FB_Live2, FB_Sodom) %>% group_by(word) %>%
        summarize(n = sum(n)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))
UK_norm <- text_norm(UK)

Women <- rbind(AW_Baby, AS_Leather, AS_Live, AS_Sex, IS_Hot, IS_Kills, MB_Baby)%>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))
Women_norm <- text_norm(Women)

Black <- rbind(DC_Fwiw, DC_Killing, DC_Spin, DC_Texas, CR_BB, CR_Never, CR_Pain, 
               DG_Weirdo, EM_Del, EM_Raw, HB_Animal, RF_HBO, RP_Here, RP_Live, RP_Smoke,
               RP_Sunset, SH_HBO, SH_Trip)%>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n)) %>% mutate(rf = n / sum(n))
Black_norm <- text_norm(Black)

#Put everything together

master <- rbind(Amy_Schumer, Aziz_Ansari, Ali_Wong, Bill_Burr, Bill_Hicks, Bill_Engvall,
                Bo_Burnham, Chris_Rock, Brian_Posehn, Dave_Chappelle, Demetri_Martin, 
                Donald_Glover, Daniel_Tosh, Eddie_Murphy, Frankie_Boyle, George_Carlin, 
                Iliza_Sch, Jeff_Foxworthy, Jimmy_Carr, Jim_Gaffigan, Jim_Jefferies, 
                John_Mulaney, Louis_CK, Lenny_Bruce, Maria_Bamford, Neal_Brennan, 
                Patton_Oswalt, Richard_Pryor, Ron_White, Ricky_Gervais, Steve_Harvey, 
                Steven_Wright, Redd_Foxx, Jerry_Seinfeld, Hannibal_Burress, Larry_Cableguy,
                Mitch_Hedberg)

ggplot(data = filter(master, word == "fucking") %>% top_n(10, rf), mapping = aes(x = name, y = rf)) +geom_col()

wordsearch <- function(term){
  ggplot(data = filter(master, word == term) %>% 
           top_n(10, rf), mapping = aes(x = reorder(name, -rf), y = rf, fill = name)) + 
    geom_col() + theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
    labs(x = "Comedian", y = "Frequency", title = "Comedians who say ___ most frequently")
  
}

wordsearch("laugh")
wordsearch("fart")
wordsearch("gay")
wordsearch("fuck")
wordsearch("food")
