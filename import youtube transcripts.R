library(tidyverse)
library(readr)
library(tidytext)
library(wordcloud)

MH_Special <- read_table("youtube//mitch hedberg special.txt", col_names = FALSE)

MH_Special <- MH_Special %>% unnest_tokens(word, X1)%>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>%   anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% mutate(comedian = "Mitch Hedberg", title = "Comedy Central Special")

data(stop_words)

MH_Special <- MH_Special %>% anti_join(stop_words) %>% count(word, sort = TRUE) 


JC_Funny <- read_table("youtube//jimmy carr funny business.txt", col_names = FALSE)

JC_Funny <- JC_Funny %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>%   anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% mutate(comedian = "Jimmy Carr", title = "Funny Business")

JC_Concert <- read_table("youtube//jimmy carr in concert.txt", col_names = FALSE)

JC_Concert <- JC_Concert %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Jimmy Carr", title = "In Concert")

JC_Laugh <- read_table("youtube//jimmy carr making people laugh.txt", col_names = FALSE)

JC_Laugh <- JC_Laugh %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Jimmy Carr", title = "Making People Laugh")

JC_Comedian <- read_table("youtube//jimmy carr comedian.txt", col_names = FALSE)

JC_Comedian <- JC_Comedian %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Jimmy Carr", title = "Comedian")

CK_Chewed <- read_table("youtube//louis ck chewed up.txt", col_names = FALSE)

CK_Chewed <- CK_Chewed %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Louis CK", title = "Chewed Up")

DM_If <- read_table("youtube//demetri martin if i.txt", col_names = FALSE)

DM_If <- DM_If %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Demetri Martin", title = "If I")

BB_What <- read_table("youtube//bo burnham what.txt", col_names = FALSE)

BB_What <- BB_What %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Bo Burnham", title = "What")

DG_Weirdo <- read_table("youtube//donald glover weirdo.txt", col_names = FALSE)

DG_Weirdo <- DG_Weirdo %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Donald Glover", title = "Weirdo")

CR_Pain <- read_table("youtube//chris rock bring the pain.txt", col_names = FALSE)

CR_Pain <- CR_Pain %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Chris Rock", title = "Bring the Pain")

DC_Killing <- read_table("youtube//dave chappelle killing them softly.txt", col_names = FALSE)

DC_Killing <- DC_Killing %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Dave Chappelle", title = "Killing Them Softly")

DC_Fwiw <- read_table("youtube//dave chappelle for what its worth.txt", col_names = FALSE)

DC_Fwiw <- DC_Fwiw %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Dave Chappelle", title = "For What It's Worth")

EM_Raw <- read_table("youtube//eddie murphy raw.txt", col_names = FALSE)

EM_Raw <- EM_Raw %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Eddie Murphy", title = "Raw")

CR_BB <- read_table("youtube//chris rock bigger and blacker.txt", col_names = FALSE)

CR_BB <- CR_BB %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Chris Rock", title = "Bigger and Blacker")

CR_Never <- read_table("youtube//chris rock never scared.txt", col_names = FALSE)

CR_Never <- CR_Never %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Chris Rock", title = "Never Scared")

HB_Animal <- read_table("youtube//hannibal burress animal furnace.txt", col_names = FALSE)

HB_Animal <- HB_Animal %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Hannibal Burress", title = "Animal Furnace")

CK_ONS <- read_table("youtube//louis ck one night stand.txt", col_names = FALSE)

CK_ONS <- CK_ONS %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Louis CK", title = "One Night Stand")

JM_Comeback <- read_table("youtube//john mulaney comeback kid.txt", col_names = FALSE)

JM_Comeback <- JM_Comeback %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "John Mulaney", title = "The Comeback Kid")

JM_New <- read_table("youtube//john mulaney new in town.txt", col_names = FALSE)

JM_New <- JM_New %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "John Mulaney", title = "New In Town")

JS_Telling <- read_table("youtube//jerry seinfeld telling you.txt", col_names = FALSE)

JS_Telling <- JS_Telling %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Jerry Seinfeld", title = "I'm Telling You For The Last Time")

RW_Tater <- read_table("youtube//ron white they call me tater salad.txt", col_names = FALSE)

RW_Tater <- RW_Tater %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Ron White", title = "They Call Me Tater Salad")

AS_Leather <- read_table("youtube//amy schumer the leather special.txt", col_names = FALSE)

AS_Leather <- AS_Leather %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Amy Schumer", title = "The Leather Special")

IS_Hot <- read_table("youtube//iliza schlesinger freezing hot.txt", col_names = FALSE)

IS_Hot <- IS_Hot %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Iliza Schlesinger", title = "Freezing Hot")

IS_Kills <- read_table("youtube//iliza schlesinger confirmed kills.txt", col_names = FALSE)

IS_Kills <- IS_Kills %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Iliza Schlesinger", title = "Confirmed Kills")

Burr_Why <- read_table("youtube//bill burr why do i do this.txt", col_names = FALSE)

Burr_Why <- Burr_Why %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Bill Burr", title = "Why Do I Do This")

Burr_You <- read_table("youtube//bill burr you people are all the same.txt", col_names = FALSE)

Burr_You <- Burr_You %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Bill Burr", title = "You People Are All The Same")

DT_Serious <- read_table("youtube//daniel tosh completely serious.txt", col_names = FALSE)

DT_Serious <- DT_Serious %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Daniel Tosh", title = "Completely Serious")

JG_Universe <- read_table("youtube//jim gaffigan mr universe.txt", col_names = FALSE)

JG_Universe <- JG_Universe %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Jim Gaffigan", title = "Mr. Universe")

JJ_Dumb <- read_table("youtube//jim jefferies freedumb.txt", col_names = FALSE)

JJ_Dumb <- JJ_Dumb %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Jim Jefferies", title = "Freedumb")

TS_Normal <- read_table("youtube//tom segura completely normal 2014.txt", col_names = FALSE)

TS_Normal <- TS_Normal %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Tom Segura", title = "Completely Normal")

RG_Out <- read_table("youtube//ricky gervais out of england.txt", col_names = FALSE)

RG_Out <- RG_Out %>% unnest_tokens(word, X1) %>% filter(word != 'laughter')%>%
  filter(word != 'applause') %>% filter(word != 'music') %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% mutate(comedian = "Ricky Gervais", title = "Out of England")


Jimmy_Carr <- rbind(JC_Comedian, JC_Concert, JC_Funny, JC_Laugh) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

Chris_Rock <- rbind(CR_BB, CR_Never, CR_Pain) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

Louis_CK <- rbind(CK_Chewed, CK_ONS) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

Bill_Burr <- rbind(Burr_Why, Burr_You) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

Iliza_Sch <- rbind(IS_Hot, IS_Kills) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

Dave_Chappelle <- rbind(DC_Fwiw, DC_Killing) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))

John_Mulaney <- rbind(JM_Comeback, JM_New) %>% group_by(word) %>%
  summarize(n = sum(n)) %>% arrange(desc(n))
