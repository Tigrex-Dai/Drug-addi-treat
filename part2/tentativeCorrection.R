library(tidyverse)



# inclusions --------------------------------------------------------------

sc1 <- read.csv(file = "../001/sc.csv", header = T)
sc2 <- read.csv(file = "../002/sc.csv", header = T)

sc <- bind_rows(sc1, sc2)

sc %>% filter(SCTESTCD == "ALLINCL") %>% 
  summarise(n=n())

sc %>% filter(SCTESTCD == "ALLINCL") %>% 
  group_by(STUDYID) %>% 
  summarise(n=n(), 
            inclus = sum(SCORRES=="YES", na.rm=T),
            exclus = sum(SCORRES=="NO", na.rm=T))


# bras de traitement ------------------------------------------------------

dm1 <- read.csv(file = "../001/dm1.csv", header = T)
dm2 <- read.csv(file = "../002/dm2.csv", header = T)
dm <- bind_rows(dm1, dm2)

dm %>% select(ARM) %>% table(.)

#Grrrrrrr!!!!!!

ds1 <- read.csv(file = "../001/ds.csv", header = T)
ds2 <- read.csv(file = "../002/ds.csv", header = T)
ds <- bind_rows(ds1, ds2)

ds %>% filter(DSOCCUR=="Y", EPOCH== "SCREENING") %>% 
  select(DSTERM) %>% table(.)
ds %>% filter(DSOCCUR=="Y", EPOCH== "SCREENING") %>% 
  select(USUBJID, DSTERM) %>% table(.) %>% dim(.)




#Participant completed active phase of study
#DSTERM/DSDECOD
fini <- ds %>% filter(DSDECOD=="PARTICIPANT COMPLETED ACTIVE PHASE OF STUDY") %>% 
  filter(VISITNUM<=15)
bras <- dm %>% select(ARM,USUBJID)

outcome <- left_join(fini, bras, by = "USUBJID")

table(outcome$ARM, outcome$DSDECOD)


#check
dm %>% select(RFSTDTC) %>% table(., useNA = "ifany")
