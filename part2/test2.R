library(tidyverse)

ds1 <- read.csv(file = "../001/ds.csv", header = T)
ds2 <- read.csv(file = "../002/ds.csv", header = T)
ds <- bind_rows(ds1, ds2)
fini2 <- ds %>% filter(DSDECOD=="PARTICIPANT COMPLETED ACTIVE PHASE OF STUDY")
bras <- dm %>% select(ARM,USUBJID)
outcome2 <- left_join(fini2, bras, by = "USUBJID")

lb1 <- read.csv(file = "../001/lb1.csv", header = T)
lb2 <- read.csv(file = "../002/lb2.csv", header = T)
lb <- bind_rows(lb1, lb2)
######
# examine <- lb %>% filter(LBTEST == "METHADONE" | LBTEST == "MORPHINE")
# 
# finiexam <- left_join(outcome2, examine, by = "USUBJID")
# posid <- finiexam$USUBJID[finiexam$VISITNUM.y==0 & finiexam$LBORRES=="NEGATIVE"]
# poscom <- finiexam[!(finiexam$USUBJID %in% posid)]
# !(finiexam$USUBJID %in% posid)
# finiexam %>% select(where(!(finiexam$USUBJID %in% posid)))
# poscom <- subset(finiexam, !(finiexam$USUBJID %in% posid))
# 
# distinct(poscom, poscom$USUBJID)
######

posid <- lb$USUBJID[lb$VISITNUM==0 & lb$LBORRES=="POSITIVE"]
poscom <- subset(lb, lb$USUBJID %in% posid)
finiexam <- left_join(outcome2, poscom, by = "USUBJID")
exambup <- finiexam %>% filter(ARM == "BUPRENORPHINE/NALOXONE")
examclo <- finiexam %>% filter(ARM == "CLONIDINE")

datapre <- finiexam %>% distinct(USUBJID)

finiexam %>% distinct(USUBJID) %>% count()

finiexam %>% filter(VISITNUM.y == 17 & LBORRES == "POSITIVE") %>% distinct(USUBJID) %>% count()

exambup %>% filter((VISITNUM.y == 17 | 
                    VISITNUM.y == 16 | 
                    VISITNUM.y == 15 | 
                    VISITNUM.y == 14 | 
                    VISITNUM.y == 13) 
                   & LBORRES == "POSITIVE") %>% distinct(USUBJID) %>% count()

examclo %>% filter((VISITNUM.y == 17 |
                    VISITNUM.y == 16 |
                    VISITNUM.y == 15 | 
                    VISITNUM.y == 14 | 
                    VISITNUM.y == 13) 
                   & LBORRES == "POSITIVE") %>% distinct(USUBJID) %>% count()



