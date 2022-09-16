# install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

######EX1######
ds1 <- read.csv(file = "../001/ds.csv", header = T)
ds2 <- read.csv(file = "../002/ds.csv", header = T)
ds <- bind_rows(ds1, ds2)
fini <- ds %>% filter(DSDECOD=="PARTICIPANT COMPLETED ACTIVE PHASE OF STUDY") %>% filter(VISITNUM<=15)
bras <- dm %>% select(ARM,USUBJID)
outcome <- left_join(fini, bras, by = "USUBJID")
outcomein <- outcome %>% filter(STUDYID == "NIDA-CTN-0001")
outcomeout <- outcome %>% filter(STUDYID == "NIDA-CTN-0002")

inBUPNA_d <- data.frame(id = filter(outcomein, outcomein$ARM == "BUPRENORPHINE/NALOXONE")$USUBJID,
                        day = filter(outcomein, outcomein$ARM == "BUPRENORPHINE/NALOXONE")$VISITNUM)
inCLO_d <- data.frame(id = filter(outcomein, outcomein$ARM == "CLONIDINE")$USUBJID,
                      day = filter(outcomein, outcomein$ARM == "CLONIDINE")$VISITNUM)
outBUPNA_d <- data.frame(id = filter(outcomeout, outcomeout$ARM == "BUPRENORPHINE/NALOXONE")$USUBJID,
                         day = filter(outcomeout, outcomeout$ARM == "BUPRENORPHINE/NALOXONE")$VISITNUM)
outCLO_d <- data.frame(id = filter(outcomeout, outcomeout$ARM == "CLONIDINE")$USUBJID,
                       day = filter(outcomeout, outcomeout$ARM == "CLONIDINE")$VISITNUM)

ex1 <- read.csv(file = "../001/ex.csv", header = T)
ex2 <- read.csv(file = "../002/ex.csv", header = T)


inBUPNA_e <- data.frame(id = filter(ex1, ex1$EXTRT == "BUPRENORPHINE/NALOXONE")$USUBJID,
                        day = filter(ex1, ex1$EXTRT == "BUPRENORPHINE/NALOXONE")$VISITNUM)
inCLO_e <- data.frame(id = filter(ex1, ex1$EXTRT == "CLONIDINE")$USUBJID,
                      day = filter(ex1, ex1$EXTRT == "CLONIDINE")$VISITNUM)
outBUPNA_e <- data.frame(id = filter(ex2, ex2$EXTRT == "BUPRENORPHINE/NALOXONE")$USUBJID,
                         day = filter(ex2, ex2$EXTRT == "BUPRENORPHINE/NALOXONE")$VISITNUM)
outCLO_e <- data.frame(id = filter(ex2, ex2$EXTRT == "CLONIDINE")$USUBJID,
                       day = filter(ex2, ex2$EXTRT == "CLONIDINE")$VISITNUM)

inBUPNA_t <- full_join(inBUPNA_e, inBUPNA_d)
inCLO_t <- full_join(inCLO_e, inCLO_d)
outBUPNA_t <- full_join(outBUPNA_e, outBUPNA_d)
outCLO_t <- full_join(outCLO_e, outCLO_d)


cal_prop <- function(groupe){
  out <- vector("list", 14)
  total <- count(distinct(groupe, groupe$id))$n
  for (i in 1:14){
    dailyreste <- groupe %>% filter(day == i) %>% distinct(id) %>% count()
    out[i] <- dailyreste$n / total
  }
  return(unlist(out))
}

inBUPprop <- cal_prop(inBUPNA_t)
outBUPprop <- cal_prop(outBUPNA_t)
inCLOprop <- cal_prop(inCLO_t)
outCLOprop <- cal_prop(outCLO_t)

final <- data.frame(
  DAY = 1:14,
  INBUP = inBUPprop,
  INCLO = inCLOprop,
  OUTBUP = outBUPprop,
  OUTCLO = outCLOprop
)

final$DAY <- factor(final$DAY)

tabf <- reshape2::melt(final, id.var = "DAY")
levels(tabf$variable)[levels(tabf$variable) == "INBUP"] <- "001 Bup-Nx"
levels(tabf$variable)[levels(tabf$variable) == "INCLO"] <- "001 Clon"
levels(tabf$variable)[levels(tabf$variable) == "OUTBUP"] <- "002 Bup-Nx"
levels(tabf$variable)[levels(tabf$variable) == "OUTCLO"] <- "002 Clon"

ggplot(tabf, aes(x=DAY, 
                 y=value, 
                 group=variable, 
                 colour=variable, 
                 shape=variable))+
  geom_line(size = 1)+
  geom_point(size = 4, fill = "white")+
  expand_limits(y=c(0,1))+
  scale_color_manual(name = "methode", 
                     values = c('#fe8080','#407ffd','#fdbe40','#7fcfa4'))+
  scale_shape_manual(name = "methode", 
                     values = c(18,23,15,22))+
  labs(x="Study days",y="Proportion retained")

######EX2######
fini2 <- ds %>% filter(DSDECOD=="PARTICIPANT COMPLETED ACTIVE PHASE OF STUDY")
outcome2 <- left_join(fini2, bras, by = "USUBJID")

lb1 <- read.csv(file = "../001/lb1.csv", header = T)
lb2 <- read.csv(file = "../002/lb2.csv", header = T)
lb <- bind_rows(lb1, lb2)

posid <- lb$USUBJID[lb$VISITNUM==0 & lb$LBORRES=="POSITIVE"]
poscom <- subset(lb, lb$USUBJID %in% posid)
finiexam <- left_join(outcome2, poscom, by = "USUBJID")
exambup <- finiexam %>% filter(ARM == "BUPRENORPHINE/NALOXONE")
examclo <- finiexam %>% filter(ARM == "CLONIDINE")

finiexam %>% distinct(USUBJID) %>% count()

finiexam %>% filter(VISITNUM.y == 17 & LBORRES == "POSITIVE") %>% distinct(USUBJID) %>% count()

retbup <-exambup %>% filter((VISITNUM.y == 17 | 
                             VISITNUM.y == 16 | 
                             VISITNUM.y == 15 | 
                             VISITNUM.y == 14) 
                            & LBORRES == "POSITIVE") %>% distinct(USUBJID) %>% count()

retclo <-examclo %>% filter((VISITNUM.y == 17 |
                      VISITNUM.y == 16 |
                      VISITNUM.y == 15 | 
                      VISITNUM.y == 14) 
                   & LBORRES == "POSITIVE") %>% distinct(USUBJID) %>% count()

abstempbup14 <-exambup %>% filter(VISITNUM.y == 14 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
abstempbup15 <-exambup %>% filter(VISITNUM.y == 15 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
abstempbup16 <-exambup %>% filter(VISITNUM.y == 16 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
abstempbup17 <-exambup %>% filter(VISITNUM.y == 17 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
absbup <- inner_join(abstempbup14,abstempbup15,by = "USUBJID") %>% 
          inner_join(abstempbup16,by = "USUBJID") %>% 
          inner_join(abstempbup17,by = "USUBJID") %>%
          count()

abstempclo14 <-examclo %>% filter(VISITNUM.y == 14 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
abstempclo15 <-examclo %>% filter(VISITNUM.y == 15 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
abstempclo16 <-examclo %>% filter(VISITNUM.y == 16 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
abstempclo17 <-examclo %>% filter(VISITNUM.y == 17 & LBORRES == "NEGATIVE") %>% distinct(USUBJID)
absclo <- inner_join(abstempclo14,abstempclo15,by = "USUBJID") %>% 
  inner_join(abstempclo16,by = "USUBJID") %>% 
  inner_join(abstempclo17,by = "USUBJID") %>%
  count()

numbup <- count(distinct(exambup, exambup$USUBJID))$n
numclo <- count(distinct(examclo, examclo$USUBJID))$n
#proportion de retention / abstinence
propretbup <- paste(c(as.character(retbup$n / numbup *100),"%"), collapse = "")
propretclo <- paste(c(as.character(retclo$n / numclo *100),"%"), collapse = "")
propabsbup <- paste(c(as.character(absbup$n / numbup *100),"%"), collapse = "")
propabsclo <- paste(c(as.character(absclo$n / numclo *100),"%"), collapse = "")
