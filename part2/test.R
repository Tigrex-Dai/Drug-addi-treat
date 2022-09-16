# install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

ds1 <- read.csv(file = "../001/ds.csv", header = T)
ds2 <- read.csv(file = "../002/ds.csv", header = T)
ds <- bind_rows(ds1, ds2)
fini <- ds %>% filter(DSDECOD=="PARTICIPANT COMPLETED ACTIVE PHASE OF STUDY") %>% filter(VISITNUM<=15)
bras <- dm %>% select(ARM,USUBJID)
outcome <- left_join(fini, bras, by = "USUBJID")
# outcome$VISITNUM[outcome$VISITNUM>=15] <- 14
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

######
# total <- ds %>% filter(VISITNUM!=0) %>% filter(VISITNUM<=15)
# categ1 <- left_join(total, bras, by = "USUBJID") %>% distinct(USUBJID, .keep_all = TRUE)
# inBUPNA <- categ1 %>% filter(STUDYID == "NIDA-CTN-0001") %>% filter(ARM == "BUPRENORPHINE/NALOXONE")
# inCLO <- categ1 %>% filter(STUDYID == "NIDA-CTN-0001") %>% filter(ARM == "CLONIDINE")
# outBUPNA <- categ1 %>% filter(STUDYID == "NIDA-CTN-0002") %>% filter(ARM == "BUPRENORPHINE/NALOXONE")
# outCLO <- categ1 %>% filter(STUDYID == "NIDA-CTN-0002") %>% filter(ARM == "CLONIDINE")
# 
# testint <- count(in2CLO)$n
# cal_14jprop <- function(groupe){
#   out <- vector("list", 15)
#   totale <- count(groupe)$n
#   dailyreste <- totale
#   for (i in 1:15){
#     dailyreste <- dailyreste - count(filter(groupe, groupe$VISITNUM == i-1))$n
#     out[i] <- dailyreste/totale
#   }
#   return(unlist(out))
# }
# 
# inBUPprop <- cal_14jprop(inBUPNA)
# outBUPprop <- cal_14jprop(outBUPNA)
# inCLOprop <- cal_14jprop(inCLO)
# outCLOprop <- cal_14jprop(outCLO)
######

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
######
# ggplot()+
#   geom_point(data=final,
#              shape = 18, 
#              color = '#fe8080', 
#              size = 4, 
#              mapping = aes(x=DAY,y=INBUP))+ 
#   geom_line(data=final,
#             size = 1,
#             mapping = aes(x=DAY,y=INBUP,group = 1),
#             color='#fe8080')+
#   geom_point(data=final,
#              shape = 15, 
#              color = '#fdbe40', 
#              size = 4, 
#              mapping = aes(x=DAY,y=OUTBUP))+
#   geom_line(data=final,
#             size = 1,
#             mapping = aes(x=DAY,y=OUTBUP,group = 2),
#             color='#fdbe40')+
#   geom_line(data=final,
#             size = 1,
#             mapping = aes(x=DAY,y=INCLO,group = 3),
#             color='#407ffd')+
#   geom_point(data=final,
#              shape = 23, 
#              color = '#407ffd', 
#              fill = "white",
#              size = 4, 
#              mapping = aes(x=DAY,y=INCLO))+
#   geom_line(data=final,
#             size = 1,
#             mapping = aes(x=DAY,y=OUTCLO,group = 4),
#             color='#7fcfa4')+
#   geom_point(data=final,
#              shape = 22, 
#              color = '#7fcfa4', 
#              fill = "white", 
#              size = 4, 
#              mapping = aes(x=DAY,y=OUTCLO))+
#   
#   expand_limits(y=c(0,1))+
#   scale_shape_manual(values=c(18,23,15,22))+
#   theme(legend.position = "right")+
#   labs(x="Study days",
#        y="Proportion retained",
#        title="      Proportion of patients retained over time for the in-patient (diamond symbols) and out-patient
# (square symbols) protocols as a function of assignment to bup-nx (closed symbols) or clonidine
# (open symbols)")












######