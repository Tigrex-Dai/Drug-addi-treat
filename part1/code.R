install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)

dm1 <- read.csv2("dm1.csv", header = TRUE, sep = ",")
ie1 <- read.csv2("ie1.csv", header = TRUE, sep = ",")
dm2 <- read.csv2("dm2.csv", header = TRUE, sep = ",")
ie2 <- read.csv2("ie2.csv", header = TRUE, sep = ",")
lb1 <- read.csv2("lb1.csv", header = TRUE, sep = ",")
lb2 <- read.csv2("lb2.csv", header = TRUE, sep = ",")

alldm <- full_join(dm1,dm2)
allie <- full_join(ie1,ie2)
alllb <- full_join(lb1,lb2)

n_distinct(alldm$USUBJID) #411
n_distinct(alllb$USUBJID) #405

{
n_distinct(lb1$USUBJID)
n_distinct(dm2$USUBJID)

exclu1 <- ie1$USUBJID[ie1$IECAT=="EXCLUSION"]
inclu1 <- ie1$USUBJID[ie1$IECAT=="INCLUSION"]

restusr1 <- dm1$USUBJID[!(dm1$USUBJID %in% exclu1)]
inclu1 %in% restusr1

restusr11 <- filter(dm1, )

exclu2 <- ie2$USUBJID[ie2$IECAT=="EXCLUSION"]
inclu2 <- ie2$USUBJID[ie2$IECAT=="INCLUSION"]

restusr2 <- dm2$USUBJID[!(dm2$USUBJID %in% exclu2)]
}
allusrdm <- alldm$USUBJID[!(alldm$USUBJID %in% allie$USUBJID[allie$IECAT=="EXCLUSION"])] #369
{
n_distinct(restusr1) # 121
n_distinct(restusr2) # 248
}
allusrdm <- c(restusr1, restusr2) #369
{
usrBN1 <- filter(dm1, dm1$ARM=="BUPRENORPHINE/NALOXONE" & dm1$USUBJID %in% allusrdm)
usrBN2 <- filter(dm2, dm2$ARM=="BUPRENORPHINE/NALOXONE" & dm1$USUBJID %in% allusrdm)
}
allusrBN <- filter(alldm, 
                   alldm$ARM=="BUPRENORPHINE/NALOXONE" &
                   alldm$USUBJID %in% allusrdm) #233
allusrC <- filter(alldm, 
                  alldm$ARM=="CLONIDINE" & 
                  alldm$USUBJID %in% allusrdm) #110
{
lbex1 <-lb1$USUBJID[!(lb1$USUBJID %in% exclu1)]
lbex2 <-lb2$USUBJID[!(lb2$USUBJID %in% exclu2)]
lbie1 <- c(lbex1,inclu1)
lbie2 <- c(lbex2,inclu2)

allusrlb <- c(lbex1, lbex2)
n_distinct(allusrlb) #365

n_distinct(lbex1) # 120
n_distinct(lbex2) # 245
n_distinct(lbie1) # 131
n_distinct(lbie2) # 252

lbfin1 <- filter(lb1, (lb1$VISITNUM==13 | lb1$VISITNUM==14)&(lb1$USUBJID %in% lbex1))
lbfin2 <- filter(lb2, (lb2$VISITNUM==13 | lb2$VISITNUM==14)&(lb2$USUBJID %in% lbex2))
lbfin11 <- filter(lb1, (lb1$VISITNUM==13 | lb1$VISITNUM==14)&(lb1$LBSTAT!="NOT DONE")&(lb1$USUBJID %in% lbie1))
lbfin22 <- filter(lb2, (lb2$VISITNUM==13 | lb2$VISITNUM==14)&(lb2$LBSTAT!="NOT DONE")&(lb2$USUBJID %in% lbie2))
n_distinct(lbfin1$USUBJID) #79
n_distinct(lbfin2$USUBJID) #161
n_distinct(lbfin11$USUBJID) #70
n_distinct(lbfin22$USUBJID) #108
}
usrBNfin <- filter(alllb, 
                   (alllb$VISITNUM==13 | alllb$VISITNUM==14)&
                     (alllb$LBSTAT!="NOT DONE")&
                     (alllb$USUBJID %in% allusrBN$USUBJID))
usrCfin <- filter(alllb, 
                  (alllb$VISITNUM==13 | alllb$VISITNUM==14)&
                    (alllb$LBSTAT!="NOT DONE")&
                    (alllb$USUBJID %in% allusrC$USUBJID))
n_distinct(usrBNfin$USUBJID) #150
n_distinct(usrCfin$USUBJID) #28
