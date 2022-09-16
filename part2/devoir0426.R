library(tidyverse)
library(mlr3)
library(mlr3learners)
library(ggplot2)
library(ranger)

ds1 <- read.csv(file = "../001/ds.csv", header = T)
ds2 <- read.csv(file = "../002/ds.csv", header = T)
ds <- bind_rows(ds1, ds2)

dm1 <- read.csv(file = "../001/dm1.csv", header = T)
dm2 <- read.csv(file = "../002/dm2.csv", header = T)
dm <- bind_rows(dm1, dm2)

bras <- dm %>% select(ARM,USUBJID)

lb1 <- read.csv(file = "../001/lb1.csv", header = T)
lb2 <- read.csv(file = "../002/lb2.csv", header = T)
lb <- bind_rows(lb1, lb2)

statusfini <- ds %>% filter(VISITNUM<=14)
totalid <- bras$USUBJID
step0 <- right_join(statusfini, bras, by = "USUBJID")
step01 <- distinct(step0, step0$USUBJID, .keep_all = TRUE)
step01$ARM[which(is.na(step01$ARM))] <- "SCREEN FAILURE"
step01$place <- ifelse(step01$STUDYID == "NIDA-CTN-0001", "IN", "OUT")
step01$choke <- ifelse(step01$DSDECOD == "PARTICIPANT COMPLETED ACTIVE PHASE OF STUDY", "Success", "Fail")
step1 <- left_join(step01, dm, by = "USUBJID")
step2 <- left_join(step01, lb, by = "USUBJID")

# length(totalid)

daytest <- function(day, base){
  out <- vector("list", length(totalid))
  temptab <- base %>% filter(VISITNUM.y == day)
  for(n in 1:length(totalid)){
    ifelse(nrow(filter(temptab, temptab$USUBJID == totalid[n] & temptab$LBORRES != "NEGATIVE"))!=0,out[n] <- "POSITIVE",out[n] <- "NEGATIVE")
  }
  return(unlist(out))
}


preres <- data.frame(sex = step1$SEX,
                     race = step1$RACE,
                     place = step01$place,
                     ARM = step01$ARM,
                     day0test = daytest(0, step2),
                   # day1test = daytest(1, step2),
                     day2test = daytest(2, step2),
                     day3test = daytest(3, step2),
                     day4test = daytest(4, step2),
                     day5test = daytest(5, step2),
                     day6test = daytest(6, step2),
                     day7test = daytest(7, step2),
                     day8test = daytest(8, step2),
                     day9test = daytest(9, step2),
                     day10test = daytest(10, step2),
                     day11test = daytest(11, step2),
                     day12test = daytest(12, step2),
                     day13test = daytest(13, step2),
                     day14test = daytest(14, step2),
                     finalres = step01$choke)
preres <- subset(preres, !is.na(preres$finalres))

# preparation
preres$finalres <- as.factor(preres$finalres)
task <- TaskClassif$new("respred", preres, target = "finalres")
train_set = sample(task$row_ids, 0.8 * task$nrow)
test_set = setdiff(task$row_ids, train_set)

# initialisation de la Regression Logistique
learner_logreg <- lrn("classif.log_reg")

# entrainer
learner_logreg$train(task, row_ids = train_set)

# initialisation du Random Forest
learner_rf <- lrn("classif.ranger", importance = "permutation")

# entrainer
learner_rf$train(task, row_ids = train_set)

# utiliser ggplot pour dessiner la graphe d'importance
importance = as.data.table(learner_rf$importance(), keep.rownames = TRUE)
colnames(importance) = c("Feature", "Importance")
ggplot(data=importance,
       aes(x = reorder(Feature, Importance), y = Importance))+ 
  geom_col() + coord_flip() + xlab("")

# commencer a la prediction utilisant donnees de test
pred_logreg <- learner_logreg$predict(task, row_ids = test_set)
pred_rf <- learner_rf$predict(task, row_ids = test_set)

# Matrice de confusion
pred_logreg$confusion
pred_rf$confusion

# Validation Croisee
cvtest <- rsmp("cv", folds = 10)
{
# res_logreg <- resample(task, learner = learner_logreg, resampling = cvtest)
# res_logreg
# res_logreg$aggregate()
}
res_rf <- resample(task, learner = learner_rf, resampling = cvtest)
res_rf
res_rf$aggregate()

# Trouver les parametres a tuner
{
  # learner_logreg$param_set
  # learner_rf$param_set
}

# mtry = floor(sqrt(ncol(data) - 1)) ici floor(sqrt(19-1)) = 4 

# Tuner hyperparametres num.trees & mtry
# num.trees: nombre d'arbre decision
# mtry: sampling de variable de chaque fois
rf_def = lrn("classif.ranger", id = "default", predict_type = "prob")

rf_low = lrn("classif.ranger", id = "low", predict_type = "prob",
             num.trees = 100, mtry = 2)

rf_high = lrn("classif.ranger", id = "high", predict_type = "prob",
              num.trees = 1000, mtry = 8)

learners_rf = list(rf_low, rf_def, rf_high)
hypertest_rf = benchmark_grid(task, learners = learners_rf, resamplings = cvtest)
bmr_rf = benchmark(hypertest_rf)
# Comparer coefficient d'erreur et auc des 3 
measures = msrs(c("classif.ce", "classif.auc"))
performances_rf = bmr_rf$aggregate(measures)
performances_rf[, .(learner_id, classif.ce, classif.auc)]


{
# # Tuner hyperparametres maxit & epsilon
# # maxit: maximum fois d'iteration
# logreg_def = lrn("classif.log_reg", id = "default", predict_type = "prob")
# 
# logreg_low = lrn("classif.log_reg", id = "low", predict_type = "prob",
#                  maxit = 12, epsilon = 0.0000000001)
# 
# logreg_high = lrn("classif.log_reg", id = "high", predict_type = "prob",
#                   maxit = 50, epsilon = 0.00001)
# 
# learners_logreg = list(logreg_low, logreg_def, logreg_high)
# hypertest_logreg = benchmark_grid(task, learners = learners_logreg, resamplings = cvtest)
# bmr_logreg = benchmark(hypertest_logreg)
# # Comparer coefficient d'erreur et auc des 3 
# measures = msrs(c("classif.ce", "classif.auc"))
# performances_logreg = bmr_logreg$aggregate(measures)
# performances_logreg[, .(learner_id, classif.ce, classif.auc)]
}

# Donc on choisit modele de Random Forest

saveRDS(learner_rf, "./meilleur_model")

pred_var <- function(bdd, var, model){
  bdd$var <- as.factor(bdd$var)
  task <- TaskClassif$new("new", bdd, target = as.character(var))
  pred <- model$predict(task, row_ids = task$row_ids)
  return(pred)
}
