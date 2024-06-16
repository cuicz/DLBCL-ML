setwd("D:/R/ML/DL")
ilpd <- read.csv("R.csv",header = T,row.names = 1)
View(ilpd)
ilpd$TR = as.factor(ilpd$TR)
str(ilpd)

library(mlr3verse)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(ggplot2)
library(gridExtra)
library(mlr3)
View(ilpd)


plot_correlation(ilpd)

po_scale = po("scale")
po_scale$param_set$values$affect_columns =
selector_name(c("MTV", "DmaxVox"))
task_liver = as_task_classif(ilpd, target = "TR", positive = "1")

ilpd_f = po_scale$train(list(task_liver))[[1]]$data()

task_liver = as_task_classif(ilpd_f, target = "TR", positive = "1")

library(e1071)
library(kknn)
library(catboost)
learners = list(
learner_logreg = lrn("classif.log_reg", predict_type = "prob",
predict_sets = c("train", "test")),
learner_lda = lrn("classif.lda", predict_type = "prob",
predict_sets = c("train", "test")),
learner_qda = lrn("classif.qda", predict_type = "prob",
predict_sets = c("train", "test")),
learner_nb = lrn("classif.naive_bayes", predict_type = "prob",
predict_sets = c("train", "test")),
learner_knn = lrn("classif.kknn", scale = FALSE,
predict_type = "prob",
predict_sets = c("train", "test")),
learner_rpart = lrn("classif.rpart",
predict_type = "prob",
predict_sets = c("train", "test")),
learner_rf = lrn("classif.ranger", num.trees = 1000,
predict_type = "prob",
predict_sets = c("train", "test")),
learner_xgb = lrn("classif.xgboost", predict_type = "prob",
predict_sets = c("train", "test"))
#learner_catb = lrn("classif.catboost", predict_type = "prob",
#  predict_sets = c("train", "test")),
#learner_gbm = lrn("classif.gbm", predict_type = "prob",
# predict_sets = c("train", "test"))
)

tune_ps_knn = ps(
k = p_int(lower = 3, upper = 50), 
distance = p_dbl(lower = 1, upper = 3),
kernel = p_fct(levels = c("rectangular", "gaussian", "rank", "optimal"))
)
tune_ps_rpart = ps(
minsplit = p_int(lower = 10, upper = 40),
cp = p_dbl(lower = 0.001, upper = 0.1) 
)
tune_ps_rf = ps(

min.node.size = p_int(lower = 10, upper = 50),

mtry = p_int(lower = 1, upper = 6)
)
View(ilpd)
View(ilpd_f)

po_over = po("classbalancing", id = "oversample", adjust = "minor",
reference = "minor", shuffle = FALSE, ratio = 88/44)
table(po_over$train(list(task_liver))$output$truth()) 
learners_bal = lapply(learners, function(x) {
GraphLearner$new(po_scale %>>% po_over %>>% x)
})
lapply(learners_bal, function(x) x$predict_sets = c("train", "test"))

resampling_outer = rsmp(id = "cv", .key = "cv", folds = 5L)

task_liver$col_roles$stratum = task_liver$target_names


design = benchmark_grid(
tasks = task_liver,
learners = c(learners, learners_bal),
resamplings = resampling_outer
)
bmr = benchmark(design, store_models = FALSE) 
measures = list(
msr("classif.auc", predict_sets = "train", id = "auc_train"),
msr("classif.auc", id = "auc_test")
)
tab = bmr$aggregate(measures)
tab_1 = tab[,c('learner_id','auc_train','auc_test')]
print(tab_1)
View(ilpd)

autoplot(bmr, measure = msr("classif.auc"))
autoplot(bmr, measure = msr("classif.auc",predict_sets = "train"))
autoplot(bmr,type = "roc")
