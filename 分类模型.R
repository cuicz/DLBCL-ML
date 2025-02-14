# Importing data
setwd("D:/R/ML/DL")
ilpd <- read.csv("R.csv",header = T,row.names = 1)
View(ilpd)
ilpd$TR = as.factor(ilpd$TR)
str(ilpd)
##  所有离散变量的频率分布
library(mlr3verse)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(ggplot2)
library(gridExtra)
library(mlr3)
View(ilpd)
##  所有离散变量的频率分布
plot_bar(ilpd,ggtheme = theme_bw())
#特征分组
plot_bar(ilpd,by = 'TR',ggtheme = theme_bw())
## View bivariate continuous distribution based on `diseased`
plot_boxplot(ilpd,by = 'TR')
#相关分析
plot_correlation(ilpd)
## 标准化
po_scale = po("scale")
po_scale$param_set$values$affect_columns =
selector_name(c("MTV", "DmaxVox"))
task_liver = as_task_classif(ilpd, target = "TR", positive = "1")
#id = deparse(substitute(pbp_task))
ilpd_f = po_scale$train(list(task_liver))[[1]]$data()
#学习器和调参
## Task definition
task_liver = as_task_classif(ilpd_f, target = "TR", positive = "1")
# detect overfitting
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
k = p_int(lower = 3, upper = 50), # Number of neighbors considered
distance = p_dbl(lower = 1, upper = 3),
kernel = p_fct(levels = c("rectangular", "gaussian", "rank", "optimal"))
)
tune_ps_rpart = ps(
# Minimum number of observations that must exist in a node in order for a
# split to be attempted
minsplit = p_int(lower = 10, upper = 40),
cp = p_dbl(lower = 0.001, upper = 0.1) # Complexity parameter
)
tune_ps_rf = ps(
# Minimum size of terminal nodes
min.node.size = p_int(lower = 10, upper = 50),
# Number of variables randomly sampled as candidates at each split
mtry = p_int(lower = 1, upper = 6)
)
View(ilpd)
View(ilpd_f)
# Oversampling minority class to get perfectly balanced classes
po_over = po("classbalancing", id = "oversample", adjust = "minor",
reference = "minor", shuffle = FALSE, ratio = 88/44)
table(po_over$train(list(task_liver))$output$truth()) # Check class balance
# Learners with balanced/oversampled data
learners_bal = lapply(learners, function(x) {
GraphLearner$new(po_scale %>>% po_over %>>% x)
})
lapply(learners_bal, function(x) x$predict_sets = c("train", "test"))
#模型拟合和基准设定
# 5-fold cross-validation
resampling_outer = rsmp(id = "cv", .key = "cv", folds = 5L)
# Stratification
task_liver$col_roles$stratum = task_liver$target_names

#使用了**基准测试(benchmarking)
design = benchmark_grid(
tasks = task_liver,
learners = c(learners, learners_bal),
resamplings = resampling_outer
)
bmr = benchmark(design, store_models = FALSE) ## 耗时较长
#通过AUC对所有学习者进行了比较（有无超采样，以及训练和测试数据）
measures = list(
msr("classif.auc", predict_sets = "train", id = "auc_train"),
msr("classif.auc", id = "auc_test")
)
tab = bmr$aggregate(measures)
tab_1 = tab[,c('learner_id','auc_train','auc_test')]
print(tab_1)
View(ilpd)
# boxplot of AUC values across the 5 folds
autoplot(bmr, measure = msr("classif.auc"))
autoplot(bmr, measure = msr("classif.auc",predict_sets = "train"))
autoplot(bmr,type = "roc")
