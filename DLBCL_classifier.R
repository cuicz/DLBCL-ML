# Importing data
setwd("D:/R/ML/DL")
data <- read.csv("CR.csv",header = T,row.names = 1)
View(data)
data$TR = as.factor(data$TR)
str(data)
#Load Package
library(mlr3verse)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(ggplot2)
library(gridExtra)
library(mlr3)
View(data)
#Frequency distribution of all discrete variables
plot_bar(data,ggtheme = theme_bw())
#grouping
plot_bar(data,by = 'TR',ggtheme = theme_bw())
#View bivariate continuous distribution based on `diseased`
plot_boxplot(data,by = 'TR')
#correlation analysis
plot_correlation(data)
#Standardization
po_scale = po("scale")
po_scale$param_set$values$affect_columns =
selector_name(c("MTV", "DmaxVox"))
task_liver = as_task_classif(data, target = "TR", positive = "1")
#id = deparse(substitute(pbp_task))
data_f = po_scale$train(list(task_liver))[[1]]$data()

#Task definition
task_liver = as_task_classif(data_f, target = "TR", positive = "1")
# detect overfitting
library(e1071)
library(kknn)
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
)

#tuning
tune_ps_logreg = ps(
  # Regularization type
  penalty = p_fct(levels = c("l1", "l2", "elasticnet")),
  #Regularization strength
  C = p_dbl(lower = 0.01, upper = 100, logscale = TRUE),
  # Elastic network mixing coefficient
  l1_ratio = p_dbl(lower = 0, upper = 1),
  # Maximum number of iterations for optimization algorithm
  max_iter = p_int(lower = 50, upper = 500)
)

tune_ps_lda = ps(
  # Stability of covariance matrix estimation control
  method = p_fct(levels = c("moment", "shrinkage", "t")), 
  # contraction intensity
  nu = p_dbl(lower = 0, upper = 1), 
  # Eigenvalue truncation threshold: filtering low variance components to enhance numerical stability
  tol = p_dbl(lower = 1e-5, upper = 1e-2, logscale = TRUE) # (默认 1e-4)
)

tune_ps_qda = ps(
  # Control the regularization strength of the covariance matrix
  gamma = p_dbl(lower = 0, upper = 1),
  # Regularization parameter λ
  lambda = p_dbl(lower = 0, upper = 1)
)

tune_ps_nb = ps(
  laplace = p_dbl(lower = 0, upper = 5),# Laplace smoothing coefficient
  adjust = p_dbl(lower = 0.1, upper = 3)# Kernel density estimation bandwidth adjustment factor
)

tune_ps_knn = ps(
k = p_int(lower = 3, upper = 50), # Number of neighbors considered
distance = p_dbl(lower = 1, upper = 3),#Parameters of Minkowski distance
kernel = p_fct(levels = c("rectangular", "gaussian", "rank", "optimal"))
)#The neighbor weight kernel function determines the voting weights of neighbors at different distances.

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

tune_ps_xgb = ps(
  #Control the contribution weight of each tree, small values require more tree iterations
  eta = p_dbl(lower = 0.01, upper = 0.3),
  #Control model complexity and overfitting risk
  max_depth = p_int(lower = 3, upper = 10),
  #Conservatism in controlling division
  min_child_weight = p_dbl(lower = 1, upper = 10),
  #The number of trees
  nrounds = p_int(lower = 100, upper = 2000)
)

# Oversampling minority class to get perfectly balanced classes
po_over = po("classbalancing", id = "oversample", adjust = "minor",
reference = "minor", shuffle = FALSE, ratio = 88/44)
table(po_over$train(list(task_liver))$output$truth()) # Check class balance
# Learners with balanced/oversampled data
learners_bal = lapply(learners, function(x) {
GraphLearner$new(po_scale %>>% po_over %>>% x)
})
lapply(learners_bal, function(x) x$predict_sets = c("train", "test"))
#Model fitting and benchmark setting
# 5-fold cross-validation
resampling_outer = rsmp(id = "cv", .key = "cv", folds = 5L)
# Stratification
task_liver$col_roles$stratum = task_liver$target_names

#Benchmarking was used
design = benchmark_grid(
tasks = task_liver,
learners = c(learners, learners_bal),
resamplings = resampling_outer
)
bmr = benchmark(design, store_models = FALSE) 
#All learners were compared using AUC (with and without oversampling, as well as training and testing data)
measures = list(
msr("classif.auc", predict_sets = "train", id = "auc_train"),
msr("classif.auc", id = "auc_test")
)
tab = bmr$aggregate(measures)
tab_1 = tab[,c('learner_id','auc_train','auc_test')]
print(tab_1)

# boxplot of AUC values across the 5 folds
autoplot(bmr, measure = msr("classif.auc"))
autoplot(bmr, measure = msr("classif.auc",predict_sets = "train"))

