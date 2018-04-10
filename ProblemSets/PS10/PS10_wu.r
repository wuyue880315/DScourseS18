install.packages('rpart')
install.packages('e1071')
install.packages('kknn')
install.packages('nnet')

library('rpart')
library('e1071')
library('kknn')
library('nnet')
library(mlr)

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)


# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#The classification task
treetask <- makeClassifTask( id="treetask" , data=income.train, target="high.earner")
logistictask <- makeClassifTask( id="logistictask" , data=income.train, target="high.earner")
neuraltask <- makeClassifTask( id="neuraltask" , data=income.train, target="high.earner")
knntask <- makeClassifTask( id="knntask" , data=income.train, target="high.earner")
svmtask <- makeClassifTask( id="svmtask" , data=income.train, target="high.earner")
nbtask <- makeClassifTask( id="nbtask" , data = income.train, target = "high.earner")


#The 3-fold cross-validation strategy
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

#6 learners (algorithms)
predAlg_trees <- makeLearner("classif.rpart",predict.type = "response")
predAlg_logistic <- makeLearner("classif.glmnet",predict.type = "response")
predAlg_neural <- makeLearner("classif.nnet",predict.type = "response")
predAlg_naive_bayes <- makeLearner("classif.naiveBayes",predict.type = "response")
predAlg_knn <- makeLearner("classif.kknn",predict.type = "response")
predAlg_svm <- makeLearner("classif.svm",predict.type = "response")

#set hyperparameters
modelParams_trees <- makeParamSet(makeIntegerParam("minsplit",lower=10,upper=50),makeIntegerParam("minbucket",lower=5,upper=50),makeNumericParam("cp",lower = 0.001,upper = 0.2))
modelParams_logistic <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))
modelParams_neural <- makeParamSet(makeIntegerParam("size",lower=1,upper=10),makeNumericParam("decay",lower=0.1,upper=0.5),makeIntegerParam("maxit",lower=1000,upper=1000))
modelParams_knn <- makeParamSet(makeIntegerParam("k",lower = 1, upper = 30))
a<-c(2^(-2), 2^(-1), 1, 2, 4, 2^(10))
modelParams_svm <- makeParamSet(makeDiscreteParam("kernel","radial"),makeDiscreteParam("cost",a),makeDiscreteParam("gamma",a))

#Times
tuneMethod <- makeTuneControlRandom(maxit = 50L)

#Tune the models
tunedModel_trees <- tuneParams(learner = predAlg_trees,
                         task = treetask,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),
                         par.set = modelParams_trees,
                         control = tuneMethod,
                         show.info = TRUE) #Result: minsplit=46; minbucket=44; cp=0.0159 : f1.test.mean=0.8959627,gmean.test.mean=0.6542037

tunedModel_logistic <- tuneParams(learner = predAlg_logistic,
                                  task = logistictask,
                                  resampling = resampleStrat,
                                  measures = list(f1, gmean),
                                  par.set = modelParams_logistic,
                                  control = tuneMethod,
                                  show.info = TRUE) #Result: lambda=0.133; alpha=0.0501 : f1.test.mean=0.8967970,gmean.test.mean=0.6382264

tunedModel_neural <- tuneParams(learner = predAlg_neural,
                                task = neuraltask,
                                resampling = resampleStrat,
                                measures = list(f1, gmean),
                                par.set = modelParams_neural,
                                control = tuneMethod,
                                show.info = TRUE) #Result: size=10; decay=0.359; maxit=1000 : f1.test.mean=0.9060970,gmean.test.mean=0.7555547

tunedModel_knn <- tuneParams(learner = predAlg_knn,
                             task = knntask,
                             resampling = resampleStrat,
                             measures = list(f1, gmean),
                             par.set = modelParams_knn,
                             control = tuneMethod,
                             show.info = TRUE) #Result: k=29 : f1.test.mean=0.8968708,gmean.test.mean=0.7436127

tunedModel_svm <- tuneParams(learner = predAlg_svm,
                             task = svmtask,
                             resampling = resampleStrat,
                             measures = list(f1, gmean),
                             par.set = modelParams_svm,
                             control = tuneMethod,
                             show.info = TRUE) #Result:Result: kernel=radial; cost=1; gamma=2 : f1.test.mean=0.8930845,gmean.test.mean=0.6630879

## Apply the optimal algorithm parameters to the model
predAlg_trees <- setHyperPars(learner=predAlg_trees, par.vals = tunedModel_trees$x)
predAlg_logistic <- setHyperPars(learner=predAlg_logistic, par.vals = tunedModel_logistic$x)
predAlg_neural <- setHyperPars(learner=predAlg_neural, par.vals = tunedModel_neural$x)
predAlg_knn <- setHyperPars(learner=predAlg_knn, par.vals = tunedModel_knn$x)
predAlg_svm <- setHyperPars(learner=predAlg_svm, par.vals = tunedModel_svm$x)

#Verify performance on cross validated sample sets
resample(predAlg_trees,treetask,resampleStrat,measures=list(f1,gmean))
resample(predAlg_logistic,logistictask,resampleStrat,measures=list(f1,gmean))
resample(predAlg_neural,neuraltask,resampleStrat,measures=list(f1,gmean))
resample(predAlg_knn,knntask,resampleStrat,measures=list(f1,gmean))
resample(predAlg_svm,svmtask,resampleStrat,measures = list(f1,gmean))

# Train the final model
finalModel_trees <- train(learner = predAlg_trees, task = treetask)
finalModel_logistic <- train(learner = predAlg_logistic, task = logistictask)
finalModel_neural <- train(learner = predAlg_neural, task = neuraltask)
finalModel_naive_bayes <- train(learner = predAlg_naive_bayes, task = nbtask)
finalModel_knn <- train(learner = predAlg_knn, task = knntask)
finalModel_svm <- train(learner = predAlg_svm, task = svmtask)

# Predict in test set!
prediction_tree <- predict(finalModel_trees, newdata = income.test)
prediction_logistic <- predict(finalModel_logistic, newdata = income.test)
prediction_neural <- predict(finalModel_neural, newdata = income.test)
prediction_naive_bayes <- predict(finalModel_naive_bayes, newdata = income.test)
prediction_knn <- predict(finalModel_knn, newdata = income.test)
prediction_svm <- predict(finalModel_svm, newdata = income.test)

# Performance
performance(prediction_tree, measures = list(f1,gmean))
performance(prediction_logistic, measures = list(f1,gmean))
performance(prediction_neural, measures = list(f1,gmean))
performance(prediction_naive_bayes, measures = list(f1,gmean))
performance(prediction_knn, measures = list(f1,gmean))
performance(prediction_svm, measures = list(f1,gmean))

