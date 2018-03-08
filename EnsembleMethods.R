library(randomForest)
library(caret)
library(data.table)
library(doParallel)
library(ResourceSelection)
library(pROC)

ccdefault = readRDS("ccdefault2.Rda")

set.seed(1)
idx = sample(1:nrow(ccdefault), nrow(ccdefault) * 0.8, replace = F)

red.form = as.formula(default ~ .-PAY_4-PAY_5-PAY_6)

computePlotROC = function(obs_class, pred_class){
  p_roc = roc(obs_class, pred_class)
  plot(p_roc, print.auc = T, print.thres = T)
  return(p_roc)
}

###############################################################################
#### Random Forest
###############################################################################

cl = makePSOCKcluster(8)
registerDoParallel(cl)

fitControl = trainControl(method = "cv", number = 10,
                          summaryFunction = twoClassSummary,
                          allowParallel = T, 
                          search = "grid",
                          classProbs = T,
                          selectionFunction = "best",
                          p = 0.6)

ccdefault$yclass = make.names(ccdefault$default)

set.seed(123)
rf = train(yclass ~ .-default, data = ccdefault[idx],
           method = "rf",
           trControl = fitControl, 
           metric = "ROC",
           tuneGrid = data.frame(mtry = 1:9),
           importance = T)

stopCluster(cl)
rm(fitControl, cl)

plot(rf)

rf.pred = predict(rf, newdata = ccdefault[-idx,], type="prob")

rf.pred = predict(rf$finalModel, newdata = ccdefault[-idx,], type = "prob")

rf.class = ifelse(rf.pred[,2] > 0.5, 1, 0)

rf.roc = computePlotROC(ccdefault[-idx, default], rf.class)

hoslem.test(ccdefault[-idx,default], rf.class)

rf.tab = table(obs = ccdefault[-idx,default], pred = rf.class)
rf.tab

### Computin the random forest again with selected hyperparameters
r.forest = randomForest(as.factor(default) ~ .-yclass-PAY_4-PAY_5-PAY_6,
                        data = ccdefault, mtry = 2, subset = idx,
                        importance = T)
summary(r.forest)
plot(r.forest)
varImpPlot(r.forest)

partialPlot(x = r.forest, pred.data = ccdefault[idx],
            x.var = PAY_0 , which.class = "1", rug = T)

partialPlot(x = r.forest, pred.data = ccdefault[idx],
            x.var = MARRIAGE , which.class = "1")

partialPlot(x = r.forest, pred.data = ccdefault[idx],
            x.var = EDUCATION , which.class = "1")

partialPlot(x = r.forest, pred.data = ccdefault[idx],
            x.var = LIMIT_BAL , which.class = "1")

partialPlot(x = r.forest, pred.data = ccdefault[idx],
            x.var = trail_balance , which.class = "1")

partialPlot(x = r.forest, pred.data = ccdefault[idx],
            x.var = SEX , which.class = "1")


###############################################################################
### Bagged Neeural Network
###############################################################################
library(nnet)

cl = makePSOCKcluster(12)
registerDoParallel(cl)

fitControl = trainControl(method = "none",# number = 10,
                          summaryFunction = twoClassSummary,
                          allowParallel = T, 
                          search = "grid",
                          classProbs = T,
                          selectionFunction = "best",
                          p = 0.8)

grid = expand.grid(size = seq(5, 10, by = 5),
                   decay = 0.01,
                   bag = seq(20, 100, by=10))

set.seed(123)
bnn = train(yclass ~ .-default, data = ccdefault[idx],
           method = "avNNet",
           trControl = fitControl, 
           metric = "ROC",
           maximize = T,
           tuneGrid = grid)

stopCluster(cl)
rm(fitControl, cl)

bnn.prob = predict(bnn, newdata = ccdefault[-idx], type="prob")
bnn.class = ifelse(bnn.prob$X1 >= 0.5, 1, 0)
bnn.tab = table(pred = bnn.class, obs = ccdefault[-idx, default])

bnn.tab




