library(ggplot2)
library(data.table)
library(pROC)
library(glmnet)
library(caret)
library(MASS)
library(doParallel)
library(ResourceSelection)

computePlotROC = function(obs, pred_probs){
  p_roc = roc(obs, pred_probs)
  plot(p_roc, print.auc = T, print.thres = T)
  return(p_roc)
}


ccdefault = readRDS("ccdefault2.Rda")

set.seed(1)
idx = sample(1:nrow(ccdefault), nrow(ccdefault) * 0.8, replace = F)

full.form = as.formula(default ~ .-yclass)
red.form = as.formula(default ~ .-PAY_4-PAY_5-PAY_6-yclass)

logis.full = glm(full.form, 
                data=ccdefault, family = binomial(link = logit), 
                subset = idx)
summary(logis.full)

all.coeffs = data.frame(logistic_F = coef(logis.full))

logis.full.probs = predict(logis.full, newdata = ccdefault[-idx], type="response")
logis.full.roc = roc(ccdefault[-idx, default], logis.full.probs)
plot(logis.full.roc, print.auc = T, print.thres = T)

logic.full.class = ifelse(logis.full.probs > 0.5, 1, 0)

hoslem.test(ifelse(logis.full.probs > 0.5, 1, 0), ccdefault[-idx, default])
losis.full.tab = table(pred = logic.full.class, obs = ccdefault[-idx, default])
losis.full.tab

logis.red = glm(red.form, 
                data=ccdefault, family = binomial(link = logit), 
                subset = idx)
summary(logis.red)

all.coeffs$logistic_R = c(coef(logis.red)[1:12], 0,0,0,coef(logis.red)[13])

# Reduced model with 2 less predictors is the same as the full model
# So using the reduced model
anova(logis.red, logis.full, test = "Chisq")

logis.red.prob = predict(logis.red, newdata = ccdefault[-idx], type="response")

logis.roc = roc(ccdefault[-idx, default], logis.red.prob)
# AUC = 0.721, threshold = 0.253
# Without PAY_4 
# AUC = 0.722, threshold = 0.243
plot(logis.roc, print.auc = T, print.thres = T)

logis.red.class = ifelse(logis.red.prob > 0.5, 1, 0)

logis.red.tab = table(pred = logis.red.class, obs = ccdefault[-idx, default])
1-sum(diag(logis.red.tab))/sum(logis.red.tab)
summary(logis.red)
hoslem.test(ccdefault[-idx, default], logis.red.class, 10)

# With PAY_4: 16% false positives and 53% Correct default predictions
# Without PAY_4: 2% false positives and 23% Correct predictions
logis.red.tab[2,1]/sum(logis.red.tab[,1]) 
logis.red.tab[2,2]/sum(logis.red.tab[,2]) 

roc.test(logis.full.roc, logis.roc)
# Although the deviance test suggests that the full and reduced models are 
# are significantly different. The results and DeLong's test to compare ROC
# curves suggest that the models are same

##############################################################################
####### QDA
####### Models from here on do not have PAY_4
##############################################################################
# 2*13*14/2 = 182 estimates i.e. a highly non flexible model
qda.red = qda(red.form, data = ccdefault, subset = idx)

qda.red.pred = predict(qda.red, newdata = ccdefault[-idx])
qda.red.tab = table(pred = qda.red.pred$class, obs = ccdefault[-idx,default])

ta = ifelse(qda.red.pred$posterior[,2] > 0.5, 1, 0)

hoslem.test(ta, ccdefault[-idx, default])

qda.red.tab[2,1]/sum(logis.red.tab[,1]) # 17.95% False positive predictions
qda.red.tab[2,2]/sum(logis.red.tab[,2]) # 59.09% Correct predictions

#compute and plot the roc curve
qda.roc = computePlotROC(ccdefault[-idx, default], qda.red.pred$posterior[,2])

# ROC test between Reduced Logistic and reduced QDA suggests that there is no
# significant difference. So, we choose the Logistic which has less parameters.
roc.test(qda.roc, logis.roc)

###############################################################################
#### Using Lasso
#### since some estimates in logistic regression were very close to zero
#### now trying the Lasso to make sure that those estimates are indeed
#### very small values.
###############################################################################
# alpha = 1 for the lasso in glmnet
mm = model.matrix(red.form, data = ccdefault)[,-1]

cl = makePSOCKcluster(10)
registerDoParallel(cl)

lasso.fit = cv.glmnet(x = mm[idx,], y = ccdefault[idx,default],
                   alpha = 1, standardize = T, parallel = T, family = "binomial")

stopCluster(cl)

plot(lasso.fit)
lasso.coeff = predict(lasso.fit, type="coefficients", s= "lambda.min")[,1]

all.coeffs$lasso_R = c(lasso.coeff[1:12], 0,0,0, lasso.coeff[13])

lasso.pred = predict(lasso.fit, newx = mm[-idx,], s = "lambda.min",
                     type="response")

lasso.roc = computePlotROC(ccdefault[-idx, default], as.vector(lasso.pred))
roc.test(lasso.roc, logis.roc)

lasso.class = ifelse(lasso.pred > 0.5, 1, 0)
lasso.tab = table(pred = lasso.class, obs = ccdefault[-idx, default])
lasso.tab
hoslem.test(ifelse(lasso.pred > 0.5, 1, 0), ccdefault[-idx, default])

lasso.tab[2,1]/sum(logis.red.tab[,1]) # 1.07% False positive predictions
lasso.tab[2,2]/sum(logis.red.tab[,2]) # 15% Correct predictions

print(paste("lambda.1se:",round(lasso.fit$lambda.1se, 3), "alpha:", 1))

# lambda.1se = 0.014 and alpha = 1
# Lambda is so close to 0 that the coefficients have minimal penalty
# Although, 5 estimates were set to zero by the lasso but three of them are
# Categorial variables and removing those levels in categorical levels will
# amount to removing data related to those levels.
# Apart from that, with lambda.1se, AGE is set to 0. Estimate for Age = 0 does
# make sense because logistic regression estimated that the odds of default 
# increase with Age, which is counterintuitive. We expect people becoming more
# responsible as Age increases.

# Using lambda.1se does put some variables to zero and the model is more
# interpretable, but this comes at the expense of predicting good results.
# Using lambda.min fails to reduced any variables to zero and results are 
# similar to Logistic Regression with 
# False Positive: 2.46% and 
# True Positives: 22.87%

###############################################################################
#### Using Elastic-Net model
###############################################################################

cl = makePSOCKcluster(6)
registerDoParallel(cl)

set.seed(1)
indexes = createDataPartition(ccdefault[idx,default], times = 10, p = 0.8)
fitControl = trainControl(method = "cv", number = 10,
                          classProbs = T, 
                          summaryFunction = twoClassSummary,
                          allowParallel = T, 
                          search = "grid",
                          selectionFunction = "best",
                          #savePredictions = TRUE, 
                          index = indexes)

grid = expand.grid(lambda = lasso.fit$lambda,
                  alpha = seq(0, 1, length.out = length(lasso.fit$lambda)))

y_class = make.names(ccdefault[,default], unique = F)

set.seed(123)
elnet = train(x = mm[idx,], y = y_class[idx], 
              preProcess = c("center", "scale"),
              method = "glmnet", metric = "ROC", maximize = T,
              tuneGrid = grid,
              trControl = fitControl,
              family = "binomial")

stopCluster(cl)
rm(cl, fitControl, grid, y_class, indexes)

# Get Elastic-Net coefficients
elnet.coef = predict(elnet$finalModel, type = "coefficients",
                s = elnet$bestTune$lambda, alpha = elnet$bestTune$alpha)[,1]

all.coeffs$elnet_R = c(elnet.coef[1:12],0,0,0, elnet.coef[13])

elnet.predict = predict(elnet, newdata = mm[-idx,], type="prob")
elnet.class = ifelse(elnet.predict[,2] > 0.5, 1, 0)

hoslem.test(ccdefault[-idx,default], elnet.class, 10)

mean(ifelse(elnet.class == ccdefault[-idx, default], 0, 1))

table(pred = elnet.class, obs = ccdefault[-idx, default])

library(tidyr)

all.coeffs$coef_names = rownames(all.coeffs)
all.coeffs.long = gather(all.coeffs, key = "method", value = "estimates",
                         logistic_F:elnet_R, factor_key = T)

ggplot(all.coeffs.long, aes(coef_names, estimates)) +
  geom_point(aes(color = method)) +
  geom_hline(yintercept = 0, color = "grey50") +
  scale_color_discrete(name = "Method", labels = c("Logistic (F)", "Logistic (R)",
                                          "Lasso (R)", "Elastic-Net (R)")) +
  labs(x = "Coefficient Names", y = "Raw Estimates",
       title = "GLM-Binomial Estimates",
       subtitle = "Odds Increase to the right and decrease left of grey line") +
  theme(axis.text.x = element_text(angle = 90, family = "serif"), 
        title = element_text(family = "serif")) +
  coord_flip()

ggsave("coefficients.png")

