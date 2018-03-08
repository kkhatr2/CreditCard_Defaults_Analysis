library(ggplot2)
library(data.table)

data = fread("ccdefault.csv", sep = ",", header = T,
             colClasses = c(rep("numeric", 24)))

# Data collected from April 2005 - September 2005
#
# Based on some of the methods and the obvious correlation between billed 
# amount, money paid and trailing pattern of delays.
# I think it will be wise to sum the billed amount and money paid, and since
# most of the methods suggest that the trailing three months are more important
# variables. I will sum only the trailing 3 months billed and payment amounts
# to compute a new variable "result_balance" variable.
# TODO
############################################################################
#### Data Cleaning
############################################################################
names(data)[length(data)] = "default"

# Convert from 2 = female and 1 = male to
# 1 = female and 0 = male
data$SEX = data$SEX - 1

# Coercing extra levels not accounted for in the documentation to "Other"
data$EDUCATION = ifelse(data$EDUCATION %in% c(0,5,6), 4, data$EDUCATION)
data$MARRIAGE = ifelse(data$MARRIAGE == 0, 3, data$MARRIAGE)
# Currency Rate as of 2/19/18
# 1 NT = 0.034 USD
data[,"LIMIT_BAL"] = data[,LIMIT_BAL] * 0.034
for(i in 1:6){
  data[,paste0("BILL_AMT", i)] = data[,paste0("BILL_AMT", i), with=F] * 0.034
  data[,paste0("PAY_AMT", i)] = data[,paste0("PAY_AMT", i), with=F] * 0.034
}
rm(i)

# convert datatypes to factors for a dummy variable approach
# I believe the dummy variable approach will be correct because
# these variables are nominal and not ordinal
data$SEX = as.factor(data[,SEX])
data$MARRIAGE = as.factor(data[,MARRIAGE])
data$EDUCATION = as.factor(data[,EDUCATION])

data2 = data[,.(default, LIMIT_BAL, SEX, EDUCATION, MARRIAGE, AGE, 
                PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6)]

trail_bill = data[,.(BILL_AMT1, BILL_AMT2, BILL_AMT3)]
# , BILL_AMT4, BILL_AMT5, BILL_AMT6
trail_pay = data[,.(PAY_AMT1, PAY_AMT2, PAY_AMT3)]
# , PAY_AMT4, PAY_AMT5, PAY_AMT6

trail_bill_sum = apply(trail_bill, 1, sum)
trail_pay_sum = apply(trail_pay, 1, sum)
result_balance = trail_bill_sum - trail_pay_sum

data2$trail_balance = result_balance

rm(trail_bill, trail_pay, trail_bill_sum, trail_pay_sum, result_balance)
############################################################################
#### End Data Cleaning
############################################################################
## Save cleaned data
#saveRDS(data, file = "ccdefault.Rda", compress = "gzip")
#saveRDS(data2, file = "ccdefault2.Rda", compress = "gzip")
data = readRDS("ccdefault2.Rda")

gender_pct = data[,.(Pct = round(.N/30000*100,2), .N), by=.(default, SEX) ]
gender_pct[3,4] = 23364
gender_pct[4,4] = 6639
  
# From the people that have defaults, a greater number of females than males
# have no default and, it is also the case that greater number of females than
# males have default
ggplot(data, aes(default)) +
  geom_bar(aes(fill=factor(SEX))) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Default", y = "Count",
       title = "Default Payments Among Gender", fill = "Gender",
       subtitle = "Distribution of Default Credit Card Customer's in Taiwan") +
  geom_text(data=gender_pct, aes(x=default, y=N, label = paste0(Pct, "%")), vjust = 3) +
  theme(title = element_text(family = "serif"))
rm(gender_pct)
# Save above plot to disc
ggsave(filename = "defaultGender.png", scale = 1)

# On Average who has the higher credit limit
ggplot(data, aes(x = SEX, y = LIMIT_BAL)) +
  stat_summary(aes(fill = SEX), geom = "bar", fun.y = mean) +
  stat_summary(aes(label = scales::dollar(round(..y.., 0))), 
               geom = "text", fun.y = mean, vjust=-0.6) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(labels = c("Male", "Female")) +
  labs(x = "Gender", y = "Average Credit Limit",
       title = "Average Credit Limit for Males and Females",
       subtitle = "Currency in USD; 1 New Taiwan Dollar = 0.034 USD") +
  theme(legend.position = "none", title = element_text(family = "serif"))
ggsave(filename = "averageLimit.png", scale = 1)
# Although average values are close to each other
# A t-test suggests that the average values are significantly different.
t.test(x = data[SEX == "1", LIMIT_BAL], y = data[SEX == "0", LIMIT_BAL])


# Average Credit limit based on gender and education level
avg_e = data[,.(edu_avg = mean(LIMIT_BAL)), by = .(SEX, EDUCATION)][order(SEX,EDUCATION)]

ggplot(avg_e, aes(EDUCATION, edu_avg)) +
  geom_bar(aes(fill = SEX), stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Gender", labels = c("Male", "Female")) +
  scale_x_discrete(breaks = as.factor(1:4), labels = c("Graduate", "University",
                              "High School", "Other")) +
  scale_y_continuous(labels = scales::dollar) +
  geom_text(aes(label = scales::dollar(round(edu_avg/1000, 2))), hjust = 0.6) +
  labs(x = "Education Level", y = "Average Credit Limit",
       title = "Average Credit Limit based on Education and Gender",
       subtitle = "Mean Currency in \"US Dollar,\" multiple of $1,000, (1 NT = 0.034 USD)") +
  theme(title = element_text(family = "serif"))
rm(avg_e)
ggsave(filename = "limitGenderEdu.png", scale = 1)

# Proportion of defaults among marital statuses
m_status = c("1" = "Married", "2" = "Single", "3" = "Other")

ggplot(data, aes(default)) +
  geom_bar(aes(y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..],
               fill = factor(default))) +
  labs(x = "Default", y = "Proportion of Defaults",
       title = "Proportion of Defaults among Marital Statuses") +
  scale_x_continuous(breaks = 0:1, labels = c("No", "Yes")) +
  ylim(0,1) +
  facet_wrap(~MARRIAGE, labeller = as_labeller(m_status)) +
  theme(legend.position = "none", title = element_text(family = "serif"))
ggsave(filename = "propMarital.png", scale = 1)
rm(m_status)

# What is the AGE where maximum defaults occur.
d1 = density(data[default == 0, AGE])
d2 = density(data[default == 1, AGE])

ggplot(data, aes(AGE)) +
  stat_density(aes(y = ..scaled../2, group = default, 
                   fill = factor(default)), alpha =0.45) +
  geom_vline(xintercept = c(round(d1$x[which.max(d1$y)]),
                            round(d2$x[which.max(d2$y)])),
             color = c("green4","red")) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_fill_discrete(name = "Default", labels = c("No", "Yes")) +
  labs(x = "Age", y = "Scaled Density",
       title = "Density of Age by Default Status") +
  annotate(geom = "text", x = c(60, 60), y = c(0.9, 0.3), 
           label = c(paste0("Max at Age = ", round(d1$x[which.max(d1$y)], 2)),
                     paste0("Max at Age = ", round(d2$x[which.max(d2$y)], 2))),
           family = "serif", color=c("green4", "red")) +
  theme(title = element_text(family = "serif"))
ggsave(filename = "densityAge.png", scale = 1)
rm(d1, d2)

# There is no relationship between the data and default status:
# Some people have been paying some part of their billed amount and still
# they are at a default status also, some people that have never used any credit
# they are labelled as at default status.
# On some instances, the amount billed is negative for all the months (Apr-Sept)
# suggesting that the customer has overpaid but in effect that customer has
# paid nothing (Apr-Sept 2005) and this customer is at default status.
library(pROC)
library(MASS)
#############################################################################
##### Logistic Regression on full and reduced datasets
# with 80:20 split of original data
#############################################################################
set.seed(1)
idx = sample(1:nrow(data), nrow(data) * 0.8, replace = F)

# Full Model
full.formula = as.formula(default ~ .)
logit.fit = glm(full.formula, data=data, family = binomial(link = logit),
                subset = idx)

summary(logit.fit)

logit.fit.pred = predict(logit.fit, newdata = data[-idx], type="response")

p_roc = roc(data[-idx, default], logit.fit.pred)
plot(p_roc, print.auc = T)

# Reduced Model
red.formula = as.formula(default ~ LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+
                           PAY_0+PAY_2+PAY_3+PAY_4+
                           BILL_AMT1+BILL_AMT2+BILL_AMT3+
                           PAY_AMT1+PAY_AMT2+PAY_AMT3)


logit.fit1 = glm(red.formula, data = data, family = binomial(link=logit), subset = idx)
summary(logit.fit1)

logit.fit1.pred = predict(logit.fit1, data[-idx], type="response")

p_roc1 = roc(data[-idx, default], logit.fit1.pred)
plot(p_roc1, print.auc = T, print.thres =T)

logit.fit1.class = ifelse(logit.fit1.pred > 0.5, "1", "0")

tab = table(pred = logit.fit1.class, data[-idx, default])
mean(ifelse(logit.fit1.class == data[-idx, default], 0, 1))

exp(coef(logit.fit1)[3])

# Model Selection using stepwise selection in both directions
model_select = stepAIC(logit.fit)


summary(model_select$anova)
model_select$anova

anova(model_select, logit.fit, test = "Chisq")


######## Weighted Logistic Regression on the reduced model
wt.logit = glm(red.formula, data = data, family = binomial(link=logit), 
               subset = idx, weights = ifelse(default == 1, 3, 1))

summary(wt.logit)

wt.logit.pred = predict(wt.logit, newdata = data[-idx], type="response")
wt.proc = roc(data[-idx,default], wt.logit.pred)
plot(wt.proc, print.auc = T, print.thres = T)

wt.logit.class = ifelse(wt.logit.pred > 0.531, 1, 0)
wt.tab = table(pred = wt.logit.class, obs = data[-idx,default])
wt.tab
mean(ifelse(wt.logit.class == data[-idx,default], 0, 1))

# Weighted logistic regression just shifts the threshold value but does not
# produce any better results.
# 15 predictors and 2 classes to predict so QDA will have to estimate
# 2*15(15+1)/2 = 240 parameters
qda.fit = qda(red.formula, data = data, subset = idx)

qda.pred = predict(qda.fit, newdata = data[-idx,])
tab.qda = table(pred = qda.pred$class, obs = data[-idx,default])
tab.qda
mean(ifelse(qda.pred$class == data[-idx,default], 0, 1))
qda.proc = roc(data[-idx,default], qda.pred$posterior[,2])
plot(qda.proc, print.auc = T, print.thres = T)


# Quadratic discriminant analysis is worse with a 
# misclassification rate of 0.4966 and a lot of false positives.
# So QDA is useless for classification on this dataset.

# Try the elastic-net approach to classification.
library(glmnet)
library(train)
library(doParallel)

# alpha = 0 is the ridge penalty
mm = model.matrix(red.formula, data=data)[,-1]

cl = makePSOCKcluster(4)
registerDoParallel(cl)

ridge.fit = cv.glmnet(x = mm[idx,], y = data[idx,default], alpha = 0, 
                      family = "binomial", standardize = T, parallel = T)
stopCluster(cl)

plot(ridge.fit)

ridge.coef = predict(ridge.fit, type = "coefficients", s = "lambda.min")

ridge.pred = predict(ridge.fit, newx = mm[-idx,], s = "lambda.min", type="response")

ridge.proc = roc(data[-idx, default], as.vector(ridge.pred))
plot(ridge.proc, print.auc = T, print.thres = T)

ridge.class = ifelse(ridge.pred > 0.5, 1, 0)
ridge.tab = table(pred = ridge.class, obs = data[-idx, default])
ridge.tab
mean(ifelse(ridge.class == data[-idx, default], 0,1))

## Ridge is no better than logistic regression on the full dataset.












