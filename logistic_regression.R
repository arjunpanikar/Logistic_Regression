## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:
setwd("C:/Users/mem-arjunp/Documents/R/Logistic_Regression")
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

install.packages(effects)
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

##Keep only relevant variables in the data set and clean the data set
NH11Test <- data.frame(NH11[c("everwrk", "age_p","r_maritl")])
NH11Final <- NH11Test[complete.cases(NH11Test),]
## Remove values that do not contribute to the efficacy of analysis
NH11Final <- subset(NH11Final,NH11Final$everwrk != "7 Refused")
NH11Final <- subset(NH11Final,NH11Final$everwrk != "9 Don't know")
NH11Final <- subset(NH11Final,NH11Final$everwrk != "8 Not ascertained")
NH11Final$everwrk <- factor(NH11Final$everwrk)
everwrk.out <- glm(everwrk~age_p+r_maritl,  data=NH11Final, family="binomial", na.action = na.omit)
summary(everwrk.out)

##  Coefficients:
##  Estimate Std. Error z value Pr(>|z|)    
##  (Intercept)                                 -0.440248   0.093538  -4.707 2.52e-06 ***
##  age_p                                       -0.029812   0.001645 -18.118  < 2e-16 ***
##  r_maritl2 Married - spouse not in household  0.049675   0.217310   0.229  0.81919    
##  r_maritl4 Widowed                            0.683618   0.084335   8.106 5.23e-16 ***
##  r_maritl5 Divorced                          -0.730115   0.111681  -6.538 6.25e-11 ***
##  r_maritl6 Separated                         -0.128091   0.151366  -0.846  0.39742    
##  r_maritl7 Never married                      0.343611   0.069222   4.964 6.91e-07 ***
##  r_maritl8 Living with partner               -0.443583   0.137770  -3.220  0.00128 ** 
##  r_maritl9 Unknown marital status             0.395480   0.492967   0.802  0.42241    
##---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##(Dispersion parameter for binomial family taken to be 1)
##Null deviance: 11082  on 14039  degrees of freedom
##Residual deviance: 10309  on 14031  degrees of freedom
##AIC: 10327
##Number of Fisher Scoring iterations: 5

everwrk.out.tab <- coef(summary(everwrk.out))
everwrk.out.tab[, "Estimate"] <- exp(coef(everwrk.out))
everwrk.out.tab

## Question: On applying reverse log, signs change on estimates for different variables. Should the interpretation be different now?
##Numbers above 1 is equivalent to Positive; Numbers less than 1 is equivalent to Negative; exp(beta) - 1

##                                              Estimate  Std. Error     z value     Pr(>|z|)
##(Intercept)                                 0.6438770 0.093537691  -4.7066328 2.518419e-06
##age_p                                       0.9706278 0.001645433 -18.1181481 2.291800e-73
##r_maritl2 Married - spouse not in household 1.0509300 0.217309587   0.2285932 8.191851e-01
##r_maritl4 Widowed                           1.9810316 0.084335382   8.1059419 5.233844e-16
##r_maritl5 Divorced                          0.4818536 0.111680788  -6.5375152 6.254929e-11
##r_maritl6 Separated                         0.8797735 0.151366140  -0.8462316 3.974236e-01
##r_maritl7 Never married                     1.4100296 0.069222260   4.9638756 6.910023e-07
##r_maritl8 Living with partner               0.6417330 0.137769623  -3.2197443 1.283050e-03
##r_maritl9 Unknown marital status            1.4850962 0.492966577   0.8022441 4.224118e-01


predictSet = predict(everwrk.out, type = "response")
summary(predictSet)

## summary(predictSet)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.02402 0.07398 0.10680 0.13440 0.17180 0.41990  
## Question: predictSet function resulted in low probabilities for predicting everwrk. 

tapply(predictSet,NH11Final$everwrk, mean)

##  1 Yes              2 No         7 Refused 8 Not ascertained      9 Don't know 
##  0.1261314         0.1876655                NA                NA                NA 

table(NH11Final$everwrk, predictSet > 0.5)
##                  FALSE
##1 Yes             12153
##2 No               1887
## Threshold value of 0.5 is too high, therefore a lower threshold value needs to be selected

table(NH11Final$everwrk, predictSet > 0.25)
##                  FALSE  TRUE
##1 Yes             10967  1186
##2 No               1342   545

table(NH11Final$everwrk, predictSet > 0.1)
##      FALSE TRUE
##1 Yes  6062 6091
##2 No    482 1405

library(ROCR)

ROCRpred <- prediction(predictSet, NH11Final$everwrk)
ROCRPerf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRPerf)
plot(ROCRPerf, colorize=TRUE)
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

