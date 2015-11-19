# STAT6306 HW6
Kevin Kyoo Ha Cha  
Nov 19, 2015  

**Install and load Sleuth3 pkg, then attach the ex1223 data.**

```r
require(Sleuth3)
```

```
## Loading required package: Sleuth3
```

```r
#install.packages("Sleuth3")
#library(Sleuth3)

#Attach Self Esteem Data
data(ex1223)
attach(ex1223)
head(ex1223)
```

```
##   Subject Imagazine Inewspaper Ilibrary MotherEd FatherEd FamilyIncome78
## 1       2         1          1        1        5        8          20000
## 2       6         0          1        1       12       12          35000
## 3       7         1          1        1       12       12           8502
## 4       8         1          1        1        9        6           7227
## 5       9         1          1        1       12       10          17000
## 6      13         1          1        1       12       16          20000
##   Race Gender Educ Science Arith Word Parag Numer Coding Auto Math
## 1    3 female   12       6     8   15     6    29     52    9    6
## 2    3   male   16      23    30   35    15    45     68   21   23
## 3    3   male   12      14    14   27     8    32     35   13   11
## 4    3 female   14      18    13   35    12    24     48   11    4
## 5    3   male   14      17    21   28    10    40     46   13   13
## 6    3   male   16      16    30   29    13    36     30   21   24
##   Mechanic Elec   AFQT Income2005 Esteem1 Esteem2 Esteem3 Esteem4 Esteem5
## 1       10    5  6.841       5500       1       1       4       1       3
## 2       21   19 99.393      65000       2       1       4       2       4
## 3        9   11 47.412      19000       2       1       3       2       3
## 4       12   12 44.022      36000       1       1       3       2       3
## 5       13   15 59.683      65000       1       1       4       1       1
## 6       19   16 72.313       8000       1       1       4       1       4
##   Esteem6 Esteem7 Esteem8 Esteem9 Esteem10
## 1       3       1       3       3        3
## 2       2       2       4       3        4
## 3       2       2       2       3        3
## 4       2       3       3       3        3
## 5       1       1       4       4        4
## 6       1       1       4       4        4
```

```r
str(ex1223)
```

```
## 'data.frame':	2584 obs. of  32 variables:
##  $ Subject       : int  2 6 7 8 9 13 16 17 18 20 ...
##  $ Imagazine     : int  1 0 1 1 1 1 1 1 1 1 ...
##  $ Inewspaper    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Ilibrary      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ MotherEd      : int  5 12 12 9 12 12 12 12 12 12 ...
##  $ FatherEd      : int  8 12 12 6 10 16 12 15 16 18 ...
##  $ FamilyIncome78: int  20000 35000 8502 7227 17000 20000 48000 15000 4510 50000 ...
##  $ Race          : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ Gender        : Factor w/ 2 levels "female","male": 1 2 2 1 2 2 1 2 2 1 ...
##  $ Educ          : int  12 16 12 14 14 16 13 13 13 17 ...
##  $ Science       : int  6 23 14 18 17 16 13 19 22 21 ...
##  $ Arith         : int  8 30 14 13 21 30 17 29 30 17 ...
##  $ Word          : int  15 35 27 35 28 29 30 33 35 28 ...
##  $ Parag         : int  6 15 8 12 10 13 12 13 14 14 ...
##  $ Numer         : int  29 45 32 24 40 36 49 35 48 39 ...
##  $ Coding        : int  52 68 35 48 46 30 58 58 61 54 ...
##  $ Auto          : int  9 21 13 11 13 21 11 18 21 18 ...
##  $ Math          : int  6 23 11 4 13 24 17 21 23 20 ...
##  $ Mechanic      : int  10 21 9 12 13 19 11 19 16 20 ...
##  $ Elec          : int  5 19 11 12 15 16 10 16 17 13 ...
##  $ AFQT          : num  6.84 99.39 47.41 44.02 59.68 ...
##  $ Income2005    : int  5500 65000 19000 36000 65000 8000 71000 43000 120000 64000 ...
##  $ Esteem1       : int  1 2 2 1 1 1 2 2 2 1 ...
##  $ Esteem2       : int  1 1 1 1 1 1 2 2 2 1 ...
##  $ Esteem3       : int  4 4 3 3 4 4 3 3 3 3 ...
##  $ Esteem4       : int  1 2 2 2 1 1 2 2 2 1 ...
##  $ Esteem5       : int  3 4 3 3 1 4 3 3 3 3 ...
##  $ Esteem6       : int  3 2 2 2 1 1 2 2 2 2 ...
##  $ Esteem7       : int  1 2 2 3 1 1 3 2 2 1 ...
##  $ Esteem8       : int  3 4 2 3 4 4 3 3 3 3 ...
##  $ Esteem9       : int  3 3 3 3 4 4 3 3 3 3 ...
##  $ Esteem10      : int  3 4 3 3 4 4 3 3 3 3 ...
```


##Step 1

**The variable Esteem1 in the data set "ex1223.csv" has values 1, 2, 3, 4 based on the answer to the question "I feel I am a person of worth." **

**Construct a new binary variable from this variable, which takes the value 1 for strongagreement and 0 for agreement, disagreement, or strong disagreement.**


```r
ex1223[,"Esteem1"] <- ifelse((ex1223[,"Esteem1"]==1),1,0)

str(ex1223)
```

```
## 'data.frame':	2584 obs. of  32 variables:
##  $ Subject       : int  2 6 7 8 9 13 16 17 18 20 ...
##  $ Imagazine     : int  1 0 1 1 1 1 1 1 1 1 ...
##  $ Inewspaper    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Ilibrary      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ MotherEd      : int  5 12 12 9 12 12 12 12 12 12 ...
##  $ FatherEd      : int  8 12 12 6 10 16 12 15 16 18 ...
##  $ FamilyIncome78: int  20000 35000 8502 7227 17000 20000 48000 15000 4510 50000 ...
##  $ Race          : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ Gender        : Factor w/ 2 levels "female","male": 1 2 2 1 2 2 1 2 2 1 ...
##  $ Educ          : int  12 16 12 14 14 16 13 13 13 17 ...
##  $ Science       : int  6 23 14 18 17 16 13 19 22 21 ...
##  $ Arith         : int  8 30 14 13 21 30 17 29 30 17 ...
##  $ Word          : int  15 35 27 35 28 29 30 33 35 28 ...
##  $ Parag         : int  6 15 8 12 10 13 12 13 14 14 ...
##  $ Numer         : int  29 45 32 24 40 36 49 35 48 39 ...
##  $ Coding        : int  52 68 35 48 46 30 58 58 61 54 ...
##  $ Auto          : int  9 21 13 11 13 21 11 18 21 18 ...
##  $ Math          : int  6 23 11 4 13 24 17 21 23 20 ...
##  $ Mechanic      : int  10 21 9 12 13 19 11 19 16 20 ...
##  $ Elec          : int  5 19 11 12 15 16 10 16 17 13 ...
##  $ AFQT          : num  6.84 99.39 47.41 44.02 59.68 ...
##  $ Income2005    : int  5500 65000 19000 36000 65000 8000 71000 43000 120000 64000 ...
##  $ Esteem1       : num  1 0 0 1 1 1 0 0 0 1 ...
##  $ Esteem2       : int  1 1 1 1 1 1 2 2 2 1 ...
##  $ Esteem3       : int  4 4 3 3 4 4 3 3 3 3 ...
##  $ Esteem4       : int  1 2 2 2 1 1 2 2 2 1 ...
##  $ Esteem5       : int  3 4 3 3 1 4 3 3 3 3 ...
##  $ Esteem6       : int  3 2 2 2 1 1 2 2 2 2 ...
##  $ Esteem7       : int  1 2 2 3 1 1 3 2 2 1 ...
##  $ Esteem8       : int  3 4 2 3 4 4 3 3 3 3 ...
##  $ Esteem9       : int  3 3 3 3 4 4 3 3 3 3 ...
##  $ Esteem10      : int  3 4 3 3 4 4 3 3 3 3 ...
```

```r
head(ex1223)
```

```
##   Subject Imagazine Inewspaper Ilibrary MotherEd FatherEd FamilyIncome78
## 1       2         1          1        1        5        8          20000
## 2       6         0          1        1       12       12          35000
## 3       7         1          1        1       12       12           8502
## 4       8         1          1        1        9        6           7227
## 5       9         1          1        1       12       10          17000
## 6      13         1          1        1       12       16          20000
##   Race Gender Educ Science Arith Word Parag Numer Coding Auto Math
## 1    3 female   12       6     8   15     6    29     52    9    6
## 2    3   male   16      23    30   35    15    45     68   21   23
## 3    3   male   12      14    14   27     8    32     35   13   11
## 4    3 female   14      18    13   35    12    24     48   11    4
## 5    3   male   14      17    21   28    10    40     46   13   13
## 6    3   male   16      16    30   29    13    36     30   21   24
##   Mechanic Elec   AFQT Income2005 Esteem1 Esteem2 Esteem3 Esteem4 Esteem5
## 1       10    5  6.841       5500       1       1       4       1       3
## 2       21   19 99.393      65000       0       1       4       2       4
## 3        9   11 47.412      19000       0       1       3       2       3
## 4       12   12 44.022      36000       1       1       3       2       3
## 5       13   15 59.683      65000       1       1       4       1       1
## 6       19   16 72.313       8000       1       1       4       1       4
##   Esteem6 Esteem7 Esteem8 Esteem9 Esteem10
## 1       3       1       3       3        3
## 2       2       2       4       3        4
## 3       2       2       2       3        3
## 4       2       3       3       3        3
## 5       1       1       4       4        4
## 6       1       1       4       4        4
```

##Step 2

**Log transform on Income2005**


```r
ex1223[,"LogIncome2005"] <- log(Income2005)
str(ex1223)
```

```
## 'data.frame':	2584 obs. of  33 variables:
##  $ Subject       : int  2 6 7 8 9 13 16 17 18 20 ...
##  $ Imagazine     : int  1 0 1 1 1 1 1 1 1 1 ...
##  $ Inewspaper    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Ilibrary      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ MotherEd      : int  5 12 12 9 12 12 12 12 12 12 ...
##  $ FatherEd      : int  8 12 12 6 10 16 12 15 16 18 ...
##  $ FamilyIncome78: int  20000 35000 8502 7227 17000 20000 48000 15000 4510 50000 ...
##  $ Race          : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ Gender        : Factor w/ 2 levels "female","male": 1 2 2 1 2 2 1 2 2 1 ...
##  $ Educ          : int  12 16 12 14 14 16 13 13 13 17 ...
##  $ Science       : int  6 23 14 18 17 16 13 19 22 21 ...
##  $ Arith         : int  8 30 14 13 21 30 17 29 30 17 ...
##  $ Word          : int  15 35 27 35 28 29 30 33 35 28 ...
##  $ Parag         : int  6 15 8 12 10 13 12 13 14 14 ...
##  $ Numer         : int  29 45 32 24 40 36 49 35 48 39 ...
##  $ Coding        : int  52 68 35 48 46 30 58 58 61 54 ...
##  $ Auto          : int  9 21 13 11 13 21 11 18 21 18 ...
##  $ Math          : int  6 23 11 4 13 24 17 21 23 20 ...
##  $ Mechanic      : int  10 21 9 12 13 19 11 19 16 20 ...
##  $ Elec          : int  5 19 11 12 15 16 10 16 17 13 ...
##  $ AFQT          : num  6.84 99.39 47.41 44.02 59.68 ...
##  $ Income2005    : int  5500 65000 19000 36000 65000 8000 71000 43000 120000 64000 ...
##  $ Esteem1       : num  1 0 0 1 1 1 0 0 0 1 ...
##  $ Esteem2       : int  1 1 1 1 1 1 2 2 2 1 ...
##  $ Esteem3       : int  4 4 3 3 4 4 3 3 3 3 ...
##  $ Esteem4       : int  1 2 2 2 1 1 2 2 2 1 ...
##  $ Esteem5       : int  3 4 3 3 1 4 3 3 3 3 ...
##  $ Esteem6       : int  3 2 2 2 1 1 2 2 2 2 ...
##  $ Esteem7       : int  1 2 2 3 1 1 3 2 2 1 ...
##  $ Esteem8       : int  3 4 2 3 4 4 3 3 3 3 ...
##  $ Esteem9       : int  3 3 3 3 4 4 3 3 3 3 ...
##  $ Esteem10      : int  3 4 3 3 4 4 3 3 3 3 ...
##  $ LogIncome2005 : num  8.61 11.08 9.85 10.49 11.08 ...
```

```r
head(ex1223)
```

```
##   Subject Imagazine Inewspaper Ilibrary MotherEd FatherEd FamilyIncome78
## 1       2         1          1        1        5        8          20000
## 2       6         0          1        1       12       12          35000
## 3       7         1          1        1       12       12           8502
## 4       8         1          1        1        9        6           7227
## 5       9         1          1        1       12       10          17000
## 6      13         1          1        1       12       16          20000
##   Race Gender Educ Science Arith Word Parag Numer Coding Auto Math
## 1    3 female   12       6     8   15     6    29     52    9    6
## 2    3   male   16      23    30   35    15    45     68   21   23
## 3    3   male   12      14    14   27     8    32     35   13   11
## 4    3 female   14      18    13   35    12    24     48   11    4
## 5    3   male   14      17    21   28    10    40     46   13   13
## 6    3   male   16      16    30   29    13    36     30   21   24
##   Mechanic Elec   AFQT Income2005 Esteem1 Esteem2 Esteem3 Esteem4 Esteem5
## 1       10    5  6.841       5500       1       1       4       1       3
## 2       21   19 99.393      65000       0       1       4       2       4
## 3        9   11 47.412      19000       0       1       3       2       3
## 4       12   12 44.022      36000       1       1       3       2       3
## 5       13   15 59.683      65000       1       1       4       1       1
## 6       19   16 72.313       8000       1       1       4       1       4
##   Esteem6 Esteem7 Esteem8 Esteem9 Esteem10 LogIncome2005
## 1       3       1       3       3        3      8.612503
## 2       2       2       4       3        4     11.082143
## 3       2       2       2       3        3      9.852194
## 4       2       3       3       3        3     10.491274
## 5       1       1       4       4        4     11.082143
## 6       1       1       4       4        4      8.987197
```

#Step 3

**Obtain logistic regression model**


```r
esteem1.glm <- glm(Esteem1~LogIncome2005+AFQT+Educ+Gender,data=ex1223,family="binomial")
summary(esteem1.glm)
```

```
## 
## Call:
## glm(formula = Esteem1 ~ LogIncome2005 + AFQT + Educ + Gender, 
##     family = "binomial", data = ex1223)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7891  -1.2240   0.8174   1.0410   1.6339  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -2.773457   0.485833  -5.709 1.14e-08 ***
## LogIncome2005  0.165970   0.047114   3.523 0.000427 ***
## AFQT           0.007603   0.001842   4.128 3.65e-05 ***
## Educ           0.076135   0.021045   3.618 0.000297 ***
## Gendermale    -0.146214   0.087097  -1.679 0.093201 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 3508.3  on 2583  degrees of freedom
## Residual deviance: 3395.5  on 2579  degrees of freedom
## AIC: 3405.5
## 
## Number of Fisher Scoring iterations: 4
```

The data provide convincing evidence that a person income, AFQT score, and education were associated with the odds of responding in the highest self esteem category since 2 sided p-value is less than 0.0003. There was suggestive but inconclusive evidence that the odds of responding in the highest self esteem category differed for males and females and 2 sided p-value is 0.09.

Therefore, our model is:

###logit(y) = -2.773 + 0.166(LogIncome2005) + 0.007(60AFQT) + 0.0761(Educ)

#Step 4

**Obtain Confidence Interval**


```r
confint(esteem1.glm)
```

```
## Waiting for profiling to be done...
```

```
##                      2.5 %      97.5 %
## (Intercept)   -3.730815455 -1.82512175
## LogIncome2005  0.073787914  0.25869833
## AFQT           0.003998658  0.01121973
## Educ           0.035034318  0.11756275
## Gendermale    -0.317172867  0.02431818
```

```r
confint.default(esteem1.glm)#based on Std. Error 
```

```
##                      2.5 %      97.5 %
## (Intercept)   -3.725672477 -1.82124116
## LogIncome2005  0.073627671  0.25831148
## AFQT           0.003993497  0.01121233
## Educ           0.034887677  0.11738307
## Gendermale    -0.316921821  0.02449307
```

```r
exp(coef(esteem1.glm))
```

```
##   (Intercept) LogIncome2005          AFQT          Educ    Gendermale 
##    0.06244577    1.18053718    1.00763189    1.07910865    0.86397247
```

```r
exp(cbind(OR = coef(esteem1.glm), confint(esteem1.glm))) #odds ratios
```

```
## Waiting for profiling to be done...
```

```
##                       OR      2.5 %   97.5 %
## (Intercept)   0.06244577 0.02397328 0.161198
## LogIncome2005 1.18053718 1.07657845 1.295243
## AFQT          1.00763189 1.00400666 1.011283
## Educ          1.07910865 1.03565525 1.124752
## Gendermale    0.86397247 0.72820486 1.024616
```

#Step 5

**Intepretation:**

Associated with each additional 1 unit of log income was an estimated 18% increase in the odds of highest self esteem response (95% CI: 7.4% to 25.8%).

Associated with each 1 percentile increase AFQT score was an estimated 0.76% increase in the odds of highest self esteem response (95% CI: 0.040% to 1.12%)

Associated with each additional year of education was an estimated 7.9% increase in the odds of highest self esteem response (95% CI: 3.5% to 11.7%)

#Step 6 

**Obtain training and test sets and a "confusion table" for your classifier** 

My classifier would be LogIncome2005, AFQT, and Educ and these are used on training, test and a "confusion table"


```r
#Get traing and test data set
set.seed(1234)
ind <- sample(2, nrow(ex1223), replace=TRUE, prob=c(0.67, 0.33))
ex1223.train <- ex1223[ind==1, c("LogIncome2005","AFQT","Educ")]
ex1223.test <- ex1223[ind==2, c("LogIncome2005","AFQT","Educ")]
dim(ex1223.test)
```

```
## [1] 819   3
```

```r
dim(ex1223.train)
```

```
## [1] 1765    3
```

```r
require(class)
```

```
## Loading required package: class
```

```r
require(gmodels)
```

```
## Loading required package: gmodels
```

```r
## Run it!
ex1223.trainLabels <- ex1223[ind==1,"Esteem1"]
ex1223.testLabels <- ex1223[ind==2,"Esteem1"]
ex1223_pred <- knn(train = ex1223.train, test = ex1223.test, cl=ex1223.trainLabels,k=2)

##
library(gmodels)
CrossTable(x = ex1223.testLabels, y = ex1223_pred, prop.chisq=FALSE)
```

```
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  819 
## 
##  
##                   | ex1223_pred 
## ex1223.testLabels |         0 |         1 | Row Total | 
## ------------------|-----------|-----------|-----------|
##                 0 |       139 |       196 |       335 | 
##                   |     0.415 |     0.585 |     0.409 | 
##                   |     0.415 |     0.405 |           | 
##                   |     0.170 |     0.239 |           | 
## ------------------|-----------|-----------|-----------|
##                 1 |       196 |       288 |       484 | 
##                   |     0.405 |     0.595 |     0.591 | 
##                   |     0.585 |     0.595 |           | 
##                   |     0.239 |     0.352 |           | 
## ------------------|-----------|-----------|-----------|
##      Column Total |       335 |       484 |       819 | 
##                   |     0.409 |     0.591 |           | 
## ------------------|-----------|-----------|-----------|
## 
## 
```

#Step 7
##Conclusion

Not all variables are needed to obtain the best classification.

According to our R results, LogIncome2005, AFQT, and Educ are best combinations (except Gender) to have the best classification.



