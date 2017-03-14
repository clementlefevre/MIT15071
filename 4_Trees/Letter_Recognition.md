# Letter recognition





```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
library(ROCR)
```

```
## Loading required package: gplots
```

```
## 
## Attaching package: 'gplots'
```

```
## The following object is masked from 'package:stats':
## 
##     lowess
```

```r
library(caTools)
library(rpart)
library(rpart.plot)
```

```r
df<-read.csv('letters_ABPR.csv')
str(df)
```

```
## 'data.frame':	3116 obs. of  17 variables:
##  $ letter   : Factor w/ 4 levels "A","B","P","R": 2 1 4 2 3 4 4 1 3 3 ...
##  $ xbox     : int  4 1 5 5 3 8 2 3 8 6 ...
##  $ ybox     : int  2 1 9 9 6 10 6 7 14 10 ...
##  $ width    : int  5 3 5 7 4 8 4 5 7 8 ...
##  $ height   : int  4 2 7 7 4 6 4 5 8 8 ...
##  $ onpix    : int  4 1 6 10 2 6 3 3 4 7 ...
##  $ xbar     : int  8 8 6 9 4 7 6 12 5 8 ...
##  $ ybar     : int  7 2 11 8 14 7 7 2 10 5 ...
##  $ x2bar    : int  6 2 7 4 8 3 5 3 6 7 ...
##  $ y2bar    : int  6 2 3 4 1 5 5 2 3 5 ...
##  $ xybar    : int  7 8 7 6 11 8 6 10 12 7 ...
##  $ x2ybar   : int  6 2 3 8 6 4 5 2 5 6 ...
##  $ xy2bar   : int  6 8 9 6 3 8 7 9 4 6 ...
##  $ xedge    : int  2 1 2 6 0 6 3 2 4 3 ...
##  $ xedgeycor: int  8 6 7 11 10 6 7 6 10 9 ...
##  $ yedge    : int  7 2 5 8 4 7 5 3 4 8 ...
##  $ yedgexcor: int  10 7 11 7 8 7 8 8 8 9 ...
```

Problem 1.1 - Predicting B or not B
2.0 points possible (graded)
Let's warm up by attempting to predict just whether a letter is B or not. To begin, load the file letters_ABPR.csv into R, and call it letters. Then, create a new variable isB in the dataframe, which takes the value "TRUE" if the observation corresponds to the letter B, and "FALSE" if it does not. You can do this by typing the following command into your R console:

letters$isB = as.factor(letters$letter == "B")

Now split the data set into a training and testing set, putting 50% of the data in the training set. Set the seed to 1000 before making the split. The first argument to sample.split should be the dependent variable "letters$isB". Remember that TRUE values from sample.split should go in the training set.

Before building models, let's consider a baseline method that always predicts the most frequent outcome, which is "not B". What is the accuracy of this baseline method on the test set?


```r
df<- df %>% mutate(isB = ifelse(letter=='B',1,0))
head(df)
```

```
##   letter xbox ybox width height onpix xbar ybar x2bar y2bar xybar x2ybar
## 1      B    4    2     5      4     4    8    7     6     6     7      6
## 2      A    1    1     3      2     1    8    2     2     2     8      2
## 3      R    5    9     5      7     6    6   11     7     3     7      3
## 4      B    5    9     7      7    10    9    8     4     4     6      8
## 5      P    3    6     4      4     2    4   14     8     1    11      6
## 6      R    8   10     8      6     6    7    7     3     5     8      4
##   xy2bar xedge xedgeycor yedge yedgexcor isB
## 1      6     2         8     7        10   1
## 2      8     1         6     2         7   0
## 3      9     2         7     5        11   0
## 4      6     6        11     8         7   1
## 5      3     0        10     4         8   0
## 6      8     6         6     7         7   0
```

```r
set.seed(1000)

splt = sample.split(df$isB,SplitRatio = 0.5)
df_train <- df[splt,]
df_test <- df[!splt,]
df_test %>% group_by(isB) %>% summarise(total=n())
```

```
## # A tibble: 2 × 2
##     isB total
##   <dbl> <int>
## 1     0  1175
## 2     1   383
```

```r
1175/nrow(df_test)
```

```
## [1] 0.754172
```
Problem 1.2 - Predicting B or not B
2.0 points possible (graded)
Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model. Remember to remove the variable "letter" out of the model, as this is related to what we are trying to predict! To just remove one variable, you can either write out the other variables, or remember what we did in the Billboards problem in Week 3, and use the following notation:

CARTb = rpart(isB ~ . - letter, data=train, method="class")

We are just using the default parameters in our CART model, so we don't need to add the minbucket or cp arguments at all. We also added the argument method="class" since this is a classification problem.

What is the accuracy of the CART model on the test set? (Use type="class" when making predictions on the test set.)


```r
CARTb = rpart(isB ~ . - letter, data=df_train, method="class")
prediction_test_1<-predict(CARTb,newdata=df_test,type='class')

CM<-table(df_test$isB,prediction_test_1)
sum(diag(CM))/sum(CM)
```

```
## [1] 0.9358151
```



Problem 1.3 - Predicting B or Not B
2.0 points possible (graded)
Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set. You should use all of the other variables as independent variables, except letter (since it helped us define what we are trying to predict!). Use the default settings for ntree and nodesize (don't include these arguments at all). Right before building the model, set the seed to 1000. (NOTE: You might get a slightly different answer on this problem, even if you set the random seed. This has to do with your operating system and the implementation of the random forest algorithm.)

What is the accuracy of the model on the test set?

```r
set.seed(1000)
df_train$isB<-as.factor(df_train$isB)
df_test$isB<-as.factor(df_test$isB)

RDF<- randomForest::randomForest(isB ~ . - letter, data=df_train)
prediction_RDF<-predict(RDF,newdata = df_test)

CM<-table(df_test$isB,prediction_RDF)
print('Confusion Matrix')
```

```
## [1] "Confusion Matrix"
```

```r
CM
```

```
##    prediction_RDF
##        0    1
##   0 1165   10
##   1    9  374
```

```r
print('Accuracy')
```

```
## [1] "Accuracy"
```

```r
sum(diag(CM))/sum(CM)
```

```
## [1] 0.9878049
```

Problem 2.1 - Predicting the letters A, B, P, R
2.0 points possible (graded)
Let us now move on to the problem that we were originally interested in, which is to predict whether or not a letter is one of the four letters A, B, P or R.

As we saw in the D2Hawkeye lecture, building a multiclass classification CART model in R is no harder than building the models for binary classification problems. Fortunately, building a random forest model is just as easy.

The variable in our data frame which we will be trying to predict is "letter". Start by converting letter in the original data set (letters) to a factor by running the following command in R:

letters$letter = as.factor( letters$letter )

Now, generate new training and testing sets of the letters data frame using letters$letter as the first input to the sample.split function. Before splitting, set your seed to 2000. Again put 50% of the data in the training set. (Why do we need to split the data again? Remember that sample.split balances the outcome variable in the training and testing sets. With a new outcome variable, we want to re-generate our split.)

In a multiclass classification problem, a simple baseline model is to predict the most frequent class of all of the options.

What is the baseline accuracy on the testing set?


```r
set.seed(2000)
df$letter<-as.factor(df$letter)
splt = sample.split(df$letter,SplitRatio = 0.5)
df_train <- df[splt,]
df_test <- df[!splt,]

table(df$letter)
```

```
## 
##   A   B   P   R 
## 789 766 803 758
```

```r
print('  ')
```

```
## [1] "  "
```

```r
print('max Letter')
```

```
## [1] "max Letter"
```

```r
which.max(table(df$letter))
```

```
## P 
## 3
```

```r
print('')
```

```
## [1] ""
```

```r
max(table(df$letter))/sum(table(df$letter))
```

```
## [1] 0.2577022
```
Problem 2.2 - Predicting the letters A, B, P, R
2.0 points possible (graded)
Now build a classification tree to predict "letter", using the training set to build your model. You should use all of the other variables as independent variables, except "isB", since it is related to what we are trying to predict! Just use the default parameters in your CART model. Add the argument method="class" since this is a classification problem. Even though we have multiple classes here, nothing changes in how we build the model from the binary case.

What is the test set accuracy of your CART model? Use the argument type="class" when making predictions.

(HINT: When you are computing the test set accuracy using the confusion matrix, you want to add everything on the main diagonal and divide by the total number of observations in the test set, which can be computed with nrow(test), where test is the name of your test set).


```r
CART_letter<- rpart(letter~.-isB,data=df_train,method='class')
prediction_CART_Letter<-predict(CART_letter,newdata = df_test,type = 'class')
CM<- table(df_test$letter,prediction_CART_Letter)
CM
```

```
##    prediction_CART_Letter
##       A   B   P   R
##   A 348   4   0  43
##   B   8 318  12  45
##   P   2  21 363  15
##   R  10  24   5 340
```

```r
print('\n accuracy:')
```

```
## [1] "\n accuracy:"
```

```r
sum(diag(CM))/sum(CM)
```

```
## [1] 0.8786906
```

Problem 2.3 - Predicting the letters A, B, P, R
2.0 points possible (graded)
Now build a random forest model on the training data, using the same independent variables as in the previous problem -- again, don't forget to remove the isB variable. Just use the default parameter values for ntree and nodesize (you don't need to include these arguments at all). Set the seed to 1000 right before building your model. (Remember that you might get a slightly different result even if you set the random seed.)

What is the test set accuracy of your random forest model?

```r
RDF_letter <- randomForest::randomForest(letter~.-isB,data=df_train)
prediction_RDF_letter <- predict(RDF_letter,newdata=df_test,type='class')

CM<- table(df_test$letter,prediction_RDF_letter)
CM
```

```
##    prediction_RDF_letter
##       A   B   P   R
##   A 390   0   3   2
##   B   0 380   1   2
##   P   0   6 393   2
##   R   0  10   0 369
```

```r
print('\n accuracy:')
```

```
## [1] "\n accuracy:"
```

```r
sum(diag(CM))/sum(CM)
```

```
## [1] 0.9833119
```
