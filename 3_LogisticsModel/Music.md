# Popularity of Music Records


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
df<- read.csv('songs.csv')

table(df$year)
```

```
## 
## 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 
##  328  196  186  324  198  258  178  329  380  357  363  282  518  434  479 
## 2005 2006 2007 2008 2009 2010 
##  392  479  622  415  483  373
```
Problem 1.2 - Understanding the Data
1 point possible (graded)
How many songs does the dataset include for which the artist name is "Michael Jackson"?

```r
nrow(df[df$artistname== "Michael Jackson",])
```

```
## [1] 18
```
Problem 1.3 - Understanding the Data
1 point possible (graded)
Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.

```r
df %>% filter(artistname== "Michael Jackson" & Top10==1)
```

```
##   year         songtitle      artistname             songID
## 1 2001 You Rock My World Michael Jackson SOBLCOF13134393021
## 2 1995 You Are Not Alone Michael Jackson SOJKNNO13737CEB162
## 3 1995    Black or White Michael Jackson SOBBRFO137756C9CB7
## 4 1995 Remember the Time Michael Jackson SOIQZMT136C9704DA5
## 5 1992     In The Closet Michael Jackson SOKIOOC12AF729ED9E
##             artistID timesignature timesignature_confidence loudness
## 1 ARXPPEY1187FB51DF4             4                    1.000   -2.768
## 2 ARXPPEY1187FB51DF4             4                    1.000   -9.408
## 3 ARXPPEY1187FB51DF4             4                    1.000   -4.017
## 4 ARXPPEY1187FB51DF4             4                    1.000   -3.633
## 5 ARXPPEY1187FB51DF4             4                    0.991   -4.315
##     tempo tempo_confidence key key_confidence    energy pitch timbre_0_min
## 1  95.003            0.892  11          0.134 0.7262479 0.004        0.500
## 2 120.566            0.805  11          0.661 0.4375286 0.002        0.000
## 3 115.027            0.535   9          0.494 0.9037198 0.015        0.007
## 4 107.921            1.000   5          0.620 0.9038237 0.003        0.000
## 5 110.501            0.949  11          0.720 0.7602297 0.008        0.000
##   timbre_0_max timbre_1_min timbre_1_max timbre_2_min timbre_2_max
## 1       59.662      -44.825      304.609     -246.769      115.353
## 2       58.673      -98.062      171.130     -194.199      103.390
## 3       57.756     -170.413      223.079     -159.855      221.169
## 4       56.621      -95.012      198.624     -180.254      135.532
## 5       58.705     -106.832      349.619     -162.832      145.596
##   timbre_3_min timbre_3_max timbre_4_min timbre_4_max timbre_5_min
## 1     -196.928      493.186      -48.145      130.931     -146.243
## 2     -204.004      163.420      -30.201      128.268     -108.666
## 3     -130.112      313.571      -46.880      136.343     -127.658
## 4     -229.012      181.773      -50.449      118.126     -112.359
## 5     -306.947      302.134      -52.034      124.492     -129.339
##   timbre_5_max timbre_6_min timbre_6_max timbre_7_min timbre_7_max
## 1      180.716     -106.361       65.637     -109.166      120.076
## 2       89.808      -79.784       67.505     -131.455       90.735
## 3      148.704     -105.303       85.290      -97.571      107.974
## 4      187.657     -103.364       72.257     -133.092      146.587
## 5      237.563     -109.739       71.140     -106.173      124.354
##   timbre_8_min timbre_8_max timbre_9_min timbre_9_max timbre_10_min
## 1      -53.839       63.576      -85.169       84.840      -102.185
## 2      -61.583       60.920      -55.904       76.632       -69.799
## 3      -55.063       52.505     -110.999       71.477      -133.939
## 4      -58.117       62.157      -54.440       94.501      -112.348
## 5      -78.303       41.322      -83.184      106.263      -136.109
##   timbre_10_max timbre_11_min timbre_11_max Top10
## 1        55.266       -48.107        56.116     1
## 2        46.173       -67.281        47.128     1
## 3        60.442       -55.008        43.473     1
## 4        90.437       -53.634        51.681     1
## 5       102.829       -48.192        74.575     1
```
Problem 1.4 - Understanding the Data
2 points possible (graded)
The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ). What are the values of this variable that occur in our dataset? Select all that apply.

```r
  table(df$timesignature)
```

```
## 
##    0    1    3    4    5    7 
##   10  143  503 6787  112   19
```
Problem 1.5 - Understanding the Data
1 point possible (graded)
Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?

```r
df[which.max(df$tempo),]
```

```
##      year                   songtitle      artistname             songID
## 6206 1995 Wanna Be Startin' Somethin' Michael Jackson SONHIQM13738B7BE80
##                artistID timesignature timesignature_confidence loudness
## 6206 ARXPPEY1187FB51DF4             3                        1  -14.528
##        tempo tempo_confidence key key_confidence    energy pitch
## 6206 244.307            0.566   6           0.44 0.6379941 0.009
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 6206        11.84       44.378        -80.4      187.038      -106.47
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 6206      220.751      -79.722      199.699      -77.158      147.564
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 6206      -68.229      186.391     -110.029       63.148      -58.978
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 6206         93.6      -52.012       95.827      -63.554       84.129
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 6206       -53.492        67.001       -73.421        67.308     0
```
Problem 2.1 - Creating Our Prediction Model
1 point possible (graded)
We wish to predict whether or not a song will make it to the Top 10. To do this, first use the subset function to split the data into a training set "SongsTrain" consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases.

How many observations (songs) are in the training set?

```r
df_train<- df[df$year<2010,]
df_test<- df[df$year==2010,]
dim(df_train)
```

```
## [1] 7201   39
```

Problem 2.2 - Creating our Prediction Model
2.0 points possible (graded)
In this problem, our outcome variable is "Top10" - we are trying to predict whether or not a song will make it to the Top 10 of the Billboard Hot 100 Chart. Since the outcome variable is binary, we will build a logistic regression model. We'll start by using all song attributes as our independent variables, which we'll call Model 1.

We will only use the variables in our dataset that describe the numerical attributes of the song in our logistic regression model. So we won't use the variables "year", "songtitle", "artistname", "songID" or "artistID".

We have seen in the lecture that, to build the logistic regression model, we would normally explicitly input the formula including all the independent variables in R. However, in this case, this is a tedious amount of work since we have a large number of independent variables.

There is a nice trick to avoid doing so. Let's suppose that, except for the outcome variable Top10, all other variables in the training set are inputs to Model 1. Then, we can use the formula

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

to build our model. Notice that the "." is used in place of enumerating all the independent variables. (Also, keep in mind that you can choose to put quotes around binomial, or leave out the quotes. R can understand this argument either way.)

However, in our case, we want to exclude some of the variables in our dataset from being used as independent variables ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. First define a vector of variable names called nonvars - these are the variables that we won't use in our model.

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

To remove these variables from your training and testing sets, type the following commands in your R console:

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

Now, use the glm function to build a logistic regression model to predict Top10 using all of the other variables as the independent variables. You should use SongsTrain to build the model.

Looking at the summary of your model, what is the value of the Akaike Information Criterion (AIC)?


```r
head(df_train)
```

```
##     year              songtitle artistname             songID
## 374 2009    The Awkward Goodbye    Athlete SOUALGK12AB017FC37
## 375 2009           Rubik's Cube    Athlete SOGPIQC12AB0182B15
## 376 2009       Superhuman Touch    Athlete SOBNYZN13774E81F76
## 377 2009            The Getaway    Athlete SOHFEOA1366EE931DD
## 378 2009        Black Swan Song    Athlete SOXXSMX12AB017F7B3
## 379 2009 Don't Hold Your Breath    Athlete SOOEDWA12AB017FC13
##               artistID timesignature timesignature_confidence loudness
## 374 ARDW3YJ1187FB4CCE5             3                    0.732   -6.320
## 375 ARDW3YJ1187FB4CCE5             3                    0.906   -9.541
## 376 ARDW3YJ1187FB4CCE5             4                    0.987   -4.842
## 377 ARDW3YJ1187FB4CCE5             4                    0.822   -5.272
## 378 ARDW3YJ1187FB4CCE5             4                    0.983   -6.233
## 379 ARDW3YJ1187FB4CCE5             4                    1.000   -6.793
##       tempo tempo_confidence key key_confidence    energy pitch
## 374  89.614            0.652   1          0.773 0.5985294 0.004
## 375 117.742            0.542   0          0.722 0.3633990 0.006
## 376 119.018            0.838   6          0.106 0.7601515 0.003
## 377  71.479            0.613   4          0.781 0.7550336 0.014
## 378  77.492            0.740   8          0.552 0.5236583 0.008
## 379  81.859            0.821   9          0.218 0.6546091 0.012
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 374        0.000       57.831      -62.306      285.818      -81.802
## 375        0.739       57.059     -220.205      241.091      -96.833
## 376        0.000       57.815     -189.660      187.282     -139.053
## 377        0.000       58.330     -113.885      171.130      -71.640
## 378        0.000       57.643     -160.579      216.778      -79.456
## 379        0.000       57.389     -103.691      227.209     -155.016
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 374      211.084     -217.025      203.151      -55.874       97.646
## 375      214.510     -201.889      124.200      -52.389      131.859
## 376      134.508     -116.316       94.698      -55.617       79.292
## 377      194.788     -276.297      146.268      -59.374      121.707
## 378      114.093     -183.559      108.719      -31.922      169.734
## 379      174.758     -386.464      185.756      -69.700      103.106
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 374      -62.492       82.169      -82.129       59.197     -109.384
## 375      -73.875       73.628      -63.496       70.133      -90.092
## 376      -73.474       41.025      -41.489       62.759      -69.311
## 377      -71.135       39.607      -77.786       94.525      -69.088
## 378      -73.004      233.930      -76.026       58.016      -78.803
## 379      -75.267      184.309      -62.755       45.283      -61.878
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 374       70.975      -71.776       58.432      -53.816       88.571
## 375      112.879      -64.470       58.086      -76.937       74.441
## 376       90.400      -52.459       40.679      -50.408       58.811
## 377       93.373      -55.811       78.963      -51.504       70.455
## 378      100.766      -61.392       50.309      -62.994       96.837
## 379       89.443      -70.718       50.515      -54.980       80.278
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 374       -89.816        38.026       -52.075        52.827     0
## 375       -88.244        42.209       -66.812        40.749     0
## 376       -78.239        35.264       -54.200        46.490     0
## 377       -74.928        30.839       -51.377        27.768     0
## 378       -90.397        60.549       -52.122        48.059     0
## 379       -70.317        54.084       -43.651        43.261     0
```

```r
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
df_train<-df_train %>% select(-one_of(nonvars))
str(df_train)
```

```
## 'data.frame':	7201 obs. of  34 variables:
##  $ timesignature           : int  3 3 4 4 4 4 4 4 4 4 ...
##  $ timesignature_confidence: num  0.732 0.906 0.987 0.822 0.983 1 0.821 0.997 0.816 1 ...
##  $ loudness                : num  -6.32 -9.54 -4.84 -5.27 -6.23 ...
##  $ tempo                   : num  89.6 117.7 119 71.5 77.5 ...
##  $ tempo_confidence        : num  0.652 0.542 0.838 0.613 0.74 0.821 0.912 0.609 0.786 0.27 ...
##  $ key                     : int  1 0 6 4 8 9 6 9 0 9 ...
##  $ key_confidence          : num  0.773 0.722 0.106 0.781 0.552 0.218 0.275 0.333 0.634 0.578 ...
##  $ energy                  : num  0.599 0.363 0.76 0.755 0.524 ...
##  $ pitch                   : num  0.004 0.006 0.003 0.014 0.008 0.012 0.002 0.003 0.001 0.006 ...
##  $ timbre_0_min            : num  0 0.739 0 0 0 ...
##  $ timbre_0_max            : num  57.8 57.1 57.8 58.3 57.6 ...
##  $ timbre_1_min            : num  -62.3 -220.2 -189.7 -113.9 -160.6 ...
##  $ timbre_1_max            : num  286 241 187 171 217 ...
##  $ timbre_2_min            : num  -81.8 -96.8 -139.1 -71.6 -79.5 ...
##  $ timbre_2_max            : num  211 215 135 195 114 ...
##  $ timbre_3_min            : num  -217 -202 -116 -276 -184 ...
##  $ timbre_3_max            : num  203.2 124.2 94.7 146.3 108.7 ...
##  $ timbre_4_min            : num  -55.9 -52.4 -55.6 -59.4 -31.9 ...
##  $ timbre_4_max            : num  97.6 131.9 79.3 121.7 169.7 ...
##  $ timbre_5_min            : num  -62.5 -73.9 -73.5 -71.1 -73 ...
##  $ timbre_5_max            : num  82.2 73.6 41 39.6 233.9 ...
##  $ timbre_6_min            : num  -82.1 -63.5 -41.5 -77.8 -76 ...
##  $ timbre_6_max            : num  59.2 70.1 62.8 94.5 58 ...
##  $ timbre_7_min            : num  -109.4 -90.1 -69.3 -69.1 -78.8 ...
##  $ timbre_7_max            : num  71 112.9 90.4 93.4 100.8 ...
##  $ timbre_8_min            : num  -71.8 -64.5 -52.5 -55.8 -61.4 ...
##  $ timbre_8_max            : num  58.4 58.1 40.7 79 50.3 ...
##  $ timbre_9_min            : num  -53.8 -76.9 -50.4 -51.5 -63 ...
##  $ timbre_9_max            : num  88.6 74.4 58.8 70.5 96.8 ...
##  $ timbre_10_min           : num  -89.8 -88.2 -78.2 -74.9 -90.4 ...
##  $ timbre_10_max           : num  38 42.2 35.3 30.8 60.5 ...
##  $ timbre_11_min           : num  -52.1 -66.8 -54.2 -51.4 -52.1 ...
##  $ timbre_11_max           : num  52.8 40.7 46.5 27.8 48.1 ...
##  $ Top10                   : int  0 0 0 0 0 0 0 0 0 0 ...
```

