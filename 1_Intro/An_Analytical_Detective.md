# Assignment 1 _An Analytical Detective


Problem 1.1 - Loading the Data
1 point possible (graded)
Read the dataset mvtWeek1.csv into R, using the read.csv function, and call the data frame "mvt". Remember to navigate to the directory on your computer containing the file mvtWeek1.csv first. It may take a few minutes to read in the data, since it is pretty large. Then, use the str and summary functions to answer the following questions.

How many rows of data (observations) are in this dataset?




```r
require(dplyr)
```

```
## Loading required package: dplyr
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
require(tidyr)
```

```
## Loading required package: tidyr
```

```r
df <- read.csv('mvtWeek1.csv')
dim(df)
```

```
## [1] 191641     11
```
Problem 1.2 - Loading the Data
1 point possible (graded)
How many variables are in this dataset?

```r
str(df)
```

```
## 'data.frame':	191641 obs. of  11 variables:
##  $ ID                 : int  8951354 8951141 8952745 8952223 8951608 8950793 8950760 8951611 8951802 8950706 ...
##  $ Date               : Factor w/ 131680 levels "10/10/01 0:00",..: 37302 37300 37300 37300 37299 37297 37296 37295 37291 37290 ...
##  $ LocationDescription: Factor w/ 78 levels "ABANDONED BUILDING",..: 72 72 62 72 72 72 72 72 72 72 ...
##  $ Arrest             : logi  FALSE FALSE FALSE FALSE FALSE TRUE ...
##  $ Domestic           : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ Beat               : int  623 1213 1622 724 211 2521 423 231 1021 1215 ...
##  $ District           : int  6 12 16 7 2 25 4 2 10 12 ...
##  $ CommunityArea      : int  69 24 11 67 35 19 48 40 29 24 ...
##  $ Year               : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ Latitude           : num  41.8 41.9 42 41.8 41.8 ...
##  $ Longitude          : num  -87.6 -87.7 -87.8 -87.7 -87.6 ...
```

Problem 1.3 - Loading the Data
1 point possible (graded)
Using the "max" function, what is the maximum value of the variable "ID"?


```r
max(df$ID)
```

```
## [1] 9181151
```
Problem 1.4 - Loading the Data
1 point possible (graded)
What is the minimum value of the variable "Beat"?

```r
min(df$Beat
    )
```

```
## [1] 111
```
Problem 1.5 - Loading the Data
1 point possible (graded)
How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?

```r
table(df$Arrest)
```

```
## 
##  FALSE   TRUE 
## 176105  15536
```
Problem 1.6 - Loading the Data
1 point possible (graded)
How many observations have a LocationDescription value of ALLEY?

```r
sum(df$LocationDescription=='ALLEY')
```

```
## [1] 2308
```
Problem 2.1 - Understanding Dates in R
1 point possible (graded)
In many datasets, like this one, you have a date field. Unfortunately, R does not automatically recognize entries that look like dates. We need to use a function in R to extract the date and time. Take a look at the first entry of Date (remember to use square brackets when looking at a certain entry of a variable).

In what format are the entries in the variable Date?

```r
df$Date[nrow(df)/2]
```

```
## [1] 5/21/06 13:00
## 131680 Levels: 10/10/01 0:00 10/10/01 0:01 10/10/01 0:30 ... 9/9/12 9:50
```
Problem 2.2 - Understanding Dates in R
1 point possible (graded)
Now, let's convert these characters into a Date object in R. In your R console, type

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

This converts the variable "Date" into a Date object in R. Take a look at the variable DateConvert using the summary function.

What is the month and year of the median date in our dataset? Enter your answer as "Month Year", without the quotes. (Ex: if the answer was 2008-03-28, you would give the answer "March 2008", without the quotes.)

```r
DateYear = as.Date(strptime(df$Date,"%m/%d/%y %H:%M"))
summary(DateYear)
```

```
##         Min.      1st Qu.       Median         Mean      3rd Qu. 
## "2001-01-01" "2003-07-10" "2006-05-21" "2006-08-23" "2009-10-24" 
##         Max. 
## "2012-12-31"
```

```r
median(DateYear)
```

```
## [1] "2006-05-21"
```
Problem 2.3 - Understanding Dates in R
1 point possible (graded)
Now, let's extract the month and the day of the week, and add these variables to our data frame mvt. We can do this with two simple functions. Type the following commands in R:

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

This creates two new variables in our data frame, Month and Weekday, and sets them equal to the month and weekday values that we can extract from the Date object. Lastly, replace the old Date variable with DateConvert by typing:

mvt$Date = DateConvert

Using the table command, answer the following questions.

In which month did the fewest motor vehicle thefts occur?

```r
df$Month = months(DateYear)
df$Weekday = weekdays(DateYear)
sort(table(df$Month),decreasing = F)
```

```
## 
##   Februar     April      März      Juni       Mai    Januar September 
##     13511     15280     15758     16002     16035     16047     16060 
##  November  Dezember    August      Juli   Oktober 
##     16063     16426     16572     16801     17086
```
Problem 2.4 - Understanding Dates in R
1 point possible (graded)
On which weekday did the most motor vehicle thefts occur?

```r
sort(table(df$Weekday),decreasing = T)
```

```
## 
##    Freitag   Mittwoch     Montag Donnerstag    Samstag   Dienstag 
##      29284      27416      27397      27319      27118      26791 
##    Sonntag 
##      26316
```
Problem 2.5 - Understanding Dates in R
1 point possible (graded)
Each observation in the dataset represents a motor vehicle theft, and the Arrest variable indicates whether an arrest was later made for this theft. Which month has the largest number of motor vehicle thefts for which an arrest was made?

```r
table1<-table(df$Month,df$Arrest)
table1
```

```
##            
##             FALSE  TRUE
##   April     14028  1252
##   August    15243  1329
##   Dezember  15029  1397
##   Februar   12273  1238
##   Januar    14612  1435
##   Juli      15477  1324
##   Juni      14772  1230
##   Mai       14848  1187
##   März      14460  1298
##   November  14807  1256
##   Oktober   15744  1342
##   September 14812  1248
```

```r
which.max(table1[,2])
```

```
## Januar 
##      5
```


```r
df_p<-tbl_df(df)
groupy <- df_p %>% group_by(Month,Arrest) %>% summarise(n()) %>% spread( Arrest,`n()` ,fill=0)


groupy<-groupy%>%rowwise()%>%mutate(ratio = `TRUE`/sum(`FALSE`,`TRUE`)*100)
groupy %>% arrange(desc(`TRUE`)) %>% filter(row_number()==1 | row_number()==n())
```

```
## # A tibble: 2 × 4
##    Month `FALSE` `TRUE`    ratio
##    <chr>   <dbl>  <dbl>    <dbl>
## 1 Januar   14612   1435 8.942481
## 2    Mai   14848   1187 7.402557
```

```r
groupy %>% arrange(desc(ratio)) %>% select()
```

```
## # A tibble: 12 × 0
```

