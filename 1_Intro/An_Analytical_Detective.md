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
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
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
df$Date = DateYear
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
groupy %>% arrange(desc(ratio)) %>% filter(`TRUE` == max(`TRUE`)) 
```

```
## # A tibble: 1 × 4
##    Month `FALSE` `TRUE`    ratio
##    <chr>   <dbl>  <dbl>    <dbl>
## 1 Januar   14612   1435 8.942481
```
**Problem 3.1 - Visualizing Crime Trends**
3 points possible (graded)
Now, let's make some plots to help us better understand how crime has changed over time in Chicago. Throughout this problem, and in general, you can save your plot to a file. For more information, this website very clearly explains the process.

First, let's make a histogram of the variable Date. We'll add an extra argument, to specify the number of bars we want in our histogram. In your R console, type

hist(mvt$Date, breaks=100)

Looking at the histogram, answer the following questions.

In general, does it look like crime increases or decreases from 2002 - 2012?


Increases
Decreases
unanswered
In general, does it look like crime increases or decreases from 2005 - 2008?


Increases
Decreases
unanswered
In general, does it look like crime increases or decreases from 2009 - 2011?


Increases
Decreases

```r
p<-ggplot(df, aes(x=Date,fill=Arrest)) + geom_histogram(binwidth=50) 
p+geom_vline(xintercept = c(as.numeric(ymd(20080101)),
                            as.numeric(ymd(20090101))),linetype="dotted")
```

![](An_Analytical_Detective_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
Problem 3.2 - Visualizing Crime Trends
1 point possible (graded)
Now, let's see how arrests have changed over time. Create a boxplot of the variable "Date", sorted by the variable "Arrest" (if you are not familiar with boxplots and would like to learn more, check out this tutorial). In a boxplot, the bold horizontal line is the median value of the data, the box shows the range of values between the first quartile and third quartile, and the whiskers (the dotted lines extending outside the box) show the minimum and maximum values, excluding any outliers (which are plotted as circles). Outliers are defined by first computing the difference between the first and third quartile values, or the height of the box. This number is called the Inter-Quartile Range (IQR). Any point that is greater than the third quartile plus the IQR or less than the first quartile minus the IQR is considered an outlier.

Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period? (Note that the time period is from 2001 to 2012, so the middle of the time period is the beginning of 2007.)

```r
ggplot(df, aes(x=Arrest, y=Date, fill=Arrest)) + geom_boxplot()
```

![](An_Analytical_Detective_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Problem 3.3 - Visualizing Crime Trends
2.0 points possible (graded)
Let's investigate this further. Use the table function for the next few questions.

For what proportion of motor vehicle thefts in 2001 was an arrest made?

Note: in this question and many others in the course, we are asking for an answer as a proportion. Therefore, your answer should take a value between 0 and 1.


```r
groupyto<-df %>% group_by(Year,Arrest)%>%summarise(n()) %>%spread(Arrest,`n()`)%>% rowwise() %>% mutate(ratio = `TRUE`/(`FALSE`+`TRUE`)*100) 
groupyto%>% filter(Year == 2001)
```

```
## Source: local data frame [1 x 4]
## Groups: <by row>
## 
## # A tibble: 1 × 4
##    Year `FALSE` `TRUE`    ratio
##   <int>   <int>  <int>    <dbl>
## 1  2001   18517   2152 10.41173
```
**Problem 3.4 - Visualizing Crime Trends**
1 point possible (graded)
For what proportion of motor vehicle thefts in 2007 was an arrest made?

```r
groupyto %>% filter(Year == 2007)
```

```
## Source: local data frame [1 x 4]
## Groups: <by row>
## 
## # A tibble: 1 × 4
##    Year `FALSE` `TRUE`    ratio
##   <int>   <int>  <int>    <dbl>
## 1  2007   13068   1212 8.487395
```
**Problem 3.5 - Visualizing Crime Trends**
1 point possible (graded)
For what proportion of motor vehicle thefts in 2012 was an arrest made?

```r
groupyto %>% filter(Year == 2012)
```

```
## Source: local data frame [1 x 4]
## Groups: <by row>
## 
## # A tibble: 1 × 4
##    Year `FALSE` `TRUE`    ratio
##   <int>   <int>  <int>    <dbl>
## 1  2012   13542    550 3.902924
```
**Problem 4.1 - Popular Locations**
1 point possible (graded)
Analyzing this data could be useful to the Chicago Police Department when deciding where to allocate resources. If they want to increase the number of arrests that are made for motor vehicle thefts, where should they focus their efforts?

We want to find the top five locations where motor vehicle thefts occur. If you create a table of the LocationDescription variable, it is unfortunately very hard to read since there are 78 different locations in the data set. By using the sort function, we can view this same table, but sorted by the number of observations in each category. In your R console, type:

sort(table(mvt$LocationDescription))

Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? You should select 5 of the following options.



```r
groupyloc<- df %>% group_by(LocationDescription) %>% summarise(n()) %>% arrange(desc(`n()`)) %>%  filter(LocationDescription!='OTHER') %>%filter(row_number()<6)
groupyloc
```

```
## # A tibble: 5 × 2
##              LocationDescription  `n()`
##                           <fctr>  <int>
## 1                         STREET 156564
## 2 PARKING LOT/GARAGE(NON.RESID.)  14852
## 3                          ALLEY   2308
## 4                    GAS STATION   2111
## 5         DRIVEWAY - RESIDENTIAL   1675
```

**Problem 4.2 - Popular Locations**
1 point possible (graded)
Create a subset of your data, only taking observations for which the theft happened in one of these five locations, and call this new data set "Top5". To do this, you can use the | symbol. In lecture, we used the & symbol to use two criteria to make a subset of the data. To only take observations that have a certain value in one variable or the other, the | character can be used in place of the & symbol. This is also called a logical "or" operation.

Alternately, you could create five different subsets, and then merge them together into one data frame using rbind.

How many observations are in Top5?


```r
groupyloc <- rename(groupyloc, 'Sumo'=`n()`)

colnames(groupyloc)
```

```
## [1] "LocationDescription" "Sumo"
```

```r
View(summarise(groupyloc,sum(Sumo)))
```

**Problem 4.3 - Popular Locations**
2.0 points possible (graded)
R will remember the other categories of the LocationDescription variable from the original dataset, so running table(Top5$LocationDescription) will have a lot of unnecessary output. To make our tables a bit nicer to read, we can refresh this factor variable. In your R console, type:

Top5$LocationDescription = factor(Top5$LocationDescription)

If you run the str or table function on Top5 now, you should see that LocationDescription now only has 5 values, as we expect.

Use the Top5 data frame to answer the remaining questions.

One of the locations has a much higher arrest rate than the other locations. Which is it? Please enter the text in exactly the same way as how it looks in the answer options for Problem 4.1.


```r
top5Locations <-df %>% filter(LocationDescription %in% as.vector(unique(groupyloc$LocationDescription)))

groupytop5loc<- top5Locations %>%  group_by(LocationDescription) %>% summarise(Ratio = mean(Arrest)*100,max(Arrest)) %>% print
```

```
## # A tibble: 5 × 3
##              LocationDescription     Ratio `max(Arrest)`
##                           <fctr>     <dbl>         <int>
## 1                          ALLEY 10.788562             1
## 2         DRIVEWAY - RESIDENTIAL  7.880597             1
## 3                    GAS STATION 20.795831             1
## 4 PARKING LOT/GARAGE(NON.RESID.) 10.793159             1
## 5                         STREET  7.405917             1
```
Problem 4.4 - Popular Locations
1 point possible (graded)
On which day of the week do the most motor vehicle thefts at gas stations happen?

```r
top5Locations %>% filter(LocationDescription=='GAS STATION') %>% group_by(Weekday) %>% summarise(counto=n()) %>% arrange(desc(counto)) %>% filter(row_number()==1)
```

```
## # A tibble: 1 × 2
##   Weekday counto
##     <chr>  <int>
## 1 Samstag    338
```
Problem 4.5 - Popular Locations
1 point possible (graded)
On which day of the week do the fewest motor vehicle thefts in residential driveways happen?

```r
top5Locations %>% filter(LocationDescription=='DRIVEWAY - RESIDENTIAL') %>% group_by(Weekday) %>% summarise(counto=n()) %>% arrange(desc(counto)) %>% filter(row_number()==n())
```

```
## # A tibble: 1 × 2
##   Weekday counto
##     <chr>  <int>
## 1 Samstag    202
```
