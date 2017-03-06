# Demographics and Employment in the United States

Problem 1.1 - Loading and Summarizing the Dataset
1 point possible (graded)
Load the dataset from CPSData.csv into a data frame called CPS, and view the dataset with the summary() and str() commands.

How many interviewees are in the dataset?




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
library(ggplot2)
df_CPS <- tbl_df(read.csv('CPSData.csv'))
df_Metro <- read.csv('MetroAreaCodes.csv')
df_Country <- read.csv('CountryCodes.csv')

dim(df_CPS)
```

```
## [1] 131302     14
```
Problem 1.2 - Loading and Summarizing the Dataset
1 point possible (graded)
Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.



```r
groupy_industry <- df_CPS %>% group_by(Industry) %>% summarise(counto = n()) %>% na.omit() %>% arrange(desc(counto))
head(groupy_industry)
```

```
## # A tibble: 6 × 2
##                             Industry counto
##                               <fctr>  <int>
## 1    Educational and health services  15017
## 2                              Trade   8933
## 3 Professional and business services   7519
## 4                      Manufacturing   6791
## 5            Leisure and hospitality   6364
## 6                       Construction   4387
```
Problem 1.3 - Loading and Summarizing the Dataset
2 points possible (graded)
Recall from the homework assignment "The Analytical Detective" that you can call the sort() function on the output of the table() function to obtain a sorted breakdown of a variable. For instance, sort(table(CPS$Region)) sorts the regions by the number of interviewees from that region.

Which state has the fewest interviewees?


  unanswered  
Which state has the largest number of interviewees?


```r
groupy_state <- df_CPS %>% group_by(State) %>% summarise(n_interview=n()) %>% arrange(desc(n_interview))
tail(groupy_state,1)
```

```
## # A tibble: 1 × 2
##        State n_interview
##       <fctr>       <int>
## 1 New Mexico        1102
```

```r
head(groupy_state,1)
```

```
## # A tibble: 1 × 2
##        State n_interview
##       <fctr>       <int>
## 1 California       11570
```

Problem 1.4 - Loading and Summarizing the Dataset
1 point possible (graded)
What proportion of interviewees are citizens of the United States?

```r
df_CPS %>% group_by(Citizenship) %>% summarise(counto=n()) %>% mutate(ratio = counto /sum(counto)*100)
```

```
## # A tibble: 3 × 3
##            Citizenship counto     ratio
##                 <fctr>  <int>     <dbl>
## 1      Citizen, Native 116639 88.832615
## 2 Citizen, Naturalized   7073  5.386818
## 3          Non-Citizen   7590  5.780567
```
Problem 1.5 - Loading and Summarizing the Dataset
1 point possible (graded)
The CPS differentiates between race (with possible values American Indian, Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity. A number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable. For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? (Select all that apply.)

```r
groupy_race<-df_CPS %>% group_by(Race,Hispanic) %>% summarise(counto=n()) %>% spread(Hispanic,counto)
colnames(groupy_race) <- c('Race','no_spanic','spanic')
groupy_race %>% mutate(ratio = spanic /(no_spanic+spanic)*100) %>% arrange(desc(ratio)) %>% filter(spanic>250)
```

```
## Source: local data frame [4 x 4]
## Groups: Race [4]
## 
##              Race no_spanic spanic     ratio
##            <fctr>     <int>  <int>     <dbl>
## 1 American Indian      1129    304 21.214236
## 2           White     89190  16731 15.795735
## 3     Multiracial      2449    448 15.464273
## 4           Black     13292    621  4.463451
```
Problem 2.1 - Evaluating Missing Values
1 point possible (graded)
Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)




```r
df_CPS %>% summarise_each(funs(sum(is.na(.)))) %>% gather() %>% filter(value>0)
```

```
## # A tibble: 5 × 2
##                key value
##              <chr> <int>
## 1    MetroAreaCode 34238
## 2          Married 25338
## 3        Education 25338
## 4 EmploymentStatus 25789
## 5         Industry 65060
```
Problem 2.2 - Evaluating Missing Values
1 point possible (graded)
Often when evaluating a new dataset, we try to identify if there is a pattern in the missing values in the dataset. We will try to determine if there is a pattern in the missing values of the Married variable. The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for whether the Married variable is missing. We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:

```r
df_CPS$missing <- is.na(df_CPS$Married)
groupy_missing <- df_CPS %>% group_by(missing,Age) %>% summarise(counto=n())
groupy_missing
```

```
## Source: local data frame [82 x 3]
## Groups: missing [?]
## 
##    missing   Age counto
##      <lgl> <int>  <int>
## 1    FALSE    15   1795
## 2    FALSE    16   1751
## 3    FALSE    17   1764
## 4    FALSE    18   1596
## 5    FALSE    19   1517
## 6    FALSE    20   1398
## 7    FALSE    21   1525
## 8    FALSE    22   1536
## 9    FALSE    23   1638
## 10   FALSE    24   1627
## # ... with 72 more rows
```

```r
ggplot(df_CPS, aes(x=Age,fill=missing)) + geom_histogram(binwidth=1) 
```

![](Demographics_and_Employment_in_US_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
Problem 2.3 - Evaluating Missing Values
2 points possible (graded)
As mentioned in the variable descriptions, MetroAreaCode is missing if an interviewee does not live in a metropolitan area. Using the same technique as in the previous question, answer the following questions about people who live in non-metropolitan areas.

How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? For this question, treat the District of Columbia as a state (even though it is not technically a state).


  unanswered  
How many states had all interviewees living in a metropolitan area? Again, treat the District of Columbia as a state.

```r
groupy_Metro<- df_CPS %>% group_by(State) %>% summarise(no_metro_area = all(is.na(MetroAreaCode))) 
print (groupy_Metro%>% filter(no_metro_area))
```

```
## # A tibble: 2 × 2
##     State no_metro_area
##    <fctr>         <lgl>
## 1  Alaska          TRUE
## 2 Wyoming          TRUE
```

```r
groupy_Metro<- df_CPS %>% group_by(State) %>% summarise(only_metro_area = all(!is.na(MetroAreaCode))) 
groupy_Metro%>% filter(only_metro_area)
```

```
## # A tibble: 3 × 2
##                  State only_metro_area
##                 <fctr>           <lgl>
## 1 District of Columbia            TRUE
## 2           New Jersey            TRUE
## 3         Rhode Island            TRUE
```
Problem 2.4 - Evaluating Missing Values
1 point possible (graded)
Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?


Midwest
Northeast
South
West

```r
df_CPS %>% group_by(Region) %>% summarise(ratio_non_metro = sum(is.na(MetroAreaCode))/n()*100)
```

```
## # A tibble: 4 × 2
##      Region ratio_non_metro
##      <fctr>           <dbl>
## 1   Midwest        34.78686
## 2 Northeast        21.62381
## 3     South        23.78440
## 4      West        24.36628
```
Problem 2.5 - Evaluating Missing Values
4.0 points possible (graded)
While we were able to use the table() command to compute the proportion of interviewees from each region not living in a metropolitan area, it was somewhat tedious (it involved manually computing the proportion for each region) and isn't something you would want to do if there were a larger number of options. It turns out there is a less tedious way to compute the proportion of values that are TRUE. The mean() function, which takes the average of the values passed to it, will treat TRUE as 1 and FALSE as 0, meaning it returns the proportion of values that are true. For instance, mean(c(TRUE, FALSE, TRUE, TRUE)) returns 0.75. Knowing this, use tapply() with the mean function to answer the following questions:

Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?


  unanswered  
Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?

```r
groupy_state_metro <-df_CPS %>% group_by(State) %>% summarise(ratio_non_metro = sum(is.na(MetroAreaCode))/n()) %>% arrange(desc(ratio_non_metro))
groupy_state_metro
```

```
## # A tibble: 51 × 2
##            State ratio_non_metro
##           <fctr>           <dbl>
## 1         Alaska       1.0000000
## 2        Wyoming       1.0000000
## 3        Montana       0.8360791
## 4  West Virginia       0.7558552
## 5   North Dakota       0.7373860
## 6   South Dakota       0.7025000
## 7    Mississippi       0.6943089
## 8        Vermont       0.6523810
## 9          Maine       0.5983208
## 10      Nebraska       0.5813238
## # ... with 41 more rows
```

Problem 3.1 - Integrating Metropolitan Area Data
2 points possible (graded)
Codes like MetroAreaCode and CountryOfBirthCode are a compact way to encode factor variables with text as their possible values, and they are therefore quite common in survey datasets. In fact, all but one of the variables in this dataset were actually stored by a numeric code in the original CPS datafile.

When analyzing a variable stored by a numeric code, we will often want to convert it into the values the codes represent. To do this, we will use a dictionary, which maps the the code to the actual value of the variable. We have provided dictionaries MetroAreaCodes.csv and CountryCodes.csv, which respectively map MetroAreaCode and CountryOfBirthCode into their true values. Read these two dictionaries into data frames MetroAreaMap and CountryMap.

How many observations (codes for metropolitan areas) are there in MetroAreaMap?


  unanswered  
How many observations (codes for countries) are there in CountryMap?



