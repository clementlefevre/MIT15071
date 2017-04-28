R Tricks for amnesics.
================

*Clement Lefevre 2017-04-28*

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

``` r
df<-read.csv('1_Intro/AnonymityPoll.csv')
```

### El classico : compute mean on columns and getting an **NA**

``` r
mean(df$Internet.Use)
```

    ## [1] NA

``` r
#ADD na.rm=TRUE
mean(df$Info.On.Internet,na.rm = TRUE)
```

    ## [1] 3.795455

### Get the **Percentage of NA per columns**

``` r
df%>% summarise_each(funs(sum(is.na(.)/n()*100))) %>% gather()
```

    ##                       key      value
    ## 1            Internet.Use  0.0998004
    ## 2              Smartphone  4.2914172
    ## 3                     Sex  0.0000000
    ## 4                     Age  2.6946108
    ## 5                   State  0.0000000
    ## 6                  Region  0.0000000
    ## 7        Conservativeness  6.1876248
    ## 8        Info.On.Internet 20.9580838
    ## 9        Worry.About.Info 21.1576846
    ## 10     Privacy.Importance 21.4570858
    ## 11     Anonymity.Possible 24.8502994
    ## 12 Tried.Masking.Identity 21.7564870
    ## 13 Privacy.Laws.Effective 10.7784431

``` r
##or :
colMeans(is.na(df))
```

    ##           Internet.Use             Smartphone                    Sex 
    ##            0.000998004            0.042914172            0.000000000 
    ##                    Age                  State                 Region 
    ##            0.026946108            0.000000000            0.000000000 
    ##       Conservativeness       Info.On.Internet       Worry.About.Info 
    ##            0.061876248            0.209580838            0.211576846 
    ##     Privacy.Importance     Anonymity.Possible Tried.Masking.Identity 
    ##            0.214570858            0.248502994            0.217564870 
    ## Privacy.Laws.Effective 
    ##            0.107784431

### Convert the index into column (useful with correlation matrix e.g)

``` r
head(tibble::rownames_to_column(data.frame(df),'indexo'))
```

    ##   indexo Internet.Use Smartphone    Sex Age          State    Region
    ## 1      1            1          0   Male  62  Massachusetts Northeast
    ## 2      2            1          0   Male  45 South Carolina     South
    ## 3      3            0          1 Female  70     New Jersey Northeast
    ## 4      4            1          0   Male  70        Georgia     South
    ## 5      5            0         NA Female  80        Georgia     South
    ## 6      6            1          1   Male  49      Tennessee     South
    ##   Conservativeness Info.On.Internet Worry.About.Info Privacy.Importance
    ## 1                4                0                1          100.00000
    ## 2                1                1                0            0.00000
    ## 3                4                0                0                 NA
    ## 4                4                3                1           88.88889
    ## 5                4               NA               NA                 NA
    ## 6                4                6                0           88.88889
    ##   Anonymity.Possible Tried.Masking.Identity Privacy.Laws.Effective
    ## 1                  0                      0                      0
    ## 2                  1                      0                      1
    ## 3                  0                      0                     NA
    ## 4                  1                      0                      0
    ## 5                 NA                     NA                     NA
    ## 6                  1                      1                      0

### Get the number of NA per row :

``` r
head(rowSums(is.na(df)))
```

    ## [1] 0 0 2 0 7 0

### Convert a model summary into a data.frame :

``` r
require(broom)
```

    ## Loading required package: broom

``` r
model1<-lm(Age~.,data=df %>% select_if(is.numeric))
df_model_summary<-tidy(model1)
head(df_model_summary)
```

    ##               term   estimate std.error  statistic      p.value
    ## 1      (Intercept) 58.8774829 5.7795014 10.1872945 1.317818e-22
    ## 2     Internet.Use -9.1035321 5.2163252 -1.7452003 8.145604e-02
    ## 3       Smartphone -9.1549271 1.4073394 -6.5051308 1.624212e-10
    ## 4 Conservativeness  2.0466691 0.6636528  3.0839454 2.135552e-03
    ## 5 Info.On.Internet -0.8276842 0.2532241 -3.2685840 1.141939e-03
    ## 6 Worry.About.Info -1.2569608 1.3762941 -0.9132937 3.614507e-01

### Get the max/min of each numeric columns :

``` r
max_min_cols<-df %>% select_if(is.numeric) %>% summarise_each(funs(max=max(., na.rm = TRUE),min=min(., na.rm = TRUE)))
head(max_min_cols %>% gather())
```

    ##                    key value
    ## 1     Internet.Use_max     1
    ## 2       Smartphone_max     1
    ## 3              Age_max    96
    ## 4 Conservativeness_max     5
    ## 5 Info.On.Internet_max    11
    ## 6 Worry.About.Info_max     1
