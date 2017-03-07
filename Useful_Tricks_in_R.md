# Useful tricks in R

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
df<- read.csv('1_Intro/AnonymityPoll.csv')
```

El classico : compute mean on columns and getting an **NA**


```r
mean(df$Internet.Use)
```

```
## [1] NA
```
ADD na.rm=TRUE

```r
mean(df$Info.On.Internet,na.rm = TRUE)
```

```
## [1] 3.795455
```


Get the number of na per columns

```r
df%>% summarise_each(funs(sum(is.na(.)))) %>% gather()
```

```
##                       key value
## 1            Internet.Use     1
## 2              Smartphone    43
## 3                     Sex     0
## 4                     Age    27
## 5                   State     0
## 6                  Region     0
## 7        Conservativeness    62
## 8        Info.On.Internet   210
## 9        Worry.About.Info   212
## 10     Privacy.Importance   215
## 11     Anonymity.Possible   249
## 12 Tried.Masking.Identity   218
## 13 Privacy.Laws.Effective   108
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
