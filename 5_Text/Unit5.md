# Unit 5 Text_1





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
library(tm)
```

```
## Loading required package: NLP
```

```
## 
## Attaching package: 'NLP'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
library(SnowballC)
```
## Prepare Corpus ##

```r
df<-read.csv('Unit5_1/tweets.csv',stringsAsFactors = FALSE)

df<- df %>% mutate(negative = Avg<=-1)
table(df$negative)
```

```
## 
## FALSE  TRUE 
##   999   182
```

```r
corpus <- Corpus(VectorSource(df$Tweet))
corpus[[1]]$content
```

```
## [1] "I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore"
```

```r
# Convert to lower-case

corpus = tm_map(corpus, content_transformer(tolower))

corpus[[1]]$content
```

```
## [1] "i have to say, apple has by far the best customer care service i have ever received! @apple @appstore"
```

```r
# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]$content
```

```
## [1] "i have to say apple has by far the best customer care service i have ever received apple appstore"
```

```r
# Look at stop words 
stopwords("english")[1:10]
```

```
##  [1] "i"         "me"        "my"        "myself"    "we"       
##  [6] "our"       "ours"      "ourselves" "you"       "your"
```

```r
# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]$content
```

```
## [1] "   say    far  best customer care service   ever received  appstore"
```

```r
# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content
```

```
## [1] "say far best custom care servic ever receiv appstor"
```
## Create Frequency Matrix

```r
frequencies <- DocumentTermMatrix(corpus)
head(as.data.frame(as.matrix(frequencies)))
```

```
##   appstor best care custom ever far receiv say servic beauti frick smooth
## 1       1    1    1      1    1   1      1   1      1      0     0      0
## 2       0    0    0      0    0   0      0   0      0      1     1      1
## 3       0    0    0      0    0   0      0   0      0      0     0      0
## 4       0    0    0      0    0   0      0   0      0      0     0      0
## 5       0    1    0      1    0   0      0   0      1      0     0      0
## 6       0    1    0      0    1   0      0   0      0      0     0      0
##   thanxappl love iphon iphone5 new pictwittercomxmhjcu4pcb thank 10min
## 1         0    0     0       0   0                       0     0     0
## 2         1    0     0       0   0                       0     0     0
## 3         0    1     0       0   0                       0     0     0
## 4         0    1     1       1   1                       1     1     0
## 5         0    0     0       0   1                       0     0     1
## 6         0    0     0       0   0                       0     0     0
##   phone amaz ear headphon inear ive pod sound can cool finger make omg
## 1     0    0   0        0     0   0   0     0   0    0      0    0   0
## 2     0    0   0        0     0   0   0     0   0    0      0    0   0
## 3     0    0   0        0     0   0   0     0   0    0      0    0   0
## 4     0    0   0        0     0   0   0     0   0    0      0    0   0
## 5     1    0   0        0     0   0   0     0   0    0      0    0   0
## 6     0    1   1        1     1   1   1     1   0    0      0    0   0
##   passcod print purchas read unlock without alway attributeownership busi
## 1       0     0       0    0      0       0     0                  0    0
## 2       0     0       0    0      0       0     0                  0    0
## 3       0     0       0    0      0       0     0                  0    0
## 4       0     0       0    0      0       0     0                  0    0
## 5       0     0       0    0      0       0     0                  0    0
## 6       0     0       0    0      0       0     0                  0    0
##   exact innov market one will 7wow bravo cant check get just spec updat
## 1     0     0      0   0    0    0     0    0     0   0    0    0     0
## 2     0     0      0   0    0    0     0    0     0   0    0    0     0
## 3     0     0      0   0    0    0     0    0     0   0    0    0     0
## 4     0     0      0   0    0    0     0    0     0   0    0    0     0
## 5     0     0      0   0    0    0     0    0     0   0    0    0     0
## 6     0     0      0   0    0    0     0    0     0   0    0    0     0
##   wait much phillydvib thnx bloodi brilliant featur fingerprint
## 1    0    0          0    0      0         0      0           0
## 2    0    0          0    0      0         0      0           0
## 3    0    0          0    0      0         0      0           0
## 4    0    0          0    0      0         0      0           0
## 5    0    0          0    0      0         0      0           0
## 6    0    0          0    0      0         0      0           0
##   httptoiinw0o3z killer scanner timesnow v2vista almost brand demis
## 1              0      0       0        0       0      0     0     0
## 2              0      0       0        0       0      0     0     0
## 3              0      0       0        0       0      0     0     0
## 4              0      0       0        0       0      0     0     0
## 5              0      0       0        0       0      0     0     0
## 6              0      0       0        0       0      0     0     0
##   fantast favorit interest mani peopl seem still what bnbuzz easier life
## 1       0       0        0    0     0    0     0    0      0      0    0
## 2       0       0        0    0     0    0     0    0      0      0    0
## 3       0       0        0    0     0    0     0    0      0      0    0
## 4       0       0        0    0     0    0     0    0      0      0    0
## 5       0       0        0    0     0    0     0    0      0      0    0
## 6       0       0        0    0     0    0     0    0      0      0    0
##   made morn nookbn nookstudi alreadi christma gift guess iphone5c keynot
## 1    0    0      0         0       0        0    0     0        0      0
## 2    0    0      0         0       0        0    0     0        0      0
## 3    0    0      0         0       0        0    0     0        0      0
## 4    0    0      0         0       0        0    0     0        0      0
## 5    0    0      0         0       0        0    0     0        0      0
## 6    0    0      0         0       0        0    0     0        0      0
##   latest watch call correct first gave store time told wasnt went wow
## 1      0     0    0       0     0    0     0    0    0     0    0   0
## 2      0     0    0       0     0    0     0    0    0     0    0   0
## 3      0     0    0       0     0    0     0    0    0     0    0   0
## 4      0     0    0       0     0    0     0    0    0     0    0   0
## 5      0     0    0       0     0    0     0    0    0     0    0   0
## 6      0     0    0       0     0    0     0    0    0     0    0   0
##   experi great job provid thinkauto user convert day galaxys2 iphone4 swap
## 1      0     0   0      0         0    0       0   0        0       0    0
## 2      0     0   0      0         0    0       0   0        0       0    0
## 3      0     0   0      0         0    0       0   0        0       0    0
## 4      0     0   0      0         0    0       0   0        0       0    0
## 5      0     0   0      0         0    0       0   0        0       0    0
## 6      0     0   0      0         0    0       0   0        0       0    0
##   come novemb orang orangehelp upgrad color till fan httpbitly18ybjlq lot
## 1    0      0     0          0      0     0    0   0                0   0
## 2    0      0     0          0      0     0    0   0                0   0
## 3    0      0     0          0      0     0    0   0                0   0
## 4    0      0     0          0      0     0    0   0                0   0
## 5    0      0     0          0      0     0    0   0                0   0
## 6    0      0     0          0      0     0    0   0                0   0
##   lovegreatdesign reason video whether worth your impress now recognit
## 1               0      0     0       0     0    0       0   0        0
## 2               0      0     0       0     0    0       0   0        0
## 3               0      0     0       0     0    0       0   0        0
## 4               0      0     0       0     0    0       0   0        0
## 5               0      0     0       0     0    0       0   0        0
## 6               0      0     0       0     0    0       0   0        0
##   that awesom blackberri clickitandlickit jimmykimmel kuqogroup unbeliev
## 1    0      0          0                0           0         0        0
## 2    0      0          0                0           0         0        0
## 3    0      0          0                0           0         0        0
## 4    0      0          0                0           0         0        0
## 5    0      0          0                0           0         0        0
## 6    0      0          0                0           0         0        0
##   earpod champagn colour luv reader demonstr explainevrythng factor
## 1      0        0      0   0      0        0               0      0
## 2      0        0      0   0      0        0               0      0
## 3      0        0      0   0      0        0               0      0
## 4      0        0      0   0      0        0               0      0
## 5      0        0      0   0      0        0               0      0
## 6      0        0      0   0      0        0               0      0
##   student understand use arent brodyknibb compani haha mint top back yaw
## 1       0          0   0     0          0       0    0    0   0    0   0
## 2       0          0   0     0          0       0    0    0   0    0   0
## 3       0          0   0     0          0       0    0    0   0    0   0
## 4       0          0   0     0          0       0    0    0   0    0   0
## 5       0          0   0     0          0       0    0    0   0    0   0
## 6       0          0   0     0          0       0    0    0   0    0   0
##   anyon courtesi found map said today way work alarm dear default goodmorn
## 1     0        0     0   0    0     0   0    0     0    0       0        0
## 2     0        0     0   0    0     0   0    0     0    0       0        0
## 3     0        0     0   0    0     0   0    0     0    0       0        0
## 4     0        0     0   0    0     0   0    0     0    0       0        0
## 5     0        0     0   0    0     0   0    0     0    0       0        0
## 6     0        0     0   0    0     0   0    0     0    0       0        0
##   rington camera good never notic pictwittercom0z6zd8fymk pug app appletv
## 1       0      0    0     0     0                       0   0   0       0
## 2       0      0    0     0     0                       0   0   0       0
## 3       0      0    0     0     0                       0   0   0       0
## 4       0      0    0     0     0                       0   0   0       0
## 5       0      0    0     0     0                       0   0   0       0
## 6       0      0    0     0     0                       0   0   0       0
##   hous leav may sec watchespn pictwittercomx5zy21xfi3 product draw mac
## 1    0    0   0   0         0                       0       0    0   0
## 2    0    0   0   0         0                       0       0    0   0
## 3    0    0   0   0         0                       0       0    0   0
## 4    0    0   0   0         0                       0       0    0   0
## 5    0    0   0   0         0                       0       0    0   0
## 6    0    0   0   0         0                       0       0    0   0
##   pictwittercomw9ckpcey0r soooo android pull saw think creat extens
## 1                       0     0       0    0   0     0     0      0
## 2                       0     0       0    0   0     0     0      0
## 3                       0     0       0    0   0     0     0      0
## 4                       0     0       0    0   0     0     0      0
## 5                       0     0       0    0   0     0     0      0
## 6                       0     0       0    0   0     0     0      0
##   jasonrein research trendsett damag drop flight stair two wooden help
## 1         0        0         0     0    0      0     0   0      0    0
## 2         0        0         0     0    0      0     0   0      0    0
## 3         0        0         0     0    0      0     0   0      0    0
## 4         0        0         0     0    0      0     0   0      0    0
## 5         0        0         0     0    0      0     0   0      0    0
## 6         0        0         0     0    0      0     0   0      0    0
##   away function hour ipad storean traceyliddl continu easi
## 1    0        0    0    0       0           0       0    0
## 2    0        0    0    0       0           0       0    0
## 3    0        0    0    0       0           0       0    0
## 4    0        0    0    0       0           0       0    0
## 5    0        0    0    0       0           0       0    0
## 6    0        0    0    0       0           0       0    0
##   pictwittercomlga2kaxa4b transfer mention nokia nokiauk sentenc hope
## 1                       0        0       0     0       0       0    0
## 2                       0        0       0     0       0       0    0
## 3                       0        0       0     0       0       0    0
## 4                       0        0       0     0       0       0    0
## 5                       0        0       0     0       0       0    0
## 6                       0        0       0     0       0       0    0
##   mfayax httpinstagramcomappsolut instagram picturesvideo
## 1      0                        0         0             0
## 2      0                        0         0             0
## 3      0                        0         0             0
## 4      0                        0         0             0
## 5      0                        0         0             0
## 6      0                        0         0             0
##   pictwittercomqqdzqe8s4f profil stun world faith httpowly22fbs4 restor
## 1                       0      0    0     0     0              0      0
## 2                       0      0    0     0     0              0      0
## 3                       0      0    0     0     0              0      0
## 4                       0      0    0     0     0              0      0
## 5                       0      0    0     0     0              0      0
## 6                       0      0    0     0     0              0      0
##   sudden charg free give imovi iphoto iwork miss move owner total
## 1      0     0    0    0     0      0     0    0    0     0     0
## 2      0     0    0    0     0      0     0    0    0     0     0
## 3      0     0    0    0     0      0     0    0    0     0     0
## 4      0     0    0    0     0      0     0    0    0     0     0
## 5      0     0    0    0     0      0     0    0    0     0     0
## 6      0     0    0    0     0      0     0    0    0     0     0
##   fingerprintsrecognitom pretti concept cycl huge improv live
## 1                      0      0       0    0    0      0    0
## 2                      0      0       0    0    0      0    0
## 3                      0      0       0    0    0      0    0
## 4                      0      0       0    0    0      0    0
## 5                      0      0       0    0    0      0    0
## 6                      0      0       0    0    0      0    0
##   pictwittercom4vkivk0s0i review shape snapto stack amount macbook
## 1                       0      0     0      0     0      0       0
## 2                       0      0     0      0     0      0       0
## 3                       0      0     0      0     0      0       0
## 4                       0      0     0      0     0      0       0
## 5                       0      0     0      0     0      0       0
## 6                       0      0     0      0     0      0       0
##   starbuck well gold ibrooklynb ftw window automat brighter code flawless
## 1        0    0    0          0   0      0       0        0    0        0
## 2        0    0    0          0   0      0       0        0    0        0
## 3        0    0    0          0   0      0       0        0    0        0
## 4        0    0    0          0   0      0       0        0    0        0
## 5        0    0    0          0   0      0       0        0    0        0
## 6        0    0    0          0   0      0       0        0    0        0
##   ill open passbook scan screen stick absolut applenw appletweet break
## 1   0    0        0    0      0     0       0       0          0     0
## 2   0    0        0    0      0     0       0       0          0     0
## 3   0    0        0    0      0     0       0       0          0     0
## 4   0    0        0    0      0     0       0       0          0     0
## 5   0    0        0    0      0     0       0       0          0     0
## 6   0    0        0    0      0     0       0       0          0     0
##   incred ios7everybodi jail kill oper readi system againth done freak
## 1      0             0    0    0    0     0      0       0    0     0
## 2      0             0    0    0    0     0      0       0    0     0
## 3      0             0    0    0    0     0      0       0    0     0
## 4      0             0    0    0    0     0      0       0    0     0
## 5      0             0    0    0    0     0      0       0    0     0
## 6      0             0    0    0    0     0      0       0    0     0
##   gonna sick key lafond66 mallow610 next noth recent upgradebrand usabl
## 1     0    0   0        0         0    0    0      0            0     0
## 2     0    0   0        0         0    0    0      0            0     0
## 3     0    0   0        0         0    0    0      0            0     0
## 4     0    0   0        0         0    0    0      0            0     0
## 5     0    0   0        0         0    0    0      0            0     0
## 6     0    0   0        0         0    0    0      0            0     0
##   2001 hatewindow instead pro stupidmicrosoft dont final hard introduc
## 1    0          0       0   0               0    0     0    0        0
## 2    0          0       0   0               0    0     0    0        0
## 3    0          0       0   0               0    0     0    0        0
## 4    0          0       0   0               0    0     0    0        0
## 5    0          0       0   0               0    0     0    0        0
## 6    0          0       0   0               0    0     0    0        0
##   suck friday got legit repair replac sent straight wednesday complet
## 1    0      0   0     0      0      0    0        0         0       0
## 2    0      0   0     0      0      0    0        0         0       0
## 3    0      0   0     0      0      0    0        0         0       0
## 4    0      0   0     0      0      0    0        0         0       0
## 5    0      0   0     0      0      0    0        0         0       0
## 6    0      0   0     0      0      0    0        0         0       0
##   iradio pictwittercomgqg5xusyf7 articl asid fastcodesign httpowlyovszn
## 1      0                       0      0    0            0             0
## 2      0                       0      0    0            0             0
## 3      0                       0      0    0            0             0
## 4      0                       0      0    0            0             0
## 5      0                       0      0    0            0             0
## 6      0                       0      0    0            0             0
##   hyperbol simpli via around creep els least let okay pall stop togeth
## 1        0      0   0      0     0   0     0   0    0    0    0      0
## 2        0      0   0      0     0   0     0   0    0    0    0      0
## 3        0      0   0      0     0   0     0   0    0    0    0      0
## 4        0      0   0      0     0   0     0   0    0    0    0      0
## 5        0      0   0      0     0   0     0   0    0    0    0      0
## 6        0      0   0      0     0   0     0   0    0    0    0      0
##   httpbitly17pdiv1 jillmartin77 music plastic see unapologet better cuz
## 1                0            0     0       0   0          0      0   0
## 2                0            0     0       0   0          0      0   0
## 3                0            0     0       0   0          0      0   0
## 4                0            0     0       0   0          0      0   0
## 5                0            0     0       0   0          0      0   0
## 6                0            0     0       0   0          0      0   0
##   iphonec name design fell like older technolog wanna big content drumrol
## 1       0    0      0    0    0     0         0     0   0       0       0
## 2       0    0      0    0    0     0         0     0   0       0       0
## 3       0    0      0    0    0     0         0     0   0       0       0
## 4       0    0      0    0    0     0         0     0   0       0       0
## 5       0    0      0    0    0     0         0     0   0       0       0
## 6       0    0      0    0    0     0         0     0   0       0       0
##   happyd join lrwblog news pleas redesignhttpowlyonazd samsung blog send
## 1      0    0       0    0     0                     0       0    0    0
## 2      0    0       0    0     0                     0       0    0    0
## 3      0    0       0    0     0                     0       0    0    0
## 4      0    0       0    0     0                     0       0    0    0
## 5      0    0       0    0     0                     0       0    0    0
## 6      0    0       0    0     0                     0       0    0    0
##   want batteri pictwittercomxwqjedhsvt year choos green guy whitesilv
## 1    0       0                       0    0     0     0   0         0
## 2    0       0                       0    0     0     0   0         0
## 3    0       0                       0    0     0     0   0         0
## 4    0       0                       0    0     0     0   0         0
## 5    0       0                       0    0     0     0   0         0
## 6    0       0                       0    0     0     0   0         0
##   birthday fake marri regist registri thing httptwitpiccomdd6xjw
## 1        0    0     0      0        0     0                    0
## 2        0    0     0      0        0     0                    0
## 3        0    0     0      0        0     0                    0
## 4        0    0     0      0        0     0                    0
## 5        0    0     0      0        0     0                    0
## 6        0    0     0      0        0     0                    0
##   itunesfestiv pictwittercomlbu9diufrf enter flashlight genius ios7 know
## 1            0                       0     0          0      0    0    0
## 2            0                       0     0          0      0    0    0
## 3            0                       0     0          0      0    0    0
## 4            0                       0     0          0      0    0    0
## 5            0                       0     0          0      0    0    0
## 6            0                       0     0          0      0    0    0
##   password simpl ahead futur ident leap manag mdm mim mobil stuck
## 1        0     0     0     0     0    0     0   0   0     0     0
## 2        0     0     0     0     0    0     0   0   0     0     0
## 3        0     0     0     0     0    0     0   0   0     0     0
## 4        0     0     0     0     0    0     0   0   0     0     0
## 5        0     0     0     0     0    0     0   0   0     0     0
## 6        0     0     0     0     0    0     0   0   0     0     0
##   6monthlifespan everi laptop microsoft hand pictwittercomxwr5hvnjyc
## 1              0     0      0         0    0                       0
## 2              0     0      0         0    0                       0
## 3              0     0      0         0    0                       0
## 4              0     0      0         0    0                       0
## 5              0     0      0         0    0                       0
## 6              0     0      0         0    0                       0
##   september20th believ tvmattscott entri glad itll level light megapixel
## 1             0      0           0     0    0    0     0     0         0
## 2             0      0           0     0    0    0     0     0         0
## 3             0      0           0     0    0    0     0     0         0
## 4             0      0           0     0    0    0     0     0         0
## 5             0      0           0     0    0    0     0     0         0
## 6             0      0           0     0    0    0     0     0         0
##   outdo sensit skip slrs war fireandice5935 forc johanbarnard strong tempt
## 1     0      0    0    0   0              0    0            0      0     0
## 2     0      0    0    0   0              0    0            0      0     0
## 3     0      0    0    0   0              0    0            0      0     0
## 4     0      0    0    0   0              0    0            0      0     0
## 5     0      0    0    0   0              0    0            0      0     0
## 6     0      0    0    0   0              0    0            0      0     0
##   tho coremot empow fit generat health actual announc number page 180 bad
## 1   0       0     0   0       0      0      0       0      0    0   0   0
## 2   0       0     0   0       0      0      0       0      0    0   0   0
## 3   0       0     0   0       0      0      0       0      0    0   0   0
## 4   0       0     0   0       0      0      0       0      0    0   0   0
## 5   0       0     0   0       0      0      0       0      0    0   0   0
## 6   0       0     0   0       0      0      0       0      0    0   0   0
##   bubblemania laurenjessa lost lxforev spotwent class experienceadditn
## 1           0           0    0       0        0     0                0
## 2           0           0    0       0        0     0                0
## 3           0           0    0       0        0     0                0
## 4           0           0    0       0        0     0                0
## 5           0           0    0       0        0     0                0
## 6           0           0    0       0        0     0                0
##   industryincred instor onlin pictwittercomjagqqkjmr3 retail sephora
## 1              0      0     0                       0      0       0
## 2              0      0     0                       0      0       0
## 3              0      0     0                       0      0       0
## 4              0      0     0                       0      0       0
## 5              0      0     0                       0      0       0
## 6              0      0     0                       0      0       0
##   applereason manufactur none roll smart softwar anythng bring cake dat
## 1           0          0    0    0     0       0       0     0    0   0
## 2           0          0    0    0     0       0       0     0    0   0
## 3           0          0    0    0     0       0       0     0    0   0
## 4           0          0    0    0     0       0       0     0    0   0
## 5           0          0    0    0     0       0       0     0    0   0
## 6           0          0    0    0     0       0       0     0    0   0
##   dint hot sell wen wil httpforbitlyscaleboxapp invit itun itunesmus
## 1    0   0    0   0   0                       0     0    0         0
## 2    0   0    0   0   0                       0     0    0         0
## 3    0   0    0   0   0                       0     0    0         0
## 4    0   0    0   0   0                       0     0    0         0
## 5    0   0    0   0   0                       0     0    0         0
## 6    0   0    0   0   0                       0     0    0         0
##   musician scaleboxapp home offic pen pictwittercomu1ifcddhei vintag
## 1        0           0    0     0   0                       0      0
## 2        0           0    0     0   0                       0      0
## 3        0           0    0     0   0                       0      0
## 4        0           0    0     0   0                       0      0
## 5        0           0    0     0   0                       0      0
## 6        0           0    0     0   0                       0      0
##   applestoreamsterdam buy didnt geniusbar someth spot tks yet beyonc
## 1                   0   0     0         0      0    0   0   0      0
## 2                   0   0     0         0      0    0   0   0      0
## 3                   0   0     0         0      0    0   0   0      0
## 4                   0   0     0         0      0    0   0   0      0
## 5                   0   0     0         0      0    0   0   0      0
## 6                   0   0     0         0      0    0   0   0      0
##   bphone pictwittercomsj86urqvm8 choicenot due ehhh sure thoughgold
## 1      0                       0         0   0    0    0          0
## 2      0                       0         0   0    0    0          0
## 3      0                       0         0   0    0    0          0
## 4      0                       0         0   0    0    0          0
## 5      0                       0         0   0    0    0          0
## 6      0                       0         0   0    0    0          0
##   upgradeth whelpim advantag api biometr dev excit releas secur soon take
## 1         0       0        0   0       0   0     0      0     0    0    0
## 2         0       0        0   0       0   0     0      0     0    0    0
## 3         0       0        0   0       0   0     0      0     0    0    0
## 4         0       0        0   0       0   0     0      0     0    0    0
## 5         0       0        0   0       0   0     0      0     0    0    0
## 6         0       0        0   0       0   0     0      0     0    0    0
##   touchid dead lion mountain save ssd follow asap courtsideassistappforio
## 1       0    0    0        0    0   0      0    0                       0
## 2       0    0    0        0    0   0      0    0                       0
## 3       0    0    0        0    0   0      0    0                       0
## 4       0    0    0        0    0   0      0    0                       0
## 5       0    0    0        0    0   0      0    0                       0
## 6       0    0    0        0    0   0      0    0                       0
##   current support idea kickbutt preinstal simonmoel subway surfer achiev
## 1       0       0    0        0         0         0      0      0      0
## 2       0       0    0        0         0         0      0      0      0
## 3       0       0    0        0         0         0      0      0      0
## 4       0       0    0        0         0         0      0      0      0
## 5       0       0    0        0         0         0      0      0      0
## 6       0       0    0        0         0         0      0      0      0
##   build httpowlyoo0u0 ngerprint sensor touch filsfan rockstar thehottspot
## 1     0             0         0      0     0       0        0           0
## 2     0             0         0      0     0       0        0           0
## 3     0             0         0      0     0       0        0           0
## 4     0             0         0      0     0       0        0           0
## 5     0             0         0      0     0       0        0           0
## 6     0             0         0      0     0       0        0           0
##   themixxhangout themixxradio astrologyzon commit forward look push week
## 1              0            0            0      0       0    0    0    0
## 2              0            0            0      0       0    0    0    0
## 3              0            0            0      0       0    0    0    0
## 4              0            0            0      0       0    0    0    0
## 5              0            0            0      0       0    0    0    0
## 6              0            0            0      0       0    0    0    0
##   burberri httponmashto1bbfsca mashabl partnership runway shoot show
## 1        0                   0       0           0      0     0    0
## 2        0                   0       0           0      0     0    0
## 3        0                   0       0           0      0     0    0
## 4        0                   0       0           0      0     0    0
## 5        0                   0       0           0      0     0    0
## 6        0                   0       0           0      0     0    0
##   teaser capabl forbid god multipl network offer resourc spend button
## 1      0      0      0   0       0       0     0       0     0      0
## 2      0      0      0   0       0       0     0       0     0      0
## 3      0      0      0   0       0       0     0       0     0      0
## 4      0      0      0   0       0       0     0       0     0      0
## 5      0      0      0   0       0       0     0       0     0      0
## 6      0      0      0   0       0       0     0       0     0      0
##   customernot data die happi polici ret sleep slowli devic mobilebiz
## 1           0    0   0     0      0   0     0      0     0         0
## 2           0    0   0     0      0   0     0      0     0         0
## 3           0    0   0     0      0   0     0      0     0         0
## 4           0    0   0     0      0   0     0      0     0         0
## 5           0    0   0     0      0   0     0      0     0         0
## 6           0    0   0     0      0   0     0      0     0         0
##   quicker rais speedux thedimassa volley yes abrsm excel onlyclass piano
## 1       0    0       0          0      0   0     0     0         0     0
## 2       0    0       0          0      0   0     0     0         0     0
## 3       0    0       0          0      0   0     0     0         0     0
## 4       0    0       0          0      0   0     0     0         0     0
## 5       0    0       0          0      0   0     0     0         0     0
## 6       0    0       0          0      0   0     0     0         0     0
##   violin homeimprov homeown tool undefin behind couldnt
## 1      0          0       0    0       0      0       0
## 2      0          0       0    0       0      0       0
## 3      0          0       0    0       0      0       0
## 4      0          0       0    0       0      0       0
## 5      0          0       0    0       0      0       0
## 6      0          0       0    0       0      0       0
##   pictwittercoms3rzjzyssj wrong becom chip domin
## 1                       0     0     0    0     0
## 2                       0     0     0    0     0
## 3                       0     0     0    0     0
## 4                       0     0     0    0     0
## 5                       0     0     0    0     0
## 6                       0     0     0    0     0
##   httpsmgtechmediacompagesapplehtm talk appl brief develop host learn
## 1                                0    0    0     0       0    0     0
## 2                                0    0    0     0       0    0     0
## 3                                0    0    0     0       0    0     0
## 4                                0    0    0     0       0    0     0
## 5                                0    0    0     0       0    0     0
## 6                                0    0    0     0       0    0     0
##   navneetsainistratixcorpcom oct stratix loveit mobilerevolut
## 1                          0   0       0      0             0
## 2                          0   0       0      0             0
## 3                          0   0       0      0             0
## 4                          0   0       0      0             0
## 5                          0   0       0      0             0
## 6                          0   0       0      0             0
##   phoneblockscom right fanboy flash greeniphon heart littl recaptur
## 1              0     0      0     0          0     0     0        0
## 2              0     0      0     0          0     0     0        0
## 3              0     0      0     0          0     0     0        0
## 4              0     0      0     0          0     0     0        0
## 5              0     0      0     0          0     0     0        0
## 6              0     0      0     0          0     0     0        0
##   yespleas entertain problem tech fashion glam goe high httpbitly160el6m
## 1        0         0       0    0       0    0   0    0                0
## 2        0         0       0    0       0    0   0    0                0
## 3        0         0       0    0       0    0   0    0                0
## 4        0         0       0    0       0    0   0    0                0
## 5        0         0       0    0       0    0   0    0                0
## 6        0         0       0    0       0    0   0    0                0
##   luxuri partner promot smartwatch 2000ad approv dredd slain submit ago
## 1      0       0      0          0      0      0     0     0      0   0
## 2      0       0      0          0      0      0     0     0      0   0
## 3      0       0      0          0      0      0     0     0      0   0
## 4      0       0      0          0      0      0     0     0      0   0
## 5      0       0      0          0      0      0     0     0      0   0
## 6      0       0      0          0      0      0     0     0      0   0
##   anod darn finish airwaysmagazin boeingairplan fascin desper need tonight
## 1    0    0      0              0             0      0      0    0       0
## 2    0    0      0              0             0      0      0    0       0
## 3    0    0      0              0             0      0      0    0       0
## 4    0    0      0              0             0      0      0    0       0
## 5    0    0      0              0             0      0      0    0       0
## 6    0    0      0              0             0      0      0    0       0
##   basic gent hottest nerd jealous put preorder pictwittercomhlvxntisug
## 1     0    0       0    0       0   0        0                       0
## 2     0    0       0    0       0   0        0                       0
## 3     0    0       0    0       0   0        0                       0
## 4     0    0       0    0       0   0        0                       0
## 5     0    0       0    0       0   0        0                       0
## 6     0    0       0    0       0   0        0                       0
##   last weekend happen httpbuffly17swtoc mashablevideo black commerc googl
## 1    0       0      0                 0             0     0       0     0
## 2    0       0      0                 0             0     0       0     0
## 3    0       0      0                 0             0     0       0     0
## 4    0       0      0                 0             0     0       0     0
## 5    0       0      0                 0             0     0       0     0
## 6    0       0      0                 0             0     0       0     0
##   httpowlyoqcgb lead reign embarass nowaday chipset engin innovatinghello
## 1             0    0     0        0       0       0     0               0
## 2             0    0     0        0       0       0     0               0
## 3             0    0     0        0       0       0     0               0
## 4             0    0     0        0       0       0     0               0
## 5             0    0     0        0       0       0     0               0
## 6             0    0     0        0       0       0     0               0
##   ali muhammad pictwittercomuhypsr33vw thegreatest thinkdiffer dream
## 1   0        0                       0           0           0     0
## 2   0        0                       0           0           0     0
## 3   0        0                       0           0           0     0
## 4   0        0                       0           0           0     0
## 5   0        0                       0           0           0     0
## 6   0        0                       0           0           0     0
##   infiltr launch hey iphonesetc jarvi somedayvia cmon giddyup homegirl
## 1       0      0   0          0     0          0    0       0        0
## 2       0      0   0          0     0          0    0       0        0
## 3       0      0   0          0     0          0    0       0        0
## 4       0      0   0          0     0          0    0       0        0
## 5       0      0   0          0     0          0    0       0        0
## 6       0      0   0          0     0          0    0       0        0
##   ihavethefirston jesus pictwittercomsjvvjka3sd 1st bit dstv
## 1               0     0                       0   0   0    0
## 2               0     0                       0   0   0    0
## 3               0     0                       0   0   0    0
## 4               0     0                       0   0   0    0
## 5               0     0                       0   0   0    0
## 6               0     0                       0   0   0    0
##   pictwittercomxshi3gqlku sad suit eldaili httpowlyor69m program recycl
## 1                       0   0    0       0             0       0      0
## 2                       0   0    0       0             0       0      0
## 3                       0   0    0       0             0       0      0
## 4                       0   0    0       0             0       0      0
## 5                       0   0    0       0             0       0      0
## 6                       0   0    0       0             0       0      0
##   bestbuy blakegrahampga gone start toy california fast httpowlyoonxw
## 1       0              0    0     0   0          0    0             0
## 2       0              0    0     0   0          0    0             0
## 3       0              0    0     0   0          0    0             0
## 4       0              0    0     0   0          0    0             0
## 5       0              0    0     0   0          0    0             0
## 6       0              0    0     0   0          0    0             0
##   infograph true origin confirm httpowlyor9j3 httpfwtoqo0dqxt connect dot
## 1         0    0      0       0             0               0       0   0
## 2         0    0      0       0             0               0       0   0
## 3         0    0      0       0             0               0       0   0
## 4         0    0      0       0             0               0       0   0
## 5         0    0      0       0             0               0       0   0
## 6         0    0      0       0             0               0       0   0
##   howev mrjamesnobl sign tri 7pm everyon creativ imor
## 1     0           0    0   0   0       0       0    0
## 2     0           0    0   0   0       0       0    0
## 3     0           0    0   0   0       0       0    0
## 4     0           0    0   0   0       0       0    0
## 5     0           0    0   0   0       0       0    0
## 6     0           0    0   0   0       0       0    0
##   pictwittercomqbi2za6xuv 64bit cement no1 pictur posit tablet yesterday
## 1                       0     0      0   0      0     0      0         0
## 2                       0     0      0   0      0     0      0         0
## 3                       0     0      0   0      0     0      0         0
## 4                       0     0      0   0      0     0      0         0
## 5                       0     0      0   0      0     0      0         0
## 6                       0     0      0   0      0     0      0         0
##   httpowlyoocpf phoneblock thinglook 2013 forecast grow idc worldwid
## 1             0          0         0    0        0    0   0        0
## 2             0          0         0    0        0    0   0        0
## 3             0          0         0    0        0    0   0        0
## 4             0          0         0    0        0    0   0        0
## 5             0          0         0    0        0    0   0        0
## 6             0          0         0    0        0    0   0        0
##   geekchic httpbitly16lfd2 motorola peterloudi smartphon game perfect
## 1        0               0        0          0         0    0       0
## 2        0               0        0          0         0    0       0
## 3        0               0        0          0         0    0       0
## 4        0               0        0          0         0    0       0
## 5        0               0        0          0         0    0       0
## 6        0               0        0          0         0    0       0
##   pr1nc355haybug singl step teamiphon cheaper httphtlyoof4v succeed case
## 1              0     0    0         0       0             0       0    0
## 2              0     0    0         0       0             0       0    0
## 3              0     0    0         0       0             0       0    0
## 4              0     0    0         0       0             0       0    0
## 5              0     0    0         0       0             0       0    0
## 6              0     0    0         0       0             0       0    0
##   hold patent possibl surpris tomorrow winphan annoy coars copyp jon4lak
## 1    0      0       0       0        0       0     0     0     0       0
## 2    0      0       0       0        0       0     0     0     0       0
## 3    0      0       0       0        0       0     0     0     0       0
## 4    0      0       0       0        0       0     0     0     0       0
## 5    0      0       0       0        0       0     0     0     0       0
## 6    0      0       0       0        0       0     0     0     0       0
##   technobuffalo differ led old solv adpromo also etcanada
## 1             0      0   0   0    0       0    0        0
## 2             0      0   0   0    0       0    0        0
## 3             0      0   0   0    0       0    0        0
## 4             0      0   0   0    0       0    0        0
## 5             0      0   0   0    0       0    0        0
## 6             0      0   0   0    0       0    0        0
##   httpinstagramcompeoodxzgvpq justinbieb nice clusocluso
## 1                           0          0    0          0
## 2                           0          0    0          0
## 3                           0          0    0          0
## 4                           0          0    0          0
## 5                           0          0    0          0
## 6                           0          0    0          0
##   httpinstagramcompeka4gsjkil steve digitallyperfect heaven match meet
## 1                           0     0                0      0     0    0
## 2                           0     0                0      0     0    0
## 3                           0     0                0      0     0    0
## 4                           0     0                0      0     0    0
## 5                           0     0                0      0     0    0
## 6                           0     0                0      0     0    0
##   pictwittercomg16dmimdt9 trick aapl elimin grey httpwpmepeksv7f ineffici
## 1                       0     0    0      0    0               0        0
## 2                       0     0    0      0    0               0        0
## 3                       0     0    0      0    0               0        0
## 4                       0     0    0      0    0               0        0
## 5                       0     0    0      0    0               0        0
## 6                       0     0    0      0    0               0        0
##   price set twerk breakthrough httpbuffly1dzbmbp iter jmlcolley spirit
## 1     0   0     0            0                 0    0         0      0
## 2     0   0     0            0                 0    0         0      0
## 3     0   0     0            0                 0    0         0      0
## 4     0   0     0            0                 0    0         0      0
## 5     0   0     0            0                 0    0         0      0
## 6     0   0     0            0                 0    0         0      0
##   pictwittercomyarbr8m7fb theespinalien wish folk ibm programm twitter
## 1                       0             0    0    0   0        0       0
## 2                       0             0    0    0   0        0       0
## 3                       0             0    0    0   0        0       0
## 4                       0             0    0    0   0        0       0
## 5                       0             0    0    0   0        0       0
## 6                       0             0    0    0   0        0       0
##   play qpr asi barato bueno como cuando mas pictwittercomlot9cy2ulv saca
## 1    0   0   0      0     0    0      0   0                       0    0
## 2    0   0   0      0     0    0      0   0                       0    0
## 3    0   0   0      0     0    0      0   0                       0    0
## 4    0   0   0      0     0    0      0   0                       0    0
## 5    0   0   0      0     0    0      0   0                       0    0
## 6    0   0   0      0     0    0      0   0                       0    0
##   chillin highlyhightori summin applefreak demand applewow naturallym33h
## 1       0              0      0          0      0        0             0
## 2       0              0      0          0      0        0             0
## 3       0              0      0          0      0        0             0
## 4       0              0      0          0      0        0             0
## 5       0              0      0          0      0        0             0
## 6       0              0      0          0      0        0             0
##   date thought dad wont 7evenstarz dude even faster nativ processor ram
## 1    0       0   0    0          0    0    0      0     0         0   0
## 2    0       0   0    0          0    0    0      0     0         0   0
## 3    0       0   0    0          0    0    0      0     0         0   0
## 4    0       0   0    0          0    0    0      0     0         0   0
## 5    0       0   0    0          0    0    0      0     0         0   0
## 6    0       0   0    0          0    0    0      0     0         0   0
##   stone jonyiv voic boutthatlif applepick cmb1974 iahhc mcmillen1 mine
## 1     0      0    0           0         0       0     0         0    0
## 2     0      0    0           0         0       0     0         0    0
## 3     0      0    0           0         0       0     0         0    0
## 4     0      0    0           0         0       0     0         0    0
## 5     0      0    0           0         0       0     0         0    0
## 6     0      0    0           0         0       0     0         0    0
##   oithelpdesk pink sprint gabriel grandi lassistenza ringrazio cute
## 1           0    0      0       0      0           0         0    0
## 2           0    0      0       0      0           0         0    0
## 3           0    0      0       0      0           0         0    0
## 4           0    0      0       0      0           0         0    0
## 5           0    0      0       0      0           0         0    0
## 6           0    0      0       0      0           0         0    0
##   missphilly2013 either hardwar model pave transit crazi ebaycheck
## 1              0      0       0     0    0       0     0         0
## 2              0      0       0     0    0       0     0         0
## 3              0      0       0     0    0       0     0         0
## 4              0      0       0     0    0       0     0         0
## 5              0      0       0     0    0       0     0         0
## 6              0      0       0     0    0       0     0         0
##   ebayindia india rip rupe welcom fav httpowlyofqgc doesnt
## 1         0     0   0    0      0   0             0      0
## 2         0     0   0    0      0   0             0      0
## 3         0     0   0    0      0   0             0      0
## 4         0     0   0    0      0   0             0      0
## 5         0     0   0    0      0   0             0      0
## 6         0     0   0    0      0   0             0      0
##   httpbuffly1egmopu malik hidden httpfbme1klk3lvn6 ibeacon advanc congrat
## 1                 0     0      0                 0       0      0       0
## 2                 0     0      0                 0       0      0       0
## 3                 0     0      0                 0       0      0       0
## 4                 0     0      0                 0       0      0       0
## 5                 0     0      0                 0       0      0       0
## 6                 0     0      0                 0       0      0       0
##   group killin opt shini text fix freeee gem keeeruh luckili darl
## 1     0      0   0     0    0   0      0   0       0       0    0
## 2     0      0   0     0    0   0      0   0       0       0    0
## 3     0      0   0     0    0   0      0   0       0       0    0
## 4     0      0   0     0    0   0      0   0       0       0    0
## 5     0      0   0     0    0   0      0   0       0       0    0
## 6     0      0   0     0    0   0      0   0       0       0    0
##   stephenclark httptimehopcom14pf3gc raven ipod ipodplayerpromo
## 1            0                     0     0    0               0
## 2            0                     0     0    0               0
## 3            0                     0     0    0               0
## 4            0                     0     0    0               0
## 5            0                     0     0    0               0
## 6            0                     0     0    0               0
##   promoipodplayerpromo elliedash parentoftheyear compet constant
## 1                    0         0               0      0        0
## 2                    0         0               0      0        0
## 3                    0         0               0      0        0
## 4                    0         0               0      0        0
## 5                    0         0               0      0        0
## 6                    0         0               0      0        0
##   windowsphon yall consum expens feel funni lmao momentsfad darrensproat
## 1           0    0      0      0    0     0    0          0            0
## 2           0    0      0      0    0     0    0          0            0
## 3           0    0      0      0    0     0    0          0            0
## 4           0    0      0      0    0     0    0          0            0
## 5           0    0      0      0    0     0    0          0            0
## 6           0    0      0      0    0     0    0          0            0
##   forget came putin redblack vladimir vladsay ask c7five honest
## 1      0    0     0        0        0       0   0      0      0
## 2      0    0     0        0        0       0   0      0      0
## 3      0    0     0        0        0       0   0      0      0
## 4      0    0     0        0        0       0   0      0      0
## 5      0    0     0        0        0       0   0      0      0
## 6      0    0     0        0        0       0   0      0      0
##   httpjmp19l1clj import question siri firm highlight ipo media post social
## 1              0      0        0    0    0         0   0     0    0      0
## 2              0      0        0    0    0         0   0     0    0      0
## 3              0      0        0    0    0         0   0     0    0      0
## 4              0      0        0    0    0         0   0     0    0      0
## 5              0      0        0    0    0         0   0     0    0      0
## 6              0      0        0    0    0         0   0     0    0      0
##   socialw topic enjoy hahaha iseeblighti pictwittercomym1a0z6rzj twit
## 1       0     0     0      0           0                       0    0
## 2       0     0     0      0           0                       0    0
## 3       0     0     0      0           0                       0    0
## 4       0     0     0      0           0                       0    0
## 5       0     0     0      0           0                       0    0
## 6       0     0     0      0           0                       0    0
##   fingerprintscan listen metal pastic touchscreen 22nd 350 brian danni
## 1               0      0     0      0           0    0   0     0     0
## 2               0      0     0      0           0    0   0     0     0
## 3               0      0     0      0           0    0   0     0     0
## 4               0      0     0      0           0    0   0     0     0
## 5               0      0     0      0           0    0   0     0     0
## 6               0      0     0      0           0    0   0     0     0
##   gazell jonjonnyp employe fire shot stock worri avail creativefutur
## 1      0         0       0    0    0     0     0     0             0
## 2      0         0       0    0    0     0     0     0             0
## 3      0         0       0    0    0     0     0     0             0
## 4      0         0       0    0    0     0     0     0             0
## 5      0         0       0    0    0     0     0     0             0
## 6      0         0       0    0    0     0     0     0             0
##   pictwittercombgfbbqiu0v uwscreat author sinc someplac accordingtonina
## 1                       0        0      0    0        0               0
## 2                       0        0      0    0        0               0
## 3                       0        0      0    0        0               0
## 4                       0        0      0    0        0               0
## 5                       0        0      0    0        0               0
## 6                       0        0      0    0        0               0
##   akemisu shastaann govern infrastructur mishiza natz0711 regul samsungsa
## 1       0         0      0             0       0        0     0         0
## 2       0         0      0             0       0        0     0         0
## 3       0         0      0             0       0        0     0         0
## 4       0         0      0             0       0        0     0         0
## 5       0         0      0             0       0        0     0         0
## 6       0         0      0             0       0        0     0         0
##   brokenhomebutton dammit money hurryup ugh johnathonv httpowlyomrmn rival
## 1                0      0     0       0   0          0             0     0
## 2                0      0     0       0   0          0             0     0
## 3                0      0     0       0   0          0             0     0
## 4                0      0     0       0   0          0             0     0
## 5                0      0     0       0   0          0             0     0
## 6                0      0     0       0   0          0             0     0
##   turn digit fun httponmashto1bq5lp3 massmarket takentooz fulli gadget
## 1    0     0   0                   0          0         0     0      0
## 2    0     0   0                   0          0         0     0      0
## 3    0     0   0                   0          0         0     0      0
## 4    0     0   0                   0          0         0     0      0
## 5    0     0   0                   0          0         0     0      0
## 6    0     0   0                   0          0         0     0      0
##   hear jenniebond1 greater ratio startup valuat laugh peasant chanc diss
## 1    0           0       0     0       0      0     0       0     0    0
## 2    0           0       0     0       0      0     0       0     0    0
## 3    0           0       0     0       0      0     0       0     0    0
## 4    0           0       0     0       0      0     0       0     0    0
## 5    0           0       0     0       0      0     0       0     0    0
## 6    0           0       0     0       0      0     0       0     0    0
##   dumb orig eltonjohn unreal httpowlyi38y2b darnit don1za lol brother ctl
## 1    0    0         0      0              0      0      0   0       0   0
## 2    0    0         0      0              0      0      0   0       0   0
## 3    0    0         0      0              0      0      0   0       0   0
## 4    0    0         0      0              0      0      0   0       0   0
## 5    0    0         0      0              0      0      0   0       0   0
## 6    0    0         0      0              0      0      0   0       0   0
##   httpowlyopeax koch libcrib prepar progress uniteblu bubbl imessag motion
## 1             0    0       0      0        0        0     0       0      0
## 2             0    0       0      0        0        0     0       0      0
## 3             0    0       0      0        0        0     0       0      0
## 4             0    0       0      0        0        0     0       0      0
## 5             0    0       0      0        0        0     0       0      0
## 6             0    0       0      0        0        0     0       0      0
##   red sender shopbreamal mikey realli tell boomheadshot gotta man nfc song
## 1   0      0           0     0      0    0            0     0   0   0    0
## 2   0      0           0     0      0    0            0     0   0   0    0
## 3   0      0           0     0      0    0            0     0   0   0    0
## 4   0      0           0     0      0    0            0     0   0   0    0
## 5   0      0           0     0      0    0            0     0   0   0    0
## 6   0      0           0     0      0    0            0     0   0   0    0
##   tap woe appleev applenew ishoes5 everyth httpbitly14mbzlm sharpmagazin
## 1   0   0       0        0       0       0                0            0
## 2   0   0       0        0       0       0                0            0
## 3   0   0       0        0       0       0                0            0
## 4   0   0       0        0       0       0                0            0
## 5   0   0       0        0       0       0                0            0
## 6   0   0       0        0       0       0                0            0
##   isfarahmad lumia potasiyam thehoneymad glossi macbookpro matt option
## 1          0     0         0           0      0          0    0      0
## 2          0     0         0           0      0          0    0      0
## 3          0     0         0           0      0          0    0      0
## 4          0     0         0           0      0          0    0      0
## 5          0     0         0           0      0          0    0      0
## 6          0     0         0           0      0          0    0      0
##   appar ghosttgam london tehgreenmc battl epic geekhelpinghand
## 1     0         0      0          0     0    0               0
## 2     0         0      0          0     0    0               0
## 3     0         0      0          0     0    0               0
## 4     0         0      0          0     0    0               0
## 5     0         0      0          0     0    0               0
## 6     0         0      0          0     0    0               0
##   httpbuffly187kjcd applecar assist dept florida polic zimmerman impati
## 1                 0        0      0    0       0     0         0      0
## 2                 0        0      0    0       0     0         0      0
## 3                 0        0      0    0       0     0         0      0
## 4                 0        0      0    0       0     0         0      0
## 5                 0        0      0    0       0     0         0      0
## 6                 0        0      0    0       0     0         0      0
##   wantappl boydscot chat deliveri kthank rush snap aim classic end
## 1        0        0    0        0      0    0    0   0       0   0
## 2        0        0    0        0      0    0    0   0       0   0
## 3        0        0    0        0      0    0    0   0       0   0
## 4        0        0    0        0      0    0    0   0       0   0
## 5        0        0    0        0      0    0    0   0       0   0
## 6        0        0    0        0      0    0    0   0       0   0
##   httpdisqus8f4kw3 wayn wayneshurt crisp hasnt pie piephon coverag wherev
## 1                0    0          0     0     0   0       0       0      0
## 2                0    0          0     0     0   0       0       0      0
## 3                0    0          0     0     0   0       0       0      0
## 4                0    0          0     0     0   0       0       0      0
## 5                0    0          0     0     0   0       0       0      0
## 6                0    0          0     0     0   0       0       0      0
##   whoever adambain term thepartycow winthemo brenberryblast echrofon
## 1       0        0    0           0        0              0        0
## 2       0        0    0           0        0              0        0
## 3       0        0    0           0        0              0        0
## 4       0        0    0           0        0              0        0
## 5       0        0    0           0        0              0        0
## 6       0        0    0           0        0              0        0
##   feedback gcyc half repli whatsapp cisco donat httpowlyobk5i oracl
## 1        0    0    0     0        0     0     0             0     0
## 2        0    0    0     0        0     0     0             0     0
## 3        0    0    0     0        0     0     0             0     0
## 4        0    0    0     0        0     0     0             0     0
## 5        0    0    0     0        0     0     0             0     0
## 6        0    0    0     0        0     0     0             0     0
##   salesforc promo waterproof ironman male sincer brown emoticon face harm
## 1         0     0          0       0    0      0     0        0    0    0
## 2         0     0          0       0    0      0     0        0    0    0
## 3         0     0          0       0    0      0     0        0    0    0
## 4         0     0          0       0    0      0     0        0    0    0
## 5         0     0          0       0    0      0     0        0    0    0
## 6         0     0          0       0    0      0     0        0    0    0
##   mean pls ppl thx timcookappleceo axi conserv construct destruct liber
## 1    0   0   0   0               0   0       0         0        0     0
## 2    0   0   0   0               0   0       0         0        0     0
## 3    0   0   0   0               0   0       0         0        0     0
## 4    0   0   0   0               0   0       0         0        0     0
## 5    0   0   0   0               0   0       0         0        0     0
## 6    0   0   0   0               0   0       0         0        0     0
##   qotd quot stevejob wordsofwisdom 20th httpbitly1ahyzmo septemb mini
## 1    0    0        0             0    0                0       0    0
## 2    0    0        0             0    0                0       0    0
## 3    0    0        0             0    0                0       0    0
## 4    0    0        0             0    0                0       0    0
## 5    0    0        0             0    0                0       0    0
## 6    0    0        0             0    0                0       0    0
##   pastel pschiller version sixtyfour slam thirtytwo bic compart fluid
## 1      0         0       0         0    0         0   0       0     0
## 2      0         0       0         0    0         0   0       0     0
## 3      0         0       0         0    0         0   0       0     0
## 4      0         0       0         0    0         0   0       0     0
## 5      0         0       0         0    0         0   0       0     0
## 6      0         0       0         0    0         0   0       0     0
##   lighter refil zippo built cnnmoney cover hall httppb2co8ppx5 iso7 town
## 1       0     0     0     0        0     0    0              0    0    0
## 2       0     0     0     0        0     0    0              0    0    0
## 3       0     0     0     0        0     0    0              0    0    0
## 4       0     0     0     0        0     0    0              0    0    0
## 5       0     0     0     0        0     0    0              0    0    0
## 6       0     0     0     0        0     0    0              0    0    0
##   tomlinshortcak bezoekj nieuwsgierigheid nyc onz plezier somemonteur veel
## 1              0       0                0   0   0       0           0    0
## 2              0       0                0   0   0       0           0    0
## 3              0       0                0   0   0       0           0    0
## 4              0       0                0   0   0       0           0    0
## 5              0       0                0   0   0       0           0    0
## 6              0       0                0   0   0       0           0    0
##   wensen wij level6 ginianight left safari highsnobieti httpbitly1earplz
## 1      0   0      0          0    0      0            0                0
## 2      0   0      0          0    0      0            0                0
## 3      0   0      0          0    0      0            0                0
## 4      0   0      0          0    0      0            0                0
## 5      0   0      0          0    0      0            0                0
## 6      0   0      0          0    0      0            0                0
##   pictwittercompoegs09trbalifbladz pictwittercomxs8q9yklaz clinton creek
## 1                                0                       0       0     0
## 2                                0                       0       0     0
## 3                                0                       0       0     0
## 4                                0                       0       0     0
## 5                                0                       0       0     0
## 6                                0                       0       0     0
##   http4sqcom17ylum partridg township alcagimizi hangisini kanka sasirdik
## 1                0        0        0          0         0     0        0
## 2                0        0        0          0         0     0        0
## 3                0        0        0          0         0     0        0
## 4                0        0        0          0         0     0        0
## 5                0        0        0          0         0     0        0
## 6                0        0        0          0         0     0        0
##   darthdream oyincansoda team cloth happier might yer
## 1          0           0    0     0       0     0   0
## 2          0           0    0     0       0     0   0
## 3          0           0    0     0       0     0   0
## 4          0           0    0     0       0     0   0
## 5          0           0    0     0       0     0   0
## 6          0           0    0     0       0     0   0
##   pictwittercomdpc2fg8jdv warn copi fluzziti ifluzzth
## 1                       0    0    0        0        0
## 2                       0    0    0        0        0
## 3                       0    0    0        0        0
## 4                       0    0    0        0        0
## 5                       0    0    0        0        0
## 6                       0    0    0        0        0
##   pictwittercom00ddso3i0o ayo fact tongu airplay makeithappen maverick abt
## 1                       0   0    0     0       0            0        0   0
## 2                       0   0    0     0       0            0        0   0
## 3                       0   0    0     0       0            0        0   0
## 4                       0   0    0     0       0            0        0   0
## 5                       0   0    0     0       0            0        0   0
## 6                       0   0    0     0       0            0        0   0
##   edg opco pictwittercomovtw9d1vaj sysco visit wassup blocklik hurri
## 1   0    0                       0     0     0      0        0     0
## 2   0    0                       0     0     0      0        0     0
## 3   0    0                       0     0     0      0        0     0
## 4   0    0                       0     0     0      0        0     0
## 5   0    0                       0     0     0      0        0     0
## 6   0    0                       0     0     0      0        0     0
##   realexpay talkingpay cecilialui hipstamat lifeinlofiblog oggl 18th
## 1         0          0          0         0              0    0    0
## 2         0          0          0         0              0    0    0
## 3         0          0          0         0              0    0    0
## 4         0          0          0         0              0    0    0
## 5         0          0          0         0              0    0    0
## 6         0          0          0         0              0    0    0
##   the1ndonly365 nava520 httponwsjcom186mzi1 ohwel place seattl wsj catscab
## 1             0       0                   0     0     0      0   0       0
## 2             0       0                   0     0     0      0   0       0
## 3             0       0                   0     0     0      0   0       0
## 4             0       0                   0     0     0      0   0       0
## 5             0       0                   0     0     0      0   0       0
## 6             0       0                   0     0     0      0   0       0
##   download uksga universityofki biz departur expand httpowlyomtd3 lowcost
## 1        0     0              0   0        0      0             0       0
## 2        0     0              0   0        0      0             0       0
## 3        0     0              0   0        0      0             0       0
## 4        0     0              0   0        0      0             0       0
## 5        0     0              0   0        0      0             0       0
## 6        0     0              0   0        0      0             0       0
##   simpler dije era frmoisesfr los muy nuevo que ser van grab million must
## 1       0    0   0          0   0   0     0   0   0   0    0       0    0
## 2       0    0   0          0   0   0     0   0   0   0    0       0    0
## 3       0    0   0          0   0   0     0   0   0   0    0       0    0
## 4       0    0   0          0   0   0     0   0   0   0    0       0    0
## 5       0    0   0          0   0   0     0   0   0   0    0       0    0
## 6       0    0   0          0   0   0     0   0   0   0    0       0    0
##   person dereksand frickfuent 65min express facial lanceulanoff special
## 1      0         0          0     0       0      0            0       0
## 2      0         0          0     0       0      0            0       0
## 3      0         0          0     0       0      0            0       0
## 4      0         0          0     0       0      0            0       0
## 5      0         0          0     0       0      0            0       0
## 6      0         0          0     0       0      0            0       0
##   wile io7 piec def kind sept20 fishbat httpbitly17wfujr revolution
## 1    0   0    0   0    0      0       0                0          0
## 2    0   0    0   0    0      0       0                0          0
## 3    0   0    0   0    0      0       0                0          0
## 4    0   0    0   0    0      0       0                0          0
## 5    0   0    0   0    0      0       0                0          0
## 6    0   0    0   0    0      0       0                0          0
##   collabor lfw mcstyle pictwittercomueyrkwl6i6 share
## 1        0   0       0                       0     0
## 2        0   0       0                       0     0
## 3        0   0       0                       0     0
## 4        0   0       0                       0     0
## 5        0   0       0                       0     0
## 6        0   0       0                       0     0
##   pictwittercomymgc0ugkoa adornar clave colorida funda gif
## 1                       0       0     0        0     0   0
## 2                       0       0     0        0     0   0
## 3                       0       0     0        0     0   0
## 4                       0       0     0        0     0   0
## 5                       0       0     0        0     0   0
## 6                       0       0     0        0     0   0
##   httpbitly1e6hrok nuestro nueva para proteg resumen csotourna dreamz
## 1                0       0     0    0      0       0         0      0
## 2                0       0     0    0      0       0         0      0
## 3                0       0     0    0      0       0         0      0
## 4                0       0     0    0      0       0         0      0
## 5                0       0     0    0      0       0         0      0
## 6                0       0     0    0      0       0         0      0
##   segar taman bar notif tweet drive knew prodmktg someon heeey heey
## 1     0     0   0     0     0     0    0        0      0     0    0
## 2     0     0   0     0     0     0    0        0      0     0    0
## 3     0     0   0     0     0     0    0        0      0     0    0
## 4     0     0   0     0     0     0    0        0      0     0    0
## 5     0     0   0     0     0     0    0        0      0     0    0
## 6     0     0   0     0     0     0    0        0      0     0    0
##   pictwittercom50fidqhmrd autocorrect onto vagin aller multi vive
## 1                       0           0    0     0     0     0    0
## 2                       0           0    0     0     0     0    0
## 3                       0           0    0     0     0     0    0
## 4                       0           0    0     0     0     0    0
## 5                       0           0    0     0     0     0    0
## 6                       0           0    0     0     0     0    0
##   chunherena note sarcasm small 2014 httpowlyopzc5 newsflash springsumm
## 1          0    0       0     0    0             0         0          0
## 2          0    0       0     0    0             0         0          0
## 3          0    0       0     0    0             0         0          0
## 4          0    0       0     0    0             0         0          0
## 5          0    0       0     0    0             0         0          0
## 6          0    0       0     0    0             0         0          0
##   long quiilucru huhuhuhuhu oonga paasa sheisyanni dan doigt inviol loeil
## 1    0         0          0     0     0          0   0     0      0     0
## 2    0         0          0     0     0          0   0     0      0     0
## 3    0         0          0     0     0          0   0     0      0     0
## 4    0         0          0     0     0          0   0     0      0     0
## 5    0         0          0     0     0          0   0     0      0     0
## 6    0         0          0     0     0          0   0     0      0     0
##   met 3rd broke pair descola logo porra detail httpfbme2tg09dhvb
## 1   0   0     0    0       0    0     0      0                 0
## 2   0   0     0    0       0    0     0      0                 0
## 3   0   0     0    0       0    0     0      0                 0
## 4   0   0     0    0       0    0     0      0                 0
## 5   0   0     0    0       0    0     0      0                 0
## 6   0   0     0    0       0    0     0      0                 0
##   manolomata peor bigger ble burgesg deal georg immers mulderc wonder
## 1          0    0      0   0       0    0     0      0       0      0
## 2          0    0      0   0       0    0     0      0       0      0
## 3          0    0      0   0       0    0     0      0       0      0
## 4          0    0      0   0       0    0     0      0       0      0
## 5          0    0      0   0       0    0     0      0       0      0
## 6          0    0      0   0       0    0     0      0       0      0
##   toddcox ayudamovistarv bought environ forb preedward snowden whack buck
## 1       0              0      0       0    0         0       0     0    0
## 2       0              0      0       0    0         0       0     0    0
## 3       0              0      0       0    0         0       0     0    0
## 4       0              0      0       0    0         0       0     0    0
## 5       0              0      0       0    0         0       0     0    0
## 6       0              0      0       0    0         0       0     0    0
##   piss 0909 0910 320k 86m ball count mileycyrus mindblowngif sourc view
## 1    0    0    0    0   0    0     0          0            0     0    0
## 2    0    0    0    0   0    0     0          0            0     0    0
## 3    0    0    0    0   0    0     0          0            0     0    0
## 4    0    0    0    0   0    0     0          0            0     0    0
## 5    0    0    0    0   0    0     0          0            0     0    0
## 6    0    0    0    0   0    0     0          0            0     0    0
##   wreck youtub account condescens httphtlyoptog photog photographi quiet
## 1     0      0       0          0             0      0           0     0
## 2     0      0       0          0             0      0           0     0
## 3     0      0       0          0             0      0           0     0
## 4     0      0       0          0             0      0           0     0
## 5     0      0       0          0             0      0           0     0
## 6     0      0       0          0             0      0           0     0
##   coupon daycar pictwittercomcm4xccfeqh 5sc confus imo meant russian 884
## 1      0      0                       0   0      0   0     0       0   0
## 2      0      0                       0   0      0   0     0       0   0
## 3      0      0                       0   0      0   0     0       0   0
## 4      0      0                       0   0      0   0     0       0   0
## 5      0      0                       0   0      0   0     0       0   0
## 6      0      0                       0   0      0   0     0       0   0
##   baixar consigo digam jogo livr size tenho 21st centuri expect irobot
## 1      0       0     0    0    0    0     0    0       0      0      0
## 2      0       0     0    0    0    0     0    0       0      0      0
## 3      0       0     0    0    0    0     0    0       0      0      0
## 4      0       0     0    0    0    0     0    0       0      0      0
## 5      0       0     0    0    0    0     0    0       0      0      0
## 6      0       0     0    0    0    0     0    0       0      0      0
##   robot where ari beidzot bus isti jaizlasa jki laikam pasludinaja
## 1     0     0   0       0   0    0        0   0      0           0
## 2     0     0   0       0   0    0        0   0      0           0
## 3     0     0   0       0   0    0        0   0      0           0
## 4     0     0   0       0   0    0        0   0      0           0
## 5     0     0   0       0   0    0        0   0      0           0
## 6     0     0   0       0   0    0        0   0      0           0
##   ticigajiem httpbuffly19ksdcr switch dougkass expert fail marketshar
## 1          0                 0      0        0      0    0          0
## 2          0                 0      0        0      0    0          0
## 3          0                 0      0        0      0    0          0
## 4          0                 0      0        0      0    0          0
## 5          0                 0      0        0      0    0          0
## 6          0                 0      0        0      0    0          0
##   profit prove realiz valu dislik httpbuffly1emp3xh shutupariell cut
## 1      0     0      0    0      0                 0            0   0
## 2      0     0      0    0      0                 0            0   0
## 3      0     0      0    0      0                 0            0   0
## 4      0     0      0    0      0                 0            0   0
## 5      0     0      0    0      0                 0            0   0
## 6      0     0      0    0      0                 0            0   0
##   french httpbitly1eokgf5 postproduct second boa nossa porem sempr tao
## 1      0                0           0      0   0     0     0     0   0
## 2      0                0           0      0   0     0     0     0   0
## 3      0                0           0      0   0     0     0     0   0
## 4      0                0           0      0   0     0     0     0   0
## 5      0                0           0      0   0     0     0     0   0
## 6      0                0           0      0   0     0     0     0   0
##   trava electrafood hah liter cheap cheapen renam stand car customerexplab
## 1     0           0   0     0     0       0     0     0   0              0
## 2     0           0   0     0     0       0     0     0   0              0
## 3     0           0   0     0     0       0     0     0   0              0
## 4     0           0   0     0     0       0     0     0   0              0
## 5     0           0   0     0     0       0     0     0   0              0
## 6     0           0   0     0     0       0     0     0   0              0
##   figur httpgoogl6jqkcv park test httpreutrs15tharz insight mainstream
## 1     0               0    0    0                 0       0          0
## 2     0               0    0    0                 0       0          0
## 3     0               0    0    0                 0       0          0
## 4     0               0    0    0                 0       0          0
## 5     0               0    0    0                 0       0          0
## 6     0               0    0    0                 0       0          0
##   trigger httpyoutube1siwez9haba parodi sagarkamesh crypto effect
## 1       0                      0      0           0      0      0
## 2       0                      0      0           0      0      0
## 3       0                      0      0           0      0      0
## 4       0                      0      0           0      0      0
## 5       0                      0      0           0      0      0
## 6       0                      0      0           0      0      0
##   httplnisbitly3hja legal marciahofmann privaci cleveland closest
## 1                 0     0             0       0         0       0
## 2                 0     0             0       0         0       0
## 3                 0     0             0       0         0       0
## 4                 0     0             0       0         0       0
## 5                 0     0             0       0         0       0
## 6                 0     0             0       0         0       0
##   httpbitlya83mmb postproducti bateria dure pedir pido solo taaaaanto una
## 1               0            0       0    0     0    0    0         0   0
## 2               0            0       0    0     0    0    0         0   0
## 3               0            0       0    0     0    0    0         0   0
## 4               0            0       0    0     0    0    0         0   0
## 5               0            0       0    0     0    0    0         0   0
## 6               0            0       0    0     0    0    0         0   0
##   coisinha gato inventem melhor parem uma backchannel birth child commerci
## 1        0    0        0      0     0   0           0     0     0        0
## 2        0    0        0      0     0   0           0     0     0        0
## 3        0    0        0      0     0   0           0     0     0        0
## 4        0    0        0      0     0   0           0     0     0        0
## 5        0    0        0      0     0   0           0     0     0        0
## 6        0    0        0      0     0   0           0     0     0        0
##   facetim japan soldier diabet doc dsma recogn type1 burst dougrtequan
## 1       0     0       0      0   0    0      0     0     0           0
## 2       0     0       0      0   0    0      0     0     0           0
## 3       0     0       0      0   0    0      0     0     0           0
## 4       0     0       0      0   0    0      0     0     0           0
## 5       0     0       0      0   0    0      0     0     0           0
## 6       0     0       0      0   0    0      0     0     0           0
##   loset prior subotag winner httpbitlyapplesecret remain strategi tightlip
## 1     0     0       0      0                    0      0        0        0
## 2     0     0       0      0                    0      0        0        0
## 3     0     0       0      0                    0      0        0        0
## 4     0     0       0      0                    0      0        0        0
## 5     0     0       0      0                    0      0        0        0
## 6     0     0       0      0                    0      0        0        0
##   messeng photorateapp worst anong mga nangyari niyo putangina devis
## 1       0            0     0     0   0        0    0         0     0
## 2       0            0     0     0   0        0    0         0     0
## 3       0            0     0     0   0        0    0         0     0
## 4       0            0     0     0   0        0    0         0     0
## 5       0            0     0     0   0        0    0         0     0
## 6       0            0     0     0   0        0    0         0     0
##   ideasamsungorappl lifebatteri mayb solar solariphon buyer cnet
## 1                 0           0    0     0          0     0    0
## 2                 0           0    0     0          0     0    0
## 3                 0           0    0     0          0     0    0
## 4                 0           0    0     0          0     0    0
## 5                 0           0    0     0          0     0    0
## 6                 0           0    0     0          0     0    0
##   httpowly22icvt ilif 244tsuyoponzu auto pictwittercomyehjtsjkmj 5sdo
## 1              0    0             0    0                       0    0
## 2              0    0             0    0                       0    0
## 3              0    0             0    0                       0    0
## 4              0    0             0    0                       0    0
## 5              0    0             0    0                       0    0
## 6              0    0             0    0                       0    0
##   nawwww neither nasa analys fysiokerkrad goed graag grati helpen
## 1      0       0    0      0            0    0     0     0      0
## 2      0       0    0      0            0    0     0     0      0
## 3      0       0    0      0            0    0     0     0      0
## 4      0       0    0      0            0    0     0     0      0
## 5      0       0    0      0            0    0     0     0      0
## 6      0       0    0      0            0    0     0     0      0
##   httpwwwdatarecuperatienl iviziapr middag recoveri wenst willen algilari
## 1                        0        0      0        0     0      0        0
## 2                        0        0      0        0     0      0        0
## 3                        0        0      0        0     0      0        0
## 4                        0        0      0        0     0      0        0
## 5                        0        0      0        0     0      0        0
## 6                        0        0      0        0     0      0        0
##   bir degil etkileyecek havasinda mediacat niyetind ortada pek rekabet
## 1   0     0           0         0        0        0      0   0       0
## 2   0     0           0         0        0        0      0   0       0
## 3   0     0           0         0        0        0      0   0       0
## 4   0     0           0         0        0        0      0   0       0
## 5   0     0           0         0        0        0      0   0       0
## 6   0     0           0         0        0        0      0   0       0
##   ucuz yoksanirim choisir lequel les nouveau oui morrer resistir tentei
## 1    0          0       0      0   0       0   0      0        0      0
## 2    0          0       0      0   0       0   0      0        0      0
## 3    0          0       0      0   0       0   0      0        0      0
## 4    0          0       0      0   0       0   0      0        0      0
## 5    0          0       0      0   0       0   0      0        0      0
## 6    0          0       0      0   0       0   0      0        0      0
##   vou 075 llombardo007 outon reales ahi por sacaba tambien tsss usted
## 1   0   0            0     0      0   0   0      0       0    0     0
## 2   0   0            0     0      0   0   0      0       0    0     0
## 3   0   0            0     0      0   0   0      0       0    0     0
## 4   0   0            0     0      0   0   0      0       0    0     0
## 5   0   0            0     0      0   0   0      0       0    0     0
## 6   0   0            0     0      0   0   0      0       0    0     0
##   deixava nao orra barackobama pictwittercom1r1qt0tg1d aint word dana
## 1       0   0    0           0                       0    0    0    0
## 2       0   0    0           0                       0    0    0    0
## 3       0   0    0           0                       0    0    0    0
## 4       0   0    0           0                       0    0    0    0
## 5       0   0    0           0                       0    0    0    0
## 6       0   0    0           0                       0    0    0    0
##   kaldirsana kilitliyken olayini tek tik blackberryq5 hello kat kit hurt
## 1          0           0       0   0   0            0     0   0   0    0
## 2          0           0       0   0   0            0     0   0   0    0
## 3          0           0       0   0   0            0     0   0   0    0
## 4          0           0       0   0   0            0     0   0   0    0
## 5          0           0       0   0   0            0     0   0   0    0
## 6          0           0       0   0   0            0     0   0   0    0
##   nobodi mute pictwittercomz7vary46yb select switcher white yellow alip
## 1      0    0                       0      0        0     0      0    0
## 2      0    0                       0      0        0     0      0    0
## 3      0    0                       0      0        0     0      0    0
## 4      0    0                       0      0        0     0      0    0
## 5      0    0                       0      0        0     0      0    0
## 6      0    0                       0      0        0     0      0    0
##   dayiya ettim hasat hurmetl olmus telefonumu tesekur yenisini
## 1      0     0     0       0     0          0       0        0
## 2      0     0     0       0     0          0       0        0
## 3      0     0     0       0     0          0       0        0
## 4      0     0     0       0     0          0       0        0
## 5      0     0     0       0     0          0       0        0
## 6      0     0     0       0     0          0       0        0
##   yolluomussun michaeljordan thephenom007 sept10 unveil andymiah
## 1            0             0            0      0      0        0
## 2            0             0            0      0      0        0
## 3            0             0            0      0      0        0
## 4            0             0            0      0      0        0
## 5            0             0            0      0      0        0
## 6            0             0            0      0      0        0
##   pictwittercomlbxctxbc8g caus clear histori movistar bro kid peac sorri
## 1                       0    0     0       0        0   0   0    0     0
## 2                       0    0     0       0        0   0   0    0     0
## 3                       0    0     0       0        0   0   0    0     0
## 4                       0    0     0       0        0   0   0    0     0
## 5                       0    0     0       0        0   0   0    0     0
## 6                       0    0     0       0        0   0   0    0     0
##   card discontinu httphtlyopzpr recommend tryna although notch
## 1    0          0             0         0     0        0     0
## 2    0          0             0         0     0        0     0
## 3    0          0             0         0     0        0     0
## 4    0          0             0         0     0        0     0
## 5    0          0             0         0     0        0     0
## 6    0          0             0         0     0        0     0
##   strictlygeeki took yeah accion bajan las telecelchitchat yourthought
## 1             0    0    0      0     0   0               0           0
## 2             0    0    0      0     0   0               0           0
## 3             0    0    0      0     0   0               0           0
## 4             0    0    0      0     0   0               0           0
## 5             0    0    0      0     0   0               0           0
## 6             0    0    0      0     0   0               0           0
##   zifmstereo anyway inspir kinda sup httpowlyolcgw rasnaj huhu jmflooor
## 1          0      0      0     0   0             0      0    0        0
## 2          0      0      0     0   0             0      0    0        0
## 3          0      0      0     0   0             0      0    0        0
## 4          0      0      0     0   0             0      0    0        0
## 5          0      0      0     0   0             0      0    0        0
## 6          0      0      0     0   0             0      0    0        0
##   onga pictwittercomndim3qbsb2 somebodi americascup cours emiratesteamnz
## 1    0                       0        0           0     0              0
## 2    0                       0        0           0     0              0
## 3    0                       0        0           0     0              0
## 4    0                       0        0           0     0              0
## 5    0                       0        0           0     0              0
## 6    0                       0        0           0     0              0
##   mirror 22000 san month though nokiaus ooooooooooooooo tiagouss add power
## 1      0     0   0     0      0       0               0        0   0     0
## 2      0     0   0     0      0       0               0        0   0     0
## 3      0     0   0     0      0       0               0        0   0     0
## 4      0     0   0     0      0       0               0        0   0     0
## 5      0     0   0     0      0       0               0        0   0     0
## 6      0     0   0     0      0       0               0        0   0     0
##   steal httpyoutubepfsj3ilxifu scene frent golazo lanzamiento mejor
## 1     0                      0     0     0      0           0     0
## 2     0                      0     0     0      0           0     0
## 3     0                      0     0     0      0           0     0
## 4     0                      0     0     0      0           0     0
## 5     0                      0     0     0      0           0     0
## 6     0                      0     0     0      0           0     0
##   nokiaco pictwittercomdxzz9vibo7 reaccion visto 550 amerikada bilinmez
## 1       0                       0        0     0   0         0        0
## 2       0                       0        0     0   0         0        0
## 3       0                       0        0     0   0         0        0
## 4       0                       0        0     0   0         0        0
## 5       0                       0        0     0   0         0        0
## 6       0                       0        0     0   0         0        0
##   diyip dolara haci inc kimi sation sayin sen telefonu turkiyed yapicam
## 1     0      0    0   0    0      0     0   0        0        0       0
## 2     0      0    0   0    0      0     0   0        0        0       0
## 3     0      0    0   0    0      0     0   0        0        0       0
## 4     0      0    0   0    0      0     0   0        0        0       0
## 5     0      0    0   0    0      0     0   0        0        0       0
## 6     0      0    0   0    0      0     0   0        0        0       0
##   yiyon display thunderbolt iltifat iyi seklidir taklit algo navidad
## 1     0       0           0       0   0        0      0    0       0
## 2     0       0           0       0   0        0      0    0       0
## 3     0       0           0       0   0        0      0    0       0
## 4     0       0           0       0   0        0      0    0       0
## 5     0       0           0       0   0        0      0    0       0
## 6     0       0           0       0   0        0      0    0       0
##   regalenm china httpcnnmonie1eur90v regain pictwittercomm4o7aewgr3
## 1        0     0                   0      0                       0
## 2        0     0                   0      0                       0
## 3        0     0                   0      0                       0
## 4        0     0                   0      0                       0
## 5        0     0                   0      0                       0
## 6        0     0                   0      0                       0
##   southern alum apa artcenteralum carshar httpbitlywkjhyg igo nih
## 1        0    0   0             0       0               0   0   0
## 2        0    0   0             0       0               0   0   0
## 3        0    0   0             0       0               0   0   0
## 4        0    0   0             0       0               0   0   0
## 5        0    0   0             0       0               0   0   0
## 6        0    0   0             0       0               0   0   0
##   pictwittercomyzh6o5yzlx project wah 3gs compar galaxi theamazingsimon
## 1                       0       0   0   0      0      0               0
## 2                       0       0   0   0      0      0               0
## 3                       0       0   0   0      0      0               0
## 4                       0       0   0   0      0      0               0
## 5                       0       0   0   0      0      0               0
## 6                       0       0   0   0      0      0               0
##   unfavour 4eva quick pictwittercom7etawoyaut glitch email arianagrand
## 1        0    0     0                       0      0     0           0
## 2        0    0     0                       0      0     0           0
## 3        0    0     0                       0      0     0           0
## 4        0    0     0                       0      0     0           0
## 5        0    0     0                       0      0     0           0
## 6        0    0     0                       0      0     0           0
##   ellenpompeo katewalsh oth starworldindia 100m hosain httpgoogl3uip5u
## 1           0         0   0              0    0      0               0
## 2           0         0   0              0    0      0               0
## 3           0         0   0              0    0      0               0
## 4           0         0   0              0    0      0               0
## 5           0         0   0              0    0      0               0
## 6           0         0   0              0    0      0               0
##   jawbon unexpect alfredopeta definitivament del hay lmuia manoloe69 paso
## 1      0        0           0              0   0   0     0         0    0
## 2      0        0           0              0   0   0     0         0    0
## 3      0        0           0              0   0   0     0         0    0
## 4      0        0           0              0   0   0     0         0    0
## 5      0        0           0              0   0   0     0         0    0
## 6      0        0           0              0   0   0     0         0    0
##   pero yatayoy ant chino dispositivo httpowlyoooo5 sorprenden usuario
## 1    0       0   0     0           0             0          0       0
## 2    0       0   0     0           0             0          0       0
## 3    0       0   0     0           0             0          0       0
## 4    0       0   0     0           0             0          0       0
## 5    0       0   0     0           0             0          0       0
## 6    0       0   0     0           0             0          0       0
##   rather beja nike race sauto wlh 1085 apportando correzionifreelysoft
## 1      0    0    0    0     0   0    0          0                    0
## 2      0    0    0    0     0   0    0          0                    0
## 3      0    0    0    0     0   0    0          0                    0
## 4      0    0    0    0     0   0    0          0                    0
## 5      0    0    0    0     0   0    0          0                    0
## 6      0    0    0    0     0   0    0          0                    0
##   laggiornamento numeros pictwittercomrauzjtpf60 rilascia hire
## 1              0       0                       0        0    0
## 2              0       0                       0        0    0
## 3              0       0                       0        0    0
## 4              0       0                       0        0    0
## 5              0       0                       0        0    0
## 6              0       0                       0        0    0
##   httpflipitwphdk ceo httpbloombg1d6rhg6 loom mcandrew pandora veteran
## 1               0   0                  0    0        0       0       0
## 2               0   0                  0    0        0       0       0
## 3               0   0                  0    0        0       0       0
## 4               0   0                  0    0        0       0       0
## 5               0   0                  0    0        0       0       0
## 6               0   0                  0    0        0       0       0
##   httpsdrvms17vnhha wp8 wpcentral pictwittercomxdkgjfx6cn
## 1                 0   0         0                       0
## 2                 0   0         0                       0
## 3                 0   0         0                       0
## 4                 0   0         0                       0
## 5                 0   0         0                       0
## 6                 0   0         0                       0
##   http4sqcom18lrqgl pculnan ross info paperworkpati previoustweet simultan
## 1                 0       0    0    0             0             0        0
## 2                 0       0    0    0             0             0        0
## 3                 0       0    0    0             0             0        0
## 4                 0       0    0    0             0             0        0
## 5                 0       0    0    0             0             0        0
## 6                 0       0    0    0             0             0        0
##   workingirl coprocessor healthcar httphealthcareoperationsmanagementnet
## 1          0           0         0                                     0
## 2          0           0         0                                     0
## 3          0           0         0                                     0
## 4          0           0         0                                     0
## 5          0           0         0                                     0
## 6          0           0         0                                     0
##   mhealth there att banca culpa josiando lisbeld manita mizbuchito rota
## 1       0     0   0     0     0        0       0      0          0    0
## 2       0     0   0     0     0        0       0      0          0    0
## 3       0     0   0     0     0        0       0      0          0    0
## 4       0     0   0     0     0        0       0      0          0    0
## 5       0     0   0     0     0        0       0      0          0    0
## 6       0     0   0     0     0        0       0      0          0    0
##   tengan toi beard internet invent bretfordmanf cart deployment13
## 1      0   0     0        0      0            0    0            0
## 2      0   0     0        0      0            0    0            0
## 3      0   0     0        0      0            0    0            0
## 4      0   0     0        0      0            0    0            0
## 5      0   0     0        0      0            0    0            0
## 6      0   0     0        0      0            0    0            0
##   httpcolefarrellmeproject materi mbair packag retrofit reus fullsiz odd
## 1                        0      0     0      0        0    0       0   0
## 2                        0      0     0      0        0    0       0   0
## 3                        0      0     0      0        0    0       0   0
## 4                        0      0     0      0        0    0       0   0
## 5                        0      0     0      0        0    0       0   0
## 6                        0      0     0      0        0    0       0   0
##   comput kemalnad mrpumpkinslic randominventor xxswegboy069xx
## 1      0        0             0              0              0
## 2      0        0             0              0              0
## 3      0        0             0              0              0
## 4      0        0             0              0              0
## 5      0        0             0              0              0
## 6      0        0             0              0              0
##   securitycompass aggress channel discount httpowlyop1xh thru unit volum
## 1               0       0       0        0             0    0    0     0
## 2               0       0       0        0             0    0    0     0
## 3               0       0       0        0             0    0    0     0
## 4               0       0       0        0             0    0    0     0
## 5               0       0       0        0             0    0    0     0
## 6               0       0       0        0             0    0    0     0
##   walmart appleberri berri blackappl gut heck keyboard mhugh brengen een
## 1       0          0     0         0   0    0        0     0       0   0
## 2       0          0     0         0   0    0        0     0       0   0
## 3       0          0     0         0   0    0        0     0       0   0
## 4       0          0     0         0   0    0        0     0       0   0
## 5       0          0     0         0   0    0        0     0       0   0
## 6       0          0     0         0   0    0        0     0       0   0
##   europa fysiek htc markt omdat ontvolgen soni toetsenbord verdommen lan
## 1      0      0   0     0     0         0    0           0         0   0
## 2      0      0   0     0     0         0    0           0         0   0
## 3      0      0   0     0     0         0    0           0         0   0
## 4      0      0   0     0     0         0    0           0         0   0
## 5      0      0   0     0     0         0    0           0         0   0
## 6      0      0   0     0     0         0    0           0         0   0
##   pictwittercomi0ega8iohp saka captur cupert httpbitly1g9fk2t press
## 1                       0    0      0      0                0     0
## 2                       0    0      0      0                0     0
## 3                       0    0      0      0                0     0
## 4                       0    0      0      0                0     0
## 5                       0    0      0      0                0     0
## 6                       0    0      0      0                0     0
##   pictwittercomp1yhsjjfzz 5cheap 5same httpowly22kxvv awar capit
## 1                       0      0     0              0    0     0
## 2                       0      0     0              0    0     0
## 3                       0      0     0              0    0     0
## 4                       0      0     0              0    0     0
## 5                       0      0     0              0    0     0
## 6                       0      0     0              0    0     0
##   httpowlyonxpx phenomenon todayb trendlab trendmicro mogsharif veryright
## 1             0          0      0        0          0         0         0
## 2             0          0      0        0          0         0         0
## 3             0          0      0        0          0         0         0
## 4             0          0      0        0          0         0         0
## 5             0          0      0        0          0         0         0
## 6             0          0      0        0          0         0         0
##   beli dunia inovasi kalo kepintaran konsumen lagi makin masih ngapain
## 1    0     0       0    0          0        0    0     0     0       0
## 2    0     0       0    0          0        0    0     0     0       0
## 3    0     0       0    0          0        0    0     0     0       0
## 4    0     0       0    0          0        0    0     0     0       0
## 5    0     0       0    0          0        0    0     0     0       0
## 6    0     0       0    0          0        0    0     0     0       0
##   ngetest pada yakin line ungod dla hejtuj myapplepl najlepsz nigdi
## 1       0    0     0    0     0   0      0         0        0     0
## 2       0    0     0    0     0   0      0         0        0     0
## 3       0    0     0    0     0   0      0         0        0     0
## 4       0    0     0    0     0   0      0         0        0     0
## 5       0    0     0    0     0   0      0         0        0     0
## 6       0    0     0    0     0   0      0         0        0     0
##   produktami stycznosci tak ten tylko wielu zostac 000 compt croi nom pour
## 1          0          0   0   0     0     0      0   0     0    0   0    0
## 2          0          0   0   0     0     0      0   0     0    0   0    0
## 3          0          0   0   0     0     0      0   0     0    0   0    0
## 4          0          0   0   0     0     0      0   0     0    0   0    0
## 5          0          0   0   0     0     0      0   0     0    0   0    0
## 6          0          0   0   0     0     0      0   0     0    0   0    0
##   nsa verizon verizonwireless whitehous yahoo axel http4sqcom14qtg
## 1   0       0               0         0     0    0               0
## 2   0       0               0         0     0    0               0
## 3   0       0               0         0     0    0               0
## 4   0       0               0         0     0    0               0
## 5   0       0               0         0     0    0               0
## 6   0       0               0         0     0    0               0
##   plugplay springer anuncio baja bolsa httpbitly17wvapz tras educ
## 1        0        0       0    0     0                0    0    0
## 2        0        0       0    0     0                0    0    0
## 3        0        0       0    0     0                0    0    0
## 4        0        0       0    0     0                0    0    0
## 5        0        0       0    0     0                0    0    0
## 6        0        0       0    0     0                0    0    0
##   httpgooglwktt0 ischool 099 asphalt8 2004 fool seri dynamictim
## 1              0       0   0        0    0    0    0          0
## 2              0       0   0        0    0    0    0          0
## 3              0       0   0        0    0    0    0          0
## 4              0       0   0        0    0    0    0          0
## 5              0       0   0        0    0    0    0          0
## 6              0       0   0        0    0    0    0          0
##   trustworthi httpfbme3ehihnvdn jimmi kimmel gen lower dyson exit joint
## 1           0                 0     0      0   0     0     0    0     0
## 2           0                 0     0      0   0     0     0    0     0
## 3           0                 0     0      0   0     0     0    0     0
## 4           0                 0     0      0   0     0     0    0     0
## 5           0                 0     0      0   0     0     0    0     0
## 6           0                 0     0      0   0     0     0    0     0
##   phonevacuum httpdlvrit3ybllz autocomplet bold curs enough
## 1           0                0           0    0    0      0
## 2           0                0           0    0    0      0
## 3           0                0           0    0    0      0
## 4           0                0           0    0    0      0
## 5           0                0           0    0    0      0
## 6           0                0           0    0    0      0
##   applequestionierr favourit fruit mikesleek tmk 30aud anybodi issu
## 1                 0        0     0         0   0     0       0    0
## 2                 0        0     0         0   0     0       0    0
## 3                 0        0     0         0   0     0       0    0
## 4                 0        0     0         0   0     0       0    0
## 5                 0        0     0         0   0     0       0    0
## 6                 0        0     0         0   0     0       0    0
##   magicmous recharg betalen bom doet gelegd heeft httpbitlyyh1pyu mee
## 1         0       0       0   0    0      0     0               0   0
## 2         0       0       0   0    0      0     0               0   0
## 3         0       0       0   0    0      0     0               0   0
## 4         0       0       0   0    0      0     0               0   0
## 5         0       0       0   0    0      0     0               0   0
## 6         0       0       0   0    0      0     0               0   0
##   mobiel niet nieuw onder tegenslag voor aheaux carri virginmobileusa
## 1      0    0     0     0         0    0      0     0               0
## 2      0    0     0     0         0    0      0     0               0
## 3      0    0     0     0         0    0      0     0               0
## 4      0    0     0     0         0    0      0     0               0
## 5      0    0     0     0         0    0      0     0               0
## 6      0    0     0     0         0    0      0     0               0
##   battalalgoo dengan ema main mata warna cambiar celu chile con entel
## 1           0      0   0    0    0     0       0    0     0   0     0
## 2           0      0   0    0    0     0       0    0     0   0     0
## 3           0      0   0    0    0     0       0    0     0   0     0
## 4           0      0   0    0    0     0       0    0     0   0     0
## 5           0      0   0    0    0     0       0    0     0   0     0
## 6           0      0   0    0    0     0       0    0     0   0     0
##   hagan llegan meno problema vienen 777 countri event ship 300 900 917
## 1     0      0    0        0      0   0       0     0    0   0   0   0
## 2     0      0    0        0      0   0       0     0    0   0   0   0
## 3     0      0    0        0      0   0       0     0    0   0   0   0
## 4     0      0    0        0      0   0       0     0    0   0   0   0
## 5     0      0    0        0      0   0       0     0    0   0   0   0
## 6     0      0    0        0      0   0       0     0    0   0   0   0
##   adob challeng dell fair rep yurbud fanci httphuffto1fxeibw huffposttech
## 1    0        0    0    0   0      0     0                 0            0
## 2    0        0    0    0   0      0     0                 0            0
## 3    0        0    0    0   0      0     0                 0            0
## 4    0        0    0    0   0      0     0                 0            0
## 5    0        0    0    0   0      0     0                 0            0
## 6    0        0    0    0   0      0     0                 0            0
##   cb4g httpowlyoqwmp monday phoneblok collect httpbitly18couwn shi showcas
## 1    0             0      0         0       0                0   0       0
## 2    0             0      0         0       0                0   0       0
## 3    0             0      0         0       0                0   0       0
## 4    0             0      0         0       0                0   0       0
## 5    0             0      0         0       0                0   0       0
## 6    0             0      0         0       0                0   0       0
##   upcom httphtlyopzpm chiiiiqu editoradraco chang httpowlyomld investor
## 1     0             0        0            0     0            0        0
## 2     0             0        0            0     0            0        0
## 3     0             0        0            0     0            0        0
## 4     0             0        0            0     0            0        0
## 5     0             0        0            0     0            0        0
## 6     0             0        0            0     0            0        0
##   chief chrischorn exepa jackson jocofino lisa marcgunth richard windsor
## 1     0          0     0       0        0    0         0       0       0
## 2     0          0     0       0        0    0         0       0       0
## 3     0          0     0       0        0    0         0       0       0
## 4     0          0     0       0        0    0         0       0       0
## 5     0          0     0       0        0    0         0       0       0
## 6     0          0     0       0        0    0         0       0       0
##   unlook httpfbmegh5nfcb ibookstor lusalazar mort olha toqu dave
## 1      0               0         0         0    0    0    0    0
## 2      0               0         0         0    0    0    0    0
## 3      0               0         0         0    0    0    0    0
## 4      0               0         0         0    0    0    0    0
## 5      0               0         0         0    0    0    0    0
## 6      0               0         0         0    0    0    0    0
##   davetwentyman jedipad pitch blindoldfreak shouldnt side throw
## 1             0       0     0             0        0    0     0
## 2             0       0     0             0        0    0     0
## 3             0       0     0             0        0    0     0
## 4             0       0     0             0        0    0     0
## 5             0       0     0             0        0    0     0
## 6             0       0     0             0        0    0     0
##   httpgoogl4jmh1h isnt list phonen applewhi differenti hate jab metro
## 1               0    0    0      0        0          0    0   0     0
## 2               0    0    0      0        0          0    0   0     0
## 3               0    0    0      0        0          0    0   0     0
## 4               0    0    0      0        0          0    0   0     0
## 5               0    0    0      0        0          0    0   0     0
## 6               0    0    0      0        0          0    0   0     0
##   confess wallpap banget berasa deh inovasinya lihat meski pelit
## 1       0       0      0      0   0          0     0     0     0
## 2       0       0      0      0   0          0     0     0     0
## 3       0       0      0      0   0          0     0     0     0
## 4       0       0      0      0   0          0     0     0     0
## 5       0       0      0      0   0          0     0     0     0
## 6       0       0      0      0   0          0     0     0     0
##   presentasi pridenya produk selalu seneng httpdlvrit3ylvbd byondbeauty09
## 1          0        0      0      0      0                0             0
## 2          0        0      0      0      0                0             0
## 3          0        0      0      0      0                0             0
## 4          0        0      0      0      0                0             0
## 5          0        0      0      0      0                0             0
## 6          0        0      0      0      0                0             0
##   httpatlawcom7rs7xt lawtechnew pros arab write bonus driven execut glehel
## 1                  0          0    0    0     0     0      0      0      0
## 2                  0          0    0    0     0     0      0      0      0
## 3                  0          0    0    0     0     0      0      0      0
## 4                  0          0    0    0     0     0      0      0      0
## 5                  0          0    0    0     0     0      0      0      0
## 6                  0          0    0    0     0     0      0      0      0
##   httpowlyoqo28 predict sauc secret derek holdold httpfbme2ckpba4ut breve
## 1             0       0    0      0     0       0                 0     0
## 2             0       0    0      0     0       0                 0     0
## 3             0       0    0      0     0       0                 0     0
## 4             0       0    0      0     0       0                 0     0
## 5             0       0    0      0     0       0                 0     0
## 6             0       0    0      0     0       0                 0     0
##   saraiva glass includ oblong recess storefront tabl trademark anunciars
## 1       0     0      0      0      0          0    0         0         0
## 2       0     0      0      0      0          0    0         0         0
## 3       0     0      0      0      0          0    0         0         0
## 4       0     0      0      0      0          0    0         0         0
## 5       0     0      0      0      0          0    0         0         0
## 6       0     0      0      0      0          0    0         0         0
##   aprovecha cobertura httpbitly17y3cmg favorita httpbitly13ry27k marca son
## 1         0         0                0        0                0     0   0
## 2         0         0                0        0                0     0   0
## 3         0         0                0        0                0     0   0
## 4         0         0                0        0                0     0   0
## 5         0         0                0        0                0     0   0
## 6         0         0                0        0                0     0   0
##   camp entir wheremyringat doctorverita 2week delay 5slfw
## 1    0     0             0            0     0     0     0
## 2    0     0             0            0     0     0     0
## 3    0     0             0            0     0     0     0
## 4    0     0             0            0     0     0     0
## 5    0     0             0            0     0     0     0
## 6    0     0             0            0     0     0     0
##   httpyoutube4il5cerxp4a ss14 womenswear pictwittercomkkq8femp04
## 1                      0    0          0                       0
## 2                      0    0          0                       0
## 3                      0    0          0                       0
## 4                      0    0          0                       0
## 5                      0    0          0                       0
## 6                      0    0          0                       0
##   pictwittercom8itaoafkp2 211 howsweleg tragic pictwittercomjhis5eispg
## 1                       0   0         0      0                       0
## 2                       0   0         0      0                       0
## 3                       0   0         0      0                       0
## 4                       0   0         0      0                       0
## 5                       0   0         0      0                       0
## 6                       0   0         0      0                       0
##   httpbitly17qodni bozaag topicshighlight quit 1415 adventur shout
## 1                0      0               0    0    0        0     0
## 2                0      0               0    0    0        0     0
## 3                0      0               0    0    0        0     0
## 4                0      0               0    0    0        0     0
## 5                0      0               0    0    0        0     0
## 6                0      0               0    0    0        0     0
##   http4sqcom1eiaj4 pittsburgh imag precis typic wet nstuff inakua kama
## 1                0          0    0      0     0   0      0      0    0
## 2                0          0    0      0     0   0      0      0    0
## 3                0          0    0      0     0   0      0      0    0
## 4                0          0    0      0     0   0      0      0    0
## 5                0          0    0      0     0   0      0      0    0
## 6                0          0    0      0     0   0      0      0    0
##   kufikiria penyevatimetufikisha wacwac counterpoint httpsshrlcpj5yjp
## 1         0                    0      0            0                0
## 2         0                    0      0            0                0
## 3         0                    0      0            0                0
## 4         0                    0      0            0                0
## 5         0                    0      0            0                0
## 6         0                    0      0            0                0
##   shareahol xconomi desktop instal pictwittercom4cfp5lk6hz tile behappi
## 1         0       0       0      0                       0    0       0
## 2         0       0       0      0                       0    0       0
## 3         0       0       0      0                       0    0       0
## 4         0       0       0      0                       0    0       0
## 5         0       0       0      0                       0    0       0
## 6         0       0       0      0                       0    0       0
##   fuel facebooktocom httpfbme1s22iorjt presentsappl store12 bewegingsproc
## 1    0             0                 0            0       0             0
## 2    0             0                 0            0       0             0
## 3    0             0                 0            0       0             0
## 4    0             0                 0            0       0             0
## 5    0             0                 0            0       0             0
## 6    0             0                 0            0       0             0
##   bewegingsprocessor gebruikt grote plannen cook john onewho recipi tim
## 1                  0        0     0       0    0    0      0      0   0
## 2                  0        0     0       0    0    0      0      0   0
## 3                  0        0     0       0    0    0      0      0   0
## 4                  0        0     0       0    0    0      0      0   0
## 5                  0        0     0       0    0    0      0      0   0
## 6                  0        0     0       0    0    0      0      0   0
##   whose croc xfrancesjoanna earli jay magnacarta samsungmobileus
## 1     0    0              0     0   0          0               0
## 2     0    0              0     0   0          0               0
## 3     0    0              0     0   0          0               0
## 4     0    0              0     0   0          0               0
## 5     0    0              0     0   0          0               0
## 6     0    0              0     0   0          0               0
##   thegoldalbum tyga cago muerto tus calm clarifi fear httpfbmet2axa0ti
## 1            0    0    0      0   0    0       0    0                0
## 2            0    0    0      0   0    0       0    0                0
## 3            0    0    0      0   0    0       0    0                0
## 4            0    0    0      0   0    0       0    0                0
## 5            0    0    0      0   0    0       0    0                0
## 6            0    0    0      0   0    0       0    0                0
##   potenti storag revenu sold benigeri given paul httpbuffly1em7i8r
## 1       0      0      0    0        0     0    0                 0
## 2       0      0      0    0        0     0    0                 0
## 3       0      0      0    0        0     0    0                 0
## 4       0      0      0    0        0     0    0                 0
## 5       0      0      0    0        0     0    0                 0
## 6       0      0      0    0        0     0    0                 0
##   diciembr tendr httpusatly1eebgc usatoday applecom girl fairlawn
## 1        0     0                0        0        0    0        0
## 2        0     0                0        0        0    0        0
## 3        0     0                0        0        0    0        0
## 4        0     0                0        0        0    0        0
## 5        0     0                0        0        0    0        0
## 6        0     0                0        0        0    0        0
##   http4sqcom14gb9qw mall summit consumerbas decis wors waathaaaaan anoth
## 1                 0    0      0           0     0    0           0     0
## 2                 0    0      0           0     0    0           0     0
## 3                 0    0      0           0     0    0           0     0
## 4                 0    0      0           0     0    0           0     0
## 5                 0    0      0           0     0    0           0     0
## 6                 0    0      0           0     0    0           0     0
##   fals pictwittercomdbwtly6moh analogdanni kms toma vai httpbuffly17vo9vn
## 1    0                       0           0   0    0   0                 0
## 2    0                       0           0   0    0   0                 0
## 3    0                       0           0   0    0   0                 0
## 4    0                       0           0   0    0   0                 0
## 5    0                       0           0   0    0   0                 0
## 6    0                       0           0   0    0   0                 0
##   activist admit evid plant georgeoak httpowlyopyn7 sobr brought crack
## 1        0     0    0     0         0             0    0       0     0
## 2        0     0    0     0         0             0    0       0     0
## 3        0     0    0     0         0             0    0       0     0
## 4        0     0    0     0         0             0    0       0     0
## 5        0     0    0     0         0             0    0       0     0
## 6        0     0    0     0         0             0    0       0     0
##   httpfbme2yczjotih httpfbme2rus2jzni cherylcol httpfbme2pmmkdyvh action
## 1                 0                 0         0                 0      0
## 2                 0                 0         0                 0      0
## 3                 0                 0         0                 0      0
## 4                 0                 0         0                 0      0
## 5                 0                 0         0                 0      0
## 6                 0                 0         0                 0      0
##   emoji exhibitk haa describ pant truli yoga betehen bize yan yapilan
## 1     0        0   0       0    0     0    0       0    0   0       0
## 2     0        0   0       0    0     0    0       0    0   0       0
## 3     0        0   0       0    0     0    0       0    0   0       0
## 4     0        0   0       0    0     0    0       0    0   0       0
## 5     0        0   0       0    0     0    0       0    0   0       0
## 6     0        0   0       0    0     0    0       0    0   0       0
##   yapilmistir kaitlin kaitlinbongo antireflect httpbitly17wle65 crocker
## 1           0       0            0           0                0       0
## 2           0       0            0           0                0       0
## 3           0       0            0           0                0       0
## 4           0       0            0           0                0       0
## 5           0       0            0           0                0       0
## 6           0       0            0           0                0       0
##   http4sqcom14rqsyb westlak airbnb european httponforbes19rhh3k ireland
## 1                 0       0      0        0                   0       0
## 2                 0       0      0        0                   0       0
## 3                 0       0      0        0                   0       0
## 4                 0       0      0        0                   0       0
## 5                 0       0      0        0                   0       0
## 6                 0       0      0        0                   0       0
##   rate seek tax travel 13th granular indic meter panel signal status
## 1    0    0   0      0    0        0     0     0     0      0      0
## 2    0    0   0      0    0        0     0     0     0      0      0
## 3    0    0   0      0    0        0     0     0     0      0      0
## 4    0    0   0      0    0        0     0     0     0      0      0
## 5    0    0   0      0    0        0     0     0     0      0      0
## 6    0    0   0      0    0        0     0     0     0      0      0
##   strength wifi pictwittercommhvci5fsl plagiaat apetec copia hacer ios7gm
## 1        0    0                      0        0      0     0     0      0
## 2        0    0                      0        0      0     0     0      0
## 3        0    0                      0        0      0     0     0      0
## 4        0    0                      0        0      0     0     0      0
## 5        0    0                      0        0      0     0     0      0
## 6        0    0                      0        0      0     0     0      0
##   ota seguridad bluetooth changer httpbitly18w3uph irmagazin landscap
## 1   0         0         0       0                0         0        0
## 2   0         0         0       0                0         0        0
## 3   0         0         0       0                0         0        0
## 4   0         0         0       0                0         0        0
## 5   0         0         0       0                0         0        0
## 6   0         0         0       0                0         0        0
##   mcommerc mpayment paypal ijordan jzfan michaeljordanstop night shift
## 1        0        0      0       0     0                 0     0     0
## 2        0        0      0       0     0                 0     0     0
## 3        0        0      0       0     0                 0     0     0
## 4        0        0      0       0     0                 0     0     0
## 5        0        0      0       0     0                 0     0     0
## 6        0        0      0       0     0                 0     0     0
##   18092013 fecha httpbitly17t8zxo tien bbm bbm4all ichooseblackberry10
## 1        0     0                0    0   0       0                   0
## 2        0     0                0    0   0       0                   0
## 3        0     0                0    0   0       0                   0
## 4        0     0                0    0   0       0                   0
## 5        0     0                0    0   0       0                   0
## 6        0     0                0    0   0       0                   0
##   ahora air cnnee desarrollo impar mimo mundo potencia subproducto collin
## 1     0   0     0          0     0    0     0        0           0      0
## 2     0   0     0          0     0    0     0        0           0      0
## 3     0   0     0          0     0    0     0        0           0      0
## 4     0   0     0          0     0    0     0        0           0      0
## 5     0   0     0          0     0    0     0        0           0      0
## 6     0   0     0          0     0    0     0        0           0      0
##   issi kno lmaoo mreasyryd slow blatant poor qualiti tast bostonreid box
## 1    0   0     0         0    0       0    0       0    0          0   0
## 2    0   0     0         0    0       0    0       0    0          0   0
## 3    0   0     0         0    0       0    0       0    0          0   0
## 4    0   0     0         0    0       0    0       0    0          0   0
## 5    0   0     0         0    0       0    0       0    0          0   0
## 6    0   0     0         0    0       0    0       0    0          0   0
##   glove dinero exijo vuelta cost katanya low php dinosaur remind retard
## 1     0      0     0      0    0       0   0   0        0      0      0
## 2     0      0     0      0    0       0   0   0        0      0      0
## 3     0      0     0      0    0       0   0   0        0      0      0
## 4     0      0     0      0    0       0   0   0        0      0      0
## 5     0      0     0      0    0       0   0   0        0      0      0
## 6     0      0     0      0    0       0   0   0        0      0      0
##   messag unsend germania httpsharesiysw0 prezzi techstationit stuff chrome
## 1      0      0        0               0      0             0     0      0
## 2      0      0        0               0      0             0     0      0
## 3      0      0        0               0      0             0     0      0
## 4      0      0        0               0      0             0     0      0
## 5      0      0        0               0      0             0     0      0
## 6      0      0        0               0      0             0     0      0
##   luggi pictwittercomlh1k8r7tzu render retina ugli 110 fulltimebro hip 200
## 1     0                       0      0      0    0   0           0   0   0
## 2     0                       0      0      0    0   0           0   0   0
## 3     0                       0      0      0    0   0           0   0   0
## 4     0                       0      0      0    0   0           0   0   0
## 5     0                       0      0      0    0   0           0   0   0
## 6     0                       0      0      0    0   0           0   0   0
##   httpbitly18xpnxl pay trade venturebeat stink lode twohat007 wold allow
## 1                0   0     0           0     0    0         0    0     0
## 2                0   0     0           0     0    0         0    0     0
## 3                0   0     0           0     0    0         0    0     0
## 4                0   0     0           0     0    0         0    0     0
## 5                0   0     0           0     0    0         0    0     0
## 6                0   0     0           0     0    0         0    0     0
##   harder httpowlyon4ub emerg attach keep somehow load httphtlyoptnw diego
## 1      0             0     0      0    0       0    0             0     0
## 2      0             0     0      0    0       0    0             0     0
## 3      0             0     0      0    0       0    0             0     0
## 4      0             0     0      0    0       0    0             0     0
## 5      0             0     0      0    0       0    0             0     0
## 6      0             0     0      0    0       0    0             0     0
##   type droclicquot soannoy cloud autotext deathmatch gamer huh davehakken
## 1    0           0       0     0        0          0     0   0          0
## 2    0           0       0     0        0          0     0   0          0
## 3    0           0       0     0        0          0     0   0          0
## 4    0           0       0     0        0          0     0   0          0
## 5    0           0       0     0        0          0     0   0          0
## 6    0           0       0     0        0          0     0   0          0
##   httpthndrit14ifdtv argh entrar imac puedo cholula stat florinnitulescu
## 1                  0    0      0    0     0       0    0               0
## 2                  0    0      0    0     0       0    0               0
## 3                  0    0      0    0     0       0    0               0
## 4                  0    0      0    0     0       0    0               0
## 5                  0    0      0    0     0       0    0               0
## 6                  0    0      0    0     0       0    0               0
##   helpthefellow background nitotv other theyr pictwittercomqgm8gdrwf gusta
## 1             0          0      0     0     0                      0     0
## 2             0          0      0     0     0                      0     0
## 3             0          0      0     0     0                      0     0
## 4             0          0      0     0     0                      0     0
## 5             0          0      0     0     0                      0     0
## 6             0          0      0     0     0                      0     0
##   letra siento tonto autofil protect theft thumb thumbprint briantong
## 1     0      0     0       0       0     0     0          0         0
## 2     0      0     0       0       0     0     0          0         0
## 3     0      0     0       0       0     0     0          0         0
## 4     0      0     0       0       0     0     0          0         0
## 5     0      0     0       0       0     0     0          0         0
## 6     0      0     0       0       0     0     0          0         0
##   bridgetcarey pyrit autoplay netflix process pictwittercomnnk3vwqalu
## 1            0     0        0       0       0                       0
## 2            0     0        0       0       0                       0
## 3            0     0        0       0       0                       0
## 4            0     0        0       0       0                       0
## 5            0     0        0       0       0                       0
## 6            0     0        0       0       0                       0
##   fault pal ybarra214ami accident calendar delet icon
## 1     0   0            0        0        0     0    0
## 2     0   0            0        0        0     0    0
## 3     0   0            0        0        0     0    0
## 4     0   0            0        0        0     0    0
## 5     0   0            0        0        0     0    0
## 6     0   0            0        0        0     0    0
##   pictwittercomdxutjk5evo short deadmau5 rememb tbt shade throwin condom
## 1                       0     0        0      0   0     0       0      0
## 2                       0     0        0      0   0     0       0      0
## 3                       0     0        0      0   0     0       0      0
## 4                       0     0        0      0   0     0       0      0
## 5                       0     0        0      0   0     0       0      0
## 6                       0     0        0      0   0     0       0      0
##   femal statement convenc httpcortas5oxn anymor phoneit real middl jerk
## 1     0         0       0              0      0       0    0     0    0
## 2     0         0       0              0      0       0    0     0    0
## 3     0         0       0              0      0       0    0     0    0
## 4     0         0       0              0      0       0    0     0    0
## 5     0         0       0              0      0       0    0     0    0
## 6     0         0       0              0      0       0    0     0    0
##   presal puttin httphtlyopzqp dump embrac httpgooglpzqzsq httpbitly1bgrund
## 1      0      0             0    0      0               0                0
## 2      0      0             0    0      0               0                0
## 3      0      0             0    0      0               0                0
## 4      0      0             0    0      0               0                0
## 5      0      0             0    0      0               0                0
## 6      0      0             0    0      0               0                0
##   nefari server sweat charger pictwittercomcxbjgfjiay iphoneteam teamappl
## 1      0      0     0       0                       0          0        0
## 2      0      0     0       0                       0          0        0
## 3      0      0     0       0                       0          0        0
## 4      0      0     0       0                       0          0        0
## 5      0      0     0       0                       0          0        0
## 6      0      0     0       0                       0          0        0
##   control defeat httpowlyoqlkj front inform ivankostynyk leakag verg beat
## 1       0      0             0     0      0            0      0    0    0
## 2       0      0             0     0      0            0      0    0    0
## 3       0      0             0     0      0            0      0    0    0
## 4       0      0             0     0      0            0      0    0    0
## 5       0      0             0     0      0            0      0    0    0
## 6       0      0             0     0      0            0      0    0    0
##   benchmark dualcor quadcor tarekalaya indirilmey sunulmus mom
## 1         0       0       0          0          0        0   0
## 2         0       0       0          0          0        0   0
## 3         0       0       0          0          0        0   0
## 4         0       0       0          0          0        0   0
## 5         0       0       0          0          0        0   0
## 6         0       0       0          0          0        0   0
##   pictwittercomhacmrms5i1 advertis terrorist won pictwittercomes1ln0abgc
## 1                       0        0         0   0                       0
## 2                       0        0         0   0                       0
## 3                       0        0         0   0                       0
## 4                       0        0         0   0                       0
## 5                       0        0         0   0                       0
## 6                       0        0         0   0                       0
##   uglyapp httpowlyon9uo socialparty2013 speck bbi ere hola sensual immedi
## 1       0             0               0     0   0   0    0       0      0
## 2       0             0               0     0   0   0    0       0      0
## 3       0             0               0     0   0   0    0       0      0
## 4       0             0               0     0   0   0    0       0      0
## 5       0             0               0     0   0   0    0       0      0
## 6       0             0               0     0   0   0    0       0      0
##   mobilekeverett greedi interchang muthafreak theyll corp disclos emis
## 1              0      0          0          0      0    0       0    0
## 2              0      0          0          0      0    0       0    0
## 3              0      0          0          0      0    0       0    0
## 4              0      0          0          0      0    0       0    0
## 5              0      0          0          0      0    0       0    0
## 6              0      0          0          0      0    0       0    0
##   facebook global httpbitly18xc8dk pwc psst cord sturdier rain vadaydg
## 1        0      0                0   0    0    0        0    0       0
## 2        0      0                0   0    0    0        0    0       0
## 3        0      0                0   0    0    0        0    0       0
## 4        0      0                0   0    0    0        0    0       0
## 5        0      0                0   0    0    0        0    0       0
## 6        0      0                0   0    0    0        0    0       0
##   bang champfer cowbel ibogost theatlantictech espera eterna quiero sido
## 1    0        0      0       0               0      0      0      0    0
## 2    0        0      0       0               0      0      0      0    0
## 3    0        0      0       0               0      0      0      0    0
## 4    0        0      0       0               0      0      0      0    0
## 5    0        0      0       0               0      0      0      0    0
## 6    0        0      0       0               0      0      0      0    0
##   addict carbon cdp emiss guardiansustbiz fixcontrast flat homescreen
## 1      0      0   0     0               0           0    0          0
## 2      0      0   0     0               0           0    0          0
## 3      0      0   0     0               0           0    0          0
## 4      0      0   0     0               0           0    0          0
## 5      0      0   0     0               0           0    0          0
## 6      0      0   0     0               0           0    0          0
##   needdepth parallax annieftmccann erth acampan afuera comprar japones
## 1         0        0             0    0       0      0       0       0
## 2         0        0             0    0       0      0       0       0
## 3         0        0             0    0       0      0       0       0
## 4         0        0             0    0       0      0       0       0
## 5         0        0             0    0       0      0       0       0
## 6         0        0             0    0       0      0       0       0
##   pictwittercom0xupojfq3z tienda tokio bed woke act lose httppostf06pn5
## 1                       0      0     0   0    0   0    0              0
## 2                       0      0     0   0    0   0    0              0
## 3                       0      0     0   0    0   0    0              0
## 4                       0      0     0   0    0   0    0              0
## 5                       0      0     0   0    0   0    0              0
## 6                       0      0     0   0    0   0    0              0
##   unmask endless httpsdrvms163brns obvious reasonsof handl theappleinc
## 1      0       0                 0       0         0     0           0
## 2      0       0                 0       0         0     0           0
## 3      0       0                 0       0         0     0           0
## 4      0       0                 0       0         0     0           0
## 5      0       0                 0       0         0     0           0
## 6      0       0                 0       0         0     0           0
##   upset weird liberacion local mando operador solicitar bdwallac
## 1     0     0          0     0     0        0         0        0
## 2     0     0          0     0     0        0         0        0
## 3     0     0          0     0     0        0         0        0
## 4     0     0          0     0     0        0         0        0
## 5     0     0          0     0     0        0         0        0
## 6     0     0          0     0     0        0         0        0
##   pictwittercomc6mgamrqmi liethey nah ihop threeleg adopt taken filter
## 1                       0       0   0    0        0     0     0      0
## 2                       0       0   0    0        0     0     0      0
## 3                       0       0   0    0        0     0     0      0
## 4                       0       0   0    0        0     0     0      0
## 5                       0       0   0    0        0     0     0      0
## 6                       0       0   0    0        0     0     0      0
##   pictwittercomz05b3sapoy remov expliqu peutil quelquun quest
## 1                       0     0       0      0        0     0
## 2                       0     0       0      0        0     0
## 3                       0     0       0      0        0     0
## 4                       0     0       0      0        0     0
## 5                       0     0       0      0        0     0
## 6                       0     0       0      0        0     0
##   httphtlyopzpn electron httpqzcom123930 label 100 alumnart curious less
## 1             0        0               0     0   0        0       0    0
## 2             0        0               0     0   0        0       0    0
## 3             0        0               0     0   0        0       0    0
## 4             0        0               0     0   0        0       0    0
## 5             0        0               0     0   0        0       0    0
## 6             0        0               0     0   0        0       0    0
##   mchoto mkt taught jazzyjeff normal queue rest pictwittercommh4qf0dam0
## 1      0   0      0         0      0     0    0                       0
## 2      0   0      0         0      0     0    0                       0
## 3      0   0      0         0      0     0    0                       0
## 4      0   0      0         0      0     0    0                       0
## 5      0   0      0         0      0     0    0                       0
## 6      0   0      0         0      0     0    0                       0
##   illumin present access silli httpnblogsp1hl pictwittercomom0lzdgir
## 1       0       0      0     0              0                      0
## 2       0       0      0     0              0                      0
## 3       0       0      0     0              0                      0
## 4       0       0      0     0              0                      0
## 5       0       0      0     0              0                      0
## 6       0       0      0     0              0                      0
##   pictwittercomdnxoecpq3t dave1010 sir bb10 httpbitly17skerj ipadmini qnx
## 1                       0        0   0    0                0        0   0
## 2                       0        0   0    0                0        0   0
## 3                       0        0   0    0                0        0   0
## 4                       0        0   0    0                0        0   0
## 5                       0        0   0    0                0        0   0
## 6                       0        0   0    0                0        0   0
##   z10 payment teas bitcoin compr lugar mal bij deukj door garanti geen
## 1   0       0    0       0     0     0   0   0     0    0       0    0
## 2   0       0    0       0     0     0   0   0     0    0       0    0
## 3   0       0    0       0     0     0   0   0     0    0       0    0
## 4   0       0    0       0     0     0   0   0     0    0       0    0
## 5   0       0    0       0     0     0   0   0     0    0       0    0
## 6   0       0    0       0     0     0   0   0     0    0       0    0
##   gekocht geld kapot maar onzin schijf doubl httpowlyomtwz riski
## 1       0    0     0    0     0      0     0             0     0
## 2       0    0     0    0     0      0     0             0     0
## 3       0    0     0    0     0      0     0             0     0
## 4       0    0     0    0     0      0     0             0     0
## 5       0    0     0    0     0      0     0             0     0
## 6       0    0     0    0     0      0     0             0     0
##   httphtlyopzp ban httpyoutubefzg4bcak064 wireless gmail gmailapocalyps
## 1            0   0                      0        0     0              0
## 2            0   0                      0        0     0              0
## 3            0   0                      0        0     0              0
## 4            0   0                      0        0     0              0
## 5            0   0                      0        0     0              0
## 6            0   0                      0        0     0              0
##   land tab antenna asia lunch nonauthor 5s5c adjust contain major shallow
## 1    0   0       0    0     0         0    0      0       0     0       0
## 2    0   0       0    0     0         0    0      0       0     0       0
## 3    0   0       0    0     0         0    0      0       0     0       0
## 4    0   0       0    0     0         0    0      0       0     0       0
## 5    0   0       0    0     0         0    0      0       0     0       0
## 6    0   0       0    0     0         0    0      0       0     0       0
##   pictwittercomepn891eak7 avidprotool cust muldoonpatrick serv potshot
## 1                       0           0    0              0    0       0
## 2                       0           0    0              0    0       0
## 3                       0           0    0              0    0       0
## 4                       0           0    0              0    0       0
## 5                       0           0    0              0    0       0
## 6                       0           0    0              0    0       0
##   httplnkdinbmwsyrr uncapit reveal allr friend lawsuit unless havent
## 1                 0       0      0    0      0       0      0      0
## 2                 0       0      0    0      0       0      0      0
## 3                 0       0      0    0      0       0      0      0
## 4                 0       0      0    0      0       0      0      0
## 5                 0       0      0    0      0       0      0      0
## 6                 0       0      0    0      0       0      0      0
##   matthouse15 consid gig suffici 2002 8bitsound lame ring tone nano
## 1           0      0   0       0    0         0    0    0    0    0
## 2           0      0   0       0    0         0    0    0    0    0
## 3           0      0   0       0    0         0    0    0    0    0
## 4           0      0   0       0    0         0    0    0    0    0
## 5           0      0   0       0    0         0    0    0    0    0
## 6           0      0   0       0    0         0    0    0    0    0
##   serious walkman itard joshanderson09 httphtlyoptod five fivefifth
## 1       0       0     0              0             0    0         0
## 2       0       0     0              0             0    0         0
## 3       0       0     0              0             0    0         0
## 4       0       0     0              0             0    0         0
## 5       0       0     0              0             0    0         0
## 6       0       0     0              0             0    0         0
##   outchea grand steep ima omfg afford log websit longer suppos
## 1       0     0     0   0    0      0   0      0      0      0
## 2       0     0     0   0    0      0   0      0      0      0
## 3       0     0     0   0    0      0   0      0      0      0
## 4       0     0     0   0    0      0   0      0      0      0
## 5       0     0     0   0    0      0   0      0      0      0
## 6       0     0     0   0    0      0   0      0      0      0
##   tomfairclough9 goddessvicki ohhhhhh lie silver httphtlyopzo9 bandwagon
## 1              0            0       0   0      0             0         0
## 2              0            0       0   0      0             0         0
## 3              0            0       0   0      0             0         0
## 4              0            0       0   0      0             0         0
## 5              0            0       0   0      0             0         0
## 6              0            0       0   0      0             0         0
##   movi redempt uvvu halp pictwittercomzrjco2lejn alleen batterij kapott
## 1    0       0    0    0                       0      0        0      0
## 2    0       0    0    0                       0      0        0      0
## 3    0       0    0    0                       0      0        0      0
## 4    0       0    0    0                       0      0        0      0
## 5    0       0    0    0                       0      0        0      0
## 6    0       0    0    0                       0      0        0      0
##   kwijt telefoon twee vervangen weken 199 549 contract httpsharesijr3q
## 1     0        0    0         0     0   0   0        0               0
## 2     0        0    0         0     0   0   0        0               0
## 3     0        0    0         0     0   0   0        0               0
## 4     0        0    0         0     0   0   0        0               0
## 5     0        0    0         0     0   0   0        0               0
## 6     0        0    0         0     0   0   0        0               0
##   nexus4 premium yike pictwittercom8c6idlywtc pictwittercom75hrorixpc
## 1      0       0    0                       0                       0
## 2      0       0    0                       0                       0
## 3      0       0    0                       0                       0
## 4      0       0    0                       0                       0
## 5      0       0    0                       0                       0
## 6      0       0    0                       0                       0
##   children millerjr99 telco kati smh millisecond unsubscrib
## 1        0          0     0    0   0           0          0
## 2        0          0     0    0   0           0          0
## 3        0          0     0    0   0           0          0
## 4        0          0     0    0   0           0          0
## 5        0          0     0    0   0           0          0
## 6        0          0     0    0   0           0          0
##   httppopsci14mhgof link popsci 2nd appt blow rude soho staff avoninspir
## 1                 0    0      0   0    0    0    0    0     0          0
## 2                 0    0      0   0    0    0    0    0     0          0
## 3                 0    0      0   0    0    0    0    0     0          0
## 4                 0    0      0   0    0    0    0    0     0          0
## 5                 0    0      0   0    0    0    0    0     0          0
## 6                 0    0      0   0    0    0    0    0     0          0
##   sellin album dgdtheband accept commod pictwittercomhgzytex3ya waay
## 1      0     0          0      0      0                       0    0
## 2      0     0          0      0      0                       0    0
## 3      0     0          0      0      0                       0    0
## 4      0     0          0      0      0                       0    0
## 5      0     0          0      0      0                       0    0
## 6      0     0          0      0      0                       0    0
##   jeanettehay 1jazzyjeff yetit previous butt reddot wtf httphtlyopzm
## 1           0          0     0        0    0      0   0            0
## 2           0          0     0        0    0      0   0            0
## 3           0          0     0        0    0      0   0            0
## 4           0          0     0        0    0      0   0            0
## 5           0          0     0        0    0      0   0            0
## 6           0          0     0        0    0      0   0            0
##   analyst boo fall nasdaq near desaparecido descontinuado esta increibl
## 1       0   0    0      0    0            0             0    0        0
## 2       0   0    0      0    0            0             0    0        0
## 3       0   0    0      0    0            0             0    0        0
## 4       0   0    0      0    0            0             0    0        0
## 5       0   0    0      0    0            0             0    0        0
## 6       0   0    0      0    0            0             0    0        0
##   sigu venta web everybodi amazon csr drlarazib notgoodenough
## 1    0     0   0         0      0   0         0             0
## 2    0     0   0         0      0   0         0             0
## 3    0     0   0         0      0   0         0             0
## 4    0     0   0         0      0   0         0             0
## 5    0     0   0         0      0   0         0             0
## 6    0     0   0         0      0   0         0             0
##   httphtlyoptmg gamemod mcpe seed whitelist yldthng sale undercut thatd
## 1             0       0    0    0         0       0    0        0     0
## 2             0       0    0    0         0       0    0        0     0
## 3             0       0    0    0         0       0    0        0     0
## 4             0       0    0    0         0       0    0        0     0
## 5             0       0    0    0         0       0    0        0     0
## 6             0       0    0    0         0       0    0        0     0
##   comment encrypt involv wether evolutionari goodtim revolutionari disast
## 1       0       0      0      0            0       0             0      0
## 2       0       0      0      0            0       0             0      0
## 3       0       0      0      0            0       0             0      0
## 4       0       0      0      0            0       0             0      0
## 5       0       0      0      0            0       0             0      0
## 6       0       0      0      0            0       0             0      0
##   ggreenwald liberationtech point tribiztech etgoldnew 30min apt
## 1          0              0     0          0         0     0   0
## 2          0              0     0          0         0     0   0
## 3          0              0     0          0         0     0   0
## 4          0              0     0          0         0     0   0
## 5          0              0     0          0         0     0   0
## 6          0              0     0          0         0     0   0
##   http4sqcom1eoiu9d crash scratch httpusatly19ofic1 40000 6000 rupeefal
## 1                 0     0       0                 0     0    0        0
## 2                 0     0       0                 0     0    0        0
## 3                 0     0       0                 0     0    0        0
## 4                 0     0       0                 0     0    0        0
## 5                 0     0       0                 0     0    0        0
## 6                 0     0       0                 0     0    0        0
##   usa agreement alar httplnkdinbncwbj3 polarmo httphtlyoptjb 719 canada
## 1   0         0    0                 0       0             0   0      0
## 2   0         0    0                 0       0             0   0      0
## 3   0         0    0                 0       0             0   0      0
## 4   0         0    0                 0       0             0   0      0
## 5   0         0    0                 0       0             0   0      0
## 6   0         0    0                 0       0             0   0      0
##   broken difficult firstworldproblem sept til httphtlyopter 13appl defend
## 1      0         0                 0    0   0             0      0      0
## 2      0         0                 0    0   0             0      0      0
## 3      0         0                 0    0   0             0      0      0
## 4      0         0                 0    0   0             0      0      0
## 5      0         0                 0    0   0             0      0      0
## 6      0         0                 0    0   0             0      0      0
##   punk240z pgaffii strang tuesday aid deterr opp scanningsend thiev cth
## 1        0       0      0       0   0      0   0            0     0   0
## 2        0       0      0       0   0      0   0            0     0   0
## 3        0       0      0       0   0      0   0            0     0   0
## 4        0       0      0       0   0      0   0            0     0   0
## 5        0       0      0       0   0      0   0            0     0   0
## 6        0       0      0       0   0      0   0            0     0   0
##   epl httpbddyme14pxi6q safcoffici sbd sue sunderland tanzania
## 1   0                 0          0   0   0          0        0
## 2   0                 0          0   0   0          0        0
## 3   0                 0          0   0   0          0        0
## 4   0                 0          0   0   0          0        0
## 5   0                 0          0   0   0          0        0
## 6   0                 0          0   0   0          0        0
##   httpyoutubedu0qahcb1 appledevcent nope richp windows8 mightttt antoneg
## 1                    0            0    0     0        0        0       0
## 2                    0            0    0     0        0        0       0
## 3                    0            0    0     0        0        0       0
## 4                    0            0    0     0        0        0       0
## 5                    0            0    0     0        0        0       0
## 6                    0            0    0     0        0        0       0
##   disappoint httpowlyomtkb delta disabl hack lock mess accur joke weather
## 1          0             0     0      0    0    0    0     0    0       0
## 2          0             0     0      0    0    0    0     0    0       0
## 3          0             0     0      0    0    0    0     0    0       0
## 4          0             0     0      0    0    0    0     0    0       0
## 5          0             0     0      0    0    0    0     0    0       0
## 6          0             0     0      0    0    0    0     0    0       0
##   forev lawschoolsrat 40k address produc bbri coffin enterpris nail demo
## 1     0             0   0       0      0    0      0         0    0    0
## 2     0             0   0       0      0    0      0         0    0    0
## 3     0             0   0       0      0    0      0         0    0    0
## 4     0             0   0       0      0    0      0         0    0    0
## 5     0             0   0       0      0    0      0         0    0    0
## 6     0             0   0       0      0    0      0         0    0    0
##   free4al mind corporatebulli irrit aaaaaappl bright dis paawey stuffjx
## 1       0    0              0     0         0      0   0      0       0
## 2       0    0              0     0         0      0   0      0       0
## 3       0    0              0     0         0      0   0      0       0
## 4       0    0              0     0         0      0   0      0       0
## 5       0    0              0     0         0      0   0      0       0
## 6       0    0              0     0         0      0   0      0       0
##   icloud stupid upload swear classi tradit tumbl cemorecake718
## 1      0      0      0     0      0      0     0             0
## 2      0      0      0     0      0      0     0             0
## 3      0      0      0     0      0      0     0             0
## 4      0      0      0     0      0      0     0             0
## 5      0      0      0     0      0      0     0             0
## 6      0      0      0     0      0      0     0             0
##   matiucurvegawd sus swaggal yooo httphtlyoptni pictwittercomilb1xrqikd
## 1              0   0       0    0             0                       0
## 2              0   0       0    0             0                       0
## 3              0   0       0    0             0                       0
## 4              0   0       0    0             0                       0
## 5              0   0       0    0             0                       0
## 6              0   0       0    0             0                       0
##   file sync complac corpor empir individu brat burn polycarbon unibodi ace
## 1    0    0       0      0     0        0    0    0          0       0   0
## 2    0    0       0      0     0        0    0    0          0       0   0
## 3    0    0       0      0     0        0    0    0          0       0   0
## 4    0    0       0      0     0        0    0    0          0       0   0
## 5    0    0       0      0     0        0    0    0          0       0   0
## 6    0    0       0      0     0        0    0    0          0       0   0
##   phonein excus gotdarn three pictwittercompshfqmmnlt cheerio extra
## 1       0     0       0     0                       0       0     0
## 2       0     0       0     0                       0       0     0
## 3       0     0       0     0                       0       0     0
## 4       0     0       0     0                       0       0     0
## 5       0     0       0     0                       0       0     0
## 6       0     0       0     0                       0       0     0
##   freakin 2005 chose coulour pictwittercomfocsb1fmqc hit httpowlyoorai
## 1       0    0     0       0                       0   0             0
## 2       0    0     0       0                       0   0             0
## 3       0    0     0       0                       0   0             0
## 4       0    0     0       0                       0   0             0
## 5       0    0     0       0                       0   0             0
## 6       0    0     0       0                       0   0             0
##   itproport plummet pricey archriv divulg outperform refus privet complex
## 1         0       0      0       0      0          0     0      0       0
## 2         0       0      0       0      0          0     0      0       0
## 3         0       0      0       0      0          0     0      0       0
## 4         0       0      0       0      0          0     0      0       0
## 5         0       0      0       0      0          0     0      0       0
## 6         0       0      0       0      0          0     0      0       0
##   head simplif troubl appleinsid canada599 canadian edtech
## 1    0       0      0          0         0        0      0
## 2    0       0      0          0         0        0      0
## 3    0       0      0          0         0        0      0
## 4    0       0      0          0         0        0      0
## 5    0       0      0          0         0        0      0
## 6    0       0      0          0         0        0      0
##   pictwittercome6nvt28xkt rofl usa99 coin fingerg stay applerort australia
## 1                       0    0     0    0       0    0         0         0
## 2                       0    0     0    0       0    0         0         0
## 3                       0    0     0    0       0    0         0         0
## 4                       0    0     0    0       0    0         0         0
## 5                       0    0     0    0       0    0         0         0
## 6                       0    0     0    0       0    0         0         0
##   extrem overpr morebut plan probabl sustain common crowd ertaysh bogus
## 1      0      0       0    0       0       0      0     0       0     0
## 2      0      0       0    0       0       0      0     0       0     0
## 3      0      0       0    0       0       0      0     0       0     0
## 4      0      0       0    0       0       0      0     0       0     0
## 5      0      0       0    0       0       0      0     0       0     0
## 6      0      0       0    0       0       0      0     0       0     0
##   highend laterfor obsolet pictwittercomicb7csr5j arrog joconfino anyth
## 1       0        0       0                      0     0         0     0
## 2       0        0       0                      0     0         0     0
## 3       0        0       0                      0     0         0     0
## 4       0        0       0                      0     0         0     0
## 5       0        0       0                      0     0         0     0
## 6       0        0       0                      0     0         0     0
##   wallmart pictwittercomykaiihu6yy delilaheven pickadilly7 baixou doer
## 1        0                       0           0           0      0    0
## 2        0                       0           0           0      0    0
## 3        0                       0           0           0      0    0
## 4        0                       0           0           0      0    0
## 5        0                       0           0           0      0    0
## 6        0                       0           0           0      0    0
##   escola espirito feio ficou pessoal samba royal run screw warranti
## 1      0        0    0     0       0     0     0   0     0        0
## 2      0        0    0     0       0     0     0   0     0        0
## 3      0        0    0     0       0     0     0   0     0        0
## 4      0        0    0     0       0     0     0   0     0        0
## 5      0        0    0     0       0     0     0   0     0        0
## 6      0        0    0     0       0     0     0   0     0        0
##   examinercom httpexmnr15tsudj surviv bite divers httpsharesiz8kj racebait
## 1           0                0      0    0      0               0        0
## 2           0                0      0    0      0               0        0
## 3           0                0      0    0      0               0        0
## 4           0                0      0    0      0               0        0
## 5           0                0      0    0      0               0        0
## 6           0                0      0    0      0               0        0
##   tcot therev agre disgrac hella ish jawn massiv ooooooooooo underwhelm
## 1    0      0    0       0     0   0    0      0           0          0
## 2    0      0    0       0     0   0    0      0           0          0
## 3    0      0    0       0     0   0    0      0           0          0
## 4    0      0    0       0     0   0    0      0           0          0
## 5    0      0    0       0     0   0    0      0           0          0
## 6    0      0    0       0     0   0    0      0           0          0
##   golden joker outta playdoh steer wsdottraff anticip deliber purpos
## 1      0     0     0       0     0          0       0       0      0
## 2      0     0     0       0     0          0       0       0      0
## 3      0     0     0       0     0          0       0       0      0
## 4      0     0     0       0     0          0       0       0      0
## 5      0     0     0       0     0          0       0       0      0
## 6      0     0     0       0     0          0       0       0      0
##   2shaneez httpbitly14oq6bd minor report conspiraci noprivaci theorist
## 1        0                0     0      0          0         0        0
## 2        0                0     0      0          0         0        0
## 3        0                0     0      0          0         0        0
## 4        0                0     0      0          0         0        0
## 5        0                0     0      0          0         0        0
## 6        0                0     0      0          0         0        0
##   pictwittercomagel53hcjh bouta cop refund tacki acquir leapmot newton
## 1                       0     0   0      0     0      0       0      0
## 2                       0     0   0      0     0      0       0      0
## 3                       0     0   0      0     0      0       0      0
## 4                       0     0   0      0     0      0       0      0
## 5                       0     0   0      0     0      0       0      0
## 6                       0     0   0      0     0      0       0      0
##   resal besid equal kitti youv 899 99preorder dodgi msm budget boy success
## 1     0     0     0     0    0   0          0     0   0      0   0       0
## 2     0     0     0     0    0   0          0     0   0      0   0       0
## 3     0     0     0     0    0   0          0     0   0      0   0       0
## 4     0     0     0     0    0   0          0     0   0      0   0       0
## 5     0     0     0     0    0   0          0     0   0      0   0       0
## 6     0     0     0     0    0   0          0     0   0      0   0       0
##   down specif tear technic fridaythe13th frustrat sky confer missstev
## 1    0      0    0       0             0        0   0      0        0
## 2    0      0    0       0             0        0   0      0        0
## 3    0      0    0       0             0        0   0      0        0
## 4    0      0    0       0             0        0   0      0        0
## 5    0      0    0       0             0        0   0      0        0
## 6    0      0    0       0             0        0   0      0        0
##   explain reset wake byby beta reimburs uggh inch six bitter find late
## 1       0     0    0    0    0        0    0    0   0      0    0    0
## 2       0     0    0    0    0        0    0    0   0      0    0    0
## 3       0     0    0    0    0        0    0    0   0      0    0    0
## 4       0     0    0    0    0        0    0    0   0      0    0    0
## 5       0     0    0    0    0        0    0    0   0      0    0    0
## 6       0     0    0    0    0        0    0    0   0      0    0    0
##   noinfoonwebsit order httpgooglilvi5t emmmahem alert bird
## 1              0     0               0        0     0    0
## 2              0     0               0        0     0    0
## 3              0     0               0        0     0    0
## 4              0     0               0        0     0    0
## 5              0     0               0        0     0    0
## 6              0     0               0        0     0    0
##   itsfreakingannoy rid increment requir base definit caterpillar
## 1                0   0         0      0    0       0           0
## 2                0   0         0      0    0       0           0
## 3                0   0         0      0    0       0           0
## 4                0   0         0      0    0       0           0
## 5                0   0         0      0    0       0           0
## 6                0   0         0      0    0       0           0
##   greenhousega shame automak httpowlyi38hxx httpowlyopjzk nissan blogdiva
## 1            0     0       0              0             0      0        0
## 2            0     0       0              0             0      0        0
## 3            0     0       0              0             0      0        0
## 4            0     0       0              0             0      0        0
## 5            0     0       0              0             0      0        0
## 6            0     0       0              0             0      0        0
##   certaintyno databas nobrain ofa suckingup taxesw pictwittercomfrvugeo5ad
## 1           0       0       0   0         0      0                       0
## 2           0       0       0   0         0      0                       0
## 3           0       0       0   0         0      0                       0
## 4           0       0       0   0         0      0                       0
## 5           0       0       0   0         0      0                       0
## 6           0       0       0   0         0      0                       0
##   tmcconnon wilcoxaj audio feed luck sireltonjohn hahahahaha
## 1         0        0     0    0    0            0          0
## 2         0        0     0    0    0            0          0
## 3         0        0     0    0    0            0          0
## 4         0        0     0    0    0            0          0
## 5         0        0     0    0    0            0          0
## 6         0        0     0    0    0            0          0
##   http9gagtvv1132 chines ironi labor reduc benefit hobbl lack reap cri
## 1               0      0     0     0     0       0     0    0    0   0
## 2               0      0     0     0     0       0     0    0    0   0
## 3               0      0     0     0     0       0     0    0    0   0
## 4               0      0     0     0     0       0     0    0    0   0
## 5               0      0     0     0     0       0     0    0    0   0
## 6               0      0     0     0     0       0     0    0    0   0
##   phonebloksinc mad 40mb 55mb dnld limit attack minut wouldnt hype ihi sim
## 1             0   0    0    0    0     0      0     0       0    0   0   0
## 2             0   0    0    0    0     0      0     0       0    0   0   0
## 3             0   0    0    0    0     0      0     0       0    0   0   0
## 4             0   0    0    0    0     0      0     0       0    0   0   0
## 5             0   0    0    0    0     0      0     0       0    0   0   0
## 6             0   0    0    0    0     0      0     0       0    0   0   0
##   spazz opinion betterstrong invest whatnot fjaskdghdka tbh sort
## 1     0       0            0      0       0           0   0    0
## 2     0       0            0      0       0           0   0    0
## 3     0       0            0      0       0           0   0    0
## 4     0       0            0      0       0           0   0    0
## 5     0       0            0      0       0           0   0    0
## 6     0       0            0      0       0           0   0    0
##   httpbitly181zl8g librari rent frozen rekali australian complain instant
## 1                0       0    0      0      0          0        0       0
## 2                0       0    0      0      0          0        0       0
## 3                0       0    0      0      0          0        0       0
## 4                0       0    0      0      0          0        0       0
## 5                0       0    0      0      0          0        0       0
## 6                0       0    0      0      0          0        0       0
##   scam happend within deliv opposit perform sacrific emperorsnewcloth
## 1    0       0      0     0       0       0        0                0
## 2    0       0      0     0       0       0        0                0
## 3    0       0      0     0       0       0        0                0
## 4    0       0      0     0       0       0        0                0
## 5    0       0      0     0       0       0        0                0
## 6    0       0      0     0       0       0        0                0
##   pictwittercomd8hsgvughg varieti crime cybersecur httponforbes17uvr33
## 1                       0       0     0          0                   0
## 2                       0       0     0          0                   0
## 3                       0       0     0          0                   0
## 4                       0       0     0          0                   0
## 5                       0       0     0          0                   0
## 6                       0       0     0          0                   0
##   risk 528 whole later reject richellelittl skype
## 1    0   0     0     0      0             0     0
## 2    0   0     0     0      0             0     0
## 3    0   0     0     0      0             0     0
## 4    0   0     0     0      0             0     0
## 5    0   0     0     0      0             0     0
## 6    0   0     0     0      0             0     0
##   httpwwwmacobservercomtmodeathknel shut anti isthatsohardtounderstand
## 1                                 0    0    0                        0
## 2                                 0    0    0                        0
## 3                                 0    0    0                        0
## 4                                 0    0    0                        0
## 5                                 0    0    0                        0
## 6                                 0    0    0                        0
##   gullibl httptinyurlcomnhou46g iphone5problem random welfar
## 1       0                     0              0      0      0
## 2       0                     0              0      0      0
## 3       0                     0              0      0      0
## 4       0                     0              0      0      0
## 5       0                     0              0      0      0
## 6       0                     0              0      0      0
##   httpindpn1efw275 ouch 2011with cellphon claim genuin beef biometricstech
## 1                0    0        0        0     0      0    0              0
## 2                0    0        0        0     0      0    0              0
## 3                0    0        0        0     0      0    0              0
## 4                0    0        0        0     0      0    0              0
## 5                0    0        0        0     0      0    0              0
## 6                0    0        0        0     0      0    0              0
##   gollmann jeremiahg 3yr glare prone skyhelpteam smartyp maam
## 1        0         0   0     0     0           0       0    0
## 2        0         0   0     0     0           0       0    0
## 3        0         0   0     0     0           0       0    0
## 4        0         0   0     0     0           0       0    0
## 5        0         0   0     0     0           0       0    0
## 6        0         0   0     0     0           0       0    0
##   pictwittercomopcgnrzyrw bout bullstuff gimmick lay accord bestit
## 1                       0    0         0       0   0      0      0
## 2                       0    0         0       0   0      0      0
## 3                       0    0         0       0   0      0      0
## 4                       0    0         0       0   0      0      0
## 5                       0    0         0       0   0      0      0
## 6                       0    0         0       0   0      0      0
##   samsungcamera samsungmobil eras reinstal declin lmkunert tire
## 1             0            0    0        0      0        0    0
## 2             0            0    0        0      0        0    0
## 3             0            0    0        0      0        0    0
## 4             0            0    0        0      0        0    0
## 5             0            0    0        0      0        0    0
## 6             0            0    0        0      0        0    0
##   pictwittercombn0cfcs1cx asleep wheel 1am cheer crappl spent niqqa murder
## 1                       0      0     0   0     0      0     0     0      0
## 2                       0      0     0   0     0      0     0     0      0
## 3                       0      0     0   0     0      0     0     0      0
## 4                       0      0     0   0     0      0     0     0      0
## 5                       0      0     0   0     0      0     0     0      0
## 6                       0      0     0   0     0      0     0     0      0
##   bridg chaban delma pictwittercomrko36pyt9v timcook darnyouiphon crap eye
## 1     0      0     0                       0       0            0    0   0
## 2     0      0     0                       0       0            0    0   0
## 3     0      0     0                       0       0            0    0   0
## 4     0      0     0                       0       0            0    0   0
## 5     0      0     0                       0       0            0    0   0
## 6     0      0     0                       0       0            0    0   0
##   pain plannedobsolesc 16gbs memori caterpillarinc comcast dynam general
## 1    0               0     0      0              0       0     0       0
## 2    0               0     0      0              0       0     0       0
## 3    0               0     0      0              0       0     0       0
## 4    0               0     0      0              0       0     0       0
## 5    0               0     0      0              0       0     0       0
## 6    0               0     0      0              0       0     0       0
##   gileadsci twc businessmen samssung vulgar peev elig 16gb 740 appletax
## 1         0   0           0        0      0    0    0    0   0        0
## 2         0   0           0        0      0    0    0    0   0        0
## 3         0   0           0        0      0    0    0    0   0        0
## 4         0   0           0        0      0    0    0    0   0        0
## 5         0   0           0        0      0    0    0    0   0        0
## 6         0   0           0        0      0    0    0    0   0        0
##   bunch forgot nimbuzz comparison flaw riveraoor terribl laps polit
## 1     0      0       0          0    0         0       0    0     0
## 2     0      0       0          0    0         0       0    0     0
## 3     0      0       0          0    0         0       0    0     0
## 4     0      0       0          0    0         0       0    0     0
## 5     0      0       0          0    0         0       0    0     0
## 6     0      0       0          0    0         0       0    0     0
##   tweetdeck wishihadandroid cupertino fli wall alshepmh gettin ecstat
## 1         0               0         0   0    0        0      0      0
## 2         0               0         0   0    0        0      0      0
## 3         0               0         0   0    0        0      0      0
## 4         0               0         0   0    0        0      0      0
## 5         0               0         0   0    0        0      0      0
## 6         0               0         0   0    0        0      0      0
##   trippin grave regular httpbitly15v6twt appoint catherin decid stuffti
## 1       0     0       0                0       0        0     0       0
## 2       0     0       0                0       0        0     0       0
## 3       0     0       0                0       0        0     0       0
## 4       0     0       0                0       0        0     0       0
## 5       0     0       0                0       0        0     0       0
## 6       0     0       0                0       0        0     0       0
##   click httpziteto181fv breakabl easili rob earphon judg satan swollen
## 1     0               0        0      0   0       0    0     0       0
## 2     0               0        0      0   0       0    0     0       0
## 3     0               0        0      0   0       0    0     0       0
## 4     0               0        0      0   0       0    0     0       0
## 5     0               0        0      0   0       0    0     0       0
## 6     0               0        0      0   0       0    0     0       0
##   length itsannoy famous misl convolut interfac 5th smmfh menu answer
## 1      0        0      0    0        0        0   0     0    0      0
## 2      0        0      0    0        0        0   0     0    0      0
## 3      0        0      0    0        0        0   0     0    0      0
## 4      0        0      0    0        0        0   0     0    0      0
## 5      0        0      0    0        0        0   0     0    0      0
## 6      0        0      0    0        0        0   0     0    0      0
##   ouukillem glorious radio climat httpgucomp3tyeptw gap bruh vibrat
## 1         0        0     0      0                 0   0    0      0
## 2         0        0     0      0                 0   0    0      0
## 3         0        0     0      0                 0   0    0      0
## 4         0        0     0      0                 0   0    0      0
## 5         0        0     0      0                 0   0    0      0
## 6         0        0     0      0                 0   0    0      0
##   beforehand
## 1          0
## 2          0
## 3          0
## 4          0
## 5          0
## 6          0
##   httpwwwwiredcomopinion201309theunexpectedresultoffingerprintauthenticationthatyoucanttakethefifthmbidsocial11837204
## 1                                                                                                                   0
## 2                                                                                                                   0
## 3                                                                                                                   0
## 4                                                                                                                   0
## 5                                                                                                                   0
## 6                                                                                                                   0
##   lawyer youd error school unus 600 imagin goaway resembl backward
## 1      0    0     0      0    0   0      0      0       0        0
## 2      0    0     0      0    0   0      0      0       0        0
## 3      0    0     0      0    0   0      0      0       0        0
## 4      0    0     0      0    0   0      0      0       0        0
## 5      0    0     0      0    0   0      0      0       0        0
## 6      0    0     0      0    0   0      0      0       0        0
##   tactustherapi unfortun ignor oblig flightradar24 seen undetail
## 1             0        0     0     0             0    0        0
## 2             0        0     0     0             0    0        0
## 3             0        0     0     0             0    0        0
## 4             0        0     0     0             0    0        0
## 5             0        0     0     0             0    0        0
## 6             0        0     0     0             0    0        0
##   wasteoftim boycott march protest street wallet applebash httpflipitruy1k
## 1          0       0     0       0      0      0         0               0
## 2          0       0     0       0      0      0         0               0
## 3          0       0     0       0      0      0         0               0
## 4          0       0     0       0      0      0         0               0
## 5          0       0     0       0      0      0         0               0
## 6          0       0     0       0      0      0         0               0
##   north road scottsdail scottsdal pick httpaddvccc5 meh reaction slick
## 1     0    0          0         0    0            0   0        0     0
## 2     0    0          0         0    0            0   0        0     0
## 3     0    0          0         0    0            0   0        0     0
## 4     0    0          0         0    0            0   0        0     0
## 5     0    0          0         0    0            0   0        0     0
## 6     0    0          0         0    0            0   0        0     0
##   asian ridicul focus myeyesonlumia nasti piriyankaa lmfao gay bare dunno
## 1     0       0     0             0     0          0     0   0    0     0
## 2     0       0     0             0     0          0     0   0    0     0
## 3     0       0     0             0     0          0     0   0    0     0
## 4     0       0     0             0     0          0     0   0    0     0
## 5     0       0     0             0     0          0     0   0    0     0
## 6     0       0     0             0     0          0     0   0    0     0
##   exchang exec minimum foh overheat failur misstep mistak past wouldv util
## 1       0    0       0   0        0      0       0      0    0      0    0
## 2       0    0       0   0        0      0       0      0    0      0    0
## 3       0    0       0   0        0      0       0      0    0      0    0
## 4       0    0       0   0        0      0       0      0    0      0    0
## 5       0    0       0   0        0      0       0      0    0      0    0
## 6       0    0       0   0        0      0       0      0    0      0    0
##   bye christ plot sake certain backdoor multitask swiftest trail
## 1   0      0    0    0       0        0         0        0     0
## 2   0      0    0    0       0        0         0        0     0
## 3   0      0    0    0       0        0         0        0     0
## 4   0      0    0    0       0        0         0        0     0
## 5   0      0    0    0       0        0         0        0     0
## 6   0      0    0    0       0        0         0        0     0
##   httpsdrvms161bqvl pissedout theregoesallmydatausag weeki trailer blind
## 1                 0         0                      0     0       0     0
## 2                 0         0                      0     0       0     0
## 3                 0         0                      0     0       0     0
## 4                 0         0                      0     0       0     0
## 5                 0         0                      0     0       0     0
## 6                 0         0                      0     0       0     0
##   hatr mislead extrodilarri freakong butthol ughnarryby usual deltaassist
## 1    0       0            0        0       0          0     0           0
## 2    0       0            0        0       0          0     0           0
## 3    0       0            0        0       0          0     0           0
## 4    0       0            0        0       0          0     0           0
## 5    0       0            0        0       0          0     0           0
## 6    0       0            0        0       0          0     0           0
##   invas catch slip nude ngo full fullcharg hrs retweet wife clean 500
## 1     0     0    0    0   0    0         0   0       0    0     0   0
## 2     0     0    0    0   0    0         0   0       0    0     0   0
## 3     0     0    0    0   0    0         0   0       0    0     0   0
## 4     0     0    0    0   0    0         0   0       0    0     0   0
## 5     0     0    0    0   0    0         0   0       0    0     0   0
## 6     0     0    0    0   0    0         0   0       0    0     0   0
##   goddarn undercov cdproject co2 freez hundr dafuq intelcputhat machin
## 1       0        0         0   0     0     0     0            0      0
## 2       0        0         0   0     0     0     0            0      0
## 3       0        0         0   0     0     0     0            0      0
## 4       0        0         0   0     0     0     0            0      0
## 5       0        0         0   0     0     0     0            0      0
## 6       0        0         0   0     0     0     0            0      0
##   footprint guardian taxavoid 55cs return deepfail eas notcool speedand
## 1         0        0        0    0      0        0   0       0        0
## 2         0        0        0    0      0        0   0       0        0
## 3         0        0        0    0      0        0   0       0        0
## 4         0        0        0    0      0        0   0       0        0
## 5         0        0        0    0      0        0   0       0        0
## 6         0        0        0    0      0        0   0       0        0
##   snapchat shortag bore scum ital jlilest punch court misspel motherfreak
## 1        0       0    0    0    0       0     0     0       0           0
## 2        0       0    0    0    0       0     0     0       0           0
## 3        0       0    0    0    0       0     0     0       0           0
## 4        0       0    0    0    0       0     0     0       0           0
## 5        0       0    0    0    0       0     0     0       0           0
## 6        0       0    0    0    0       0     0     0       0           0
##   cow edit photo beast bum respectroyalty5 asham dem yal s2g bankrupt
## 1   0    0     0     0   0               0     0   0   0   0        0
## 2   0    0     0     0   0               0     0   0   0   0        0
## 3   0    0     0     0   0               0     0   0   0   0        0
## 4   0    0     0     0   0               0     0   0   0   0        0
## 5   0    0     0     0   0               0     0   0   0   0        0
## 6   0    0     0     0   0               0     0   0   0   0        0
##   envious loyal oxymoron despis pictwittercom29piieq0u belong scumbag
## 1       0     0        0      0                      0      0       0
## 2       0     0        0      0                      0      0       0
## 3       0     0        0      0                      0      0       0
## 4       0     0        0      0                      0      0       0
## 5       0     0        0      0                      0      0       0
## 6       0     0        0      0                      0      0       0
##   scumbaggeri telus biggest encount iphonecompani ruin cancer cheep loos
## 1           0     0       0       0             0    0      0     0    0
## 2           0     0       0       0             0    0      0     0    0
## 3           0     0       0       0             0    0      0     0    0
## 4           0     0       0       0             0    0      0     0    0
## 5           0     0       0       0             0    0      0     0    0
## 6           0     0       0       0             0    0      0     0    0
##   nut testicular flame freakappl stabl plz telstra agounalaki
## 1   0          0     0         0     0   0       0          0
## 2   0          0     0         0     0   0       0          0
## 3   0          0     0         0     0   0       0          0
## 4   0          0     0         0     0   0       0          0
## 5   0          0     0         0     0   0       0          0
## 6   0          0     0         0     0   0       0          0
```



