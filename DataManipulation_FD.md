---
title: "Data Manipulation in R"
output:
  html_document:
    keep_md: yes
---

Load the required packages


```r
suppressPackageStartupMessages({
    library(dplyr, quietly = T)
    library(lubridate, quietly = T)
})
```


Let's read the data set into R studio environment


```r
setwd("F:\\E-learn\\Jigsaw\\Intro to R\\Data Manipulation")
fd=read.csv("FlightDelays.csv")
class(fd)
```

```
## [1] "data.frame"
```

```r
dim(fd)
```

```
## [1] 2201   13
```

```r
str(fd)
```

```
## 'data.frame':	2201 obs. of  13 variables:
##  $ schedtime   : int  1455 1640 1245 1715 1039 840 1240 1645 1715 2120 ...
##  $ carrier     : Factor w/ 8 levels "CO","DH","DL",..: 5 2 2 2 2 2 2 2 2 2 ...
##  $ deptime     : int  1455 1640 1245 1709 1035 839 1243 1644 1710 2129 ...
##  $ dest        : Factor w/ 3 levels "EWR","JFK","LGA": 2 2 3 3 3 2 2 2 2 2 ...
##  $ distance    : int  184 213 229 229 229 228 228 228 228 228 ...
##  $ date        : Factor w/ 31 levels "01-01-2004","01-02-2004",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ flightnumber: int  5935 6155 7208 7215 7792 7800 7806 7810 7812 7814 ...
##  $ origin      : Factor w/ 3 levels "BWI","DCA","IAD": 1 2 3 3 3 3 3 3 3 3 ...
##  $ weather     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ dayweek     : int  4 4 4 4 4 4 4 4 4 4 ...
##  $ daymonth    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ tailnu      : Factor w/ 549 levels "N10323","N10575",..: 526 263 382 350 385 374 241 227 246 372 ...
##  $ delay       : Factor w/ 2 levels "delayed","ontime": 2 2 2 2 2 2 2 2 2 2 ...
```

```r
summary(fd)
```

```
##    schedtime       carrier       deptime      dest         distance    
##  Min.   : 600   DH     :551   Min.   :  10   EWR: 665   Min.   :169.0  
##  1st Qu.:1000   RU     :408   1st Qu.:1004   JFK: 386   1st Qu.:213.0  
##  Median :1455   US     :404   Median :1450   LGA:1150   Median :214.0  
##  Mean   :1372   DL     :388   Mean   :1369              Mean   :211.9  
##  3rd Qu.:1710   MQ     :295   3rd Qu.:1709              3rd Qu.:214.0  
##  Max.   :2130   CO     : 94   Max.   :2330              Max.   :229.0  
##                 (Other): 61                                            
##          date       flightnumber  origin        weather           dayweek     
##  1/22/2004 :  86   Min.   : 746   BWI: 145   Min.   :0.00000   Min.   :1.000  
##  01-06-2004:  85   1st Qu.:2156   DCA:1370   1st Qu.:0.00000   1st Qu.:2.000  
##  01-08-2004:  85   Median :2385   IAD: 686   Median :0.00000   Median :4.000  
##  1/13/2004 :  85   Mean   :3815              Mean   :0.01454   Mean   :3.905  
##  1/20/2004 :  85   3rd Qu.:6155              3rd Qu.:0.00000   3rd Qu.:5.000  
##  1/21/2004 :  85   Max.   :7924              Max.   :1.00000   Max.   :7.000  
##  (Other)   :1690                                                              
##     daymonth         tailnu         delay     
##  Min.   : 1.00   N225DL :  65   delayed: 428  
##  1st Qu.: 8.00   N242DL :  56   ontime :1773  
##  Median :16.00   N223DZ :  50                 
##  Mean   :16.02   N221DL :  45                 
##  3rd Qu.:23.00   N241DL :  36                 
##  Max.   :31.00   N722UW :  36                 
##                  (Other):1913
```


Variables `weather`, `dayweek` and `daymonth` are stored as numrical variables. These are supposed to be categorical variables. Let's convert them into categorical variables 


```r
fd$weather=as.factor(fd$weather)
fd$dayweek=as.factor(fd$dayweek)
fd$daymonth=as.factor(fd$daymonth)
str(fd)
```

```
## 'data.frame':	2201 obs. of  13 variables:
##  $ schedtime   : int  1455 1640 1245 1715 1039 840 1240 1645 1715 2120 ...
##  $ carrier     : Factor w/ 8 levels "CO","DH","DL",..: 5 2 2 2 2 2 2 2 2 2 ...
##  $ deptime     : int  1455 1640 1245 1709 1035 839 1243 1644 1710 2129 ...
##  $ dest        : Factor w/ 3 levels "EWR","JFK","LGA": 2 2 3 3 3 2 2 2 2 2 ...
##  $ distance    : int  184 213 229 229 229 228 228 228 228 228 ...
##  $ date        : Factor w/ 31 levels "01-01-2004","01-02-2004",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ flightnumber: int  5935 6155 7208 7215 7792 7800 7806 7810 7812 7814 ...
##  $ origin      : Factor w/ 3 levels "BWI","DCA","IAD": 1 2 3 3 3 3 3 3 3 3 ...
##  $ weather     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ dayweek     : Factor w/ 7 levels "1","2","3","4",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ daymonth    : Factor w/ 31 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ tailnu      : Factor w/ 549 levels "N10323","N10575",..: 526 263 382 350 385 374 241 227 246 372 ...
##  $ delay       : Factor w/ 2 levels "delayed","ontime": 2 2 2 2 2 2 2 2 2 2 ...
```



```r
head(fd$date)
```

```
## [1] 01-01-2004 01-01-2004 01-01-2004 01-01-2004 01-01-2004 01-01-2004
## 31 Levels: 01-01-2004 01-02-2004 01-03-2004 01-04-2004 ... 1/31/2004
```

```r
str(fd$date)
```

```
##  Factor w/ 31 levels "01-01-2004","01-02-2004",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Converting the variable `date` into Date format. Looking at the first 6 and last 6 observations in the date column it is clear that the date is of the format mdy Date column is converted into date format using the `lubridate()` library.



```r
fd$date=mdy(fd$date)

head(fd$date)
```

```
## [1] "2004-01-01" "2004-01-01" "2004-01-01" "2004-01-01" "2004-01-01"
## [6] "2004-01-01"
```

```r
str(fd$date)
```

```
##  Date[1:2201], format: "2004-01-01" "2004-01-01" "2004-01-01" "2004-01-01" "2004-01-01" ...
```


**Q1.Find out the number of delayed flights for all weekdays**


```r
fd%>%filter(delay=="delayed"&weekdays(date)!="Saturday"&weekdays(date)!="Sunday")%>%nrow()
```

```
## [1] 336
```


**Q2 Find the average distance, total distance, and count for all delayed flights on Friday**



```r
fd%>%filter(weekdays(date)=="Friday" & delay=="delayed")%>%summarize(mean(distance),sum(distance),length(distance))
```

```
##   mean(distance) sum(distance) length(distance)
## 1         210.28         15771               75
```


**Q3 Find out how many flights were on time on Week days and Weekends (Consider Saturday and Sunday as weekends)**



```r
fd%>%filter(delay=="ontime",weekdays(date)!="Saturday"&weekdays(date)!="Sunday")%>%nrow()# number of delayed flights on weekdays
```

```
## [1] 1362
```

```r
fd%>%filter(delay=="ontime",weekdays(date)=="Saturday"|weekdays(date)=="Sunday")%>%nrow()# number of delayed flights on weekends
```

```
## [1] 411
```


**Q4 Find out the number of flights for each destination across all weekdays**


```r
table(weekdays(fd$date),fd$dest)
```

```
##            
##             EWR JFK LGA
##   Friday    122  64 205
##   Monday     90  47 171
##   Saturday   86  59 105
##   Sunday     72  55 126
##   Thursday  113  63 196
##   Tuesday    86  48 173
##   Wednesday  96  50 174
```


**Q5 Find out the number of times weather was bad across all weekdays. (1 indicates bad weather)**



```r
fd%>%filter(weather==1)%>%group_by(dayweek)%>%summarise(n=n())
```

```
## # A tibble: 4 x 2
##   dayweek     n
##   <fct>   <int>
## 1 1          14
## 2 2          15
## 3 5           1
## 4 7           2
```

