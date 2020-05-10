---
title: "How to correctly weight Eurobarometer data on R using the SPSS file?"
comments: yes
date: '2020-05-10T00:00:00Z'
output:
  html_document:
    df_print: paged
  pdf_document: default
  word_document: default
header:
  caption: ''
  image: ''
profile: yes
reading_time: no
share: yes
summary: For the Institute of European Democrats
tags:
- Eurobarometer
- Public Opinion
commentable: yes
---


First thing is to download the SPSS file with the Eurobarometer data.The Eurobarometer SPSS files are available on the GESIS website. 
https://www.gesis.org/eurobarometer-data-service/search-data-access/data-access

Once the data are downloaded, we import them into R. To do so, we will use the foreign library as in the example below:
```{r eval = FALSE} 
library(foreign)
dataSPSS<-read.spss("eb91_spss_en.sav", to.data.frame=TRUE)
```

Now, the goal is to build a matrix which will have the same data as in the Eurobarometer data in xlsx format. 
Those xlsx files are called "Volumes" available here:
https://data.europa.eu/euodp/en/data/dataset/S2253_91_5_STD91_ENG
The goal is to reproduce the Volume A which is the set of data showing the results per country. 

One of the first task will be to rename the country using their ISO. The countries of the respondents corresponds to the question labeled "b". 
This is useful for different reasons: 
- aesthetically as it is easier to read the data on a table with smaller labels but also
- programmatically as we will be able then to link the column to other libraries in R such as countrycode. We will come back to this aspect later on. 
Below how to correctly recode "b". 

```{r eval = FALSE} 
library(tidyverse)
# Rename the countries with their ISO
dataSPSS <- dataSPSS %>% 
  mutate(b = recode(b, "LUXEMBOURG"="LU","DANMARK"="DK","NEDERLAND"="NL","SUOMI"="FI","ÖSTERREICH"="AT","DEUTSCHLAND WEST"="DE","CESKA REPUBLIKA"="CZ","EESTI"="EE","SVERIGE"="SE","IRELAND"="IE","DEUTSCHLAND OST"="DE","SLOVENIJA"="SI","POLSKA"="PL","MAGYARORSZAG"="HU","MALTA"="MT","BELGIQUE"="BE","TURKIYE"="TR","CRNA GORA"="ME","LIETUVA"="LT","KYPROS"="CY","PORTUGAL"="PT","LATVIA"="LV","SHQIPERIA"="AL","ROMANIA"="RO","SLOVENSKA REPUBLIC"="SK","SRPSKI"="RS","REPUBLIKA MAKEDONIJA"="MK","UNITED KINGDOM"="GB","BALGARIJA"="BG","FRANCE"="FR","HRVATSKA"="HR","ESPANA"="ES","ITALIA"="IT","ELLADA"="GR","KUZEY KIBRIS TÜRK CUMHURIYETI"="CY_tcc")) 
```

Now, we will create the first data frame. Using some features from the tidyverse libraries. 
We are counting the number of answers of a particular question (qa1a_1RGPS, this is the answer for QA1a.1 and RGPS means "regroupement" which corresponds to the Totals of this question) using "count". "Count" has a very useful argument "wt" which allow to weight the data. In the Eurobarometer SPSS files, the weight corresponding to the country weight is w1. 
An overview of the different wieghts are available on the GESIS website: 
https://www.gesis.org/eurobarometer-data-service/survey-series/standard-special-eb/weighting-overview 
Then we apply a spread in order to have the countries as column, we remove the column with the labels which is not useful here. We remove as well the column corresponding to Nothern Cyprus (CY_tcc, this quesiton is not asked there) and Germany (DE). 

```{r eval = FALSE} 
#Weighting of the data, and remove CY(tcc) and DE
VolumeA <- dataSPSS %>% 
  count(qa1a_1RGPS, b, wt = w1, sort = TRUE) %>% 
  spread(b, n) %>% 
  select(-qa1a_1RGPS) %>% 
  select(-CY_tcc) %>% 
  select(-DE)
```

The removing of Germany is necessary as, historically, Germany is split in the Eurobarometer data between Western germany and Eastern Germany. In order to have only Germany as a whole, we have to apply another weight on the German data. This German weight is w3 and is applied following the same method:

```{r eval = FALSE} 
#Compute DE with the right weithing
VolumeA_DE <- dataSPSS %>% 
  count(qa1a_1RGPS, b, wt = w3, sort = TRUE) %>% 
  spread(b, n) %>% 
  select(-qa1a_1RGPS) %>% 
  select(DE)
```

Then, we add the correctly weighted DE data to our original VolumeA dataframe. 

```{r eval = FALSE} 
#Add DE to the VolumeA dataframe
VolumeA$DE = VolumeA_DE$DE
```

Here the result we have so far: 

```r 
view(VolumeA)
```
```
# A tibble: 4 x 33
      BE    DK      GR    ES    FI    FR    IE    IT     LU     NL    AT    PT     SE    GB    BG     CY
   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl>
1 709.   914.  146.    326.  907.  360.  761.  273.  473.   943.   851.  448.  827.   347.  303.  252.  
2 345.    88.8 865.    665.   80.8 631.  253.  736.   26.7   75.2  158.  540.  185.   626.  678.  248.  
3   3.84  10.0   0.943  15.2  15.8  22.5  13.6  16.8   6.47   1.45  13.1  20.1   3.07  59.5  50.0   4.24
4  NA     NA    NA      NA    NA    NA    NA    NA    NA     NA     NA    NA    NA     NA    NA    NA   
# … with 17 more variables: CZ <dbl>, EE <dbl>, HU <dbl>, LV <dbl>, LT <dbl>, MT <dbl>, PL <dbl>, RO <dbl>,
#   SK <dbl>, SI <dbl>, TR <dbl>, HR <dbl>, MK <dbl>, ME <dbl>, RS <dbl>, AL <dbl>, DE <dbl>
```

As we can see, we only have the bases but not the percentages. To compute the percentages, we use the questionr library which contain this feature. To do so, we have first to convert our Volume A to a Matrix, transform the NA in 0 before using questionr in order to produce the percentages. 

```{r eval = FALSE} 
#Creation of the matrix for questionr 
VolumeA_matrix <- data.matrix(VolumeA)
VolumeA_matrix[is.na(VolumeA_matrix)] <- 0

library(questionr)
#Create the matrix with the percentages
VolumeA_Perc<-questionr::cprop(VolumeA_matrix, digits=0, total=FALSE, n=FALSE, percent=TRUE)
```

The last step is to convert our VolumeA_Perc as a dataframe. In the same time, for convenience, we will transpose the dataframe so that the countries are in rows. This is generally better in order to build plots. 

```{r eval = FALSE} 
## Transpose so that countries are rows, and make it a data frame (unexpectedly, it's not)
## Note that we need to use the special data.frame.matrix() function and not just data.frame()
VolumeA_Perc2<-as.data.frame.matrix(t(VolumeA_Perc)) 
names(VolumeA_Perc2) <- c("Total_Good", "Total_Bad", "DK")
```

Now we can start to analyse the data such as ordering them. 

```{r eval = FALSE} 
VolumeA_Perc2 <- VolumeA_Perc2[order(VolumeA_Perc2$Total_Good, decreasing = TRUE),]
```

VolumeA_Perc2

```
   Total_Good Total_Bad         DK
LU   93.45266  5.267743 1.27959358
NL   92.48828  7.369534 0.14218636
FI   90.38298  8.044121 1.57289565
DK   90.24181  8.769949 0.98824382
AT   83.26907 15.451636 1.27929325
SE   81.46648 18.230996 0.30252235
DE   78.05275 20.810070 1.13717827
MT   76.20189 16.807272 6.99084001
EE   74.20940 22.425339 3.36526004
IE   74.03719 24.635998 1.32681386
CZ   67.60729 30.873856 1.51885171
PL   67.38727 27.311859 5.30086788
BE   67.03006 32.606737 0.36320678
SI   64.52626 34.109190 1.36455238
LT   61.38024 36.449714 2.17004215
HU   60.11313 38.922472 0.96440260
SK   52.43486 44.663066 2.90207473
ME   51.28096 44.836791 3.88224889
CY   49.96497 49.195507 0.83952309
LV   48.51482 46.947289 4.53789550
AL   47.07051 52.929488 0.00000000
MK   46.70557 51.546679 1.74774605
PT   44.48121 53.524681 1.99410751
TR   40.01730 58.510491 1.47220544
RS   39.30711 56.984344 3.70854747
RO   36.16685 62.128999 1.70415327
FR   35.53125 62.250376 2.21837227
GB   33.60111 60.635439 5.76345257
ES   32.41223 66.082274 1.50549201
BG   29.37130 65.782597 4.84610233
IT   26.58340 71.781431 1.63516521
HR   24.59170 73.858967 1.54933551
GR   14.41550 85.491345 0.09315223
```

The data have now a format that might be use in order to produce nice visualisation with the Eurobarometer data as the one below.



This visualisation is inspired by Dimiter Toshkov's work.
http://dimiter.eu/Visualizations_files/ESS/Visualizing_ESS_data.html
A description of the code with Eurobaromter data will be provided in a future article. 


