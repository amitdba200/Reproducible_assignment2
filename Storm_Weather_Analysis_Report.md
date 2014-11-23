---
title: "Storm_Weather_Analysis_Report"
author: "amit sanghi"
date: "Sunday, November 23, 2014"
output: html_document
---

####An analysis report of the health and economic impact caused by severe weather events across the USA, based on the data sourced from the U.S. National Oceanic and Atmospheric Administration's (NOAA) Storm Database####


###SYNOPSIS###

*Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. In this report we present some aspects of major weather events impact on population health and on economy. We have used data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) Storm database. We classify top most serious events and visualize results by plotting related distribution diagrams. As a conclusion we observe that "Tornado"","Excessive heat" and "flood" are events which causes most health impact.We observe that events "Huricane/Typhoon" and "flood" causes maximum economic impact*


I provide environment information when prepared this report to support reproducibility


```r
## environment for this anlysis
sessionInfo()
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_India.1252  LC_CTYPE=English_India.1252   
## [3] LC_MONETARY=English_India.1252 LC_NUMERIC=C                  
## [5] LC_TIME=English_India.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.8        rmarkdown_0.2.64 RCurl_1.95-4.3   bitops_1.0-6    
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.4    evaluate_0.5.5  formatR_1.0     htmltools_0.2.6
## [5] stringr_0.6.2   tools_3.1.1     yaml_2.1.13
```


##Section 1.Data Processing ##

if the file doesn't exists in local direcotry,we download it from internet location.
It is a csv file name "repdata-data-StormData.csv"
We also check the dimesnsion of the data.

```r
if (!file.exists("repdata-data-StormData.csv")) 
{ 
### download from internet
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "repdata-data-StormData.csv.bz2") 
dateDownloaded=date() 
paste("The data downloaded on ",dateDownloaded)
bunzip2(file="repdata-data-StormData.csv.bz2",destname="repdata-data-StormData.csv")
}
data<-read.csv("repdata-data-StormData.csv")     
###using ggplot function
library(ggplot2)
## Using stringr library for "str_trim" function.
library(stringr)
library(plyr)
### number of rows in data is 902297
dim(data)
```

```
## [1] 902297     37
```

The storm data set has 902297 rows and 37 columns.

The column names of the storm dataset are: 

```r
colnames(data)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

The report uses only the data that were collected from 1 January, 1996 to 30 November 2011, since according to Storm Events Database of NOAA, 48 event types were used from 1 January, 1996.

```r
## Converting the BGN_DATE column to date type.
data$BGN_DATE <- as.Date(data$BGN_DATE,"%m/%d/%Y")
## Extract records starting from 1 January 1996.
result_data <- data[data$BGN_DATE >= as.Date("1996/01/01","%Y/%m/%d"),]
```

To provide the required analysis,the required columns are : 
"EVTYPE", "PROPDMG", "PROPDMGEXP", "FATALITIES", "INJURIES", "CROPDMG", "CROPDMGEXP"

```r
### Select required columns from the data
result_data<-result_data[, c("EVTYPE", "PROPDMG", "PROPDMGEXP", "FATALITIES", "INJURIES", "CROPDMG", "CROPDMGEXP")]
```
The number of Events in the data set 

```r
length(unique(result_data$EVTYPE))
```

```
## [1] 516
```

To clean the Event name column values , Remove the event name which contains "Summary" as these events are not valid as per available documentation. Update the case of event name to uppr case and remove leading whitespaces.

```r
### removing invalid event names containing "Summary"
result_data<-result_data[!grepl("^Summary", result_data$EVTYPE),]
### Change event name to upper case
result_data$EVTYPE<-toupper(result_data$EVTYPE)
### Removing Whitespaces
result_data$EVTYPE <- str_trim(result_data$EVTYPE)
```



The updated number of Events in the cleaned data set 

```r
length(unique(result_data$EVTYPE))
```

```
## [1] 367
```

The updated storm data set has number of rows : 


```r
nrow(result_data)
```

```
## [1] 653458
```

Ignoring the records where PROPDMGEXP and CROPDMGEXP contains invalid values.
The valid values are "B"- Billion   "M" -Million   "K" - thousand as per documentation.
There is no mention of "H" in the documenation.
Number of such records is :

```r
vec<-length(result_data$PROPDMGEXP %in% c("B")|result_data$PROPDMGEXP %in% c("M")|result_data$PROPDMGEXP %in% c("K")|result_data$PROPDMGEXP %in% c(""))
table(vec)["FALSE"]
```

```
## <NA> 
##   NA
```
The total rows are less than 0.01%. It will not make a noticable impact on analysis result.Ignoring the records : 


```r
### get the number of rows in data set
nRowCount<-nrow(result_data)
### subset data with interpretable units
result_data<-subset(result_data,result_data$PROPDMGEXP %in% c("K","M","B",""))
### subset data with interpretable units
result_data<-subset(result_data,result_data$CROPDMGEXP %in% c("K","M","B",""))
```

To compute the total dollar amounts for property damage and crop damage, the approach being followed, as per the documentation and analysis based on examining the dataset, is to multiply the PROPDMGEXP values with the PROPDMG values, as well as the CROPDMGEXP values with the CROPDMG values to compute the total property damages and crop damages by event respectively. B- Billion , M - Million and K - thousand


```r
  ### Convert PROPDMG and CROPDMG values with reference to units
  ### update "PROPDMG" value
  index_list <- (result_data$PROPDMGEXP %in% c("M"))
  result_data$PROPDMG[index_list] = result_data$PROPDMG[index_list]*1000000
    index_list <- (result_data$PROPDMGEXP %in% c("K"))
  result_data$PROPDMG[index_list] = result_data$PROPDMG[index_list]*1000
    index_list <- (result_data$PROPDMGEXP %in% c("B"))
  result_data$PROPDMG[index_list] = result_data$PROPDMG[index_list]*1000000000
    ### update "CROPDMG" value
    index_list <- (result_data$CROPDMGEXP %in% c("B"))
  result_data$CROPDMG[index_list] = result_data$CROPDMG[index_list]*1000000000
      index_list <- (result_data$CROPDMGEXP %in% c("M"))
  result_data$CROPDMG[index_list] = result_data$CROPDMG[index_list]*1000000
      index_list <- (result_data$CROPDMGEXP %in% c("K"))
  result_data$CROPDMG[index_list] = result_data$CROPDMG[index_list]*1000
```



##Section 2.Results ##

*Reports addresse following questions - 1: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health? - 2: Across the United States, which types of events have the greatest economic consequences?*

We make plot to show the top event for fatality count :

```r
### Subset the data where fatality count is greater than 0
fatality_data<-subset(result_data,result_data$FATALITIES>0)
### Summation of fatality count gropu by event type
event_fatality_result<-aggregate(fatality_data$FATALITIES, by=list(fatality_data$EVTYPE), FUN=sum)
### update column name of the result data set
colnames(event_fatality_result)<-c("EVTYPE","total_fatalities")
### arrange the data in the order
event_ord <- event_fatality_result$EVTYPE [order (event_fatality_result$total_fatalities, decreasing = TRUE)]
###  Create a plot
out<-ggplot(data = subset (event_fatality_result, EVTYPE %in% event_ord [1 : 10]), aes(reorder(EVTYPE, total_fatalities), total_fatalities))
out<-out+geom_bar(stat = "identity", colour = "blue")+ xlab("EVTYPE")+ylab("fatalities") 
out+ coord_flip() +theme(axis.text.x = element_text(angle = 90))+ ggtitle("Total top 10 events fatalities count")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

We make plot to show the top event for Injury count :


```r
### Subset the data where injury count is greater than 0
injury_data<-subset(result_data,result_data$INJURIES>0)
### Summation of injury count gropu by event type
event_injury_result<-aggregate(injury_data$INJURIES, by=list(injury_data$EVTYPE), FUN=sum)
### update column name of the result data set
colnames(event_injury_result)<-c("EVTYPE","total_injuries")
event_ord <- event_injury_result$EVTYPE [order (event_injury_result$total_injuries, decreasing = TRUE)]
out<-ggplot(data = subset (event_injury_result, EVTYPE %in% event_ord [1 : 10]), aes(reorder(EVTYPE, total_injuries), total_injuries))
out<-out+geom_bar(stat = "identity", colour = "blue")+ xlab("EVTYPE")+ylab("injuries") 
out+  coord_flip() +theme(axis.text.x = element_text(angle = 90))+ ggtitle("Total top 10 events injuries count")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

*From above analysis we found that most harmful weather events are "TORNADO","Excessive Heat" and "Flood" with highest number of Injuries and high number of Fatalities.*

- 2: Across the United States, which types of events have the greatest economic consequences?

We create plot of total damage ( PROPDMG + CROPDMG)


```r
### Subset the data set where damage value is >0
result_data<-subset(result_data,(result_data$PROPDMG>0)&(result_data$CROPDMG>0))
### Calculate total of PROPDMG and CROPDMG
for (index_value in 1:nrow(result_data))
{
result_data$total[index_value]<-as.numeric(sum(result_data$PROPDMG[index_value],result_data$CROPDMG[index_value]))
}
### summation of total event wise
impact_data<-aggregate(result_data$total, by=list(result_data$EVTYPE), FUN=sum)
colnames(impact_data)<-c("EVTYPE","total_of_event")
### find  the order
event_ord <- impact_data$EVTYPE [order (impact_data$total_of_event, decreasing = TRUE)]
### plot the graph
out<-ggplot(data = subset(impact_data, EVTYPE %in% event_ord [1 : 10]), aes(reorder(EVTYPE, total_of_event), total_of_event))
out<-out+geom_bar(stat = "identity", colour = "blue")+ xlab("EVTYPE")+ylab("total financial damage- USD") 
out+  coord_flip() +theme(axis.text.x = element_text(angle = 90))+ ggtitle("Total top 10 events-damage due to events")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

*From above analysis we found that events "Huricane/Typhoon" and "flood" causes maximum economic impact*
