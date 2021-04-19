# FORD DATA / RFM
# Gülhan Damla Aþýk - 2000136

# RFM helps to identify customers who are more likely to respond to promotions 
# by segmenting them into various categories.

# free memory
rm(list = ls())
gc()
getwd()

library(dplyr)
library(tidyr)
install.packages("stringr")  # filtering strings on dplyr
library(stringr)
install.packages("rfm")
library(rfm)

FordDataAll <- read.csv("Forddata.csv", header = T , sep = ";")

head(FordDataAll)
glimpse(FordDataAll)
# 635 rows

# Convert columns types
FordDataAll$Date.bought <- as.Date(FordDataAll$Date.bought, format = "%d.%m.%Y")
FordDataAll$Amount <- as.numeric(sub(",",".",FordDataAll$Amount, fixed = TRUE))

max(FordDataAll[,1]) # "2012-04-05"
min(FordDataAll[,1]) # "1990-08-15"

# Set Start and End dates (10years period)
startDateF <- as.Date("19910101","%Y%m%d")
endDateF <- as.Date("20110101","%Y%m%d")

# order by date
FordDataAll <- FordDataAll[order(FordDataAll[,1],decreasing = TRUE),]

# filter by Start and End dates
FordDataAll <- FordDataAll[FordDataAll[,1]>= startDateF,]
FordDataAll <- FordDataAll[FordDataAll[,1]<= endDateF,]

glimpse(FordDataAll)
# 515 rows remain

# divide data: Ford and Other
FordData <- FordDataAll %>%
  filter(str_detect(Bought, "ford"))
OtherData <- FordDataAll %>%
  filter(!str_detect(Bought, "ford"))

# create main data.frame by removing duplicated ID's
FordDataMain <- FordData[!duplicated(FordData[,2]),]
glimpse(FordDataMain)
# 82 rows
OtherDataMain <- OtherData[!duplicated(OtherData[,2]),]
glimpse(OtherDataMain)
# 104 rows


### now we will calculate some columns for RFM model and combine these columns to the main DF
# 1) RECENCY
RecencyFord <-as.numeric(difftime(endDateF,FordDataMain[,1],units="days"))
FordDataMain <-cbind(FordDataMain,RecencyFord)
head(FordDataMain)

RecencyOther <-as.numeric(difftime(endDateF,OtherDataMain[,1],units="days"))
OtherDataMain <-cbind(OtherDataMain,RecencyOther)
head(OtherDataMain)

# 2) FREQUENCY
freqFord <- as.data.frame(table(FordData[,2]))
FrequencyFord <- freqFord[,2]
head(FrequencyFord)
FordDataMain <- cbind(FordDataMain,FrequencyFord)
# check if Customer Frequency values are True for some rows
table(FordData$Customer)  
head(FordDataMain) # customer == 1 occurs 3 times. Looks ok

freqOther <- as.data.frame(table(OtherData[,2]))
FrequencyOther <- freqOther[,2]
head(FrequencyOther)
OtherDataMain <- cbind(OtherDataMain,FrequencyOther)
# check if Customer Frequency values are True for some rows
table(OtherData$Customer)  
head(OtherDataMain) # customer == 5 occurs 2 times. Looks ok

# 3) MONETARY
# Total amount spend per customer
mFord <- as.data.frame(tapply(FordData[,4],FordData[,2],sum))
head(mFord)
MonetaryFord <- round(mFord[,1]/FrequencyFord,2)
FordDataMain <- cbind(FordDataMain,MonetaryFord)
head(FordDataMain)

mOther <- as.data.frame(tapply(OtherData[,4],OtherData[,2],sum))
head(mOther)
MonetaryOther <- round(mOther[,1]/FrequencyOther,2)
OtherDataMain <- cbind(OtherDataMain,MonetaryOther)
head(OtherDataMain)

par(mfrow = c(1,3))
hist(FordDataMain$RecencyFord)
hist(FordDataMain$FrequencyFord)
hist(FordDataMain$MonetaryFord)

par(mfrow = c(1,3))
hist(OtherDataMain$RecencyOther)
hist(OtherDataMain$FrequencyOther)
hist(OtherDataMain$MonetaryOther)

mean(FordDataMain$Amount) # 23.54317
mean(OtherDataMain$Amount) # 16.48375

################################################################################
#	Scoring the Recency, Frequency, and Monetary in r, f, and m in aliquots independently
#	r -  The highest point of Recency
#	f -  The highest point of Frequency
#	m -  The highest point of Monetary
#################################################################################
# FORD
#################################################################################

FordDataMainRFM <- FordDataMain

quantile(FordDataMainRFM$MonetaryFord)
# 0%     25%     50%     75%    100% 
# 3.1400 17.9050 25.8050 32.2875 48.6400
rankMonetaryFord <- cut(FordDataMainRFM$MonetaryFord, breaks=c(0,18,26,33,50))
levels(rankMonetaryFord)
#  "(0,18]"  "(18,26]" "(26,33]" "(33,50]"
levels(rankMonetaryFord) <- c(1,2,3,4,5)
levels(rankMonetaryFord)
# "1" "2" "3" "4" "5"

quantile(FordDataMainRFM$FrequencyFord)
# 0%     25%     50%     75%    100% 
#  1    1    2    3    5
rankFreqFord <- cut(FordDataMainRFM$FrequencyFord, breaks=c(0,1,2,3,5,6))
levels(rankFreqFord)
#  "(0,1]" "(1,2]" "(2,3]" "(3,5]" "(5,6]"
levels(rankFreqFord) <- c(1,2,3,4,5)
levels(rankFreqFord)
# "1" "2" "3" "4" "5"

quantile(FordDataMainRFM$RecencyFord)
# 0%     25%     50%     75%    100% 
# 669.00 2358.00 3684.00 4592.75 6631.00
rankRecencyFord <- cut(FordDataMainRFM$RecencyFord, breaks=c(0,670,2360,3690,4595,6635))
levels(rankRecencyFord)
#   "(0,670]"             "(670,2.36e+03]"      "(2.36e+03,3.69e+03]" "(3.69e+03,4.6e+03]"  "(4.6e+03,6.64e+03]"
levels(rankRecencyFord) <- c(5,4,3,2,1)
levels(rankRecencyFord)
# "5" "4" "3" "2" "1"

FordDataMainRFM <- data.frame(cbind(FordDataMainRFM, rankRecencyFord, rankFreqFord, rankMonetaryFord))
head(FordDataMainRFM)
glimpse(FordDataMainRFM)
FordDataMainRFM$rankFreqFord <- as.integer(FordDataMainRFM$rankFreqFord)
FordDataMainRFM$rankRecencyFord <- as.integer(as.character(FordDataMainRFM$rankRecencyFord))
FordDataMainRFM$rankMonetaryFord <- as.integer(FordDataMainRFM$rankMonetaryFord)

# calculate the total score
Total_ScoreFord <- c(100*FordDataMainRFM$rankRecencyFord + 10*FordDataMainRFM$rankFreqFord + FordDataMainRFM$rankMonetaryFord)
FordDataMainRFM <- cbind(FordDataMainRFM,Total_ScoreFord)
head(FordDataMainRFM)

# target customers 
quantile(FordDataMainRFM$Total_ScoreFord)
# 0%     25%     50%     75%    100% 
# 111.00 159.25 277.50 394.25 533.00
Total_ScoreFordScored <- cut(FordDataMainRFM$Total_ScoreFord, breaks=c(0,120,160,280,400,550))
levels(Total_ScoreFordScored)
#  "(0,120]"   "(120,160]" "(160,280]" "(280,400]" "(400,550]"
levels(Total_ScoreFordScored) <- c(1,2,3,4,5)
levels(Total_ScoreFordScored)
# "1" "2" "3" "4" "5"
FordDataMainRFM <- data.frame(cbind(FordDataMainRFM,Total_ScoreFordScored))
head(FordDataMainRFM)
FordDataMainRFM$Total_ScoreFordScored <- as.integer(FordDataMainRFM$Total_ScoreFordScored)
glimpse(FordDataMainRFM)
table(FordDataMainRFM$Total_ScoreFordScored)
# 1  2  3  4  5 
# 5 16 20 20 21

FordDataMainRFMb <- FordDataMainRFM[order(FordDataMainRFM[,12], decreasing = TRUE),]
head(FordDataMainRFMb)
# In FORD data we can target 5th group 