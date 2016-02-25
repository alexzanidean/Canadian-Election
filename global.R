library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggvis)
library(manipulate)
library(shiny)
library("RColorBrewer", lib.loc="/usr/local/lib/R/site-library")

Aggdata <- read.csv("AggDataedit.csv")
Normaldata<- Aggdata %>% group_by(Date, searchterm, Theme) %>% summarise_each(funs(sum), -themetweet, -sentitweet)
Normaldata <- merge.data.frame(Aggdata, Normaldata, by = c("Date","searchterm","Theme"))
Normaldata$normalizednumber <- (Normaldata$Number.x/Normaldata$Number.y)*100

Normaldata$fcategory <- factor(Normaldata$searchterm)
