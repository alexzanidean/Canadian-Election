library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggvis)
library(manipulate)
library(shiny)
library("RColorBrewer", lib.loc="/usr/local/lib/R/site-library")
shinyUI(fluidPage(theme= "bootstrap.css",
                  titlePanel("Topic Score by Sentiment Score for Tweets during the 2015 Canadian Election"),
                 
                  sidebarPanel(
                    checkboxGroupInput("searchtermcheck", "Term Searched on Twitter",
                                       c("Harper", "Trudeau", "Mulcair", "ElizabethMay", "Conservative","Liberal", "NDP", "GreenParty", "Canada", 
                                         "Canadian Politics", "cdnpoli", "oct19", "elxn42", "elxn2015"),
                                       selected = "Harper"),
                    
                    dateInput('date',
                              label = "Select Date between Sept 28 and Oct 30",
                              value = "2015-09-28",
                              min = "2015-09-28", max = "2015-10-30",
                              format = "dd/mm/yyyy",
                              startview = 'day', language = 'en'
                    )
                  ),
                  mainPanel(
                    plotlyOutput("plott1"),
                    plotlyOutput("plott2"),
                    plotlyOutput("plott3")
                  )
        )
)

