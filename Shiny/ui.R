library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggvis)
library(manipulate)
library(shiny)
shinyUI(fluidPage(theme= "bootstrap.css",
                  titlePanel("Topic Score by Sentiment Score for Tweets during the 2015 Canadian Election"),
                 
                  sidebarPanel(
                    radioButtons("searchtermcheck", "Term Searched on Twitter (Select one)",
                                       c("Harper", "Trudeau", "Mulcair", "ElizabethMay", "Conservative","Liberal", "NDP", "GreenParty", 
                                         "Canada", "CanadianPolitics", "cdnpoli", "oct19", "elxn42", "elxn2015"),
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
                    tabsetPanel(
                      tabPanel("Over Time",
                               plotOutput("plott4"),
                               plotOutput("plott5"),
                               plotOutput("plott6")),
                      tabPanel("In Detail",
                               plotlyOutput("plott1"),
                               plotlyOutput("plott2"),
                               plotlyOutput("plott3"))
                     
                  )
                  )
        )
)

