shinyServer(function(input, output) {
  
  output$dateText2 <- renderText({as.character.Date(input$date, format="%d/%m/%Y")
    output$scoretype <- renderText({as.character(input$score)})   
    
  })
  output$plott1 <- renderPlotly({
    gg <- ggplot(
      data=subset(Normaldata, Theme == "econ" & searchterm == as.character(input$searchtermcheck) 
                  & Date == as.character.POSIXt(input$date)),
      aes_string(x= "themetweet", y="sentitweet", size= "normalizednumber", label= "normalizednumber", 
                 colour = "searchterm", group = as.factor("searchterm")),guide=TRUE)+
      geom_point(alpha=0.5)+
      scale_x_continuous(name= "Economic Score", limits=c(-1,5))+
      scale_y_continuous(name="Sentiment Score", limits=c(-10,10))+
      geom_text(size=4)+
      theme_bw()+
      ggtitle("Economic Sentiment Scores")+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    p <- ggplotly(gg)
    p
  })
  output$plott2 <- renderPlotly({
    gg <- ggplot(
      data=subset(Normaldata, Theme == "healthcare" & searchterm == as.character(input$searchtermcheck) 
                  & Date == as.character.POSIXt(input$date)),
      aes_string(x= "themetweet", y="sentitweet", size= "normalizednumber", label= "normalizednumber", 
                 colour = "searchterm", group = as.factor("searchterm")),guide=TRUE)+
      geom_point(alpha=0.5)+
      scale_x_continuous(name= "Healthcare Score", limits=c(-1,5))+
      scale_y_continuous(name="Sentiment Score", limits=c(-10,10))+
      geom_text(size=4)+
      theme_bw()+
      ggtitle("Healthcare Sentiment Scores")+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    p <- ggplotly(gg)
    p
  })
  output$plott3 <- renderPlotly({
    gg <- ggplot(
      data=subset(Normaldata, Theme == "Climate" & searchterm == as.character(input$searchtermcheck) 
                  & Date == as.character.POSIXt(input$date)),
      aes_string(x= "themetweet", y="sentitweet", size= "normalizednumber", label= "normalizednumber", 
                 colour = "searchterm", group = as.factor("searchterm")),guide=TRUE)+
      geom_point(alpha=0.5)+
      scale_x_continuous(name= "Climate Score", limits=c(-1,5))+
      scale_y_continuous(name="Sentiment Score", limits=c(-10,10))+
      geom_text(size=4)+
      theme_bw()+
      ggtitle("Climate Sentiment Scores")+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    p <- ggplotly(gg)
    p
  })
  output$plott4 <- renderPlot({
    gg <- ggplot(
      data=subset(Normaldata, themetweet != 0 & sentitweet != 0 & Theme == "econ" & searchterm == as.character(input$searchtermcheck)),
     aes_string(x= "Date1", y="normalizednumber", colour= "sentitweet", group= "themetweet"), guide=TRUE)+
     geom_point(aes(shape=as.factor(themetweet), size=0.5))+
     scale_shape_manual(values = c(65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82))+
     ggtitle("Economic Scores over time, Colour by Sentiment")
  gg
})
output$plott5 <- renderPlot({
  gg <- ggplot(
    data=subset(Normaldata, themetweet != 0 & sentitweet != 0 & Theme == "healthcare" & searchterm == as.character(input$searchtermcheck)),
    aes_string(x= "Date1", y="normalizednumber", colour= "sentitweet", group= "themetweet"), guide=TRUE)+
    geom_point(aes(shape=as.factor(themetweet), size=0.5))+
    scale_shape_manual(values = c(65,66,67,68,69,70,71,72,73,74,75))+
    ggtitle("Healthcare Scores over time, Colour by Sentiment")
  gg
})
output$plott6 <- renderPlot({
  gg <- ggplot(
    data=subset(Normaldata, themetweet != 0 & sentitweet != 0 & Theme == "Climate" & searchterm == as.character(input$searchtermcheck)),
    aes_string(x= "Date1", y="normalizednumber", colour= "sentitweet", group= "themetweet"), guide=TRUE)+
    geom_point(aes(shape=as.factor(themetweet), size=0.5))+
    scale_shape_manual(values = c(65,66,67,68,69,70,71,72,73,74,75))+
    ggtitle("Climate Scores over time, Colour by Sentiment")
  gg
   })

})
