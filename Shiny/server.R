shinyServer(function(input, output) {
  
  output$dateText2 <- renderText({as.character.Date(input$date, format="%d/%m/%Y")
    output$scoretype <- renderText({as.character(input$score)})   
    
  })
  output$plott1 <- renderPlotly({
    gg <- ggplot(
      data=subset(Normaldata, Theme == "econ" & searchterm == as.character(input$searchtermcheck) & Date == as.character.POSIXt(input$date)),
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
      data=subset(Normaldata, Theme == "healthcare" & searchterm == as.character(input$searchtermcheck) & Date == as.character.POSIXt(input$date)),
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
      data=subset(Normaldata, Theme == "Climate" & searchterm == as.character(input$searchtermcheck) & Date == as.character.POSIXt(input$date)),
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
  
})
