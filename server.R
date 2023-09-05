
#library(dplyr)
library(shinydashboard)
library(shiny)
library(threejs)

# library(plotly)
# library(ggplot2)
# library(readr)
# library(ggmap)
# library(rworldmap)
Previous_Button = tags$div( actionButton( "Prev_Tab", HTML( '<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>' )))
Next_Button = div(actionButton( "Next_Tab", HTML( '<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>' )))

shinyServer( function( input, output, session ) {
  bo <- reactive({
    x = as.numeric( input$content )
    if(x == 1 || x == 2){
      a <- df %>% summarise( nr_of_attacks = n() )
      b <- a$nr_of_attacks
      c <- list( "No.of attacks",b,"bomb" )
    }
    if(x == 3){
      a <- df %>% group_by( gname ) %>% summarise( terr = n() )
      b <- as.numeric( count(a) )
      c <- list( "Terrorist Groups",b,"exclamation-triangle" )
    }
    if(x == 4||x == 5){
      a <- df %>% group_by( gname ) %>% summarise( terr = n() )
      b <- as.numeric( count(a) )
      c <- list( "Terrorist Groups",b,"exclamation-triangle" )
    }
    if(x == 6){
      a <- sum( b$c )
      c <- list( "No. of Kills",a,"exclamation-triangle" )
    }
    return( c )
  })
  ch<-reactive({
    x = as.numeric(input$content)
    if(x == 3){
      selectInput( "mapt", "Select Terrorist Group :", 
                   choices = c( "Taliban", "Shining Path (SL)", "Farabundo Marti National Liberation Front (FMLN)", "Communist Party of India - Maoist (CPI-Maoist)", 
                                "Liberation Tigers of Tamil Eelam (LTTE)", "Irish Republican Army (IRA)", "Revolutionary Armed Forces of Colombia (FARC)" ))
    }
  })
  output$leaf<-renderUI({
    ch()
  })
  plo<-reactive({
    x = as.numeric( input$content )
    if(x == 3){
      leafletOutput( "mapleaf" )
    }
    else {
      plotlyOutput( "plot1",height = 500,width = "100%" )
    }
  })
  output$plotgraph <- renderUI({
    plo()
  })
  output$mapleaf <- renderLeaflet({
    v <- subset( df500, gname == as.character( input$mapt ) )
    title <- "Location of terrorist attacks by group"
    leaflet() %>%
      addProviderTiles( "CartoDB.Positron" ) %>% # Add default OpenStreetMap map tiles
      addMarkers( lng = v$longitude, lat = v$latitude , popup = v$gname, clusterOptions = markerClusterOptions() ) %>%
      addControl( title, position = "topleft", className="map-title" )
  })
  barplottest <- reactive({
    x = as.numeric(input$content)
    if (x == 1){
      k <- plot_ly( gra1,x = ~gra1$year,y = ~gra1$tr,type = 'scatter',mode = "line+markers",
                    text = ~paste( 'Year :',gra1$year,'</br> Attacks :',gra1$tr )) %>%
        layout( title = "Terrorism attacks over time",xaxis = list( title = "Year" ),
                yaxis = list(title="Attacks") )
    }
    if (x == 2){
      k <- ggplot( data = df, aes(x = targtype1, fill = decade)) +
        geom_histogram( stat = 'count' ) + xlab( "Target_Type" ) +
        theme( axis.text.x = element_text( angle = 45, hjust = 1 )) +
        labs( title = 'Target distribution of terrorism over time' )
      k <- ggplotly(k)
    }
    if(x == 4){
      Year <- top10$year
      Attacks <- top10$nr_of_attacks
      k <- plot_ly( top10, x = ~Year, y = ~Attacks, color = top10$gname,
                    text = ~paste( 'Year:',top10$year,'</br> Attack: ',
                                   top10$nr_of_attacks, '</br> Terrorist:', top10$gname), mode = 'lines+markers' ) %>% 
        layout( title = "Terrorist Group activity over time", xaxis = list( title = "Year" ),
                yaxis = list( title = "Attacks" ) )
    }
    if(x == 5){
      k <- plot_ly( x = a$year, y = a$ta, type = 'bar', name = 'Growth Of Attacks' ) %>%
        add_trace( x = a$year, y = a$PopTotal/400, type = 'scatter', name = 'Population' ) %>%
        layout( title = "Terrorism Growth with Population" , xaxis = list(title = 'Year' ),
                yaxis = list( title = 'Count' ), barmode = 'stack' )
    }
    if(x == 6){
      k <-plot_ly( x = b$year, y = b$c, type = 'scatter', mode = 'lines', name = "No. of Kills",
                   text = ~paste("Year :",b$year,",</br> No. of Kills :", b$c ) ) %>%
        layout( title = "Number of Kills per year ", xaxis = list( title = "year" ),
                yaxis = list( title = "count" ) )
    }
    return(k)
  })
  
  information<-reactive({
    x=as.numeric( input$content )
    if(x == 1){
      l <- "Terrorism in the broadest sense, the use of intentionally indiscriminate violence 
      as a means to create terror among masses of people or fear to achieve a financial, 
      political, religious or ideological aim.It has been increasing over each year, number of 
      attacks has been increasing each year.You are exploring the Global Terrorism Database (GTD),with information on terrorist 
      attacks around the world from 1970 - 2017 with more than 170,000 cases"
    }
    if(x == 2){
      l <- "This graph shows Target distribution of terrorism over time,i.e,
      what kind of targets does terrorists hit.It seems \"Private Citizens\" have become 
      a bigger target."
    }
    if(x == 3){
      l <- "This is an interactive graph you can locate the attacks by terrorist groups."
    }
    if(x == 4){
      l <- "This identify the top ten Terror Groups in terms of number of attacks.
      The current spike in Terror Activity has happened on year 2016 and 2017.
      The Terrorist groups behind this attacks are Taliban and Islamic State of Iraq
      and the Levant."
    }
    if(x == 5){
      l <- "Enviromental Factors also has an impact on Terrorism Growth as you can the 
      terrorism increases with population.As a matter of fact, the ethnicity and/or 
      religion are illusory causes of terrorism, because they are not an environmental stressor per se,
      whereas a basic determinant of terrorism may be a critical demographic mass and high population 
      growth that, in certain environments with problematic socioeconomic factors, leads to disrupt the 
      stability of societies/communities, induce frustration and anger of people, and terrorism as a result.
      (*World population dataset from the United Nations population forecasts)" 
      
    }
    if(x == 6){
      l <- "Graph reperesents the number of kills happened in each year, it seems 
      to rocket from 2013 to 2014 and more killing had happened next few years also"
    }
    return(l)
    })
  output$info <- renderText({
    i <- information()
    i
  })
  
  
  output$plot1 <- renderPlotly({
    dataplots = barplottest()
    dataplots
  })
  output$infoBox1 <- renderInfoBox({
    c=bo()
    infoBox( title = tags$p( style = "font-size: 20px;", c[1] ), value = tags$p( style = "font-size: 30px;",c[2] ),
             icon = shiny::icon( c[3] ), fill = T, color = "red")
  })
  
  output$infoBox2 <- renderInfoBox({
    infoBox(title = tags$p( style = "font-size: 20px;", "Year"), value = tags$p( style = "font-size: 30px;", "1970 - 2017" ), icon = shiny::icon("calendar"), fill = T, color = "yellow" )
  })
  output$Next_Previous=renderUI({
    tab_list = input$List_of_tab[-length(input$List_of_tab)]
    nb_tab = length( tab_list )
    if (which(tab_list == input$tabBox_next_previous) == nb_tab)
      column( 1, offset=1, Previous_Button )
    else if (which(tab_list == input$tabBox_next_previous) == 1)
      column( 1, offset = 10, Next_Button )
    else
      div( column(1, offset=1, Previous_Button), column(1, offset=8, Next_Button) )
  })
  observeEvent( input$Prev_Tab,{
    tab_list = input$List_of_tab
    current_tab = which( tab_list == input$tabBox_next_previous )
    updateTabsetPanel( session,"tabBox_next_previous",selected = tab_list[current_tab-1] )
  }
  )
  observeEvent(input$Next_Tab,{
    tab_list = input$List_of_tab
    current_tab = which(tab_list == input$tabBox_next_previous)
    updateTabsetPanel(session, "tabBox_next_previous", selected=tab_list[current_tab+1])
  }
  )
  output$plot2 <- renderPlotly({
    k<-ggplot() +
      geom_point( data = df3, aes( x = year, y = terrorist_attacks_count, col = 'Original Data')) +
      geom_point( data = fpopworld1, aes( x = year, y = terrorist_attacks_count, col = 'Model 1')) +
      geom_point( data = fpopworld2, aes(x = year, y = terrorist_attacks_count, col = '2nd order polynomial - Model 2')) +
      geom_point( data = fpopworld3, aes(x = year, y = terrorist_attacks_count, col = '3rd order polynomial - Model 3')) +
      labs( title = 'Predicted amount of terrorist attacks' ) +
      theme( legend.position = c(0.2, 0.85) ) +
      labs( x= 'Year', y= 'Number of Terrorist Attacks', colour = 'Legend' ) +
      scale_x_continuous( breaks = seq( 1970, 2040, 2 ) ) +
      theme( axis.text.x = element_text( angle = 45, hjust = 1 ) )
    ggplotly(k)
  })
  output$plot3 <- renderPlotly({
    #influencePlot(m1, id.method = "noteworthy")
    #title(main = "Outliers")
    plot_ly( gra1, y = ~tr, type = "box", text = ~paste( "Year :", year, "</br>Attacks :", tr )) %>%
      layout( title = " Outliers ", yaxis = list( title="Attacks" ) )
    
  })
  output$plot4<-renderPlotly({
    plot_ly( isi, x=~targtype1, y=~targcount, type="scatter", mode = 'lines+markers', color =~targtype1 ) %>%
      layout( title = "Target distribution of terrorism over 2014" , xaxis = list( title = "Target Type" ),
              yaxis=list(title="Count"))
    # plot_ly(clusterOutput,x=~clusterOutput$region,y=~clusterOutput$`clusters$cluster`,color = ~clusterOutput$attacktype1,
    #         text=~paste('Target :',clusterOutput$targtype1,'</br>Terrorist :',clusterOutput$gname)) %>%
    # layout(title="2014 data",xaxis=list(title="Region"),yaxis=list(title ="Cluster"))
  })
  output$isileaf <- renderLeaflet({
    title = "2014 Attacks"
    leaflet() %>%
      addProviderTiles( "CartoDB.Positron" ) %>% # Add default OpenStreetMap map tiles
      addMarkers( lng = isimap$longitude, lat = isimap$latitude , popup = isimap$gname, 
                  clusterOptions = markerClusterOptions()) %>%
      addControl( title, position = "topleft", className="map-title" )
    # plot_ly(out,x=~out$region ,y=~out$arr,color=~out$attacktype1,
    #         text=~paste('Target :',out$targtype1,'</br>Terrorist :',out$gname))%>%
    #   layout(title="Conclusion",xaxis=list(title="Region"),yaxis=list(title="cluster"))
  })
  output$info2 <- renderText({
    "As can be confirmed in the graph the 2nd order polynomial seems to be 
    giving the most accurate prediction based on the upswing from the past 
    decades but assuming that's just a blip in the data the first model gives
    the best prediction. To test this correctly we would have to compare it with
    new data coming in for the coming years. However, with ISIL currently being 
    'defeated' we suspect that the Model 1 prediction will be most correct."
  })
  output$info3 <- renderText({
    "We can find that the outlier is 2014, hence something is different happened in this year"
  })
  output$info4 <- renderText({
    "This graph shows Target distribution of terrorism over 2014 year"
  })
  output$info5 <- renderText({
    "Location of attacks happened in 2014"
  })
  output$myglobe <- renderGlobe({
    earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
    #globejs(img=earth, bg="white")
    globejs(img = earth,
            lat = c( glo$latitude ),
            long = c( glo$longitude ),
            value = glo$total,color = "red", bg = "white")
  })
  #create the test cases
  #pred <- reactive({
  observeEvent(input$predB, {
    r = as.character(input$reg)
    cn = as.character(input$con)
    a = as.character(input$attack)
    test.case = data.frame(region=c(r), country=c(cn), attacktype1=c(a))
    out = predict(modFit,test.case)
    shinyalert(title = as.character(out),text = "According to the provided region, country and attack type this one was targeted", type = "success")
  })
  
  
  })

