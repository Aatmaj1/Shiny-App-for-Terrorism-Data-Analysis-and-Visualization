#library(dplyr)
library(shinydashboard)
library(shiny)
library(threejs)

# library(plotly)
# library(ggplot2)
# library(car)
# library(readr)
# library(ggmap)
# library(rworldmap)
shinyUI( dashboardPage( skin="purple",
                        dashboardHeader( title = "Global Terrorism"),
                        dashboardSidebar(#tags$head(tags$style(HTML("
                          #.main-sidebar{width: 300px;}"))),
                          sidebarMenu(
                            menuItem( "Explore Data", tabName = "graph", icon = icon("wpexplorer")),
                            menuItem( "Story", tabName = "story", icon = icon("object-group")),
                            menuItem( "Globe", tabName = "menuGlobe", icon = icon("globe")),
                            menuItem("Prediction",tabName = "prediction", icon=icon("bullseye"))
                          )
                          
                          #selectInput("dateyear", "Choose a Year:",
                          #choices = 1970:2015
                        ),
                        dashboardBody(
                          tags$head( 
                            tags$style( HTML( ".main-sidebar { font-size: 22px; }" ) ) #change the font size to 20
                          ),
                          tabItems(
                            # First tab content
                            tabItem( tabName = "graph",
                                     fluidRow( infoBoxOutput( 'infoBox1' ), infoBoxOutput( 'infoBox2' ),
                                               box(
                                                 title = "Explore Global Terrorism Data", width = 3, status = "primary", solidHeader = TRUE,
                                                 collapsible = TRUE , selectInput("content", "Select One :", 
                                                                                  choices = c("Terrorism attacks over time"  = 1,"Target distribution of terrorism over time"= 2,
                                                                                              "Location of terrorist attacks by group" = 3,"Terrorist Group activity over time" = 4,
                                                                                              "Terrorism growth Over Population growth" = 5,"Number of Kills per year" = 6),
                                                                                  selected="Terrorism attacks over time")
                                               ),
                                               fluidRow(
                                                 box( width = 9, withSpinner(uiOutput( "plotgraph" )) ),
                                                 box( width = 3, style = "font-size: 130%;", textOutput("info"), uiOutput("leaf"))
                                               )
                                     )),
                            tabItem( tabName = "story",
                                     box( width=12,
                                          tabBox( width=12, id="tabBox_next_previous",
                                                  tabPanel( "Story 1", box( width = 12, withSpinner(plotlyOutput( "plot2", height = 450, 
                                                                                                                  width = "100%" ))), textOutput( "info2" )),
                                                  tabPanel("Story 2", box( width = 12, withSpinner(plotlyOutput("plot3", height = 450, 
                                                                                                                width = "100%" ))),textOutput( "info3"  )),
                                                  tabPanel("Story 3",box( width = 12,withSpinner(plotlyOutput("plot4", height = 450,
                                                                                                              width = "100%" ))),textOutput( "info4" )),
                                                  tabPanel("Story 4",box( width = 12,withSpinner(leafletOutput( "isileaf" ))),textOutput( "info5" )),
                                                  tags$script("
                                                              $('body').mouseover(function() {
                                                              list_tabs=[];
                                                              $('#tabBox_next_previous li a').each(function(){
                                                              list_tabs.push($(this).html())
                                                              });
                                                              Shiny.onInputChange('List_of_tab', list_tabs);})
                                                              "
                                                  )
                                          ),
                                          uiOutput( "Next_Previous" )
                                          )
                            ),
                            tabItem(tabName = "menuGlobe",
                                    fluidPage(
                                      title = "Globe",
                                      fluidRow(
                                        column( width = 12,
                                                box(
                                                  width = 12,
                                                  height = 1030,
                                                  status = "primary",
                                                  solidHeader = TRUE,
                                                  style = "font-size: 120%;",
                                                  style = "color: #444",
                                                  
                                                  h1( "Global Terrorist Attacks", style = "text-align: center" ),
                                                  
                                                  h4("Global terrorist attacks has been increasing each year. The bar rising from 3D
                                                     globe represents the total attacks happened in that location over the years.", style = "text-align: center"),
                                                  withSpinner(globeOutput('myglobe'))
                                                  ))))
                            ),
                            tabItem(tabName = "prediction",
                                    fluidPage(
                                      title = "Prediction",
                                      fluidRow(
                                        column( width = 12,
                                                box(
                                                  width = 12,
                                                  height = 1030,
                                                  status = "primary",
                                                  solidHeader = TRUE,
                                                  style = "font-size: 120%;",
                                                  style = "color: #444",
                                                  
                                                  h1( "Prediction Of Targets", style = "text-align: center" ),
                                                  
                                                  h4("Global terrorist attacks has been increasing each year. Each region, each country is targeted with an aim 
                                                     to accomplish.The terrorist attacks has its own pattern for each country.", style = "text-align: center"),
                                                  fluidRow(
                                                    column(width = 12,
                                                           box(
                                                             title = "Region", status = "primary", solidHeader = TRUE,width = 4,
                                                             collapsible = TRUE,selectInput("reg", "Select One :", choices = c(region.a))),
                                                           
                                                           box(
                                                             title = "Country", status = "primary", solidHeader = TRUE,width = 4,
                                                             collapsible = TRUE ,selectInput("con", "Select One :", choices = c(country.a))),
                                                           
                                                           box(
                                                             title = "Attack Type", status = "primary", solidHeader = TRUE,width = 4,
                                                             collapsible = TRUE ,selectInput("attack","Select One :",
                                                                                             choices = c(attack.a))))
                                                    
                                                  ),
                                                  fluidRow(
                                                    column(width=12,
                                                           useShinyalert(),
                                                           div(style = "background-color:white; text-align:center;", actionButton("predB", "PREDICT",style='padding:15px; font-size:50%'))
                                                    )
                                                  )
                                                  )
                                                
                                      )
                                    )
                            )
                            
                            )
                            
                            
                            
                            )
                                      )
                        )
                        )


