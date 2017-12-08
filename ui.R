
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(plotly)
library(networkD3)


shinyUI(dashboardPage(skin = "green",
  dashboardHeader(title = "Manufacturing data"),
  
  dashboardSidebar(
      sidebarMenu(id = "tab",
        menuItem("About", tabName = "about", icon = icon("info-circle")),
        menuItem("Station/Feature stats", tabName = "stationStats", icon = icon("dashboard")),
        menuItem("Fault/Type stats", tabName = "faultStats", icon = icon("dashboard")),
        uiOutput("sidebarui")
      )
  ),
  
  dashboardBody(
      tabItems(
        tabItem(tabName = "about", 
                tabBox(title = "About production line data", side = "right", width = 12,
                  tabPanel("Overview",
                           fluidRow(
                             box(width = 6,
                                 selectInput("filterBy", "Filter by", selected = "-none-", c("-none-", "Type", "Response")),
                                 uiOutput("overviewui")
                             ),
                             box(width = 6,
                                 DT::dataTableOutput("overviewTable")
                             )
                           )
                  ),
                  tabPanel("Visualization",
                           sankeyNetworkOutput("sankeyNetwork")
                  )
                )
        ),
        tabItem(tabName = "stationStats",
                tabBox(title = "Station/feature analysis", side = "right", width = 12, id = "featsTab",
                       tabPanel("Histograms", value="histograms",
                                plotOutput(outputId = "listFeaturesByStation"),
                                uiOutput("buttons")
                       ),
                       tabPanel("Correlogram", value="correlogram",
                                plotOutput(outputId = "corrgramByStation")
                       ),
                       tabPanel("Measured values", value="measuredvalues",
                                plotlyOutput(outputId = "plotbyfeature")
                       )
                )
        ),
        tabItem(tabName = "faultStats",
                tabBox(title = "Fault/type analysis", side = "right", width = 12, id = "faultsTab",
                       tabPanel("Overview", value="overview",
                                plotlyOutput(outputId = "percentageofFaultsByTypes")
                       ),
                       tabPanel("Parallel coordinates", value="parcoord",
                                plotlyOutput(outputId = "parcoordByType")
                       ),
                       tabPanel("Time spent on each line", value="dateline",
                                plotlyOutput(outputId = "datesByLine")
                       )
                )
        )
      )
  )         
                   
))
