
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(elastic)
library(elasticdsl)
library(ggplot2)
library(scales)
library(plotly)
library(data.table)
library(dtplyr)
library(dplyr)
library(corrplot)
library(stats)
library(janitor)
library(reshape)
library(gridExtra)
library(corrgram)
library(networkD3)
library(gtools)

elastic::connect(es_port = 9200)

shinyServer(function(input, output) {
  
  # SelectInput for feature (constricted by station)
  output$featureui <- renderUI({
    if (is.null(input$selectedStation)) {
      return()
    }
    features <- mixedsort(meta_df[meta_df$Station == input$selectedStation,]$FeatureId)
    return(
        selectInput("selectedFeature", 
                    span(tagList(icon("cog", class = "fa-lg"), "Select a feature")), 
                    selected = features[[1]], 
                    features)
    )
  })

  # Sidebar UI
  output$sidebarui <- renderUI({
    if (input$tab == "stationStats") {
      if (input$featsTab == "measuredvalues") {
        return(
          list(
            selectInput("selectedStation", 
                        span(tagList(icon("inbox", class = "fa-lg"), "Select a station")), 
                        selected = mixedsort(unique(meta_df$Station))[[1]], 
                        mixedsort(unique(meta_df$Station))),
            uiOutput("featureui")
          )
        )
      } else {
        return(selectInput("selectedStation", 
                           span(tagList(icon("inbox", class = "fa-lg"), "Select a station")), 
                           selected = mixedsort(unique(meta_df$Station))[[1]], 
                           mixedsort(unique(meta_df$Station))))
      }
    }
    if (input$tab == "faultStats" && input$faultsTab != "overview") {
      return(
        selectInput("selectedType", 
                    span(tagList(icon("archive", class = "fa-lg"), "Select a type")), 
                    selected = unique(types_df$Type)[[1]], 
                    unique(types_df$Type))
      )
    }
  })
  
  # Sankey Visualization
  output$sankeyNetwork <- renderSankeyNetwork({
    links <- clean_data_frame(
      Search(index = "sankey_links", 
             size = 10000, 
             asdf = TRUE
      )$hits$hits
    )
    
    group <- character(0)
    
    for (i in 0:51) {
      if (i <= 23) 
        group <- c(group, 1)
      else if (i <= 25)
        group <- c(group, 2)
      else if (i <= 28)
        group <- c(group, 3)
      else if (i <= 49)
        group <- c(group, 4)
      else if (i == 50)
        group <- c(group, 5)
      else
        group <- c(group, 6)
    }
    
    nodes = data.frame("name" = c(mixedsort(unique(meta_df$Station)), "Response0", "Response1"), "group"= group)
    names(links) = c("source", "target", "value")
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  NodeGroup = "group", 
                  colourScale = 'd3.scaleOrdinal().range(["#f3d612","#605ca8", "#f39c12", "#39cccc", "#03c86d", "#ff0000"])',
                  fontSize= 12, nodeWidth = 10, sinksRight = TRUE)
    
  })
  
  # Overview UI
  output$overviewui <- renderUI({
    if (!is.null(input$filterBy) && input$filterBy == "Type") {
      return(selectInput("selectedType", "Select a type", selected = unique(types_df$Type)[[1]], unique(types_df$Type)))
    }
    if (!is.null(input$filterBy) && input$filterBy == "Response") {
      return(selectInput("selectedResponse", "Select a response", selected = 0, c(0,1)))
    }
  })
  
  # Data table
  output$overviewTable <- DT::renderDataTable({
    
    measurements_df <- clean_data_frame(Search(index = "bosch_features", 
                                         body = '{"sort" : {
                                                    "Id" : {
                                                      "order" : "asc"
                                                    }
                                                  },
                                                  "_source": ["Id", "Response"]
                                                  }', 
                                         size = size, 
                                         asdf = TRUE)$hits$hits)
    
    table <- merge(types_df, measurements_df, by="Id")[, c("Id", "Response", "Type")]
    
    if (!is.null(input$filterBy) && input$filterBy == "Type") {
      if (is.null(input$selectedType)) {
        return()
      }
      table <- table[table$Type == input$selectedType,]
    }
    if (!is.null(input$filterBy) && input$filterBy == "Response") {
      if (is.null(input$selectedResponse)) {
        return()
      }
      table <- table[table$Response == input$selectedResponse,]
    }
    DT::datatable(table, options = list(lengthMenu = c(5, 10, 20), pageLength = 5))
  })
  
  # Percentage of faults by types
  output$percentageofFaultsByTypes <- renderPlotly({
    
    measurements_df <- clean_data_frame(Search(index = "bosch_features", 
                                               body = '{"sort" : {
                                                    "Id" : {
                                                      "order" : "asc"
                                                    }
                                                  },
                                                  "_source": ["Id", "Response"]
                                                  }', 
                                               size = size, 
                                               asdf = TRUE)$hits$hits)
    
    sub <- merge(types_df, measurements_df, by="Id")[, c("Id", "Type", "Response")]
    ggplotly(
      ggplot(sub, aes(x = Type, y = (..count..)/sum(..count..) ,fill = factor(Response), label = Id)) + 
        geom_area(stat = "bin", binwidth = 1) +
        scale_fill_manual(values = c("#03c86d", "#ff0000")) + labs(
          x = 'Type',
          y = 'percentage',
          fill = 'Response'
        ) + scale_y_continuous(labels = percent) + 
        theme(rect = element_blank(),
              panel.grid = element_blank()
        )
    )
  })
  
  # Buttons for stations w/ more that 12 features
  output$buttons <- renderUI({
    if (is.null(input$selectedStation)) {
      return()
    }
    if (nrow(meta_df[meta_df$Station == input$selectedStation,]) > 12) {
      
      return(list(
        actionButton("prevButton", "Previous"),
        actionButton("nextButton", "Next"),
        tags$span(paste("Page ", 
                     values$page, 
                     "/", 
                     ceiling(nrow(meta_df[meta_df$Station == input$selectedStation,])/12), 
                     sep=''))
        ))
    }
  })
  
  observeEvent(input$selectedStation, {
    values$page <- 1
    values$from <- 1
  })
  
  observeEvent(input$nextButton, {
    if (is.null(input$selectedStation)) {
      return()
    }
    
    if(values$from + 12 < nrow(meta_df[meta_df$Station == input$selectedStation,])) {
      values$page <- values$page +1
      values$from <- values$from + 12
    }
  })
  
  observeEvent(input$prevButton, {
    if (is.null(input$selectedStation)) {
      return()
    }
    
    if(values$from - 12 > 0) {
      values$page <- values$page - 1
      values$from <- values$from - 12
    }
  })
  
  values <- reactiveValues(from = 1, page = 1)
  
  # Histograms by features
  output$listFeaturesByStation <- renderPlot({
    if (is.null(input$selectedStation)) {
      return()
    }
    
    querystring <- paste('{"sort" : {"Id" : { "order" : "asc"}},"_source": ["Response", "*', 
                         input$selectedStation, 
                         '_*"]}', 
                         sep='')
    
    sub <- clean_data_frame(
      Search(index = "bosch_features", 
             body = querystring, 
             size = size, 
             asdf = TRUE
        
      )$hits$hits
    )
    
    sub <- sub[ , mixedsort(names(sub))]
    
    plotlist <- list()
    j <- 1
    for (i in values$from:(values$from+11)) {
      if (i == length(sub)) {
        break
      }
      
      name <- names(sub)[i]
      
      hist <- ggplot(sub, aes_string(x=names(sub)[i])) + 
        xlim(c(-1,1)) + 
        geom_histogram(position = "stack", aes(fill = factor(Response))) + 
        guides(fill=FALSE) +
        scale_fill_manual(values = c("#03c86d", "#ff0000")) + 
        theme(rect = element_blank(),
              panel.grid = element_blank()
        )
      
      plotlist[[j]] <-  hist
      j <- j+1
    }
    
    grid.arrange(grobs = plotlist, ncol = 4, nrow = 3)
  })
  
  # Corrgram / station
  output$corrgramByStation <- renderPlot({
    if (is.null(input$selectedStation)) {
      return()
    }    
    selectedFeatures <- as.character(meta_df[meta_df$Station == input$selectedStation,]$FeatureId)
    
    querystring <- '{"sort" : {"Id" : { "order" : "asc"}},"_source": ["Response", "'
    
    for (i in 1:length(selectedFeatures)) {
      if (i!= length(selectedFeatures)) {
        querystring <- paste(querystring, selectedFeatures[[i]], '", "', sep='')
      }
      else {
        querystring <- paste(querystring, selectedFeatures[[i]], '"', sep='')
      }
    }
    querystring <- paste(querystring, ']}', sep='')
    
    featuresOfStation <- clean_data_frame(
      Search(index = "bosch_features", 
             body = querystring, 
             size = size, 
             asdf = TRUE
      )$hits$hits
    )
    
    setnames(featuresOfStation, "Response", "!Response")
    
    featuresOfStation <- remove_empty_cols(featuresOfStation)
    featuresOfStation <- featuresOfStation[ , mixedsort(names(featuresOfStation))]
    
    
    corrgram(featuresOfStation,
             col.regions=colorRampPalette(c("#dd4b39", "#ffe7e7",
                                            "#e3ffff", "#03c86d"))
    )
  })

  # Parcoord
  output$parcoordByType <- renderPlotly({
    if (is.null(input$selectedType)) {
      return()
    }
    
    ids_to_display <- types_df[types_df$Type == input$selectedType,]$Id
    
    querystring <- '{"sort" : {"Id" : { "order" : "asc"}},
    "query" : {
    "bool" : {
    "filter" : [
    {
    "terms" : {
    "Id" : ['
    
    for (i in 1:length(ids_to_display)) {
      if (i!= length(ids_to_display)) {
        querystring <- paste(querystring, ids_to_display[[i]], ',', sep='')
      } else {
        querystring <- paste(querystring, ids_to_display[[i]], sep='')
      }
    }
    
    querystring <- paste(querystring, ']}}]}}}', sep='')
    
    measurements_to_display <- clean_data_frame(
      Search(index = "bosch_features",
             body = querystring,
             size = size,
             asdf = TRUE
      )$hits$hits
    )
    
    measurements_to_display <- measurements_to_display[ , mixedsort(names(measurements_to_display))]
    
    melted_mes <- melt(measurements_to_display, id=c("Id","Response"))
    
    joined <- melted_mes[complete.cases(melted_mes), ]
    
    ggplotly(
      ggplot(joined, aes(variable, value, color = factor(Response))) + ylim(-1, 1) +
        geom_line( aes(group = Id), alpha = 0.4 )+
        scale_colour_manual(values = c("#03c86d", "#ff0000"))+labs(
          x = 'Feature',
          y = 'Measured value',
          color = 'Response'
        ) + theme(rect = element_blank(),
                  panel.grid = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey70"),
                  axis.text.x=element_text(size=8, angle=45,hjust=1)
          )
    )
    
  })
  
  # Feature stats
  output$plotbyfeature <- renderPlotly({
    if (is.null(input$selectedFeature)) {
      return()
    }
    
    selectedFeature <- input$selectedFeature
    selectedDateFeature <- as.character(meta_df[meta_df$FeatureId == input$selectedFeature,]$DateColumn)

    querystring <- paste('{"sort" : {"Id" : { "order" : "asc"}},"_source": ["Id", "Response", "', 
                         selectedFeature, 
                         '"]}', 
                         sep='')
    
    mes_df <- clean_data_frame(
      Search(index = "bosch_features",
             body = querystring,
             size = size,
             asdf = TRUE
             )$hits$hits
    )
    
    querystring <- paste('{"sort" : {"Id" : { "order" : "asc"}},"_source": ["Id", "',
                         selectedDateFeature, 
                         '"]}', 
                         sep='')
    
    date_df <- clean_data_frame(
      Search(index = "bosch_dates",
             body = querystring,
             size = size,
             asdf = TRUE
             )$hits$hits
    )
    
    df <- merge(mes_df, date_df, by="Id")
    
    plot <- ggplot(df, aes(df[,selectedDateFeature], df[,selectedFeature], color = factor(Response)))+ ylim(-1, 1)+
      geom_jitter(size = 1)+labs(
        x = 'Date',
        y = paste('Measured value of', selectedFeature),
        color = 'Response'
      ) + scale_colour_manual(values = c("#03c86d", "#ff0000")) + 
      theme(rect = element_blank(),
            panel.grid = element_blank()
      )
    
    
    values <- df[, selectedFeature]
    values <- values[!is.na(values)]
    
    if (length(values) > 0) {
      percentiles <- quantile(values, c(.1, .9))
      plot + geom_hline(yintercept = percentiles[[1]], size = 0.2) +
        geom_hline(yintercept = percentiles[[2]], size = 0.2)
    }

    ggplotly()
  })
  
  # Dates by Lines
  output$datesByLine <- renderPlotly({
    if (is.null(input$selectedType)) {
      return()
    }
    
    dates_by_lines <- clean_data_frame(Search(index = "dates_by_lines", 
                                               body = '{"sort" : {
                                               "Id" : {
                                               "order" : "asc"
                                               }
                                               }
                                               }', 
                                         size = size, 
                                         asdf = TRUE)$hits$hits)
    
    melted <- melt(dates_by_lines, id=c("Id","Type", "Response"))
    
    type <- melted[melted$Type == input$selectedType,]
    

    ggplotly(
      ggplot(type, aes(variable, value, color = factor(Response))) +
        geom_line( aes(group = Id), alpha = 0.3 )+
        scale_colour_manual(values = c("#03c86d", "#ff0000"))+labs(
          x = 'Line',
          y = 'Time spent on each line',
          color = 'Response'
        ) + theme(rect = element_blank(),
                  panel.grid = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey70")
        )
    )
    
  })
  
  })
