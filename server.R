#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Display result
  output$result = DT::renderDataTable(DT::datatable({
    user_input = c(Season = input$input_season,
                   HolidayType = input$input_holidayType,
                   Price = input$input_price,
                   NumberOfPersons = input$input_numOfPerson,
                   Region = input$input_region,
                   Transportation = input$input_transportation,
                   Duration = input$input_duration,
                   Accommodation = input$input_accommodation)
    
    #user_input = c(HolidayType =  "Bathing", Price = 2498, Transportation = "Plane", NumberOfPersons = 2, Duration = 3, Season = "April", Accommodation = "TwoStars")
    getTop5Case(user_input)
  }))
  
  selectedData <- reactive({
    cl_season = input$cluster_season
    cl_price = input$cluster_price
    matrix(c(convert_season_to_value(cl_season), as.numeric(parse_user_input(cl_price))), ncol = 2)
  })
  
  kmeanSample = ({
    new_data[, c("nSeasons", "nPrice")]
  })
  
  clusters <- reactive({
    set.seed(1213)
    kmeansResult <<- kmeans(kmeanSample, input$numOfClusters)
    kmeansResult
  })
  
  output$result_kmean <- DT::renderDataTable(DT::datatable({
    selectedData(); clusters()
    
    user_input = c(Season = input$cluster_season,
                   Price = input$cluster_price)
    
    getClusteredList(user_input)
  }))
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(kmeanSample,
         col = clusters()$cluster,
         pch = 20, cex = 3,
         xlab = "Seasons", ylab = "Price")
    points(clusters()$centers, pch = 3, cex = 2, lwd = 2, col = "ivory4")
    #print(c(input$cluster_season, input$cluster_price))
    points(selectedData(), pch = "O", cex = 4, lwd = 8, col = clusterNo)
    }#,
    #width = 1200,
    #height = 1200
  )
  
  tot_similarity <- reactive({
    input$input_season; input$input_region; input$input_holidayType; input$input_numOfPerson; input$input_duration; input$input_transportation; input$input_price; input$input_accommodation
    sim_table2
  })
  # 
  # # Similarity
  datasetInput <- reactive({
    tot_similarity()
    # head(sim_table2)
    # switch(input$similarity,
    #        "Total Similarity" = sim_table2,
    #        "HolidayType" = cbind(lookup_holiday["holidayType"], lookup_holiday["groupID"]),
    #        "Price" = table_sim_price,
    #        # "NumberOfPersons",
    #        # "Region" =,
    #        # "Transportation" =,
    #        # "Duration" =,
    #        "Season" = table_sim_season,
    #        "Accommodation" = table_sim_accommodation)
  })

  output$similarityView <- DT::renderDataTable(DT::datatable({
    datasetInput()
  }))
  #
  #
  
  # Display raw data
  output$dataset <- DT::renderDataTable(DT::datatable({
    data = travelCases
    data
  }))
})
