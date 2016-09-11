#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

#system.time(source(paste(filePath, "main.R", sep = "")))
source("main.R")

numOfcases = nrow(travelCases)
numOfclusters = round(sqrt(numOfcases))
cl_min = round(numOfclusters * .8)
cl_max = round(numOfclusters * 1.2)

# palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Define UI for application that draws a histogram
shinyUI(navbarPage("Your Travel Plan!",
  tabPanel("Recommendation",
           sidebarLayout(
             sidebarPanel(
               selectInput("input_season", "Season", seasons),
               selectInput("input_region", "Region", region),
               #selectizeInput('input_region', 'Region', choices = region),
               selectInput("input_holidayType", "Holiday Type", holidayType),
               #numericInput("input_numOfPerson", "Number Of Persons", value = 1, min = 1, max = 12),
               sliderInput("input_numOfPerson", "Number Of Persons", min=1, max=12, value=4),
               #numericInput("input_duration", "Duration", value = 1, min = 1, max = 25),
               sliderInput("input_duration", "Duration", min=1, max=25, value=5),
               selectInput("input_transportation", "Transportation", transportation),
               selectInput("input_price", "Price", price),
               selectInput("input_accommodation", "Accommodation", accommodation),
               
               h3("Submit"),
               submitButton("Update Result")
             ),
             
             mainPanel(
               h4("TOP 5 Best Travel Cases for You"),
               DT::dataTableOutput("result")
               #verbatimTextOutput("result")
             )
           )
  ),
  # #
  # tabPanel("Similarity Table",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("similarity", "Choose a similarity table:",
  #                          choices = c("Total Similarity", names(travelCases)[3:10])),
  #                          #choices = "All"),
  #              helpText("Note: while the data view will show only the specified",
  #                       "number of observations, the summary will still be based",
  #                       "on the full dataset."),
  #              submitButton("Update View")
  #            ),
  # 
  #            mainPanel(
  #              h4("Observations"),
  #              #tableOutput("similarityView")
  #              DT::dataTableOutput("similarityView")
  #            )
  #          )
  # ),
  #
  tabPanel("Similarity Table",
           h4("Observations"),
           DT::dataTableOutput("similarityView")
  ),
  tabPanel("K-means Clustering",
           sidebarLayout(
             sidebarPanel(
               h3("Quick Search using Clustering Algorithm"),
               helpText("I DON'T KNOW WHAT I WANT!"),
               helpText("Simiply input the information when you want to leave,",
                        "and how much your budget is."),
               selectInput("cluster_season", "Season", seasons, selected = "June"),
               selectInput("cluster_price", "Price", price, selected = "> 1500 and <= 2000"),
               sliderInput('numOfClusters', 'Cluster count', numOfclusters,
                            min = cl_min, max = cl_max),
               h3("Submit"),
               submitButton("Update Result")
             ),
             mainPanel(
               plotOutput('plot1'),
               h4("Result"),
               DT::dataTableOutput("result_kmean")
             )
           )
  ),
  tabPanel("Travel Dataset",
           DT::dataTableOutput("dataset"))
))
