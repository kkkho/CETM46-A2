## app.R ##
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(priceR)
library(scales)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

histdata <- read.csv('Data/upload.csv')

ui <- dashboardPage(
  dashboardHeader(title = "Cost of Real Twitter Follower"),

  #sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Yours", tabName = "upload", icon = icon("upload")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard", lib = "glyphicon")),
      menuItem("Your Data", tabName = "data", icon = icon("th"))
    )
  ),

  #Body
  dashboardBody(
    # First tab content
    tabItems(
      
      # Upload tab content
      tabItem(tabName = "upload",
              box(title = "Check your cost of Real Twitter follower",
                  # Input: Select a file ----
                  fileInput("file1", "Choose CSV File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  numericInput("cost1","Cost", 100000,min = 0),
                  actionButton("submit1", "Submit", icon = NULL)
              
                  )
              
      ),
      #Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Cost Of Real Followers", width = 5, solidHeader = TRUE, status = "warning",
                  textOutput("cost_of_real"),
                             tags$head(tags$style("#cost_of_real{color: black;font-size: 20px;font-style: bold;}"))
                ), #box,
                box(
                  title = "Cost Of Fake Followers", width = 5, solidHeader = TRUE, status = "warning",
                  textOutput("cost_of_fake"),
                  tags$head(tags$style("#cost_of_fake{color: red;font-size: 20px;font-style: bold;}"))
                ),#box
                hr(),
                box(
                  title = "Percentage Of Real Followers", width = 5, solidHeader = TRUE, status = "warning",
                  textOutput("perc_of_real"),
                  tags$head(tags$style("#perc_of_real{color: black;font-size: 20px;font-style: bold;}"))
                ), #box,
                box(
                  title = "Percentage Of Fake Followers", width = 5, solidHeader = TRUE, status = "warning",
                  textOutput("perc_of_fake"),
                  tags$head(tags$style("#perc_of_fake{color: red;font-size: 20px;font-style: bold;}"))
                ),#box
                box(title = "Charts of Real and Fake Followers", width = 10, solidHeader = TRUE, status = "warning",
                    plotOutput("plot1", height = 250))

              )              
      ),#tabitem
      
      # data content
      tabItem(tabName = "data",
              #fluidRow
              fluidRow(
                box(width = 2, 
                       checkboxGroupInput("checkGroup", 
                                          h3("Elements"), 
                                          choices = names(histdata),
                                          selected = names(histdata))),
                box(
                  width = 9, h1('Your Dataset'), hr(),
                  DT::dataTableOutput('x11'),
                  verbatimTextOutput('y11')
                )#fluidRow
              )
      )
    )
    
  ) #tabItems
)#DashboardBody

server <- function(input, output, session) {
  
  #For real product, histdata should get from user input file, but in prototype, we use the default csv data.  
  inputfile1 <- reactive({input$file1})
  
  
  summary(histdata)
  
  histdata$friends_follower <- histdata$friends_count/histdata$followers_count
  histdata$favourites_follower <- histdata$favourites_count/histdata$followers_count
  
  #In real product, here should call machine learning method to predict the follower account is fake or not.
  # rf is the random forest model we have trained.
  
  #rf_pred <- predict(rf, test_data, type="prob")
  #rf_pred <- as.factor(ifelse(rf_pred$no > 0.5, "no","yes"))

  mydf <- histdata %>%
    group_by(isFake) %>%
    summarise(count=n())
  
  mydf

  #Plot the number of Fake and Real accounts of the dataset
  output$plot1 <- renderPlot({
  ggplot(mydf, aes(x=isFake, y=count)) + geom_bar(stat = "identity")
  })

  #calculate the cost of the Real followers  
  costReal = reactive({format_dollars(round(input$cost1 / as.numeric((mydf[mydf$isFake== "N",]$count+mydf[mydf$isFake== "Y",]$count)) * as.numeric(mydf[mydf$isFake== "N",]$count),digits=2),2)})
  #calculate the cost of the fake followers
  costFake = reactive({format_dollars(round(input$cost1 / as.numeric((mydf[mydf$isFake== "N",]$count+mydf[mydf$isFake== "Y",]$count)) * as.numeric(mydf[mydf$isFake== "Y",]$count),digits=2),2)})

  #calculate the percentage of the Real followers  
  percReal = reactive({label_percent()(round(input$cost1 / as.numeric((mydf[mydf$isFake== "N",]$count+mydf[mydf$isFake== "Y",]$count)) * as.numeric(mydf[mydf$isFake== "N",]$count) / input$cost1,digits=4))})
  #calculate the percentage of the Fake followers  
  percFake = reactive({label_percent()(round(input$cost1 / as.numeric((mydf[mydf$isFake== "N",]$count+mydf[mydf$isFake== "Y",]$count)) * as.numeric(mydf[mydf$isFake== "Y",]$count) / input$cost1,digits=4))})
  
  #set the result to output screen     
  output$cost_of_real = costReal
  output$cost_of_fake = costFake
  
  output$perc_of_real = percReal
  output$perc_of_fake = percFake
  
  
  #Display the predicted dataset as table
  options(DT.options = list(pageLength = 10))
  output$x11 = DT::renderDataTable(histdata[,input$checkGroup, drop = FALSE], server = FALSE, selection = 'single', options = list(scrollX = TRUE))
  output$y11 = renderPrint(input$x11_rows_selected)
  

} #server

shinyApp(ui, server)

