#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)
library(tidyverse)
library(gridExtra)
library(scales)
library(DT)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Select themes 
  shinythemes::themeSelector(),
  
  # Application title
  titlePanel("Univariate Distribution"),
  
  
  sidebarLayout(
    # Sidebar to upload data and select variable
    sidebarPanel(
      # Upload dataset 
      fileInput("dataset", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Select variable - now defined in server
      uiOutput("VarForTable_input")),
    
    # Main panel to show plot of the chosen variable
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Univariate Dist",
                           br(),
                           textOutput("filename"),
                           tags$head(tags$style("#filename{color: #E87722;
                                                font-size: 20px;
                                                font-style: italic;
                                                }"
                         )),
                         hr(), br(),
                         plotOutput("ChosenVar_Plots")),
                  tabPanel("Full Data - Normal",
                           tableOutput("data_raw")),
                  tabPanel("Full Data - DT",
                           DT::dataTableOutput("data_DT"))
                           )
                  )
                  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get dataset
  dat <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$dataset
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = TRUE)
  })
  
  # Display sample size and number of columns imported
  output$filename <- renderText({
    req(dat())
    paste0("The dataset you have uploaded contains ",nrow(dat()), " rows and ",ncol(dat())," columns.")
  })
  
  # Make dropdown with the  variables in dataset 
  output$VarForTable_input <- renderUI({
    req(dat())
    # First valid variable is default
    dat() %>% 
      select_if(function(col) is.numeric(col) | length(unique(col)) < 50) %>% 
      names() -> validCols
    selectInput("varForTable",
                label = 'Choose variable:',selected=validCols[1], 
                c(Choose='',  colnames(dat())), selectize=TRUE)
  })
  
  # Print full dataset 
  # No formatting
  output$data_raw <- renderTable(dat())
  
  # # Use DT package
  output$data_DT <- DT::renderDataTable(dat())
  
  # Histogram and boxplot or barplot
  output$ChosenVar_Plots <- renderPlot({ 
    # Don't display anything if dat() and variable are not defined
    req(dat())
    req(input$varForTable)
    
    var_here_name =  input$varForTable
    var_here_values =  dat() %>% pull(!!var_here_name)
    dat() %>%
      filter(!is.na(!!sym(var_here_name))) %>%
      mutate(var_here = !!sym(var_here_name)) %>% 
      select(var_here) ->
      var_here_dataset
    
    # If numeric
    if(is.numeric(var_here_values) & length(unique(var_here_values)) > 3){
      
      # Make histogram
      var_here_dataset %>%
        ggplot(aes(var_here)) +
        geom_histogram(aes(fill="#CF4520"),bins = 30) +
        labs(title = paste0(var_here_name," (n=",length(var_here_values),")"),
             x = "") +
        guides(fill=FALSE) +
        theme(text = element_text(size = 20)) ->
        plotHist
      
      # Make Boxplot
      var_here_dataset %>%
        ggplot(aes(x="1",y=var_here)) +
        geom_jitter(width=.05,alpha=.5,height=0) +
        geom_boxplot(width=.25,outlier.colour = NA,fill="#CF4520") +
        labs(title="",x="",y=var_here_name) +
        guides(fill=FALSE) +
        theme(text = element_text(size = 20),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) ->
        plotBplot
      
      # Arrange side by side 
      grid.arrange(plotHist,plotBplot,nrow=1,widths=c(2,1))
    }else{
      # Categorical variable 
      
      # Check that barplot will not have too many categories
      validate(
        need(length(unique(var_here_values)) <= 50, 
             "Please choose another variable. The variable you have chosen is not numeric and has too many levels to plot.")
      )
      # Create barplot
      var_here_dataset %>%
        mutate(var_plot = as.factor(var_here)) %>%
        ggplot(aes(var_plot, fill=var_plot, group="1")) +
        # Add percentages
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes(label = ..count.., y= ..prop..), stat= "count", vjust = -.6) +
        geom_text(aes(label = paste0(round(..prop..*100),"%"), y= ..prop..), stat= "count", vjust = 1.2) +
        labs(y = "Percent") +
        scale_y_continuous(labels=percent) +
        labs(title=paste0(var_here_name," (n=",length(var_here_values),")"),
             x="") +
        guides(fill=FALSE) +
        theme(text = element_text(size = 20)) -> 
        barplot
      # If more than 6 categories, rotate plot
      if(length(unique(var_here_values)) >6) {
        barplot = barplot + coord_flip()
      }
      barplot
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

