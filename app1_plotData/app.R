#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app 
#

library(shiny)
library(tidyverse)
library(gridExtra)
library(scales)
dat_here <- readRDS("myDataset_1.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Univariate Distribution"),
   
   
   sidebarLayout(
     # Sidebar to select variable
     sidebarPanel(
       selectInput('varForTable',
                 label = 'Choose variable:',selected=colnames(dat_here)[1], 
                 c(Choose='',  colnames(dat_here)), selectize=TRUE)),
     
     # Main panel to show plot of the chosen variable
     mainPanel(
       plotOutput("ChosenVar_Plots")
       )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Histogram and boxplot or barplot
  output$ChosenVar_Plots <- renderPlot({ 
    req(input$varForTable)
    var_here_name =  input$varForTable
    var_here_values =  dat_here %>% pull(!!var_here_name)
    dat_here %>%
      filter(!is.na(!!sym(var_here_name))) %>%
      mutate(var_here = !!sym(var_here_name)) %>% 
      select(var_here) ->
      var_here_dataset
    
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
             "Please choose another variable.\n
             The variable you have chosen is not numeric and has too many levels to plot.")
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

