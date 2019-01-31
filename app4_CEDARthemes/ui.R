#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Themes 
  ## Choose theme
  shinythemes::themeSelector(),
  # Set theme
  theme = shinytheme("simplex"),
  
  # Application title
  titlePanel(title=div(img(src='WCM_PulmonaryCriticalCareMedicine.png', 
                           align = "right",height='95px',width='380px'),
                       img(src="WCM Biostats.png",height='130px',width='120px'), "CEDAR Cohort Discovery")),
  #titlePanel("CEDAR Cohort Discovery"),
  
  sidebarPanel(
    # make scrollable
    id = "tPanel",style = "overflow-y:scroll; max-height: 700px",
    #h4("Dataset:"),
    #h6("(Scroll for more)"),
    
    actionButton("resetData", "Reset to full dataset"),
    br(),br(),
    # Sidebar with a slider input for the years to include
    #uiOutput("years_ranges"), - remove all dates
    # Select variables of interest
    fluidRow(h4("Categorical constraint:")),
    uiOutput("constraintA_var"),
    fluidRow(
      column(6,uiOutput("constraintA_choices")),
      column(6,h5(textOutput("constraintA_counts"))),
      column(1,uiOutput("constraintA_alterData"))
    ),
    br(), br(),
    fluidRow(h4("Numerical constraint:")),
    uiOutput("constraint1_var"),
    uiOutput("constraint1_ranges"),
    fluidRow(
      column(6,h5(textOutput("constraint1_counts"))),
      column(6,uiOutput("constraint1_alterData"))
    ),
    br(), br(),
    
    fluidRow(h4("Require complete data for the following variables:")),
    uiOutput("varsOfInterest")
  ),
  # Output: Tabset w/ plot, summary, and table ----
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Distribution", 
                         br(),
                         fluidRow(h4(textOutput("dataSummary"))),
                         br(), br(),
                         fluidRow(h4("Distribution of particular measure")),
                         uiOutput("VarForTable_input"),
                         br(), br(),
                         plotOutput("ChosenVar_Plots"),
                         br(), br(),
                         fluidRow(tableOutput("ChosenVar_Table")),
                         br(), br(),
                         
                         br(), br(),
                         br(), br()),
                tabPanel("Additional constraints", 
                         fluidRow(h3(textOutput("sampSizeTitle"))),
                         fluidRow(h4("Categorical constraint:")),
                         fluidRow(
                           column(4,uiOutput("constraintB_var")),
                           column(4,uiOutput("constraintB_choices")),
                           column(2,h5(textOutput("constraintB_counts"))),
                           column(1,uiOutput("constraintB_alterData"))
                         ),
                         br(), br(),
                         fluidRow(h4("Categorical constraint:")),
                         fluidRow(
                           column(4,uiOutput("constraintC_var")),
                           column(4,uiOutput("constraintC_choices")),
                           column(2,h5(textOutput("constraintC_counts"))),
                           column(1,uiOutput("constraintC_alterData"))
                         ),
                         
                         br(), br(),
                         fluidRow(h4("Numerical constraint:")),
                         fluidRow(
                           column(4,uiOutput("constraint2_var")),
                           column(4,uiOutput("constraint2_ranges")),
                           column(2,h5(textOutput("constraint2_counts"))),
                           column(1,uiOutput("constraint2_alterData"))
                         ),
                         br(), br(),
                         fluidRow(h4("Numerical constraint:")),
                         fluidRow(
                           column(4,uiOutput("constraint3_var")),
                           column(4,uiOutput("constraint3_ranges")),
                           column(2,h5(textOutput("constraint3_counts"))),
                           column(1,uiOutput("constraint3_alterData"))
                         )
                ),
                tabPanel("Data dictionary", 
                         fluidRow(h4("Data dictionary:")),
                         fluidRow(
                           column(12,tableOutput("dataDictio_table"))
                         )
                ),
                tabPanel("Data notes", 
                         br(), br(),
                         fluidRow(h4("Data Quirks:")),
                         fluidRow(
                           column(12,uiOutput("dataQuirks_list"))
                         ),
                         fluidRow(h4("Data Issues:")),
                         fluidRow(
                           column(12,uiOutput("dataIssues_list"))
                         )
                ),
                br(), br(),br(), br(),br(), br(),br(), br()
    )),
  hr(),
  print("~~~ WCM Biostatistics - Version January 9, 2019 ~~~~")
))


