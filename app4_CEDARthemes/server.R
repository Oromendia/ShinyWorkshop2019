#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#

# Define backend of app
shinyServer(function(input, output,session) {
  
  # Declare variables (datasets and ranges) as reactive values 
  rv <- reactiveValues(
    # Keep years specified, for now all 
    dat_years = dat_merged, 
    # Drop rows missing any variable of interest, for now same as dat_years
    dat_compl = dat_merged,
    # Variables to require complete
    vars_notMissing = c("ICUwing_cat"),
    # Impose constraints
    imposeConstraints = rep("Clear",10),
    # Impose categorical constraints
    imposeConstraints_Cat = rep("Clear",10)
  )
  
  #####################
  # Update datasets
  #####################
  # # Update dat_years when years change - remove all dates
  # observeEvent(input$years, {
  #   rv$dat_years <- dat_merged %>%
  #     filter(year(icu_overall_start_date) >= input$years[1] & year(icu_overall_start_date) <= input$years[2])
  #   })
  
  #Update dat_compl when selected vars change
  observeEvent(c(input$years,input$varsOfInterest), {
    rv$dat_compl <- rv$dat_years %>%
      drop_na(c("ICUwing_cat",input$varsOfInterest)) # Drop rows missing any variable of interest
  })
  
  # Update dat_compl when constraint actions change
  observeEvent(c(rv$imposeConstraints,rv$imposeConstraints_Cat), {
    dat_here <- rv$dat_years %>% drop_na(input$varsOfInterest)
    if(rv$imposeConstraints[1] == TRUE) {
      dat_here <- dat_here %>% 
        filter(!! rlang::sym(input$constraint1_var) >= input$range_constraint1[1] & 
                 !! rlang::sym(input$constraint1_var) <= input$range_constraint1[2])
    }
    if(rv$imposeConstraints[2] == TRUE) {
      dat_here <- dat_here %>% 
        filter(!! rlang::sym(input$constraint2_var) >= input$range_constraint2[1] & 
                 !! rlang::sym(input$constraint2_var) <= input$range_constraint2[2])
    }
    if(rv$imposeConstraints[3] == TRUE) {
      dat_here <- dat_here %>% 
        filter(!! rlang::sym(input$constraint3_var) >= input$range_constraint3[1] & 
                 !! rlang::sym(input$constraint3_var) <= input$range_constraint3[2])
    }
    # Categorical constraints
    if(rv$imposeConstraints_Cat[1] == TRUE & ("" %in% input$constraintA_choices)) {
      dat_here <- dat_here %>% 
        filter(!! rlang::sym(input$constraintA_var) %in% input$constraintA_choices |
                 is.na(!! rlang::sym(input$constraintA_var)))
    }
    if(rv$imposeConstraints_Cat[1] == TRUE & !("" %in% input$constraintA_choices)) {
      dat_here <- dat_here %>%
        filter(!! rlang::sym(input$constraintA_var) %in% input$constraintA_choices)
    }
    if(rv$imposeConstraints_Cat[2] == TRUE & ("" %in% input$constraintB_choices)) {
      dat_here <- dat_here %>% 
        filter(!! rlang::sym(input$constraintB_var) %in% input$constraintB_choices |
                 is.na(!! rlang::sym(input$constraintB_var)))
    }
    if(rv$imposeConstraints_Cat[2] == TRUE & !("" %in% input$constraintB_choices)) {
      dat_here <- dat_here %>%
        filter(!! rlang::sym(input$constraintB_var) %in% input$constraintB_choices)
    }
    if(rv$imposeConstraints_Cat[3] == TRUE & ("" %in% input$constraintC_choices)) {
      dat_here <- dat_here %>% 
        filter(!! rlang::sym(input$constraintC_var) %in% input$constraintC_choices |
                 is.na(!! rlang::sym(input$constraintC_var)))
    }
    if(rv$imposeConstraints_Cat[3] == TRUE & !("" %in% input$constraintC_choices)) {
      dat_here <- dat_here %>%
        filter(!! rlang::sym(input$constraintC_var) %in% input$constraintC_choices)
    }
    rv$dat_compl <- dat_here
  })
  
  # ################################
  # # Subset variables with complete data
  # ################################
  output$varsOfInterest <- renderUI({
    checkboxGroupInput("varsOfInterest", label = "",
                       choices = colnames(dat_merged),
                       selected = rv$vars_notMissing,
                       inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL)
  })
  output$varsOfInterest_list <- renderUI({
    selectInput("varsOfInterest_list", label = "",
                choices = colnames(dat_merged),
                selected = rv$vars_notMissing,
                multiple = TRUE)
  })
  # Observe and update these so they are the same
  
  # Years in dataset
  output$yearsDataset <- renderText({
    req(input$years)
    paste("Include years",min(year(rv$dat_years$icu_overall_start_date),na.rm=T), "to",
          max(year(rv$dat_years$icu_overall_start_date),na.rm=T))
  })
  observeEvent(input$varsOfInterest_list, {
    rv$vars_notMissing = unique(c("List",rv$vars_notMissing,input$varsOfInterest_list))
  })
  observeEvent(input$varsOfInterest, {
    rv$vars_notMissing = unique(c("Check",rv$vars_notMissing,input$varsOfInterest))
  })
  
  ################################
  # Additional constraints
  ################################
  # Update which to impose
  observeEvent(input$constraint1_alterData, {
    rv$imposeConstraints[1] <- input$constraint1_alterData
  })
  observeEvent(input$constraint2_alterData, {
    rv$imposeConstraints[2] <- input$constraint2_alterData
  })
  observeEvent(input$constraint3_alterData, {
    rv$imposeConstraints[3] <- input$constraint3_alterData
  })
  # Categorical 
  observeEvent(input$constraintA_alterData, {
    rv$imposeConstraints_Cat[1] <- input$constraintA_alterData
  })
  observeEvent(input$constraintB_alterData, {
    rv$imposeConstraints_Cat[2] <- input$constraintB_alterData
  })
  observeEvent(input$constraintC_alterData, {
    rv$imposeConstraints_Cat[3] <- input$constraintC_alterData
  })
  
  
  
  # Constraint 1
  ################################
  # Chose variable for additional constraint
  output$constraint1_var <- renderUI({
    selectInput('constraint1_var', 'Choose variable:', 
                c(Choose='',  colnames(isolate(rv$dat_compl))), selectize=TRUE)
  })
  output$constraint1_ranges <- renderUI({
    req(input$constraint1_var)
    fn_range <- function(var_here){
      enquo_var_here <- enquo(var_here)
      range(rv$dat_years %>% pull(!!var_here),na.rm=T)
    }
    range_here = fn_range(input$constraint1_var)
    sliderInput("range_constraint1",
                "Range to allow:",
                min = range_here[1],
                max = range_here[2],
                value = range_here)
  })
  # Patient included in new constraint
  output$constraint1_counts <- renderText({
    req(input$constraint1_var)
    dat_here <- as.data.frame(rv$dat_compl)
    nPts = sum(dat_here[,input$constraint1_var] >= input$range_constraint1[1] & 
                 dat_here[,input$constraint1_var] <= input$range_constraint1[2],na.rm=T)
    paste0("Subsets to N = ",nPts, " patients")
  })
  # Button to trigger change in data
  output$constraint1_alterData <- renderUI({
    req(input$constraint1_var)
    checkboxInput("constraint1_alterData", "Apply", value = FALSE, width = NULL)
    #radioButtons("constraint1_alterData", "", choices = c("Clear","Apply"), selected = "Clear",
    #            inline = TRUE)
  })
  # If anything changes, un-apply constraint
  observeEvent(c(input$constraint1_var,input$range_constraint1), {
    updateCheckboxInput(session, "constraint1_alterData", value = FALSE)
  })
  
  
  # Constraint 2
  ################################
  # Chose variable for additional constraint
  output$constraint2_var <- renderUI({
    selectInput('constraint2_var', 'Choose variable:', 
                c(Choose='',  colnames(isolate(rv$dat_compl))), selectize=TRUE)
  })
  output$constraint2_ranges <- renderUI({
    req(input$constraint2_var)
    fn_range <- function(var_here){
      enquo_var_here <- enquo(var_here)
      range(rv$dat_years %>% pull(!!var_here),na.rm=T)
    }
    range_here = fn_range(input$constraint2_var)
    sliderInput("range_constraint2",
                "Range to allow:",
                min = range_here[1],
                max = range_here[2],
                value = range_here)
  })
  # Patient included in new constraint
  output$constraint2_counts <- renderText({
    req(input$constraint2_var)
    dat_here <- as.data.frame(rv$dat_compl)
    nPts = sum(dat_here[,input$constraint2_var] >= input$range_constraint2[1] & 
                 dat_here[,input$constraint2_var] <= input$range_constraint2[2],na.rm=T)
    paste0("Subsets to N = ",nPts, " patients")
  })
  # Button to trigger change in data
  output$constraint2_alterData <- renderUI({
    req(input$constraint2_var)
    checkboxInput("constraint2_alterData", "Apply", value = FALSE, width = NULL)
    #radioButtons("constraint2_alterData", "", choices = c("Clear","Apply"), selected = "Clear",
    #            inline = TRUE)
  })
  # If anything changes, un-apply constraint
  observeEvent(c(input$constraint2_var,input$range_constraint2), {
    updateCheckboxInput(session, "constraint2_alterData", value = FALSE)
  })  
  
  
  # Constraint 3
  ################################
  # Chose variable for additional constraint
  output$constraint3_var <- renderUI({
    selectInput('constraint3_var', 'Choose variable:', 
                c(Choose='',  colnames(isolate(rv$dat_compl))), selectize=TRUE)
  })
  output$constraint3_ranges <- renderUI({
    req(input$constraint3_var)
    fn_range <- function(var_here){
      enquo_var_here <- enquo(var_here)
      range(rv$dat_years %>% pull(!!var_here),na.rm=T)
    }
    range_here = fn_range(input$constraint3_var)
    sliderInput("range_constraint3",
                "Range to allow:",
                min = range_here[1],
                max = range_here[2],
                value = range_here)
  })
  # Patient included in new constraint
  output$constraint3_counts <- renderText({
    req(input$constraint3_var)
    dat_here <- as.data.frame(rv$dat_compl)
    nPts = sum(dat_here[,input$constraint3_var] >= input$range_constraint3[1] & 
                 dat_here[,input$constraint3_var] <= input$range_constraint3[2],na.rm=T)
    paste0("Subsets to N = ",nPts, " patients")
  })
  # Button to trigger change in data
  output$constraint3_alterData <- renderUI({
    req(input$constraint3_var)
    checkboxInput("constraint3_alterData", "Apply", value = FALSE, width = NULL)
    #radioButtons("constraint3_alterData", "", choices = c("Clear","Apply"), selected = "Clear",
    #            inline = TRUE)
  })
  # If anything changes, un-apply constraint
  observeEvent(c(input$constraint3_var,input$range_constraint3), {
    updateCheckboxInput(session, "constraint3_alterData", value = FALSE)
  })  
  
  # Categorical constraint A:
  ################################
  # Chose variable for additional categorical constraint
  output$constraintA_var <- renderUI({
    selectInput('constraintA_var', 'Choose variable:',selected="abx_usage_within_day_2_3)", 
                c(Choose='',  colnames(isolate(rv$dat_compl))), selectize=TRUE)
  })
  output$constraintA_choices <- renderUI({
    req(input$constraintA_var)
    choices_here = unique(rv$dat_years %>% pull(!!input$constraintA_var))[
      order(unique(rv$dat_years %>% pull(!!input$constraintA_var)))] %>% head(20)
    checkboxGroupInput("constraintA_choices", "Values", choices_here, selected = NULL, inline = FALSE,
                       width = NULL)
  })
  # Patients included in new constraint
  output$constraintA_counts <- renderText({
    req(input$constraintA_var)
    dat_here <- as.data.frame(rv$dat_compl)
    nPts = sum(dat_here[,input$constraintA_var] %in% input$constraintA_choices)
    nPtsNA = sum(is.na(dat_here[,input$constraintA_var]))
    if("" %in% input$constraintA_choices) nPts = nPts + nPtsNA
    paste0("Subsets to N = ",nPts, " patients")
  })
  # Button to trigger change in data
  output$constraintA_alterData <- renderUI({
    req(input$constraintA_var)
    checkboxInput("constraintA_alterData", "Apply", value = FALSE, width = NULL)
    #radioButtons("constraintA_alterData", "", choices = c("Clear","Apply"), selected = "Clear",
    #            inline = TRUE)
  })
  
  # Categorical constraint B:
  ################################
  # Chose variable for additional categorical constraint
  output$constraintB_var <- renderUI({
    selectInput('constraintB_var', 'Choose variable:',selected="abx_usage_within_day_2_3)", 
                c(Choose='',  colnames(isolate(rv$dat_compl))), selectize=TRUE)
  })
  output$constraintB_choices <- renderUI({
    req(input$constraintB_var)
    choices_here = unique(rv$dat_years %>% pull(!!input$constraintB_var))[
      order(unique(rv$dat_years %>% pull(!!input$constraintB_var)))]%>% head(20)
    checkboxGroupInput("constraintB_choices", "Values", choices_here, selected = NULL, inline = FALSE,
                       width = NULL)
  })
  # Patients included in new constraint
  output$constraintB_counts <- renderText({
    req(input$constraintB_var)
    dat_here <- as.data.frame(rv$dat_compl)
    nPts = sum(dat_here[,input$constraintB_var] %in% input$constraintB_choices)
    nPtsNA = sum(is.na(dat_here[,input$constraintB_var]))
    if("" %in% input$constraintB_choices) nPts = nPts + nPtsNA
    paste0("Subsets to N = ",nPts, " patients")
  })
  # Button to trigger change in data
  output$constraintB_alterData <- renderUI({
    req(input$constraintB_var)
    checkboxInput("constraintB_alterData", "Apply", value = FALSE, width = NULL)
    #radioButtons("constraintB_alterData", "", choices = c("Clear","Apply"), selected = "Clear",
    #            inline = TRUE)
  })
  
  # Categorical constraint C:
  ################################
  # Chose variable for additional categorical constraint
  output$constraintC_var <- renderUI({
    selectInput('constraintC_var', 'Choose variable:',selected="abx_usage_within_day_2_3)", 
                c(Choose='',  colnames(isolate(rv$dat_compl))), selectize=TRUE)
  })
  output$constraintC_choices <- renderUI({
    req(input$constraintC_var)
    choices_here = unique(rv$dat_years %>% pull(!!input$constraintC_var))[
      order(unique(rv$dat_years %>% pull(!!input$constraintC_var)))]%>% head(20)
    checkboxGroupInput("constraintC_choices", "Values", choices_here, selected = NULL, inline = FALSE,
                       width = NULL)
  })
  # Patient included in new constraint
  output$constraintC_counts <- renderText({
    req(input$constraintC_var)
    dat_here <- as.data.frame(rv$dat_compl)
    nPts = sum(dat_here[,input$constraintC_var] %in% input$constraintC_choices)
    nPtsNA = sum(is.na(dat_here[,input$constraintC_var]))
    if("" %in% input$constraintC_choices) nPts = nPts + nPtsNA
    paste0("Subsets to N = ",nPts, " patients")
  })
  # Button to trigger change in data
  output$constraintC_alterData <- renderUI({
    req(input$constraintC_var)
    checkboxInput("constraintC_alterData", "Apply", value = FALSE, width = NULL)
    #radioButtons("constraintC_alterData", "", choices = c("Clear","Apply"), selected = "Clear",
    #            inline = TRUE)
  })
  
  #####################
  # Reset to full data (refreshed page)
  #####################
  observeEvent(input$resetData, {
    rv$dat_years = dat_merged
    rv$dat_compl = dat_merged
    rv$imposeConstraints = rep("Clear",10)
    rv$imposeConstraints_Cat = rep("Clear",10)
    # Clear vars of interest
    updateCheckboxGroupInput(session, "varsOfInterest", label = "", 
                             choices = colnames(dat_merged), 
                             selected = c("ICUwing_cat"),
                             inline = FALSE, choiceNames = NULL, choiceValues = NULL)
    # # Clear constraints
    updateSelectInput(session, 'constraint1_var', 'Choose variable:', 
                      c(Choose='',  colnames(isolate(rv$dat_compl))))
    updateSelectInput(session, 'constraint2_var', 'Choose variable:', 
                      c(Choose='',  colnames(isolate(rv$dat_compl))))
    updateSelectInput(session, 'constraint3_var', 'Choose variable:', 
                      c(Choose='',  colnames(isolate(rv$dat_compl))))
    updateSelectInput(session, 'constraintA_var', 'Choose variable:', 
                      c(Choose='',  colnames(isolate(rv$dat_compl))))
    updateSelectInput(session, 'constraintB_var', 'Choose variable:', 
                      c(Choose='',  colnames(isolate(rv$dat_compl))))
    updateSelectInput(session, 'constraintC_var', 'Choose variable:', 
                      c(Choose='',  colnames(isolate(rv$dat_compl))))
  })  
  
  #####################
  # Data summary
  #####################
  # Current sample size
  output$sampSizeTitle <- renderText({
    paste0("Current sample size: ",nrow(rv$dat_compl))
  })
  
  # draw the histogram of N patients with complete data
  output$distPlotComplete <- renderPlot({
    rv$dat_compl %>% 
      # draw the histogram with the specified number of bins
      ggplot(aes(year(icu_overall_start_date))) + 
      geom_histogram(binwidth = 1,fill="#B31B1B") + 
      xlim(min(year(rv$dat_compl$icu_overall_start_date)-1,na.rm=T),max(year(rv$dat_compl$icu_overall_start_date),na.rm=T)+1) +
      labs(title=paste0("Total patient counts per year (N=",nrow(rv$dat_compl),")"),
           subtitle=str_wrap(paste0("Complete ",paste(input$varsOfInterest,collapse=", ")),90),
           x="Year of Admission",y="N paitents with complete data") +
      theme(text = element_text(size = 18),
            plot.subtitle=element_text(size=10, face="italic", color="darkgrey"))
  }, width=500, height=400)
  
  # Chose variable for univariable summary
  output$VarForTable_input <- renderUI({
    selectInput('varForTable', 'Choose variable:',selected="ICUwing_cat", 
                c(Choose='',  colnames(dat_merged)), selectize=TRUE)
  })
  
  # Histogram and boxplot
  output$ChosenVar_Plots <- renderPlot({
    req(input$varForTable)
    var_here =  ifelse(!is.null(input$varForTable),input$varForTable,"ICUwing_cat")
    
    if(is.numeric(rv$dat_compl %>% pull(!!var_here)) & 
       length(unique(rv$dat_compl %>% pull(!!var_here))) > 3){
      rv$dat_compl %>% 
        filter(!is.na(!!sym(var_here))) %>% 
        ggplot(aes_string(paste0("as.numeric(",var_here,")"))) +
        geom_histogram(aes(fill="#CF4520"),bins = 30) +
        labs(title=paste0(var_here," (n=",
                          nrow(rv$dat_compl %>% filter(!is.na(!!sym(var_here)))),")"),
             x="") +
        guides(fill=FALSE) +
        theme(text = element_text(size = 20)) ->
        plotHist
      
      rv$dat_compl %>%
        filter(!is.na(!!sym(var_here))) %>%
        ggplot(aes_string(x="1",y=paste0("as.numeric(",var_here,")"))) +
        geom_jitter(width=.05,alpha=.5,height=0) +
        geom_boxplot(width=.25,outlier.colour = NA,fill="#CF4520") +
        labs(title="",x="",y=var_here) +
        guides(fill=FALSE) +
        theme(text = element_text(size = 20),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) ->
        plotBplot
      grid.arrange(plotHist,plotBplot,nrow=1,widths=c(2,1))
    }else{
      if(length(unique(rv$dat_compl %>% pull(!!var_here))) > 200){stop("Too many levels")}
      rv$dat_compl %>% 
        mutate(var_plot = as.factor(!!sym(var_here))) %>% 
        ggplot(aes_string("var_plot",fill="var_plot",group="1")) +
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes(label = ..count.., y= ..prop..), stat= "count", vjust = -.6) +
        geom_text(aes(label = paste0(round(..prop..*100),"%"), y= ..prop..), stat= "count", vjust = 1.2) +
        labs(y = "Percent") +
        scale_y_continuous(labels=percent) +
        labs(title=paste0(var_here," (n=",
                          nrow(rv$dat_compl %>% filter(!is.na(!!sym(var_here)))),")"),
             x="") +
        guides(fill=FALSE) +
        theme(text = element_text(size = 20)) -> barplot
      if(length(unique(rv$dat_compl %>% pull(!!var_here))) >6) barplot = barplot + coord_flip()
      barplot 
    }
  })
  
  #Contingency table
  # output$ChosenVar_Table <- renderDataTable({
  #   var_here =  ifelse(!is.null(input$varForTable),input$varForTable,"ICUwing")
  #   tab <- rv$dat_compl %>%
  #     count(!!sym(var_here)) %>%
  #     rename(N_patients = n) %>%
  #     select(N_patients,everything())
  #   DT::datatable(tab,
  #                options = list(pageLength = 20,orderClasses = TRUE, autoWidth = TRUE,
  #                                     sDom  = '<"top">lrt<"bottom">ip'),
  #                 rownames = FALSE, escape=FALSE)
  # })
  output$ChosenVar_Table <- renderTable({
    req(input$varForTable)
    var_here =  ifelse(!is.null(input$varForTable),input$varForTable,"ICUwing_cat")
    rv$dat_compl %>%
      group_by(!!sym(var_here)) %>% 
      summarise(N_patients = n()) %>%
      mutate(Percent = round(N_patients/sum(N_patients)*100,2) )
    #select(N_patients,everything()) 
    # Doesn't show up if I add options - reason unclear
  }
  )
  #####################
  # Data summary of dat_compl
  #####################
  output$dataSummary <- renderText({
    summ <- paste("This dataset consists of N =",length(rv$dat_compl$Pt_Enc),
                  "unique admissions to the ICU among",
                  length(unique(rv$dat_compl$PATIENT_NUM))," patients. Only the first admission to ICU per hospitalization is included")
    # Add more specifications
    summ <- paste0(summ,".")
    summ
  })
  
  #####################
  # Data dictionary of variables in dat_compl
  #####################
  output$dataDictio_table <- renderDataTable({
    datatable(data_dictionary)
    # Doesn't show up if I add options - reason unclear
    #DT::datatable(data_dictionary, options = list(lengthMenu = c(50), pageLength = 5))
  })
  
  #####################
  # Data quirks and issues
  #####################
  output$dataQuirks_list <- renderUI({
    HTML("<ul><li>
         This dataset was extracted 12/01/19, and comorbidities were determined 12/01/19
         </li><li>
         Tables available in SQL but not here include SOFA for days other than 1,3,7, transfusions, IO, and others.
         </li><li>
         BMI is chart or calculated BMI, and if both are available it is the closest to 35.
         </li><li>
         Only cultures that have an organism are included.
         </li><li>
         Dates are shifted up to 365 days for deintentification.
         </li><li>
         RESPIRATORY_RATE_MIN is the setting on the ventilator, while RR is the measurement on the patient. 
         </li></ul>")
  })
  output$dataIssues_list <- renderUI({
    HTML("<ul><li>
         Some temps and other min/max values are unreasonable.
         </li></ul>")
  })
  output$dataQuestions_list <- renderUI({
    HTML("<ul><li>
         Is duration_of_invasive_mechanical_ventilation_days number or last day?
         </li></ul>")
  })
})
