# load libraries
library(shiny)
library(bslib)
#library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(splines)
library(bsicons)
library(shinyjs)
library(DiagrammeR)
library(purrr)
library(deSolve)
library(RSQLite)
#library(geomtextpath)
library(plotly)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(kableExtra)
#library(shinyBS)
library(withr)
#library(reactlog)
library("pool")
library(waiter)
library(future)
library(future.callr)
library(promises)

plan(callr)

f = file.path(
  'output', 'power_study_merged', '2024-09-04-power_study_merged.sqlite3'
  
  
)


#con = dbConnect(SQLite(), dbname = f, flags = RSQLite::SQLITE_RO)
pool_db = dbPool(drv = SQLite(),
                 dbname = f)

onStop(function() poolClose(pool_db))


#source(file.path('utils', 'power_study_interpolation.R'))

#reactlog_enable()

# load utilities
invisible(
  lapply(dir(path = 'utils', pattern = '*.R', full.names = TRUE), source)
)

# load plotting
source(file.path('plotting', 'graph_freedom_estimation.R'))
source(file.path('plotting', 'graph_prevalence_estimation.R'))
source(file.path('plotting', 'graph_prevalence_bounds.R'))
source(file.path('plotting', 'graph_nimble_freedom.R'))
source(file.path('plotting', 'graph_nimble_freedom_2.R'))
# source(file.path('run_nimble.R'))
#source(file.path('graph_sibr.R'))

# card design for landing page modules
card1 <- card(
  HTML(r"(<h3 class ="card-header">Detection</h2>)"),
  "Is there disease in a population?",
  img(src = "det.png",style = "display: block; margin-left: auto; margin-right: auto;", width = "150px"),
  #HTML(r"(<a href="#" class="btn btn-primary">Go</a>)"),
  actionButton(inputId = "Detection", label = "Enter module")
)

card2<- card(
  HTML(r"(<h3 class ="card-header">Prevalence</h2>)"),
  "How much disease is in a population?",
  img(src = "prevalence.png", style = "display: block; margin-left: auto; margin-right: auto;", width = "200px"),
  actionButton(inputId= "Prevalence", label = "Enter module")
)

card4<- card(
  HTML(r"(<h3 class ="card-header">Epidemiological Dynamics</h2>)"),
  "How fast does disease spread through a population?  How long does disease persist in a population?",
  img(src = "SIBR.png", style = "display: block; margin-left: auto; margin-right: auto;", width = "200px"),
  actionButton(inputId = "Epi", label = "Enter module")
)


# ui
ui <- fluidPage(shinyjs::useShinyjs(),
                navbarPage(
                  theme = bslib::bs_theme(bootswatch = "morph"),
                  title = "SASSE",
                  id = "navbar",
                  tabPanel("Home",
                           h1(paste0("Surveillance Analysis and Sample Size Explorer (SASSE)")),
                           ## design optimization for wildlife disease surveillance
                           br(),
                           fluidRow(
                             h5("Use this tool to explore sample size needs and data analyses as they relate to common wildlife disease surveillance questions.")),
      useWaiter(),
      br(),
      layout_column_wrap(width = 1/2, height = 375,
                         card1, card2
      ),
      layout_column_wrap(width = 1/2, height = 375,
                         card4
                         
                         )),
      navbarMenu("Detection",
                 tabPanel("Detection Activity",
                          h3("Detection Analysis"),
                          h6("Use this tool to explore the probability that disease is present in a target population based on inputs that describe:"), ## TODO: animate input sliders
                          
                          layout_column_wrap(width = 1/2,
                                             card(card_header("True Ecological Process"),
                                                  img(src = "observed.png",style = "display: block; margin-left: auto; margin-right: auto;"),
                                                  br(),
                                                  h5("Expected Presence"),
                                                  sliderInput("presence_prob", label = "How probable do you think it is that disease is present?", min=0, max=1, value = 0.5),
                                                  br(),
                                                  h5("Sampling Strata"),
                                                  h6("(e.g., sex, age class, surveillance method)"),
                                                  uiOutput("addInputs"),
                                                  #uiOutput("addInputs2"),
                                                  fluidRow(column(width = 3, actionButton(inputId = "add", label = "Add stratum")),
                                                           column(width = 3, actionButton(inputId = "rmv", label = "Remove stratum")))),
                                             
                                             card(card_header("Observation Process"),
                                                  img(src = "trueEco.png", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                  br(),
                                                  #fluidRow(),
                                                  #h5(""),
                                                  uiOutput("dataInputs"),
                                                  #h5("Sample Inputs"),
                                                  #uiOutput("dataInputsObs"),
                                                  #h5("Assay Inputs"),
                                                  #uiOutput("dataInputsDiag"),
                                                  fluidRow(column(width = 3, actionButton(inputId = "addData", label = "Add data")),
                                                           column(width = 3, actionButton(inputId = "rmvData", label = "Remove data"))))
                          ),
                          
                          fluidRow(
                            #column(width = 5, tableOutput("test")),
                            column(width = 8, div(style = "height:300px;", grVizOutput('diagram', height = "300px")))),
                          actionButton(inputId = "run", label = "Analyze"),
                          br(),
                          br(),
                          h4("Key takeaways"),
                          fluidRow(
                            column( width = 4, value_box(
                              title = "Initial expectations",
                              value = textOutput("prior"),
                              style= "background-color: #2c7f94; color: white",
                              showcase = bs_icon("journal-richtext"),
                              p("Probability that disease is present")
                            )),
                            column(width = 4,value_box(
                              title = "Observations",
                              value = textOutput("data"),theme_color = "warning",
                              
                              showcase = bs_icon("ui-checks-grid"),
                              #p("Positive samples were found in the sample data collected")
                              p(textOutput("data_sample"))
                            )),
                            column(width = 4,value_box(
                              title = "Analytic results",
                              value = textOutput("posterior"),style= "background-color: #2c7f94; color: white",
                              showcase = bs_icon("intersect"),
                              #p("Probability the sampling populations are free from disease, vs. YYYY probability that disease is present")
                              p(textOutput("posterior2"))
                            ))
                          ),
                          h4("Detailed results"),
                          #h5("What is the probability the population is free from disease?"),
                          # bold probailtiy
                          card(h5("What is the probability the population is free from disease?"),
                               plotlyOutput("freedom2")),
                          card(h5("What is the probability that disease is present in the population?"),
                               plotlyOutput("freedom")),
                          card(h5("If the analysis does not result in 100% probability of disease freedom, what prevalence of disease could exist in the population?"),
                               selectInput("plot_by_name", label = "Select sampling population to view:", choices = c()),
                               plotlyOutput("freedom_bounds")),
                          br(),
                          div(style = "display:inline-block; float:right", actionButton(inputId = "power", label = "Go to Power Study")),
                          div(style = "display:inline-block; float:left", actionButton(inputId = "back", label = "Return to home page")),
                          br(),
                          br()
                          
                          
                 ),
                 tabPanel("Power Study", ## want to eval the design itself
                          h3("Power Study"),
                          h6("Use this tool to determine a good sample size for your study."),
                          
                          
                          h4("Inputs"),
                          card(
                            fluidRow(
                              column(width = 6, radioButtons("prevPrior", label = "What range of prevalence is possible for this sampling stratum?", choices = list("0%-100% (I'm not sure)" = 1,
                                                                                                                                                                    "0%-30%"=2,
                                                                                                                                                                    "10%-40%"=3,
                                                                                                                                                                    "20%-50%"=4,
                                                                                                                                                                    "30%-60%"=5,
                                                                                                                                                                    "40%-70%"=6,
                                                                                                                                                                    "50%-80%"=7,
                                                                                                                                                                    "60%-90%"=8,
                                                                                                                                                                    "70%-100%"=9), selected = 1)),
                              column(width = 6, sliderInput("prior_disease", label = "How probable do you think it is that disease is present?", min =0, max = 1, value = 0.3, step = 0.1))),
                            
                            fluidRow(column(width = 4, sliderInput("sensitivity", label = "What is the assay type sensitivity?", min = 0.5, max = 1, value = 0.7, step = 0.05)),
                                     column(width = 4, sliderInput("specificity", label = "What is the assay type specificity?", min = 0.5, max = 1, value = 1, step = 0.05)),
                                     column(width = 4,sliderInput("true_prevalence", label = "What is the true prevalence?",min =0, max = 1, value = 0.05, step = 0.05)))),
                          h4("Outputs"),
                          card(
                            h5(textOutput("descr")),
                            br(),
                            fluidRow(plotlyOutput("power_graph")),
                            br(),
                            h5(textOutput("descr2")),
                            br(),
                            fluidRow(plotlyOutput("power_graph_presence")),
                            br(),
                            
                          ),
                          
                          div(style = "display:inline-block; float:right", actionButton(inputId = "det", label = "Go to Detection Activity")),
                          div(style = "display:inline-block; float:left", actionButton(inputId = "back6", label = "Return to home page")),
                          br())


      ),
      navbarMenu("Prevalence",
                 tabPanel("Prevalence Activity",
                          h3("Prevalence Analysis"),
                          h6("Use this tool to explore the prevalence estimation and uncertainty in a target population based on inputs that describe:"), ## TODO: animate input sliders
                          layout_column_wrap(width = 1/2,
                                             card(card_header("True Ecological Process"),
                                                  img(src = "observed.png",style = "display: block; margin-left: auto; margin-right: auto;"),
                                                  br(),
                                                  h5("Expected Presence"),
                                                  sliderInput("presence_prob_prev", label = "How probable do you think it is that disease is present?", min=0, max=1, value = 0.5),
                                                  br(),
                                                  h5("Sampling Strata"),
                                                  h6("(e.g., sex, age class, surveillance method)"),
                                                  uiOutput("addInputs_prev"),
                                                  fluidRow(column(width = 3, actionButton(inputId = "add_prev", label = "Add stratum")),
                                                           column(width = 3, actionButton(inputId = "rmv_prev", label = "Remove stratum")))),
                                             
                                             card(card_header("Observation Process"),
                                                  img(src = "trueEco.png", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                  br(),
                                                  uiOutput("dataInputs_prev"),
                                                  fluidRow(column(width = 3, actionButton(inputId = "addData_prev", label = "Add data")),
                                                           column(width = 3, actionButton(inputId = "rmvData_prev", label = "Remove data")))),
                          ),  
                          
                          fluidRow(
                            column(width = 8, div(style = "height:300px;", grVizOutput('prev_diagram', height = "300px")))),
                          actionButton(inputId = "run_prev", label = "Analyze"),
                          br(),
                          h4("Results"),
                          card(
                            selectInput("plot_by_pop", label = "Which sampling population would you like to view results for?", choices = c()),
                            fluidRow(
                              column( width = 4, value_box(
                                title = "Initial expectations",
                                value = textOutput("prior_p"),style= "background-color: #2c7f94; color: white",
                                showcase = bs_icon("journal-richtext"),
                                p("Prevalence possible for this sampling population")
                              )),
                              column(width = 4,value_box(
                                title = "Observations",
                                value = textOutput("data_p"),theme_color = "warning",
                                showcase = bs_icon("ui-checks-grid"),
                                p("Tests were positive for this sampling population")
                              )),
                              column(width = 4,value_box(
                                title = "Analytic results",
                                value = textOutput("posterior_p"),style= "background-color: #2c7f94; color: white",
                                showcase = bs_icon("intersect"),
                                p(textOutput("posterior_p_bound"))
                              ))
                            ),
                            h5("What is the prevalence estimated to be?"),
                            fluidRow(plotlyOutput("prevGraph"))
                          ),
                          br(),
                          div(style = "display:inline-block; float:right", actionButton(inputId = "powerprev", label = "Go to Power Study")),
                          div(style = "display:inline-block; float:left", actionButton(inputId = "back2", label = "Return to home page")),
                          br(),
                          br()
                 ),
                 tabPanel("Power Study (Prevalence)",
                          h3("Power Study"),
                          h6("Use this tool to determine a good sample size for your study."),
                          h4("Inputs"),
                          card(
                            fluidRow(
                              column(width = 6, radioButtons("prevPrior_p", label = "What range of prevalence is possible for this sampling stratum?", choices = list("0%-100% (I'm not sure)" = 1,
                                                                                                                                                                      "0%-30%"=2,
                                                                                                                                                                      "10%-40%"=3,
                                                                                                                                                                      "20%-50%"=4,
                                                                                                                                                                      "30%-60%"=5,
                                                                                                                                                                      "40%-70%"=6,
                                                                                                                                                                      "50%-80%"=7,
                                                                                                                                                                      "60%-90%"=8,
                                                                                                                                                                      "70%-100%"=9), selected = 1)),
                              column(width = 6, sliderInput("prior_disease_p", label = "How probable do you think it is that disease is present?", min =0, max = 1, value = 0.3, step = 0.1))),
                            fluidRow(  
                              
                              column(width = 4, sliderInput("sensitivity_p", label = "What is the assay type sensitivity?", min = 0.5, max = 1, value = 0.7, step = 0.05)),
                              column(width = 4, sliderInput("specificity_p", label = "What is the assay type specificity?", min = 0.5, max = 1, value = 1, step = 0.05)),
                              column(width = 4,sliderInput("true_prevalence_p", label = "What is the true prevalence?",min =0, max = 1, value = 0.05, step = 0.05)))),
                          h4("Outputs"),
                          card(h5(textOutput("descr_prev")),
                               br(),
                               fluidRow(plotlyOutput("power_graph_p")),
                               br(),
                               h5("Analytic assumptions can also influence the precision of a prevalence analysis, quantified by the amount of uncertainty in a prevalence estimate (i.e., the width of the 95% credible interval):"),
                               HTML("<ul>
                         <li> For example, if prevalence is estimated to be 35% with a credible interval of 15-45%, then the credible interval width is 45% - 15% = 30%.</li>
                         <li> In general, the graph below shows how the average credible interval width depends on sample size:</li>
                         </ul>"),
                         br(),
                         fluidRow(plotlyOutput("ci_graph_p"))),
                         div(style = "display:inline-block; float:left", actionButton(inputId = "back7", label = "Return to home page")),
                         div(style = "display:inline-block; float:right", actionButton(inputId = "Navprev", label = "Go to Prevalence Activity")),
                         br(),
                         br())
 

      ),
      tabPanel("Epidemiological Dynamics",
               h2("Epidemiological Dynamics"),
                 img(src = "CompartmentalModel.png", style = "display: block; margin-left: auto; margin-right: auto;", width = "550px"),
               h3("Inputs"),
               card(
               fluidRow(
                 h4("Epidemiological parameters"),
                 column(width = 3, numericInput("infDays", label = "Average Infectious Days:", 7)),
                 column(width = 3, numericInput("recDays", label = "Average Recovery Days:", 30)),
                 column(width = 3, numericInput("waneDays", label = "Average Waning Days:", 180)),
                 column(width = 3, numericInput("R0", label = "R0:", 1.5))
               ),
               
               fluidRow(
                 h4("Initial population sizes at time point 0"),
                 column(width = 3, numericInput("susc", label = "Susceptible:", 1e3)),
                 column(width = 3, numericInput("inf", label = "Infectious:", 1)),
                 column(width = 3, numericInput("broadRec", label = "Broadly Recovered:", 0)),
                 column(width = 3, numericInput("Rec", label = "Recovered:", 0))
               ),
               
               fluidRow(
                 h4("Outbreak timing"),
                 column(width = 4, dateInput("start", label = "Outbreak Start Date:", value = ymd('2021-03-01'))),
                 column(width = 4, dateInput("pulse", label = "Birth Pulse Date:", value = ymd('2021-06-01'))),
                 column(width = 4, sliderInput("propPulse", label = "Proportion of new, susceptible animals after pulse:", min = 0, max = 1, value = 0))
                 #column(width = 6, sliderInput("plotMonth", label = "Range of Months to Plot:", min = 0, max = 24, value = c(0,24)))
               ),

               fluidRow(
                 h4("Diagnostic parameters"),
                 column(width = 3, sliderInput("path_sens", label = "Pathogen Assay Sensitivity:", min = 0, max = 1, value = 0.95)),
                 column(width = 3, sliderInput("path_spec", label = "Pathogen Assay Specificity:", min = 0, max = 1, value = 0.99)),
                 column(width = 3, sliderInput("ant_sens", label = "Antibody Assay Sensitivity:", min = 0, max = 1, 0.95)),
                 column(width = 3, sliderInput("ant_spec", label = "Antibody Assay Specificity:", min = 0, max = 1, 0.99))
               )),
               
               actionButton("update", label = "Update"),
               br(),
               br(),
               h4("Output"),
               fluidRow(card(h5("Over time, what proportion of the population will fall into each of the SIBR compartments?"),
                 plotlyOutput("sibr"))),
               fluidRow(card(h5("Over time, what proportion of each test type do you expect will be positive?"),
                 plotlyOutput("sibr2"))),
               
               fluidRow(card(
                 h5("Guidance:"),
                 "Use patterns in the graphs to identify when, and how much, disease ebbs and flows.  Then, use the Detection and Prevalence tools to plan sampling around predicted disease patterns."
               )),
               actionButton(inputId = "back4", label = "Return to home page")

      )
      
      
                ))


server <- function(input, output, session) {
  
  # module navigation
  observeEvent(input$Detection,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Detection Activity")
  })
  observeEvent(input$power,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Power Study")
  })
  observeEvent(input$det,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Detection Activity")
  })
  observeEvent(input$Prevalence,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Prevalence Activity")
  })
  observeEvent(input$powerprev,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Power Study (Prevalence)")
  })
  observeEvent(input$Navprev,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Prevalence Activity")
  })
  observeEvent(input$Epi,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Epidemiological Dynamics")
  })
  observeEvent(input$back,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Home")
  })
  observeEvent(input$back2,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Home")
  })
  observeEvent(input$back4,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Home")
  })
  observeEvent(input$back6,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Home")
  })
  observeEvent(input$back7,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Home")
  })
  
  ################################# Detection ###########################################
  
  ########### power study ################

  
  #
  # open connection to power study results
  #
  
  power_plot <- reactive({
    
    conn = poolCheckout(pool_db)
    
    # example of pulling data:
    plot_var = 'prob_signif_disease_freedom'
    true_prevalence = input$true_prevalence
    prior_disease = input$prior_disease
    sensitivity = input$sensitivity
    specificity = input$specificity
    
    power_study_results = dbGetQuery(
      conn = conn,
      statement = paste(
        'select sample_size,', plot_var, 'from power_study_deduplicated',
        'where prior_prevalence = ? and prior_disease > ? and prior_disease < ?',
        'and sensitivity > ? and sensitivity < ? and specificity > ?',
        'and specificity < ? and true_prevalence > ? and true_prevalence < ?'
      ),
      params = list( # TODO: connect flagged items to user input
        input$prevPrior, # prior_prevalence  # TODO: connect to user input
        # account for numerical precision issues
        prior_disease - .005,
        prior_disease + .005,
        sensitivity - .005,
        sensitivity + .005,
        specificity - .005,
        specificity + .005,
        true_prevalence - .005,
        true_prevalence + .005
      )
    )%>% 
    arrange(sample_size)
    
    poolReturn(conn)


  sample_size_seq = seq(
    from = min(power_study_results$sample_size), 
    to = max(power_study_results$sample_size), 
    by = 5
  )
  
  df = data.frame(
    sample_size = sample_size_seq,
    value = power_study_interpolation(
      x = power_study_results$sample_size, 
      y = power_study_results[,2],
      x0 = sample_size_seq, 
      tol = .01
    )
  )
  colnames(df)[2] = plot_var
  
  
  df
  
  })

  output$power_table <- renderTable({
    df <- power_plot()
    
    df
  })
  #

  
  output$descr <- renderText({
    truePrev <- input$true_prevalence
    
    paste0("Accuracy: How often will studies with the stated assumptions find statistically significant evidence for disease freedom when 
 ",
           truePrev,
           " is the actual prevalence in the population being studied?")
  })
  output$descr2 <- renderText({
    truePrev <- input$true_prevalence
    
    paste0("Accuracy: How often will studies with the stated assumptions find statistically significant evidence for disease presence when 
 ",
 truePrev,
 " is the actual prevalence in the population being studied?")
  })
  
  
  output$power_graph <- renderPlotly({
    waiter_show(
      color = 'rgba(217, 227, 241,.75)',
      html = tagList(
        div(
          spin_flowers(),
          style = paste(
            'margin:auto; position:relative; top:unset; left:unset;',
            'transform:unset'
          )
        ),
        br(),
        tagAppendAttributes(
          style = 'color:black',
          p('Visualizing inputs')
        )
      )
    )


    df <- power_plot()
    plot_var = 'prob_signif_disease_freedom'
    #label_plot <- label_plot()
    
    
    # disease freedom conclusions
    pl_results = df %>% 
      # plot layers
      ggplot(
        mapping = aes(
          x = sample_size, 
          y = .data[[plot_var]]
        )
      ) + 
      # interpolate across sample sizes
      geom_line()+ 
      # formatting
      xlab('Sample size') + 
      scale_y_continuous(
        name = 'Percent of studies', # TODO: make name change with plot_var
        labels = scales::percent, # TODO: make scales change with plot_var
        limits = c(0,1) # TODO: make limits change with plot_var
      ) + 
      theme_few() + 
      theme(
        panel.grid.major = element_line(colour = 'grey90')
      )
    
    # initialize plot
    pl_out = ggplotly(pl_results)
    
    # edit tooltips
    for(tooltip_ind in seq_along(pl_out$x$data[[1]]$text)) {
      # custom tooltip text
      tooltip = paste0(
        'Sample size: ', pl_out$x$data[[1]]$x[tooltip_ind], 
        '<br />',
        'Percent of time study will declare disease freedom: ',
        paste0(round(pl_out$x$data[[1]]$y[tooltip_ind]*100), '%')
      )
      # save custom tooltip
      pl_out$x$data[[1]]$text[[tooltip_ind]] = tooltip
    }
    
    waiter_hide()

    # return final plot
    pl_out
    
    
  })
  

  
  power_plot_presence <- reactive({
    
    
    conn = poolCheckout(pool_db)
    
    # example of pulling data:
    plot_var = 'prob_signif_disease'
    true_prevalence = input$true_prevalence
    prior_disease = input$prior_disease
    sensitivity = input$sensitivity
    specificity = input$specificity
    
    power_study_results = dbGetQuery(
      conn = conn,
      statement = paste(
        'select sample_size,', plot_var, 'from power_study_deduplicated',
        'where prior_prevalence = ? and prior_disease > ? and prior_disease < ?',
        'and sensitivity > ? and sensitivity < ? and specificity > ?',
        'and specificity < ? and true_prevalence > ? and true_prevalence < ?'
      ),
      params = list( # TODO: connect flagged items to user input
        input$prevPrior, # prior_prevalence  # TODO: connect to user input
        # account for numerical precision issues
        prior_disease - .005,
        prior_disease + .005,
        sensitivity - .005,
        sensitivity + .005,
        specificity - .005,
        specificity + .005,
        true_prevalence - .005,
        true_prevalence + .005
      )
    )%>% 
      arrange(sample_size)
    
    poolReturn(conn)
    
    
    sample_size_seq = seq(
      from = min(power_study_results$sample_size), 
      to = max(power_study_results$sample_size), 
      by = 5
    )
    
    df = data.frame(
      sample_size = sample_size_seq,
      value = power_study_interpolation(
        x = power_study_results$sample_size, 
        y = power_study_results[,2],
        x0 = sample_size_seq, 
        tol = .01
      )
    )
    colnames(df)[2] = plot_var
    
    
    df
    
  })
  
  output$power_table <- renderTable({
    df <- power_plot()
    
    df
  })
  
  
  output$power_graph_presence <- renderPlotly({
    
    df <- power_plot_presence()
    plot_var = 'prob_signif_disease'
    
    
    # disease freedom conclusions
    pl_results = df %>% 
      # plot layers
      ggplot(
        mapping = aes(
          x = sample_size, 
          y = .data[[plot_var]]
        )
      ) + 
      # interpolate across sample sizes
      geom_line()+ 
      # formatting
      xlab('Sample size') + 
      scale_y_continuous(
        name = 'Percent of studies', # TODO: make name change with plot_var
        labels = scales::percent, # TODO: make scales change with plot_var
        limits = c(0,1) # TODO: make limits change with plot_var
      ) + 
      theme_few() + 
      theme(
        panel.grid.major = element_line(colour = 'grey90')
      )
    
    
    # initialize plot
    pl_out = ggplotly(pl_results)
    
    # edit tooltips
    for(tooltip_ind in seq_along(pl_out$x$data[[1]]$text)) {
      # custom tooltip text
      tooltip = paste0(
        'Sample size: ', pl_out$x$data[[1]]$x[tooltip_ind], 
        '<br />',
        'Percent of time study will declare disease presence: ',
        paste0(round(pl_out$x$data[[1]]$y[tooltip_ind]*100), '%')
      )
      # save custom tooltip
      pl_out$x$data[[1]]$text[[tooltip_ind]] = tooltip
    }
    
    # return final plot
    pl_out
    
    
  })
  
  ########### activity ###################
  
  # dynamic ui rows for pop
  popRow <- reactiveValues(n=1)
  
  observeEvent(input$add, {popRow$n <- popRow$n +1})
  
  observeEvent(input$rmv,{
    if(popRow$n > 0){popRow$n <- popRow$n -1}
  })
  
  output$addInputs <- renderUI({
    n <- popRow$n
    selectInputs <- lapply(1:n, function(i) {
      fluidRow(
        column(width = 6, textInput(paste0("name",i), label = "What is the name of the sampling stratum?", value = paste0("Stratum ", i))),
        #column(width = 6, textInput(paste0("name_test",i), label = "What is the name of the surveillance method?", value = paste0("Method ", i))))
        column(width = 6, sliderInput(paste0("groupPrev",i), "What range of prevalence is possible for this sampling stratum?", min = 0, max = 1, value = c(0, 0.5))))
    })
    do.call(tagList, selectInputs)
  })
  

  # dynamic ui rows for data
  dataRow <- reactiveValues(n=1)
  
  observeEvent(input$addData, {dataRow$n <- dataRow$n +1})
  
  observeEvent(input$rmvData,{
    if(dataRow$n > 0){dataRow$n <- dataRow$n -1}
  })
  
  output$dataInputs <- renderUI({
    n <- dataRow$n
    x <- popRow$n
    name_val <- lapply(1:x, function(i){
      input[[paste0("name",i)]]
    })
    name_val_other <- lapply(1:x, function(i){
      input[[paste0("name_test",i)]]
    })
    selectInputs <- lapply(1:n, function(i) {
      card(fluidRow(
        column(width = 8,selectInput(paste0("pop", i), label = "What ecological process is this data informing?", choices = c(unlist(unlist(name_val)))))),
      fluidRow(column(width = 6, numericInput(paste0("samples", i), label = "Number of samples:",60)),
               column(width = 6, numericInput(paste0("pos", i), label = "Number of positives:",0))),
      fluidRow(column(width = 4, textInput(paste0("xx",i), label = "Assay type:", value = paste0("Data type ",i))),
               column(width = 4, sliderInput(paste0("sens",i),label = "Sensitivity:", min = 0, max = 1, value = c(0.95))),
               column(width = 4, sliderInput(paste0("spec",i,":"),label = "Specificity:", min = 0, max = 1, value = c(1))))
      )
    })
    do.call(tagList, selectInputs)
  })


  # create dfs for dynamic flow chart
  pop_df <- reactive({
    req(input$name1)
    n <- popRow$n
    name_val <- lapply(1:n, function(i){
      input[[paste0("name",i)]]
    })
    
    for (i in 1:n){
        population <- data.frame(
          Disease = "Disease Presence",
          Population = unlist(name_val)
          )
    }
    
    population
    
    
  })
  
  
  
  data_df <- reactive({
    n <- dataRow$n
    num <- lapply(1:n, function(i){
      paste0("Data",i)
    })
    pop_val <- lapply(1:n, function(i){
      input[[paste0("pop",i)]]
    })
    data_val <- lapply(1:n, function(i){
      input[[paste0("xx",i)]]
    })
    samples_val <- lapply(1:n, function(i){
      input[[paste0("samples",i)]]
    })
    pos_val <- lapply(1:n, function(i){
      input[[paste0("pos",i)]]
    })
    
    for (i in 1:n){
      data <- data.frame(
        Disease = "Disease Presence",
        Population = unlist(pop_val),
        Name = unlist(num),
        DataType = unlist(data_val),
        Samples = as.numeric(unlist(samples_val)),
        Positive = as.numeric(unlist(pos_val))
      )
    }
    
    
    data <- data %>%
      mutate(prop = round(Positive/Samples, 2))

    data <- data %>%
      mutate(Data = paste0("Data: ", prop))
    
  })
  

  
  ## flow chart
  output$diagram <- renderGrViz({
    pop <- pop_df()
    
    for (row in 1:length(pop$Population)){
      rows = paste0("'", pop$Population, "'")
      subs1 = paste0("'", pop$Population, "'[label = '", pop$Population, "']")
    }
    
    test = paste(rows, collapse = " ")
    subs3 <- paste(subs1, collapse = " ")
    
    data <- data_df()
    
    for (row1 in 1:length(data$Population)){
      rows1 = paste0("'", data$Population, "'->'", data$Name,"'")
      subs = paste0("'", data$Name, "'[label = '", data$DataType, "']")
    }
    
    test2 <- paste( rows1, collapse = " ")
    subs2 <- paste(subs, collapse = " ")
    
    grViz(paste0("digraph {
    graph [layout = dot, rankdir = LR]
    node [shape = rectangle, style = filled, fillcolor = Linen]
    bgcolor = '#d9e3f1'
    
    subgraph cluster_0 {
      graph[shape = rectangle]
      style = rounded
      bgcolor = '#3090C7'
      
      label = 'Disease Status'
      node[shape = rectangle, fillcolor = '#AFDCEC', margin = 0.25]
      'Disease Presence'[label = 'Disease Presence']
    }
    
    subgraph cluster_1 {
      graph[shape = rectangle]
      style = rounded
      bgcolor = '#3EA055'
      
      label = 'Sampling Strata'
      node[shape = rectangle, fillcolor = '#DBF9DB', margin = 0.25]
      ",subs3,"
    }
    
    subgraph cluster_2 {
      graph[shape = rectangle]
      style = rounded
      bgcolor = '#4863A0'
      
      label = 'Data'
      node[shape = rectangle, fillcolor = '#C9DFEC', margin = 0.25]
      ",subs2,"
    }
    
    'Disease Presence' -> {",test,"}
    ",test2,"
    
    
  }"))
    
  })
  

  observeEvent(input$run, {
    x <- popRow$n
    name_val <- lapply(1:x, function(i){
      input[[paste0("name",i)]]
    })
    
    updateSelectInput(
      session,
      "plot_by_name",
      choices = c(unlist(name_val)),
      selected = input$name1
    )
  })
  
  

    
    # observe event button click to analyze & render output
    detGraph_list <- eventReactive(input$run, {
      
      waiter_show(
        color = 'rgba(217, 227, 241,.75)',
        html = tagList(
          div(
            spin_flowers(),
            style = paste(
              'margin:auto; position:relative; top:unset; left:unset;',
              'transform:unset'
            )
          ),
          br(),
          tagAppendAttributes(
            style = 'color:black',
            p('Analyzing data')
          )
        )
      )
      
      
      
      future_promise_data <- list(n = popRow$n,
                                  x = dataRow$n)
      
      
      if(future_promise_data$n == 1 & future_promise_data$x == 1) {


      # specify prior distribution via bounds
      prior_hpd = c(lower = input$groupPrev1[1], upper = input$groupPrev1[2])
      # specify prior probability that disease is present
      prior_disease = input$presence_prob

      observations = c(npos = input$pos1, nsamples = input$samples1)
      
      # analyze prior distribution
      prior_params = beta_params(
        lower = prior_hpd['lower'], 
        upper = prior_hpd['upper']
      )
      
      # evaluate posterior distribution
      post = presence_prevalence_estimation(
        x = observations['npos'],
        n = observations['nsamples'],
        prior_prevalence = prior_params,
        prior_disease = prior_disease,
        sensitivity = input$sens1, # TODO: make user input
        specificity = input$spec1, # TODO: make user input
        prevalence_conf = .95, 
        prevalence_quantile = .95
      )
      
      graph_det <- data.frame(
        post = post$prob_disease,
        detect_data = input$pos1,
        post_freedom = post$prob_disease_freedom,
        pos = observations['npos'],
        samples = observations['nsamples']
      )
      
      
      graph_bounds <- data.frame(
        name = input$name1,
        prevalence_lower = input$groupPrev1[1],
        prevalence_upper = input$groupPrev1[2],
        prevalence_bound = post$prevalence_bound
      )
      
      detGraph_list <- list(graph_det = graph_det, graph_bounds = graph_bounds)
      
      promise(action = function(resolve, reject) {
        resolve(detGraph_list)
      })
      
      } else if(future_promise_data$n > 1 | future_promise_data$x > 1) {
      
      prior_presence = input$presence_prob
      
      # build sampled pop df
      nP <- future_promise_data$n
      
      
      group_name <- lapply(1:nP, function(i){
        input[[paste0("name",i)]]
      })
      prior_low <- lapply(1:nP, function(i){
        input[[paste0("groupPrev",i)]][1]
      })
      prior_upp <- lapply(1:nP, function(i){
        input[[paste0("groupPrev",i)]][2]
      })
      
      
        sampling_population_priors <- data.frame(
          name = unlist(group_name),
          prevalence_lower = as.numeric(unlist(prior_low)),
          prevalence_upper = as.numeric(unlist(prior_upp))
          
        )
        
      
      
      # build data df
      data_n <- future_promise_data$x
      
      
      data_name <- lapply(1:data_n, function(i){
        input[[paste0("pop",i)]]
      })
      samples_val <- lapply(1:data_n, function(i){
        input[[paste0("samples",i)]]
      })
      pos_val <- lapply(1:data_n, function(i){
        input[[paste0("pos",i)]]
      })
      sens_val <- lapply(1:data_n, function(i){
        input[[paste0("sens",i)]]
      })
      spec_val <- lapply(1:data_n, function(i){
        input[[paste0("spec",i)]]
      })
      
        data <- data.frame(
          population = unlist(data_name),
          sample_size = as.numeric(unlist(samples_val)),
          positive_tests = as.numeric(unlist(pos_val)),
          sensitivity = as.numeric(unlist(sens_val)),
          specificity = as.numeric(unlist(spec_val))
          
        )
        
      
      
      
      future_promise({
        
        source(file.path('utils','run_nimble.R'))
        source(file.path('utils','beta_params.R'))
        library(dplyr)
        
      samples <- run_nimble(sampling_population_priors= sampling_population_priors, data = data, prior_presence = prior_presence)
      # run nimble

      
      post_inds = unique(round(seq(
        from = nrow(samples) / 2, 
        to = nrow(samples), 
        length.out = 1e4
      )))
      
      m = mcmc(samples[post_inds,])
      
      #
      # posterior distribution output
      #
      graph_det <- data.frame(
        post = mean(m[,'presence'] == 1),
        detect_data = sum(unlist(as.numeric(pos_val))),
        post_freedom = mean(m[,'presence'] == 0),
        pos = sum(data$positive_tests),
        samples = sum(data$sample_size)
      )
      
      ## prevalence bounds instead of freedom
      # posterior quantile to use for prevalence bound
      prevalence_quantile = .95
      
      # model nodes containing prevalence samples
      prevalence_tgts = paste0('prevalence[', 1:nrow(sampling_population_priors), ']')
      
      prevalence_bound = apply(
        X = m[, prevalence_tgts, drop = FALSE],
        MARGIN = 2,
        FUN = quantile, 
        p = prevalence_quantile
      )
      
      bounds <- as.data.frame(prevalence_bound)
      bounds$ID <- seq.int(nrow(bounds))
      
      
      prior_info <- sampling_population_priors
      prior_info$ID <- seq.int(nrow(prior_info))
      
      
      graph_bounds <- merge(prior_info, bounds, by = 'ID')
      
      detGraph_list <- list(graph_det = graph_det, graph_bounds = graph_bounds)
      detGraph_list
      
      })
      
      }
      
    })
    
    #print(graph_det$post)
    
    output$freedom2 <- renderPlotly({
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det
          detect_data <- graph_det$detect_data
          
          plot <- graph_nimble_freedom_2(prior = input$presence_prob, p = detect_data, s = input$samples1, post = graph_det$post_freedom)
          plot <- ggplotly(plot)
          pl_out <- plotly_build(plot)
          pl_out$x$data[[1]]$hoverinfo = 'skip'
          pl_out$x$data[[2]]$text[1] = paste0(round(pl_out$x$data[[2]]$y[1]*100), '%')
          pl_out$x$data[[2]]$text[2] = paste0(round(pl_out$x$data[[2]]$y[2]*100), '%')
          pl_out$x$data[[3]]$text = paste0(round(pl_out$x$data[[3]]$y*100), '%')
          pl_out
        }
      )
      

    })
    
    # render graph when analyze button is clicked
    output$freedom <- renderPlotly({
      
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det
          detect_data <- graph_det$detect_data
          
      plot <- graph_nimble_freedom(prior = input$presence_prob, p = detect_data, s = input$samples1, post = graph_det$post)
      plot <- ggplotly(plot)
      pl_out <- plotly_build(plot)
      pl_out$x$data[[1]]$hoverinfo = 'skip'
      pl_out$x$data[[2]]$text[1] = paste0(round(pl_out$x$data[[2]]$y[1]*100), '%')
      pl_out$x$data[[2]]$text[2] = paste0(round(pl_out$x$data[[2]]$y[2]*100), '%')
      pl_out$x$data[[3]]$text = paste0(round(pl_out$x$data[[3]]$y*100), '%')
      pl_out
        })
      
    })
    
    output$freedom_bounds <- renderPlotly({
      req(input$plot_by_name)
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det
          
          graph_bounds <- detGraph_list$graph_bounds
          
      graph_bounds <- graph_bounds %>%
        filter(name == input$plot_by_name)
      
      freedom_bounds <- graph_prevalence_bounds(graph_bounds$prevalence_lower, graph_bounds$prevalence_upper, post_prevalence_bound = graph_bounds$prevalence_bound)
      plot <- ggplotly(freedom_bounds)
      pl_out <- plotly_build(plot)
      pl_out$x$data[[1]]$hoverinfo = 'skip'
      pl_out$x$data[[2]]$text[1] = paste0(round(pl_out$x$data[[2]]$y[1]*100), '%')
      pl_out$x$data[[2]]$text[2] = paste0(round(pl_out$x$data[[2]]$y[2]*100), '%')
      
      waiter_hide()
      pl_out
      
        })
      
    })
    
    # render value boxes when analyze button is clicked
    # value boxes
    output$prior <- renderText({
      prior = input$presence_prob*100
      
      paste(prior, "%")
    })
    
    output$data <- renderText({
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det
      data = graph_det$pos
      
      paste(data)
      
        })
    })
    
    output$data_sample <- renderText({
      
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det
      
      data2 = graph_det$samples
      
      paste0("Total positive samples out of ", data2, " samples collected")
        })
    })
    
    output$posterior <- renderText({
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det

      post <- graph_det$post_freedom*100

      paste(round(post), "%")
        })
      
    })
    output$posterior2 <- renderText({
      
      then(
        promise = detGraph_list(),
        onFulfilled = function(detGraph_list){
          
          graph_det <- detGraph_list$graph_det

      post2 <- graph_det$post*100
      
      post2 <- paste(round(post2), "%")
      
      paste0("Probability the sampling populations are free from disease, vs. ",
             post2, " probability that disease is present")
      
        })
      
    })
    
    
    
  
  ######################### Prevalence #################################
  

  #
  # open connection to power study results
  #
  
  power_plot_p <- reactive({
    
    conn = poolCheckout(pool_db)
    
    # example of pulling data:
    plot_var = 'prob_prevalence_hpd_covers'
    true_prevalence = input$true_prevalence_p
    prior_disease = input$prior_disease_p
    sensitivity = input$sensitivity_p
    specificity = input$specificity_p
    
    power_study_results = dbGetQuery(
      conn = conn,
      statement = paste(
        'select sample_size,', plot_var, 'from power_study_deduplicated',
        'where prior_prevalence = ? and prior_disease > ? and prior_disease < ?',
        'and sensitivity > ? and sensitivity < ? and specificity > ?',
        'and specificity < ? and true_prevalence > ? and true_prevalence < ?'
      ),
      params = list( # TODO: connect flagged items to user input
        input$prevPrior_p, # prior_prevalence  # TODO: connect to user input
        # account for numerical precision issues
        prior_disease - .005,
        prior_disease + .005,
        sensitivity - .005,
        sensitivity + .005,
        specificity - .005,
        specificity + .005,
        true_prevalence - .005,
        true_prevalence + .005
      )
    )%>% 
      arrange(sample_size)
    
    poolReturn(conn)
    
    
    sample_size_seq = seq(
      from = min(power_study_results$sample_size), 
      to = max(power_study_results$sample_size), 
      by = 5
    )
    
    df = data.frame(
      sample_size = sample_size_seq,
      value = power_study_interpolation(
        x = power_study_results$sample_size, 
        y = power_study_results[,2],
        x0 = sample_size_seq, 
        tol = .01
      )
    )
    colnames(df)[2] = plot_var
    
    
    df
    
  })
  
  output$power_table_p <- renderTable({
    df <- power_plot_p()
    
    df
  })
  
  output$descr_prev <- renderText({
    truePrev <- input$true_prevalence_p
    
    paste0("Accuracy: How often will studies with the stated assumptions have a 95% credible interval that is correct (i.e., covers the true prevalence) when ", truePrev,
           " is the actual prevalence in the population being studied?")

  })

  
  output$power_graph_p <- renderPlotly({
    waiter_show(
      color = 'rgba(217, 227, 241,.75)',
      html = tagList(
        div(
          spin_flowers(),
          style = paste(
            'margin:auto; position:relative; top:unset; left:unset;',
            'transform:unset'
          )
        ),
        br(),
        tagAppendAttributes(
          style = 'color:black',
          p('Visualizing inputs')
        )
      )
    )
    
    
    df <- power_plot_p()
    plot_var = 'prob_prevalence_hpd_covers'
    #label_plot <- label_plot_p()
    
    # disease freedom conclusions
    # disease freedom conclusions
    pl_results = df %>% 
      # plot layers
      ggplot(
        mapping = aes(
          x = sample_size, 
          y = .data[[plot_var]]
        )
      ) + 
      # interpolate across sample sizes
      geom_line()+ 
      # formatting
      xlab('Sample size') + 
      scale_y_continuous(
        name = 'Percent of studies', # TODO: make name change with plot_var
        labels = scales::percent, # TODO: make scales change with plot_var
        limits = c(0,1) # TODO: make limits change with plot_var
      ) + 
      theme_few() + 
      theme(
        panel.grid.major = element_line(colour = 'grey90')
      )
    
    
    # initialize plot
    pl_out = ggplotly(pl_results)
    
    # edit tooltips
    for(tooltip_ind in seq_along(pl_out$x$data[[1]]$text)) {
      # custom tooltip text
      tooltip = paste0(
        'Sample size: ', pl_out$x$data[[1]]$x[tooltip_ind], 
        '<br />',
        'Percent of time credible interval will cover true prevalence: ',
        paste0(round(pl_out$x$data[[1]]$y[tooltip_ind]*100), '%')
      )
      # save custom tooltip
      pl_out$x$data[[1]]$text[[tooltip_ind]] = tooltip
    }
    
    waiter_hide()
    
    # return final plot
    pl_out
    
    
  })
  
  power_plot_p_ci <- reactive({
    
    
    conn = poolCheckout(pool_db)
    
    # example of pulling data:
    plot_var = 'mean_prevalence_ci_width'
    true_prevalence = input$true_prevalence_p
    prior_disease = input$prior_disease_p
    sensitivity = input$sensitivity_p
    specificity = input$specificity_p
    
    power_study_results = dbGetQuery(
      conn = conn,
      statement = paste(
        'select sample_size,', plot_var, 'from power_study_deduplicated',
        'where prior_prevalence = ? and prior_disease > ? and prior_disease < ?',
        'and sensitivity > ? and sensitivity < ? and specificity > ?',
        'and specificity < ? and true_prevalence > ? and true_prevalence < ?'
      ),
      params = list( # TODO: connect flagged items to user input
        input$prevPrior_p, # prior_prevalence  # TODO: connect to user input
        # account for numerical precision issues
        prior_disease - .005,
        prior_disease + .005,
        sensitivity - .005,
        sensitivity + .005,
        specificity - .005,
        specificity + .005,
        true_prevalence - .005,
        true_prevalence + .005
      )
    )%>% 
      arrange(sample_size)
    
    poolReturn(conn)

    
    
    sample_size_seq = seq(
      from = min(power_study_results$sample_size), 
      to = max(power_study_results$sample_size), 
      by = 5
    )
    
    df = data.frame(
      sample_size = sample_size_seq,
      value = power_study_interpolation(
        x = power_study_results$sample_size, 
        y = power_study_results[,2],
        x0 = sample_size_seq, 
        tol = .01
      )
    )
    colnames(df)[2] = plot_var
    

    
    df
    
  })
  
  
  output$ci_graph_p <- renderPlotly({

    df <- power_plot_p_ci()
    plot_var = 'mean_prevalence_ci_width'
    #label_plot <- label_plot_p()

    pl_results = df %>% 
      # plot layers
      ggplot(
        mapping = aes(
          x = sample_size, 
          y = .data[[plot_var]]
          #text = paste("Freedom conclusion probability:",scales::percent(.data[[plot_var]]))
        )
      ) + 
      # interpolate across sample sizes
      geom_line()+ 
      # formatting
      xlab('Sample size') + 
      scale_y_continuous(
        name = 'Average credible interval width (precision)', # TODO: make name change with plot_var
        labels = scales::percent, # TODO: make scales change with plot_var
        limits = c(0,1) # TODO: make limits change with plot_var
      ) + 
      theme_few() + 
      theme(
        panel.grid.major = element_line(colour = 'grey90')
      )
    
    # initialize plot
    pl_out = ggplotly(pl_results)
    
    # edit tooltips
    for(tooltip_ind in seq_along(pl_out$x$data[[1]]$text)) {
      # custom tooltip text
      tooltip = paste0(
        'Sample size: ', pl_out$x$data[[1]]$x[tooltip_ind], 
        '<br />',
        'Average width of credible interval: ',
        paste0(round(pl_out$x$data[[1]]$y[tooltip_ind]*100), '%')
      )
      # save custom tooltip
      pl_out$x$data[[1]]$text[[tooltip_ind]] = tooltip
    }
    
    # return final plot
    pl_out
    
    
  })
  
  ########### activity ###################
  
  
  # dynamic ui rows for pop
  popRow_prev <- reactiveValues(n=1)
  
  observeEvent(input$add_prev, {popRow_prev$n <- popRow_prev$n +1})
  
  observeEvent(input$rmv_prev,{
    if(popRow_prev$n > 0){popRow_prev$n <- popRow_prev$n -1}
  })
  
  output$addInputs_prev <- renderUI({
    n <- popRow_prev$n
    selectInputs <- lapply(1:n, function(i) {
      fluidRow(
        column(width = 6, textInput(paste0("name_prev",i), label = "What is the name of the sampling stratum?", value = paste0("Stratum ", i))),
        column(width = 6, sliderInput(paste0("groupPrev_prev",i), "What range of prevalence is possible for this sampling stratum?", min = 0, max = 1, value = c(0, 0.5))))
    })
    do.call(tagList, selectInputs)
  })

  # dynamic ui rows for data
  dataRow_prev <- reactiveValues(n=1)
  
  observeEvent(input$addData_prev, {dataRow_prev$n <- dataRow_prev$n +1})
  
  observeEvent(input$rmvData_prev,{
    if(dataRow_prev$n > 0){dataRow_prev$n <- dataRow_prev$n -1}
  })
  
  output$dataInputs_prev <- renderUI({
    n <- dataRow_prev$n
    x <- popRow_prev$n
    name_val_prev <- lapply(1:x, function(i){
      input[[paste0("name_prev",i)]]
    })
    selectInputs <- lapply(1:n, function(i) {
      card(
      fluidRow(
        column(width = 8, selectInput(paste0("pop_prev", i), label = "What ecological process is this data informing?", choices = c(unlist(name_val_prev))))),
      fluidRow(
        column(width = 6, numericInput(paste0("samples_prev", i), label = "Number of samples",60)),
        column(width = 6, numericInput(paste0("pos_prev", i), label = "Number of positives",0))),
      fluidRow(
        column(width = 4, textInput(paste0("xx_prev",i), label = "Assay type:", value = paste0("Data type ",i))),
        column(width = 4, sliderInput(paste0("sens_prev",i),label = "Sensitivity:", min = 0, max = 1, value = c(0.95))),
        column(width = 4, sliderInput(paste0("spec_prev",i,":"),label = "Specificity:", min = 0, max = 1, value = c(1)))))
    })
    do.call(tagList, selectInputs)
  })
  
  # create dfs for dynamic flow chart
  pop_df_prev <- reactive({
    req(input$name_prev1)
    n <- popRow_prev$n
    name_val_prev <- lapply(1:n, function(i){
      input[[paste0("name_prev",i)]]
    })
    
    for (i in 1:n){
      population_prev <- data.frame(
        Disease = "Disease Presence",
        Population = unlist(name_val_prev)
      )
    }
    
    population_prev
    
    
  })
  
  
  
  data_df_prev <- reactive({
    n <- dataRow_prev$n
    num <- lapply(1:n, function(i){
      paste0("Data",i)
    })
    data_val_prev <- lapply(1:n, function(i){
      input[[paste0("xx_prev",i)]]
    })
    pop_val_prev <- lapply(1:n, function(i){
      input[[paste0("pop_prev",i)]]
    })
    
    samples_val_prev <- lapply(1:n, function(i){
      input[[paste0("samples_prev",i)]]
    })
    pos_val_prev <- lapply(1:n, function(i){
      input[[paste0("pos_prev",i)]]
    })
    
    for (i in 1:n){
      data_prev <- data.frame(
        Disease = "Disease Presence",
        Population = unlist(pop_val_prev),
        DataType = unlist(data_val_prev),
        Name = unlist(num),
        Samples = as.numeric(unlist(samples_val_prev)),
        Positive = as.numeric(unlist(pos_val_prev))
      )
    }
    
    
    data_prev <- data_prev %>%
      mutate(prop = round(Positive/Samples, 2))
    
    data_prev <- data_prev %>%
      mutate(Data = paste0("Data: ", prop))
    
  })
  
  
  
  ## flow chart
  output$prev_diagram <- renderGrViz({
    pop <- pop_df_prev()
    
    for (row in 1:length(pop$Population)){
      rows = paste0("'", pop$Population, "'")
      subs1 = paste0("'", pop$Population, "'[label = '", pop$Population, "']")
    }
    
    test = paste(rows, collapse = " ")
    subs3 <- paste(subs1, collapse = " ")
    
    data <- data_df_prev()
    
    for (row1 in 1:length(data$Population)){
      rows1 = paste0("'", data$Population, "'->'", data$Name,"'")
      subs = paste0("'", data$Name, "'[label = '", data$DataType, "']")
    }
    
    test2 <- paste( rows1, collapse = " ")
    subs2 <- paste(subs, collapse = " ")
    
    grViz(paste0("digraph {
    graph [layout = dot, rankdir = LR]
    node [shape = rectangle, style = filled, fillcolor = Linen]
    bgcolor = '#d9e3f1'
    
    subgraph cluster_0 {
      graph[shape = rectangle]
      style = rounded
      bgcolor = '#3090C7'
      
      label = 'Disease Status'
      node[shape = rectangle, fillcolor = '#AFDCEC', margin = 0.25]
      'Disease Presence'[label = 'Disease Presence']
    }
    
    subgraph cluster_1 {
      graph[shape = rectangle]
      style = rounded
      bgcolor = '#3EA055'
      
      label = 'Sampling Strata'
      node[shape = rectangle, fillcolor = '#DBF9DB', margin = 0.25]
      ",subs3,"
    }
    
    subgraph cluster_2 {
      graph[shape = rectangle]
      style = rounded
      bgcolor = '#4863A0'
      
      label = 'Data'
      node[shape = rectangle, fillcolor = '#C9DFEC', margin = 0.25]
      ",subs2,"
    }
    
    'Disease Presence' -> {",test,"}
    ",test2,"
    
    
  }"))
    
  })
  

  
  observeEvent(input$run_prev, {
    x <- popRow_prev$n
    name_val_prev <- lapply(1:x, function(i){
      input[[paste0("name_prev",i)]]
    })
    #req(data())
    updateSelectInput(
      session,
      "plot_by_pop",
      choices = c(unlist(name_val_prev)),
      selected = input$name_prev1
    )
  })

  # observe event button click to analyze & render output
  prevGraph_list <- eventReactive(input$run_prev, {
    
    waiter_show(
      color = 'rgba(217, 227, 241,.75)',
      html = tagList(
        div(
          spin_flowers(),
          style = paste(
            'margin:auto; position:relative; top:unset; left:unset;',
            'transform:unset'
          )
        ),
        br(),
        tagAppendAttributes(
          style = 'color:black',
          p('Analyzing data')
        )
      )
    )
    

    
    future_promise_data <- list(n = popRow_prev$n,
                                x = dataRow_prev$n)
    
    
    if(future_promise_data$n == 1 & future_promise_data$x == 1) {
      
      #   # specify prior distribution via bounds
      prior_hpd = c(lower = input$groupPrev_prev1[1], upper = input$groupPrev_prev1[2])
      #   # specify prior probability that disease is present
      prior_disease = input$presence_prob_prev
      #   
      observations = c(npos = input$pos_prev1, nsamples = input$samples_prev1)
      #   
      #   # analyze prior distribution
      prior_params = beta_params(
        lower = prior_hpd['lower'], 
        upper = prior_hpd['upper']
      )
      #   
      #   # evaluate posterior distribution
      post = presence_prevalence_estimation(
        x = observations['npos'],
        n = observations['nsamples'],
        prior_prevalence = prior_params,
        prior_disease = prior_disease,
        sensitivity = input$sens_prev1, # TODO: make user input
        specificity = input$spec_prev1, # TODO: make user input
        prevalence_conf = .95, 
        prevalence_quantile = .95
      )
      
      prior_mean = prior_params[1] / sum(prior_params)
      post_hpd = c(
        lower = post$prevalence_hpd_lower,
        upper = post$prevalence_hpd_upper
      )
      post_mean = post$prevalence_mean
      #   # extract posterior components
      # post_freedom = post$prob_disease_freedom
      #post_prevalence_bound = post$prevalence_bound
      #   
      #   ## need for graph: prior_hpd, post_hpd, prior_mean, post_mean, observations['npos'], observations['nsamples']
      #   
      prev_df <- data.frame(
        name = input$name_prev1,
        prevalence_lower = input$groupPrev_prev1[1],
        prevalence_upper = input$groupPrev_prev1[2],
        prior_mean = prior_mean,
        post_mean = post$prevalence_mean,
        prevalence_hpd_lower = post$prevalence_hpd_lower,
        prevalence_hpd_upper = post$prevalence_hpd_upper
        
      )
      #   
      prior_data <- data.frame(
        population = input$name_prev1,
        sample_size = observations['nsamples'],
        positive_tests = observations['npos']
      )
      #   
      prior_data <- prior_data %>%
        group_by(population) %>%
        nest()
      #   
      prevGraph_list <- list(prev_df = prev_df, prior_data = prior_data)
      
      
      ## simple promise
      
      promise(action = function(resolve, reject) {
        resolve(prevGraph_list)
      })
      
    } else if(future_promise_data$n > 1 | future_promise_data$x > 1) {
      
      prior_presence = input$presence_prob_prev
      
      # build sampled pop df
      nP <- future_promise_data$n
      
      
      group_name <- lapply(1:nP, function(i){
        input[[paste0("name_prev",i)]]
      })
      prior_low <- lapply(1:nP, function(i){
        input[[paste0("groupPrev_prev",i)]][1]
      })
      prior_upp <- lapply(1:nP, function(i){
        input[[paste0("groupPrev_prev",i)]][2]
      })
      
        sampling_population_priors <- data.frame(
          name = unlist(group_name),
          prevalence_lower = as.numeric(unlist(prior_low)),
          prevalence_upper = as.numeric(unlist(prior_upp))
          
        )
        
      
      # build data df
      data_n <- future_promise_data$x
      
      
      data_name <- lapply(1:data_n, function(i){
        input[[paste0("pop_prev",i)]]
      })
      samples_val <- lapply(1:data_n, function(i){
        input[[paste0("samples_prev",i)]]
      })
      pos_val <- lapply(1:data_n, function(i){
        input[[paste0("pos_prev",i)]]
      })
      sens_val <- lapply(1:data_n, function(i){
        input[[paste0("sens_prev",i)]]
      })
      spec_val <- lapply(1:data_n, function(i){
        input[[paste0("spec_prev",i)]]
      })
      
      
      data <- data.frame(
        population = unlist(data_name),
        sample_size = as.numeric(unlist(samples_val)),
        positive_tests = as.numeric(unlist(pos_val)),
        sensitivity = as.numeric(unlist(sens_val)),
        specificity = as.numeric(unlist(spec_val))
        
      )
      
      future_promise({
        
        source(file.path('utils','run_nimble.R'))
        source(file.path('utils','beta_params.R'))
        library(dplyr)
        
        # run nimble
        samples <- run_nimble(sampling_population_priors= sampling_population_priors, data = data, prior_presence = prior_presence)
        # run nimble
        # browser()
        
        post_inds = unique(round(seq(
          from = nrow(samples) / 2,
          to = nrow(samples),
          length.out = 1e4
        )))
        
        
        #m = mcmc(samples[post_inds,])
        # posterior distribution output
        #
        
        
        # posterior distribution output
        #
        #browser()
        tgt_nodes = paste('prevalence[', 1:nrow(sampling_population_priors), ']', sep = '')
        #m = mcmc(samples[post_inds, tgt_nodes])
        
        m2 = mcmc(samples[post_inds,])
        #post_freedom = mean(m == 0)
        
        #prevalence_mean = colMeans(m[, prevalence_tgts, drop = FALSE])
        #prevalence_tgts = paste0('prevalence[', 1:pkg$constants$n_populations, ']')
        means <- colMeans(m2[, tgt_nodes, drop = FALSE])
        means <- as.data.frame(means)
        means$names <- rownames(means)
        
        interval <- HPDinterval(m2[, tgt_nodes, drop = FALSE])
        interval <- as.data.frame(interval)
        interval$names <- rownames(interval)
        
        post <- merge(means, interval, by = 'names')
        
        colnames(post) <- c("val", "post_mean", "prevalence_hpd_lower", "prevalence_hpd_upper")
        
        post$ID <- seq.int(nrow(post))
        
        prior_data <- data %>%
          select(-c(sensitivity,specificity)) %>%
          group_by(population) %>%
          nest()
        
        prior_info <- sampling_population_priors
        prior_info$ID <- seq.int(nrow(prior_info))
        
        
        prev_df <- merge(prior_info, post, by = 'ID')
        
        prev_df <- prev_df %>%
          select(-c(ID,val))
        
        prevGraph_list <- list(prev_df = prev_df, prior_data = prior_data)
        prevGraph_list
      })
      
    }
    
    
  
    
  })
  
  output$prevGraph <- renderPlotly({
    req(input$plot_by_pop)
    
    then(
      promise = prevGraph_list(),
      onFulfilled = function(prevGraph_list){
        
        plotPrev <- prevGraph_list$prev_df %>%
          filter(name == input$plot_by_pop)
        

          plotData <- prevGraph_list$prior_data %>%
            filter(population == input$plot_by_pop)
        


        prevGraph <- graph_prevalence_estimation(lower_hpd = plotPrev$prevalence_lower, upper_hpd = plotPrev$prevalence_upper, data = plotData, post = plotPrev$post_mean, prevalence_hpd_lower= plotPrev$prevalence_hpd_lower,
                                                 prevalence_hpd_upper = plotPrev$prevalence_hpd_upper)
        
        plot <- ggplotly(prevGraph)
        pl_out <- plotly_build(plot)
        pl_out$x$data[[1]]$hoverinfo = 'skip'
        pl_out$x$data[[2]]$text[1] = paste0(round(pl_out$x$data[[2]]$y[1]*100), '%')
      pl_out$x$data[[2]]$text[2] = paste0(round(pl_out$x$data[[2]]$y[2]*100), '%')
      
      if(!is.null(pl_out$x$data[[3]]$text)){
        
        pl_out$x$data[[3]]$text = paste0(round(pl_out$x$data[[3]]$y*100), '%')}
      waiter_hide()
      pl_out
      }
    )
    
 
    

  }) 
  
  # render value boxes when analyze button is clicked
  # value boxes
  output$prior_p <- renderText({
    req(input$plot_by_pop)
    
    then(
      promise = prevGraph_list(),
        onFulfilled = function(prevGraph_list){

        plotPrev <- prevGraph_list$prev_df %>%
          filter(name == input$plot_by_pop)
        
        
        low = plotPrev$prevalence_lower*100
        high = plotPrev$prevalence_upper*100
        
        #prior = prior_mean*100
        
        paste(low, "% - ",high, "%" )
        
      }
    )
    
  })
  
  output$data_p <- renderText({

    req(input$plot_by_pop)
    then(
      promise = prevGraph_list(),
      onFulfilled = function(prevGraph_list){
        data <- prevGraph_list$prior_data %>%
          filter(population == input$plot_by_pop) %>%
          unnest(cols = c(data))
        
        data = data$positive_tests/data$sample_size*100
        
        
        paste(round(data), "%")
      }
    )
    
    
  })
  output$posterior_p <- renderText({
    
    req(input$plot_by_pop)
    then(
      promise = prevGraph_list(),
      onFulfilled = function(prevGraph_list){
        plotPrev <-prevGraph_list$prev_df %>%
          filter(name == input$plot_by_pop)
        
        paste(round(plotPrev$post_mean*100), "%")
      }
    )

    
    
  })
  
  output$posterior_p_bound <- renderText({
    req(input$plot_by_pop)
    
    then(
      promise = prevGraph_list(),
      onFulfilled = function(prevGraph_list){
        plotPrev <- prevGraph_list$prev_df %>%
          filter(name == input$plot_by_pop)
        
        low = plotPrev$prevalence_hpd_lower*100
        high = plotPrev$prevalence_hpd_upper*100
        
        
        
        paste0("Prevalence estimated for this sampling population (95% credible interval: ", round(low), "%-",
               round(high), "%)")
      }
    )
    
    
  })
  
  
##################### Seasonality ################################


  
  




  
  
##################### Epi Dynamics ###############################
  

  
  observeEvent(input$update,{
    
    waiter_show(
      color = 'rgba(217, 227, 241,.75)',
      html = tagList(
        div(
          spin_flowers(),
          style = paste(
            'margin:auto; position:relative; top:unset; left:unset;',
            'transform:unset'
          )
        ),
        br(),
        tagAppendAttributes(
          style = 'color:black',
          p('Updating data')
        )
      )
    )
    
    # epi parameters
    avg_infectious_days_input = input$infDays
    avg_recovery_days_input = input$recDays
    avg_waning_days_input = input$waneDays
    R0 = input$R0
    
    # TODO: add variation to reconfigure as standard SIR model (i.e., no B part)
    # TODO: add variation to reconfigure as standard SIBR model (i.e., no waning)
    
    # initial conditions at t=0
    initial_susceptible_members = input$susc
    initial_infectious_members = input$inf
    initial_broadly_recovered_members = input$broadRec
    initial_recovered_members = input$Rec
    
    # TODO: Make user input (i.e., so infection can start in spring vs winter, etc.)
    # reference date for outbreak start
    outbreak_start_date = input$start
    
    # plotting window
    plot_min_date = outbreak_start_date
    plot_max_date = outbreak_start_date + duration(num = 3, units = 'years')
    
    # plotting window (months following outbreak start)
    #plot_min_month = input$plotMonth[1]
    #plot_max_month = input$plotMonth[2]
    
    # diagnostic parameters
    pathogenic_sensitivity = input$path_sens
    pathogenic_specificity = input$path_spec
    antibody_sensitivity = input$ant_sens
    antibody_specificity = input$ant_spec
    
    birth_pulse_dates = data.frame(
      time = input$pulse,
      # proportion of total population that are new, susceptible animals after pulse
      proportion = input$propPulse
    )

    #
    # solve model
    #
    
    # initial proportions at t=0
    total_population = initial_susceptible_members + initial_infectious_members + initial_broadly_recovered_members + initial_recovered_members
    initial_susceptible = initial_susceptible_members / total_population
    initial_infectious = initial_infectious_members / total_population
    initial_broadly_recovered = initial_broadly_recovered_members / total_population
    initial_recovered = initial_recovered_members / total_population
    
    # threshold below which there are no infectious individuals in the population
    minimum_infectious_proportion = 1 / total_population
    
    # translate to average days in class
    avg_infectious_days = avg_infectious_days_input
    avg_recovery_days = avg_recovery_days_input - avg_infectious_days
    avg_waning_days = avg_waning_days_input - avg_recovery_days
    
    # convert birth pulse dates to model times (negatives are ok)
    birth_pulses = birth_pulse_dates %>% 
      mutate(
        time = as.numeric(
          difftime(time1 = time, time2 = outbreak_start_date, units = 'days')
        )
      )
    
    # clean up workspace
    rm(sibr_solution)
    
    # define model
    sibr_equations <- function(time, variables, parameters) {
      with(as.list(c(variables, parameters)), {
        ds = -beta * s * i + psi * r
        di =  beta * s * i - gamma * i
        db =  gamma * i - eta * b
        dr =  eta * b - psi * r
        return(list(c(ds, di, db, dr)))
      })
    }
    
    # parameterize model
    gamma = 1 / avg_infectious_days
    parameter_values = c(
      beta = R0 * gamma,
      gamma = gamma, 
      eta = 1 / avg_recovery_days,
      psi = 1 / avg_waning_days
    )
    
    # format initial conditions
    initial_values = c(
      s = initial_susceptible,
      i = initial_infectious,
      b = initial_broadly_recovered,
      r = initial_recovered
    )
    
    # normalize initial conditions
    initial_values = initial_values / sum(initial_values)
    
    # set ode solver times, making sure we pass t=0 to solver
    plot_min = as.numeric(
      difftime(time1 = plot_min_date, time2 = outbreak_start_date, units = 'days')
    )
    plot_max = as.numeric(
      difftime(time1 = plot_max_date, time2 = outbreak_start_date, units = 'days')
    )
    
    # set times to solve for inclusion in plot
    plot_times = seq(from = plot_min, to = plot_max, length.out = 500)
    
    # solve ode in chunks, following birth pulses
    if(!is.null(birth_pulses)) {
      if(any(birth_pulses$time < plot_max)) {
        
        # enumerate time points with birth pulses
        homogeneous_period_ends = rbind(
          # birth pulse data
          do.call(rbind, apply(
            X = birth_pulses, 
            MARGIN = 1, 
            FUN = function(r) {
              data.frame(
                time = seq(from = r['time'], to = plot_max, by = 365),
                proportion = unname(r['proportion'])
              )
            }
          )),
          # final timepoint
          data.frame(time = plot_max, proportion = 0)
        ) %>% 
          # make sure times are after model start
          filter(time >= 0) %>% 
          # merge duplicates
          group_by(time) %>% 
          summarise(proportion = sum(proportion)) %>% 
          ungroup()
        
        # initialize ode solution
        sibr_solution = data.frame(time = 0, t(initial_values))
        
        # solve ode in chunks
        for(i in 1:nrow(homogeneous_period_ends)) {
          # time range during period
          t = unique(c(
            tail(sibr_solution$time, 1),
            data.frame(t = plot_times) %>% 
              filter(
                tail(sibr_solution$time, 1) <= t, 
                t <= homogeneous_period_ends$time[i]
              ) %>% 
              unlist() %>% 
              unname(),
            homogeneous_period_ends$time[i]
          ))
          # evaluate ode, but skip saving the first row since it duplicates start
          incremental_solution = data.frame(
            ode(
              y = initial_values,
              times = t,
              func = sibr_equations,
              parms = parameter_values
            )[-1,]
          )
          # re-solve, as needed if disease leaves population
          if(any(incremental_solution$i < minimum_infectious_proportion)) {
            # identify when infection ends (i.e., 0 infected individuals)
            infection_end_index = min(which(
              incremental_solution$i < minimum_infectious_proportion
            ))
            # formally remove infection
            incremental_solution$i[infection_end_index] = 0
            mass = sum(
              incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')]
            )
            incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')] = 
              incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')] / 
              mass
            # re-solve sibr equations for post-disease state
            resolve_inds = infection_end_index:nrow(incremental_solution)
            incremental_solution[resolve_inds, ] = data.frame(
              ode(
                y = unlist(
                  incremental_solution[infection_end_index, c('s', 'i', 'b', 'r')]
                ),
                times = incremental_solution$time[resolve_inds],
                func = sibr_equations,
                parms = parameter_values
              )
            )
          }
          # append solution
          sibr_solution = rbind(
            sibr_solution,
            incremental_solution
          )
          # apply birth pulse to last observation
          sibr_solution[nrow(sibr_solution), c('s','i','b','r')] =
            sibr_solution[nrow(sibr_solution), c('s','i','b','r')] *
            (1 - homogeneous_period_ends$proportion[i]) +
            c(homogeneous_period_ends$proportion[i], rep(0, 3))
          # update initial conditions
          initial_values = c(
            s = tail(sibr_solution$s, 1),
            i = tail(sibr_solution$i, 1),
            b = tail(sibr_solution$b, 1),
            r = tail(sibr_solution$r, 1)
          )
          initial_values = initial_values / sum(initial_values)
        }
      }
    }
    
    # default: solve ode in one chunk
    if(!exists('sibr_solution')) {
      t = unique(c(0, plot_times))
      sibr_solution = data.frame(
        ode(
          y = initial_values,
          times = t,
          func = sibr_equations,
          parms = parameter_values
        )
      )
      # re-solve, as needed if disease leaves population
      if(any(sibr_solution$i < minimum_infectious_proportion)) {
        # identify when infection ends (i.e., 0 infected individuals)
        infection_end_index = min(which(
          sibr_solution$i < minimum_infectious_proportion
        ))
        # formally 0-out infection
        sibr_solution$i[infection_end_index] = 0
        mass = sum(
          sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')]
        )
        sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')] = 
          sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')] / 
          mass
        # re-solve sibr equations for post-disease state
        resolve_inds = infection_end_index:nrow(sibr_solution)
        sibr_solution[resolve_inds, ] = data.frame(
          ode(
            y = unlist(
              sibr_solution[infection_end_index, c('s', 'i', 'b', 'r')]
            ),
            times = sibr_solution$time[resolve_inds],
            func = sibr_equations,
            parms = parameter_values
          )
        )
      }
    }
    
    # format plotting frame
    sibr_solution = sibr_solution %>% 
      # restrict output to the requested plotting times
      filter(plot_min <= time, time <= plot_max) %>% 
      # enrich output
      mutate(
        # get date scale correct
        time = outbreak_start_date + duration(time, 'days'),
        # translate compartment model to ideal test results
        pathogenic_detection = (i + b) * pathogenic_sensitivity + 
          (s + r) * (1 - pathogenic_specificity),
        antibody_detection = (b + r) * antibody_sensitivity + 
          (s + i) * (1 - antibody_specificity)
      )

    
    output$sibr <- renderPlotly({
      req(sibr_solution)
      
      # visualize epidemic trajectory
      pl_trajectory = ggplot(
        data = sibr_solution %>% 
          pivot_longer(
            cols = c('s', 'i', 'b', 'r'), 
            names_to = 'compartment',
            values_to = 'population_proportion'
          ) %>% 
          mutate(
            compartment = factor(
              x = compartment,
              levels = c('s', 'i', 'b', 'r'),
              labels = c('Susceptible', 'Infectious', 'Broadly recovered', 
                         'Fully recovered')
            )
          ),
        mapping = aes(
          x = time, y = population_proportion, col = compartment)
      ) +
        geom_line() + 
        scale_x_datetime(date_labels = '%D') +
        scale_y_continuous(
          'Percent of population',
          labels = scales::percent,
          limits = c(0,1)
        ) + 
        scale_color_brewer(type = 'qual', palette = 'Dark2') + 
        theme_few() + 
        theme(
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = 'grey95'),
          axis.title.x = element_blank()
        )
      
      pl_out <- ggplotly(pl_trajectory)
      for(tooltip_ind in seq_along(pl_out$x$data[[1]]$text)) {
        # custom tooltip text
        tooltip = paste0(
          'Time: ', paste0(format(as.POSIXct(x = pl_out$x$data[[1]]$x[tooltip_ind], origin = '1970-01-01 00:00.00', tz = 'UTC'), format = '%Y-%m-%d')), 
          '<br />',
          'Percent of population: ',
          paste0(round(pl_out$x$data[[1]]$y[tooltip_ind]*100), '%'),
          '<br />',
          'Compartment: Susceptible'
        )
        # save custom tooltip
        pl_out$x$data[[1]]$text[[tooltip_ind]] = tooltip
      }
      for(tooltip_ind in seq_along(pl_out$x$data[[2]]$text)) {
        # custom tooltip text
        tooltip = paste0(
          'Time: ', paste0(format(as.POSIXct(x = pl_out$x$data[[2]]$x[tooltip_ind], origin = '1970-01-01 00:00.00', tz = 'UTC'), format = '%Y-%m-%d')), 
          '<br />',
          'Percent of population: ',
          paste0(round(pl_out$x$data[[2]]$y[tooltip_ind]*100), '%'),
          '<br />',
          'Compartment: Infectious'
        )
        # save custom tooltip
        pl_out$x$data[[2]]$text[[tooltip_ind]] = tooltip
      }
      for(tooltip_ind in seq_along(pl_out$x$data[[3]]$text)) {
        # custom tooltip text
        tooltip = paste0(
          'Time: ', paste0(format(as.POSIXct(x = pl_out$x$data[[3]]$x[tooltip_ind], origin = '1970-01-01 00:00.00', tz = 'UTC'), format = '%Y-%m-%d')), 
          '<br />',
          'Percent of population: ',
          paste0(round(pl_out$x$data[[3]]$y[tooltip_ind]*100), '%'),
          '<br />',
          'Compartment: Broadly recovered'
        )
        # save custom tooltip
        pl_out$x$data[[3]]$text[[tooltip_ind]] = tooltip
      }
      for(tooltip_ind in seq_along(pl_out$x$data[[4]]$text)) {
        # custom tooltip text
        tooltip = paste0(
          'Time: ', paste0(format(as.POSIXct(x = pl_out$x$data[[4]]$x[tooltip_ind], origin = '1970-01-01 00:00.00', tz = 'UTC'), format = '%Y-%m-%d')), 
          '<br />',
          'Percent of population: ',
          paste0(round(pl_out$x$data[[4]]$y[tooltip_ind]*100), '%'),
          '<br />',
          'Compartment: Fully recovered'
        )
        # save custom tooltip
        pl_out$x$data[[4]]$text[[tooltip_ind]] = tooltip
      }
      pl_out$x$layout$legend$title$text = 'Compartment'
      pl_out
      
      # TODO: dynamic x-axis plot scale
      # ggarrange(
      #   pl_trajectory, pl_test_positivity,
      #   ncol = 1,
      #   align = 'v'
      # )
      
  #     geom_GeomDrawGrob() has yet to be implemented in plotly.
  #     If you'd like to see this geom implemented,
  # Please open an issue with your example code at
  # https://github.com/ropensci/plotly/issues

      # TODO plot separately and use plotly
      # TODO some switching of tables to clean up
      
    })
    
    output$sibr2 <- renderPlotly({
      req(sibr_solution)
      
      # visualize marginal test rates
      # visualize marginal test rates
      pl_test_positivity = ggplot(
        data = sibr_solution %>% 
          select(
            time, pathogenic_detection, antibody_detection
          ) %>% 
          pivot_longer(
            cols = c('pathogenic_detection', 'antibody_detection'),
            names_to = 'detection_type',
            values_to = 'positivity_rate'
          ) %>% 
          mutate(
            detection_type = factor(
              x = detection_type,
              levels = c('pathogenic_detection', 'antibody_detection'),
              labels = c('Positive pathogen tests', 'Positive antibody tests')
            )
          ),
        mapping = aes(x = time, y = positivity_rate, col = detection_type)
      ) + 
        geom_line() + 
        scale_color_brewer(
          type = 'qual', palette = 'Dark2'
        ) + 
        scale_x_datetime(date_labels = '%D') +
        scale_y_continuous(
          'Expected proportion of tests',
          labels = scales::percent,
          limits = c(0,1)
        ) + 
        theme_few() + 
        theme(
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = 'grey95'),
          panel.grid.minor.x = element_line(colour = 'grey95'),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)
        )
      
      pl_out <- ggplotly(pl_test_positivity)
      for(tooltip_ind in seq_along(pl_out$x$data[[1]]$text)) {
        # custom tooltip text
        tooltip = paste0(
          'Time: ', paste0(format(as.POSIXct(x = pl_out$x$data[[1]]$x[tooltip_ind], origin = '1970-01-01 00:00.00', tz = 'UTC'), format = '%Y-%m-%d')), 
          '<br />',
          'Proportion of tests: ',
          paste0(round(pl_out$x$data[[1]]$y[tooltip_ind]*100), '%'),
          '<br />',
          'Detection type: Positive pathogen tests'
        )
        # save custom tooltip
        pl_out$x$data[[1]]$text[[tooltip_ind]] = tooltip
      }
      for(tooltip_ind in seq_along(pl_out$x$data[[2]]$text)) {
        # custom tooltip text
        tooltip = paste0(
          'Time: ', paste0(format(as.POSIXct(x = pl_out$x$data[[2]]$x[tooltip_ind], origin = '1970-01-01 00:00.00', tz = 'UTC'), format = '%Y-%m-%d')), 
          '<br />',
          'Proportion of tests: ',
          paste0(round(pl_out$x$data[[2]]$y[tooltip_ind]*100), '%'),
          '<br />',
          'Detection type: Positive antibody tests'
        )
        # save custom tooltip
        pl_out$x$data[[2]]$text[[tooltip_ind]] = tooltip
      }
      pl_out$x$layout$legend$title$text = 'Detection type'
      
      pl_out
    })
    
    waiter_hide()
    
    

    
  })

  
  
  
###################### Spatial deleted ##################################
  
 

  
  
  
  
    
}

shinyApp(ui, server)

