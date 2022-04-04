#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(CAMPSITE)
library(plotly)

source("R/theme.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = campsite_theme,
  
  ## App title
  titlePanel(fluidRow(column(width = 2, tags$img(src = "TUOS_PRIMARY_LOGO_LINEAR_BLACK.png", width = "100%")),
  column(width = 10, h1("Opposing effects of competition and selection on macroevolutionary dynamics", style="margin-top: 0;"), style="display: flex; align-items: center;")),
windowTitle= "Opposing effects of competition and selection on macroevolutionary dynamics"),

  ## Sidebar layout with input and output definitions
  sidebarLayout(
    
    ## Sidebar panel for inputs
    sidebarPanel(
      
      helpText("Select the strength of competition and selection:"),
      numericInput(
        "competition",
        "Competition value:",
        0,
        min = 0,
        max = 0.1,
        step = 0.005,
        width = NULL
      ),
      
      numericInput(
        "selection",
        "Selection value:",
        0,
        min = 0,
        max = 0.1,
        step = 0.005,
        width = NULL
      ),
      actionButton("sim_button", "Simulate!"),
      br(),
      helpText("Repeat simulations using same competion and selection values to explore replicate variability. \n Changing any of the values resets replicates"),
      br(),
      br(),
      downloadButton("report", "Generate report"),
      width = 3
      
      
    ),
    
    ## Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Traits at tips", plotlyOutput("trait_tip_plot")),
        tabPanel("Trait disparity through time", plotlyOutput("trait_disp_plot")),
        tabPanel("MNND through time", plotlyOutput("mnnd_plot")),
        tabPanel("Diversification rates", plotlyOutput("div_plot"))
      ),
      width = 9) 
  ),
  
  fluidRow(
    column(2,
           h5("Example extant phylogeny"),
           plotOutput("extant_phyloplot")),
    column(2,
           h5("Example full phylogeny"),
           plotOutput("full_phyloplot")),
    column(3,
           h5("Richness through time"),
           plotlyOutput("ltt")),
    column(5,
           h5("Example trait evolution"),
           plotOutput("traitplot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  v <- reactiveValues(sim = NULL, 
                      sim_reps = NULL, 
                      summary =  NULL,
                      #competition = input$competition,
                      #selection = input$selection,
                      replicate = 1)
  
  resetReplicate <- eventReactive(c(input$competition, 
                                    input$selection), {
      v$replicate <- 1
      v$sim <- NULL
      v$summary_reps <- NULL
      v$summary <-  NULL
    })
  
  observeEvent(input$sim_button, {
    resetReplicate()
    
    withProgress(message = 'Simulating Trait evolution!', value = 0, {
    v$sim <-  CAMPSITE::cs_simulate(pars = cs_pars(alpha1 = input$competition, 
                                                      alpha2 = input$competition,
                                                      alpha3 = input$selection), 
                                       ou = list(opt = NULL, alpha4 = NULL),
                                       root.value = 0, age.max = 20, 
                                       age.ext = NULL, step_size = 0.01, bounds = c(-Inf, Inf), 
                                       plot = FALSE, ylims = NULL, full_results = TRUE,
                                    show_shiny_i = TRUE) 
    })
    
    v$sim$replicate <- v$replicate
    v$summary <- CAMPSITE::cs_summarise_results(v$sim)
    v$summary_reps <- c(v$summary_reps, list(v$summary))
    v$replicate <- v$replicate + 1
    
    
    
    # if(any(input$competition == v$competion))
    
  })
  
  ## Simulate and plot a tree and save it in output$treeplot
  output$trait_tip_plot <- renderPlotly({
    plotly::ggplotly(
    CAMPSITE::plot_tip_trait_distribution_replicates(req(v$summary_reps)))
    
  })
  
  output$trait_disp_plot <- renderPlotly({
    plotly::ggplotly(
    CAMPSITE::plot_var_vs_time_replicates(req(v$summary_reps), variable = "VAR", 
                               ylab = "Trait disparity through time"))
  })
  
  output$mnnd_plot <- renderPlotly({
    plotly::ggplotly(
    CAMPSITE::plot_var_vs_time_replicates(req(v$summary_reps), variable = "MNND", 
                               ylab = "MNND through time"))
  })
  
  output$div_plot <- renderPlotly({
    plotly::ggplotly(
    CAMPSITE::plot_diversification(req(v$summary)))
  })
  
  
  output$extant_phyloplot <- renderPlot({
    plot(req(v$sim)$trees$gsp_extant$tree, edge.width = 2, node.pos = 1,
         show.tip.label = FALSE,  no.margin = TRUE)
  })
  
  output$full_phyloplot <- renderPlot({
    plot(req(v$sim)$trees$all$tree, edge.width = 2, node.pos = 1,
         show.tip.label = FALSE,  no.margin = TRUE)
  })
  
  output$ltt <- renderPlotly({
    plotly::ggplotly(
    plot_lineages_through_time_replicates(req(v$summary_reps)))
    
  })
  
  output$traitplot <- renderPlot({
    
    plot(req(v$sim), incipient_col = harrypotter::hp(n = 6, option = "Ravenclaw")[6])
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("comp", input$competition, 
                     "selec",   input$selection, "report.pdf", sep = "-"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      #params <- list(#summary_reps = v$summary_reps,
                     #sim = v$sim, 
        #             competition = input$competition,
         #            selection = input$selection)
      #
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file
      )
    }
  )
}
  


# Run the application 
shinyApp(ui = ui, server = server)
