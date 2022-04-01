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


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  ## App title
  titlePanel("Macroevolution Workshop"), ## Note this comma here!
  ## This is like in a normal function
  ## to separate argument. You'll easily
  ## forget it so remember to always check
  ## it for debugging your app!
  
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
      actionButton("sim_button", "Simulate!")
      
      
    ),
    
    ## Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Traits at tips", plotOutput("trait_tip_plot")),
        tabPanel("Trait disparity through time", plotOutput("trait_disp_plot")),
        tabPanel("MNND through time", plotOutput("mnnd_plot")),
        tabPanel("Diversification rates", plotOutput("div_plot"))
      )
      
    ) 
  ),
  
  fluidRow(
    column(2,
           h4("Example extant phylogeny"),
           plotOutput("extant_phyloplot")),
    column(2,
           h4("Example full phylogeny"),
           plotOutput("full_phyloplot")),
    column(2,
           h4("Lineages through time"),
           plotOutput("ltt")),
    column(6,
           h4("Example trait evolution"),
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
  output$trait_tip_plot <- renderPlot({
    CAMPSITE::plot_tip_trait_distribution_replicates(req(v$summary_reps))
    
  })
  
  output$trait_disp_plot <- renderPlot({
    CAMPSITE::plot_var_vs_time_replicates(req(v$summary_reps), variable = "VAR", 
                               ylab = "Trait disparity through time")
  })
  
  output$mnnd_plot <- renderPlot({
    CAMPSITE::plot_var_vs_time_replicates(req(v$summary_reps), variable = "MNND", 
                               ylab = "MNND through time")
  })
  
  output$div_plot <- renderPlot({
    CAMPSITE::plot_diversification(req(v$summary))
  })
  
  
  output$extant_phyloplot <- renderPlot({
    plot(req(v$sim)$trees$gsp_extant$tree, edge.width = 2, node.pos = 1,
         show.tip.label = FALSE,  no.margin = TRUE)
  })
  
  output$full_phyloplot <- renderPlot({
    plot(req(v$sim)$trees$all$tree, edge.width = 2, node.pos = 1,
         show.tip.label = FALSE,  no.margin = TRUE)
  })
  
  output$ltt <- renderPlot({
    
    
  })
  
  output$traitplot <- renderPlot({
    
    plot(req(v$sim))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
