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
                titlePanel(fluidRow(column(width = 2, 
                                           tags$img(src = "TUOS_PRIMARY_LOGO_LINEAR_BLACK.png", width = "100%")),
                                    column(width = 10, 
                                           h1("Opposing effects of competition and selection on macroevolutionary dynamics", 
                                              style="margin-top: 0;"),
                                           style = "display: flex; align-items: center;")
                ),
                windowTitle= "Opposing effects of competition and selection on macroevolutionary dynamics"),
                
                h5("Explore the joint influence of competition and stabilising 
                         selection on rates of trait evolution, speciation completion, 
                         and extinction using the Competition And Multiple-Peak Selection 
                         Integrated Trait Evolution (CAMPSITE) model."),
                helpText("The app is associated with the work described in: ", 
                         em('"Slavenko & Thomas (2022), Opposing effects of 
                            competition and selection on macroevolutionary dynamics".'), 
                         "using associated R package ", 
                         a("CAMPSITE. ", 
                           href="https://github.com/annakrystalli/CAMPSITE", 
                           target="_blank"),
                         "Please consult the paper for more details on the model 
                         and analysis methods"),
                hr(),
                
                
                ## Sidebar layout with input and output definitions
                sidebarLayout(
                  
                  ## Sidebar panel for inputs
                  sidebarPanel(
                    
                    helpText("Select the strength of competition and selection:"),
                    sliderInput(
                      "competition",
                      "Competition value:",
                      0,
                      min = 0,
                      max = 0.1,
                      step = 0.005,
                      round = -4,
                      width = NULL
                    ),
                    
                    sliderInput(
                      "selection",
                      "Selection value:",
                      0,
                      min = 0,
                      max = 0.1,
                      step = 0.005,
                      width = NULL
                    ),
                    fluidRow(
                      column(7,
                             actionButton("sim_button", "Simulate!", icon = icon("code-branch")),
                      ),
                      column(5,
                             actionButton("reset_button", "Reset", class = "btn-primary", icon = icon("rotate-right"))
                      )
                    ),
                    br(),
                    helpText("Repeat simulations using same competion and selection 
                             values to explore replicate variability. \n 
                             Changing any of the values resets replicates"),
                    br(),
                    br(),
                    downloadButton("report", "Generate report"),
                    width = 3
                    
                    
                  ),
                  
                  ## Main panel for displaying outputs
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Traits at tips", 
                               actionButton("ttpInfo", "Details about this plot",
                                            icon = icon("info-circle"),
                                            class="btn btn-light"),
                               plotlyOutput("trait_tip_plot")),
                      tabPanel("Trait disparity through time", 
                               actionButton("tdttInfo", "Details about this plot",
                                            icon = icon("info-circle"),
                                            class="btn btn-light"),
                               plotlyOutput("trait_disp_plot")),
                      tabPanel("MNND through time", 
                               actionButton("mnndInfo", "Details about this plot",
                                            icon = icon("info-circle"),
                                            class="btn btn-light"),
                               plotlyOutput("mnnd_plot")),
                      tabPanel("Diversification rates", 
                               actionButton("divInfo", "Details about this plot",
                                            icon = icon("info-circle"),
                                            class="btn btn-light"),
                               plotlyOutput("div_plot"))
                    ),
                    width = 9) 
                ),
                
                fluidRow(
                  column(2,
                         h5("Example extant phylogeny",
                            actionButton("phyexInfo", "",
                                         icon = icon("info-circle"),
                                         class="btn btn-light")),
                         plotOutput("extant_phyloplot")),
                  column(2,
                         h5("Example full phylogeny",
                            actionButton("phyfullInfo", "",
                                         icon = icon("info-circle"),
                                         class="btn btn-light")),
                         plotOutput("full_phyloplot")),
                  column(4,
                         h5("Richness through time",
                            actionButton("richInfo", "",
                                         icon = icon("info-circle"),
                                         class="btn btn-light")),
                         plotlyOutput("ltt")),
                  column(4,
                         h5("Example trait evolution",
                            actionButton("trevInfo", "",
                                         icon = icon("info-circle"),
                                         class="btn btn-light")),
                         plotOutput("traitplot"))
                ),
                br(),
                fluidRow(
                  hr(),
                  helpText("The code for this app is published at", 
                           a("10.15131/shef.data.XXXXX", 
                             href="https://doi.org/10.15131/shef.data.XXXXX", 
                             target="_blank")),
                  helpText("The code repository can be found on GitHub at ", 
                           a("github.com/annakrystalli/campsite-shiny", 
                             href="https://github.com/annakrystalli/campsite-shiny", 
                             target="_blank")),
                  br(),
                  helpText("This project was funded by a Royal Society grant 
                           RGF/EA/181082 and a Royal Society University Research 
                           fellowship to Gavin H. Thomas (UF120016, URF/R/180006).")
                  
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  v <- reactiveValues(sim = NULL, 
                      sim_reps = NULL, 
                      summary =  NULL,
                      replicate = 1)
  
  resetReplicate <- eventReactive(
    c(input$competition, 
      input$selection), {
        # reset reactive values
        v$replicate <- 1
        v$sim <- NULL
        v$summary_reps <- NULL
        v$summary <-  NULL
        
      })
  
  observeEvent(input$reset_button, {
    
    # reset reactive values
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
      CAMPSITE::plot_diversification_replicates(req(v$summary)))
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
    
    plot(req(v$sim), incipient_col = harrypotter::hp(n = 6, option = "Ravenclaw")[1])
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
  
  # ---- Modal Info Boxes ------------------------------------------------------
  observeEvent(input$ttpInfo, {
    showModal(modalDialog(
      title = "Trait at Tips",
      p("Distributions of trait values of extant lineages at the tips of the simulated phylogenies."),
      p("The dashed red lines represent θ = 10, the optimum trait value set for simulations with selection terms."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tdttInfo, {
    showModal(modalDialog(
      title = "Trait disparity through time",
      p("Trait disparity (variance of traits of extant lineages at each time step) through time under the CAMPSITE model."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$mnndInfo, {
    showModal(modalDialog(
      title = "MNND through time",
      p("Mean nearest neighbour distances (MNND) of trait values of extant lineages through time."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$divInfo, {
    showModal(modalDialog(
      title = "Diversification through time",
      p("Rates of realised per-branch speciation (measured as the number of speciation 
        completion events in each time step divided by the sum of branch lengths in that time step), 
        extinction (measured as the number of extinction events in each time step 
        divided by the sum of branch lengths in that time step) and net diversification 
        through time (measured as realised speciation minus realised extinction at each time step)."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$phyexInfo, {
    showModal(modalDialog(
      title = "Example extant phylogeny",
      p("Phylogeny of extant lineages only (incipient and extinct excluded) in the last simulated replicate."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$phyfullInfo, {
    showModal(modalDialog(
      title = "Example full phylogeny",
      p("Full phylogeny including all lineages (extant, incipient and extinct) in the last simulated replicate."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$richInfo, {
    showModal(modalDialog(
      title = "Species richness through time",
      p("The number of extant completed species at each time step in the last simulated replicate."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$trevInfo, {
    showModal(modalDialog(
      title = "Example of trait evolution",
      p("Trait and phylogeny evolution through time under the CAMPSITE model of the last simulated replicate."),
      p("Incipient stages of evolution are highlighted in orange."),
      easyClose = TRUE
    ))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
