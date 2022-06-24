#' @title Launch SqueakR Dashboard
#'
#' @description Run the SqueakR Shiny Dashboard locally to interface with experimental
#' data.
#'
#' @return A Shiny Dashboard
#'
#' @examples \dontrun{squeakRDashboard()}
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom plotly plotlyOutput renderPlotly
#' @import RColorBrewer
#' @import report
#' @import rlist
#' @importFrom utils object.size capture.output
#' @export
squeakRDashboard <- function() {

ui <- dashboardPage(
  dashboardHeader(title = "SqueakR Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "main"),
      menuItem("Descriptive Statistics", tabName = "desc_stats"),
      menuItem("Metadata Distribution Plots", tabName = "meta_dist"),
      menuItem("Ethnogram Plots", tabName = "ethnograms"),
      menuItem("Density Plots", tabName = "densities"),
      menuItem("Supplemental Plots", tabName = "misc_graphs"),
      menuItem("Data Tables", tabName = "data_tables"),
      menuItem("3D Cluster Plots", tabName = "spa_clus"),
      menuItem("3D Surface Plots", tabName = "spa_surf"),
      menuItem("Group Difference Plots", tabName = "compare_groups"),
      menuItem("ANOVA Table", tabName = "anova_groups"),
      div(img(imageOutput("package_image")), style="text-align: center;")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                div(img(imageOutput("homepage_image")), style="text-align: center; height:330px;")
              ),
              fluidRow(
                box(title = "Upload your experiment here.",
                  fileInput("experiment_upload", label = "Browse for the appropriate .RData experiment below, and click Load Experiment:", multiple = FALSE, accept = ".RData", placeholder = "No experiment uploaded"),
                  actionButton(inputId="regen_plots","Load Experiment"), width = 12
                )
              ),
              fluidRow(
                infoBoxOutput("experiment_box"),
                infoBoxOutput("group_box"),
                infoBoxOutput("experimenter_box"),
                infoBoxOutput("saved_box"),
                infoBoxOutput("datasets_box"),
                infoBoxOutput("size_box")
              ),
              tags$a(href="https://osimon81.github.io/SqueakR", "Click here to learn more about the SqueakR package.")
      ),

      tabItem(tabName = "meta_dist",
              h2("Metadata Distribution"),
              fluidRow(
                box(
                  title = "Sunburst plots with animal distribution (left) and experimenter distribution (right) across groups",
                  width = 12),
                box(
                    column(
                    plotlyOutput("ani_dist"), width = 6),
                    column(
                      plotlyOutput("expt_dist"), width = 6), width = 12)
                ),
      ),

      tabItem(tabName = "data_tables",
              h2("Data Tables"),
              fluidRow(
                box(
                  title = "Select a data point to generate the original data table:",
                  selectInput("pickdata_tab",
                              label = "Data point",
                              choices = c("Load an experiment first."),
                              selected = 1), width = 12
                )
              ),
              fluidRow(
                div(style = 'overflow-x: scroll', tableOutput("table"))
              )
      ),

      tabItem(tabName = "spa_clus",
              h2("3D-Plotted Call Clusters"),
              fluidRow(
                box(
                  selectInput('pickdata_cluster',
                              label = "Data point to be graphed:",
                              choices = c("Load an experiment first."),
                              selected = 1))
              ),
              fluidRow(
                plotlyOutput('cluster_plot', height = "600px")
              )
      ),

      tabItem(tabName = "spa_surf",
              h2("3D-Plotted Call Surface"),
              fluidRow(
                box(
                  selectInput('pickdata_surface',
                              label = "Data point to be graphed:",
                              choices = c("Load an experiment first."),
                              selected = 1))
              ),
              fluidRow(
                plotlyOutput('surface_plot', height = "600px")
              )
      ),

      tabItem(tabName = "ethnograms",
              h2("Ethnogram-Style Plots"),
              fluidRow(
                box(
                  title = "Select data point to graph:",
                  selectInput("pickdata_eth",
                              label = "Data point",
                              choices = c("Load an experiment first."),
                              selected = 1), width = 12
                )
              ),
              fluidRow(
                box(plotOutput("eth", width = "100%")),
                box(plotOutput("eth_ton", width = "100%"))
              )
      ),
      tabItem(tabName = "densities",
              h2("Density-Style Plots"),
              fluidRow(
                box(
                  title = "Select data point to graph:",
                  selectInput("pickdata_den",
                              label = "Data point",
                              choices = c("Load an experiment first."),
                              selected = 1), width = 12
                )
              ),
              fluidRow(
                box(plotOutput("den_freq_sta", width = "100%")),
                box(plotOutput("den_freq_spl", width = "100%"))
              ),
              fluidRow(
                box(plotOutput("den_cust_sta", width = "100%")),
                box(plotOutput("den_cust_spl", width = "100%"))
              ),
              fluidRow(
                box(plotOutput("den_dura_sta", width = "100%")),
                box(plotOutput("den_dura_spl", width = "100%"))
              )
      ),
      tabItem(tabName = "misc_graphs",
              h2("Supplemental Plots"),
              fluidRow(
                box(
                  title = "Select data point to graph:",
                  selectInput("pickdata_misc",
                              label = "Data point",
                              choices = c("Load an experiment first."),
                              selected = 1), width = 12
                )
              ),
              fluidRow(
                box(plotOutput("delt_hist", width = "100%")),
                box(plotOutput("princ_box", width = "100%"))
              ),
              fluidRow(
                box(plotOutput("corr_plot"), width = 12)
              )
      ),
      tabItem(tabName = "compare_groups",
              h2("Analyze comparisons between groups"),
              fluidRow(
                box(
                  title = "Select factor to compare across experimental groups:",
                  selectInput("pickdata_factor",
                              label = "Choose a variable below:",
                              choices = c("Load an experiment first."),
                              selected = 1), width = 12
                )
              ),
              fluidRow(
                box(plotOutput("compare_groups", width = "100%"), width = 12)
              )
      ),
      tabItem(tabName = "desc_stats",
              h2("Descriptive Statistics for Experiment"),
              fluidRow(
                box(
                  title = "Summary statistics for experiment, grouped by experimental group:"),
                width = 12
              ),
              fluidRow(
                box(
                  shiny::htmlOutput("summary_stats"), width = 12
                )
              )
      ),
      tabItem(tabName = "anova_groups",
              h2("Analysis of Variance (ANOVA)"),
              fluidRow(
                box(
                  title = "Select factor to run ANOVA across experimental groups:",
                  selectInput("pickdata_anova",
                              label = "Choose a variable below:",
                              choices = c("Load an experiment first."),
                              selected = 1), width = 12
                )
              ),
              fluidRow(
                box(
                  column(align = "center", div(style = 'overflow-x: scroll', shiny::dataTableOutput("anova_groups")),
                  width = 12)
                  )
                )
      )
      )
  )
)



server <- function(input, output, session) {

  # Home Functions

  output$package_image <- renderImage({
    list(src = "man/dashboard_images/SqueakR.png",
         width = 120,
         height = 120,
         alt = "SqueakR Package Image")
  }, deleteFile = FALSE)

  output$homepage_image <- renderImage({
    list(src = "man/dashboard_images/SqueakR_badgeless.png",
         width = 350,
         height = 350,
         alt = "SqueakR Homepage Image")
  }, deleteFile = FALSE)

  observeEvent(input$regen_plots, {
    if (is.null(input$experiment_upload)) return(NULL)
    inFile <- input$experiment_upload
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    my_expt <- load(file, envir = e)
    experiment <- e[[my_expt]]

    # Info Boxes

    output$experiment_box <- renderInfoBox({
      infoBox("Experiment", experiment$name, icon = icon("microscope"), fill = TRUE)
    })
    output$group_box <- renderInfoBox({
      infoBox("Group(s)", paste(noquote(experiment$groups),collapse=', '), icon = icon("vials"), fill = TRUE, color = "green")
    })
    output$experimenter_box <- renderInfoBox({
      infoBox("Experimenter(s)", paste(noquote(experiment$experimenters),collapse=', '), icon = icon("book"), fill = TRUE, color = "yellow")
    })
    output$saved_box <- renderInfoBox({
      infoBox("Last Saved", format(experiment$last_saved, format="%B %d, %Y"), icon = icon("atom"), fill = FALSE)
    })
    output$datasets_box <- renderInfoBox({
      infoBox("Call Datasets", length(experiment$experimental_data), icon = icon("chart-line"), fill = FALSE, color = "green")
    })
    output$size_box <- renderInfoBox({
      infoBox("Experiment Size", format(object.size(experiment), units = "auto"), icon = icon("desktop"), fill = FALSE, color = "yellow")
    })

    # Metadata Distributions

    output$ani_dist <- renderPlotly({
      data <- plotSunburstAnimals(experiment)
      data
    })

    output$expt_dist <- renderPlotly({
      data <- plotSunburstExperimenters(experiment)
      data
    })

    # Select inputs

    updateSelectInput(session, "pickdata_tab", choices = c(1:length(experiment$experimental_data)), selected = 1)
    updateSelectInput(session, "pickdata_eth", choices = c(1:length(experiment$experimental_data)), selected = 1)
    updateSelectInput(session, "pickdata_den", choices = c(1:length(experiment$experimental_data)), selected = 1)
    updateSelectInput(session, "pickdata_misc", choices = c(1:length(experiment$experimental_data)), selected = 1)
    updateSelectInput(session, "pickdata_factor", choices = c("call_length", "delta_frequency",
                                                              "high_frequency", "low_frequency",
                                                              "peak_frequency", "power",
                                                              "principal_frequency", "sinuosity",
                                                              "slope", "tonality"),
                      selected = "call_length")

    these_names <- names(experiment$experimental_data[1]$call_data$raw)[7:length(names(experiment$experimental_data[1]$call_data$raw))]
    these_names <- trimws(gsub(r"{\s*\([^\)]+\)}","", these_names))
    these_names <- gsub(" ", "_", these_names, fixed = TRUE)

    updateSelectInput(session, "pickdata_anova", choices = these_names, selected = 1)
    updateSelectInput(session, "pickdata_cluster", choices = c(1:length(experiment$experimental_data)), selected = 1)
    updateSelectInput(session, "pickdata_surface", choices = c(1:length(experiment$experimental_data)), selected = 1)

    # 3D Plots

    output$cluster_plot <- renderPlotly({
      data <- plotClusters(experiment$experimental_data[as.numeric(input$pickdata_cluster)]$call_data$raw)
      data
    })

    output$surface_plot <- renderPlotly({
      data <- plotCallDataSurface(experiment$experimental_data[as.numeric(input$pickdata_surface)]$call_data$raw)
      data
    })


    # Data Tables

    output$table <- renderTable(experiment$experimental_data[as.numeric(input$pickdata_tab)]$call_data$raw)

    # Ethnogram Plots

    output$eth <- renderPlot({
      data <- plotEthnogram(experiment$experimental_data[as.numeric(input$pickdata_eth)]$call_data$raw)
      data
    })
    output$eth_ton <- renderPlot({
      data <- plotEthnogramSplitByTonality(experiment$experimental_data[as.numeric(input$pickdata_eth)]$call_data$raw)
      data
    })

    # Density Plots

    output$den_freq_sta <- renderPlot({
      data <- plotDensityStackedByFrequency(experiment$experimental_data[as.numeric(input$pickdata_den)]$call_data$raw)
      data
    })
    output$den_freq_spl <- renderPlot({
      data <- plotDensitySplitByFrequency(experiment$experimental_data[as.numeric(input$pickdata_den)]$call_data$raw)
      data
    })
    output$den_cust_sta <- renderPlot({
      data <- plotDensityStackedByCustom(experiment$experimental_data[as.numeric(input$pickdata_den)]$call_data$raw)
      data
    })
    output$den_cust_spl <- renderPlot({
      data <- plotDensitySplitByCustom(experiment$experimental_data[as.numeric(input$pickdata_den)]$call_data$raw)
      data
    })
    output$den_dura_sta <- renderPlot({
      data <- plotDensityStackedByDuration(experiment$experimental_data[as.numeric(input$pickdata_den)]$call_data$raw)
      data
    })
    output$den_dura_spl <- renderPlot({
      data <- plotDensitySplitByDuration(experiment$experimental_data[as.numeric(input$pickdata_den)]$call_data$raw)
      data
    })

    # Misc Graphs

    output$delt_hist <- renderPlot({
      data <- plotDeltaHistogram(experiment$experimental_data[as.numeric(input$pickdata_misc)]$call_data$raw)
      data
    })
    output$princ_box <- renderPlot({
      data <- plotPrincipalBoxplot(experiment$experimental_data[as.numeric(input$pickdata_misc)]$call_data$raw)
      data
    })
    output$corr_plot <- renderPlot({
      data <- plotCorrelations(experiment$experimental_data[as.numeric(input$pickdata_misc)]$call_data$raw)
      data
    })

    # Comparisons between groups

    output$compare_groups <- renderPlot({
      data <- analyze_factor(experiment = experiment, analysis_factor = input$pickdata_factor)
      data
    })

    output$anova_groups <- shiny::renderDataTable({
      squeakrANOVA(experiment = experiment, analysis_factor = input$pickdata_anova)
    })

    # Summary statistics

    output$summary_stats <- shiny::renderUI({
      HTML(
        paste(
          c("<pre>", capture.output(print(squeakrSummary(experiment = experiment))), "</pre>"),
          collapse = "<br>"
        )
      )
    })

  })

}

shinyApp(ui, server)

}
