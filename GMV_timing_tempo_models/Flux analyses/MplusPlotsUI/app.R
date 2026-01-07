# Mplus Plots User Interface, Shiny web application for viewing Mplus
# plots in an Mplus GH5 file.  Uses the rhdf5 package for loading the
# the GH5 file.
#
# Uses Mplus R functions in mplus.R.
#
# Version history:
#
# 2023-02-28 Initial release
# 2023-03-08 Add Shiny options to increase allowable upload file size.
#            Cleanup calls for Bayesian predictive scatterplots and distribution plots.
# 2023-11-14 Add Shiny options for Between-level histograms and scatterplots.
# 2024-05-07 Add Shiny options for Autoregressive curves.
#
# Written by: Thuy Nguyen
#             Muthen & Muthen
#



library(shiny)
#library(rhdf5)
library(DT)
library(data.table)
library(ggplot2)
source('mplus.R')

# Define UI for application that draws a histogram
ui <- fluidPage(id = "MainPage",

	# Application title
	titlePanel("Mplus Plots User Interface"),

	sidebarLayout(
		sidebarPanel(
		  helpText("Open an Mplus GH5 file from an Mplus run."),
		  uiOutput("fileInputSelection"),

			uiOutput("plotOptions"),
			uiOutput("subPlotOptions"),
			uiOutput("subPlotTypeOptions"),
			uiOutput("processOptions"),
			uiOutput("variableOptions"),
			uiOutput("variableYOptions"),
			uiOutput("groupOptions"),
			uiOutput("catOptionTypes"),
			uiOutput("catOption1"),
			uiOutput("catOption2"),
			uiOutput("binOptions"),
			uiOutput("idOption"),

			uiOutput('covariateSection'),

			uiOutput("displayOptions"),

		),

		mainPanel(
			hr(),

			tabsetPanel(id = "tabs",
				tabPanel("Plot", value = "plot",
						plotOutput("distPlot"),
						textOutput("textMessage")),
			),
		)
	)
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)

  myinputs <- reactiveValues(ofile = NULL, plotWidth = 500, decimals = 3)

  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  covariateModal <- function(failed = FALSE) {
    modalDialog(
      h5("Covariates for adjusted estimated means"),
      helpText("Double-click on a cell to change its value.  NA entries will be replaced by sample means in the computation."),
      dataTableOutput("covariatesdf"),
      options(htmlwidgets.TOJSON_ARGS = list(na = 'string')),
      actionButton(inputId = "resetX",label = "Reset covariates", options = list(na = 'string')),

      if (failed)
        div(tags$b("Invalid name of data object", style = "color: red;")),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("covariateOK", "OK")
      )
    )
  }

  ###############################################
  #
  # observeEvent statements
  #
  ###############################################

  # Reset each input variable when the ofile is changed and add an observeEvent for each input variable.
  observeEvent(input$userfile, {
    myinputs$ofile <- input$userfile$datapath
    myinputs$selectedProcess <- NULL
    myinputs$selectedPlot <- NULL
    #    myinputs$selectedSubPlot <- NULL
    myinputs$selectedVar <- NULL
    myinputs$selectedVarY <- NULL
    myinputs$cleardb <- NULL
    myinputs$binCount <- NULL
    myinputs$plotWidth <- input$plotWidth
    myinputs$selectedCat1 <- NULL
    myinputs$selectedCat2 <- NULL
    myinputs$selectedID <- NULL
    myinputs$selectedVarType <- NULL
    myinputs$selectedVarYType <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$ofile, {
    myinputs$ofile <- input$ofile
    myinputs$selectedProcess <- NULL
    myinputs$selectedPlot <- NULL
#    myinputs$selectedSubPlot <- NULL
    myinputs$selectedVar <- NULL
    myinputs$selectedVarY <- NULL
    myinputs$cleardb <- NULL
    myinputs$binCount <- NULL
    myinputs$plotWidth <- input$plotWidth
    myinputs$selectedCat1 <- NULL
    myinputs$selectedCat2 <- NULL
    myinputs$selectedID <- NULL
    myinputs$selectedVarType <- NULL
    myinputs$selectedVarYType <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedPlot, {
    myinputs$selectedPlot <- input$selectedPlot
    myinputs$plotWidth <- input$plotWidth
    myinputs$selectedVar <- NULL
    myinputs$selectedGroup <- NULL
    myinputs$selectedCat1 <- NULL
    myinputs$selectedCat2 <- NULL
    myinputs$selectedID <- NULL
    myinputs$cleardb <- NULL
    myinputs$resetX <- NULL
    myinputs$selectedVarType <- NULL
    myinputs$selectedVarYType <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedSubPlot, {
    myinputs$selectedSubPlot <- input$selectedSubPlot
    myinputs$numItems <- NULL
    myinputs$selectedVar <- NULL
    myinputs$binCount <- NULL
    myinputs$selectedCat1 <- NULL
    myinputs$selectedCat2 <- NULL
    myinputs$selectedID <- NULL
    myinputs$cleardb <- NULL
    myinputs$resetX <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedHistSubPlot, {
    myinputs$selectedHistSubPlot <- input$selectedHistSubPlot
    myinputs$numItems <- NULL
    #myinputs$selectedVar <- NULL
    myinputs$binCount <- NULL
    myinputs$selectedCat1 <- NULL
    myinputs$selectedCat2 <- NULL
    myinputs$selectedID <- NULL
    myinputs$cleardb <- NULL
    myinputs$resetX <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$plotWidth, {
    myinputs$plotWidth <- input$plotWidth
  })
  observeEvent(input$binCount, {
    myinputs$binCount <- input$binCount
  })
  observeEvent(input$selectedProcess, {
    myinputs$selectedProcess <- input$selectedProcess
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedVar, {
    myinputs$selectedVar <- input$selectedVar
    myinputs$selectedVarType <- NULL
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedVarY, {
    myinputs$selectedVarY <- input$selectedVarY
    myinputs$selectedVarYType <- NULL
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedID, {
    myinputs$selectedID <- input$selectedID
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedVarType, {
    myinputs$selectedVarType <- input$selectedVarType
  })
  observeEvent(input$selectedVarYType, {
    myinputs$selectedVarYType <- input$selectedVarYType
  })
  observeEvent(input$numItems, {
    myinputs$numItems <- input$numItems
    myinputs$selectedVar <- NULL
    myinputs$selectedVarY <- NULL
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedCat1, {
    myinputs$selectedCat1 <- input$selectedCat1
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedCat2, {
    myinputs$selectedCat2 <- input$selectedCat2
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$selectedGroup, {
    myinputs$selectedGroup <- input$selectedGroup
    myinputs$cleardb <- NULL
    myPlotData <- NULL
  })
  observeEvent(input$cleardb, {
    myinputs$cleardb <- NULL
    myPlotData <- NULL
    updateTabsetPanel(session, "tabs", selected = "plot")
  })
  observeEvent(input$showdb, {
    myinputs$cleardb <- 1
    updateTabsetPanel(session, "tabs", selected = "plot_data")
  })
  observeEvent(input$resetX, {
    myCovariateDF$Data <- NULL
  })
  observeEvent(input$covariatesdf_cell_edit, {
    if (input$covariatesdf_cell_edit$col > 0) {
      myinputs$row  <- input$covariatesdf_cell_edit$row
      myinputs$column <- input$covariatesdf_cell_edit$col
      myinputs$newvalue <- input$covariatesdf_cell_edit$value
      myCovariateDF$Data[myinputs$row,myinputs$column] <- as.numeric(myinputs$newvalue)
    }
  })
  observeEvent(input$numXsets, {
    myinputs$numXsets <- input$numXsets
    myCovariateDF$Data <- NULL
  })
  # Show modal when button is clicked.
  observeEvent(input$showCovariates, {
    myinputs$selectedVar <- NULL
    showModal(covariateModal())
  })
  observeEvent(input$covariateOK, {
    # Check that data object exists and is data frame.
    if (is.null(input$covariateDF)) {
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })


  ###############################################
  #
  # renderUI statements
  #
  ###############################################

  output$showDBbutton <- renderUI({
    if (input$tabs == "plot") {
      actionButton("showdb", "Show plot data")
    }
  })
  output$clearDBbutton <- renderUI({
    if (input$tabs == "plot_data") {
      actionButton("cleardb", "Clear plot data")
    }
  })


  output$covariateSection <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Adjusted estimated means" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      fluidPage(
        numericInput('numXsets',"Number of covariate sets",1),
        actionButton("showCovariates", "Show covariates dialog"),
        hr()
      )
    }
  })

  output$displayOptions <- renderUI({
    if (input$tabs == "plot_data") {
      sliderInput(inputId = "decimals",
                  label = "Number of decimals:",
                 min = 1,
                 max = 5,
                 value = 3)
    } else if (input$tabs == "plot") {
      sliderInput("plotWidth",
                  label = "Size of plot window (width)",
                  min = 300,
                  max = 1000,
                  value = 800)
    }
  })

  output$fileInputSelection <- renderUI({
    if (input$tabs == "plot") {
      fileInput("userfile",
        "Choose GH5 file",
        accept = "*.gh5",
        width = 800,
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      )
    }
  })
  ## Show the plots that are available.
  output$plotOptions <- renderUI({
    if (input$tabs == "plot") {
      if (!is.null(myinputs$ofile)) {
        plots <- mplus.get.available.plots(myinputs$ofile)
        selectInput("selectedPlot",
                    label = "Select a plot to view",
                    choices = plots,
                    width = 800,
                    selected = myinputs$selectedPlot) #plots[1])
      }
    }
  })

  output$processOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Dropout means" ||
               myinputs$selectedPlot == "Sample means" ||
               myinputs$selectedPlot == "Estimated means" ||
               myinputs$selectedPlot == "Sample and estimated means" ||
               myinputs$selectedPlot == "Estimated means, medians, modes, and percentiles" ||
               myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities" ||
               myinputs$selectedPlot == "Observed individual values" ||
               myinputs$selectedPlot == "Estimated individual values" ||
               myinputs$selectedPlot == "Estimated means and observed individual values" ||
               myinputs$selectedPlot == "Estimated means and estimated individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      nums <- mplus.get.num.processes(myinputs$ofile)
      if (nums > 1) {
        choicelist <- seq.int(1,nums)
        for (p in c(1:nums)) {
          append(choicelist, p)
        }
        selectInput("selectedProcess",
                    label = "Choose series to view",
                    choices = choicelist,
                    width = 500,
                    selected = 1)
      }
    } else {
      # show nothing
    }
  })
  output$subPlotTypeOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      plotSeries <- TRUE
    } else if (myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities") {
      radioButtons("plotSeries", "Plot for:",
                   c("Variable" = FALSE,
                     "Series" = TRUE),
                   selected = TRUE)
    } else if (myinputs$selectedPlot == "Item characteristic curves") {
      radioButtons("numItems", "Plot for:",
                   c("A single item" = 1,
                     "Multiple items" = 2,
                     "All items" = 0),
                   selected = 1)
    } else if (myinputs$selectedPlot == "Information curves") {
      radioButtons("numItems", "Select curve(s) to be plotted",
                   c("Item information curve(s) (IIC) for a single or multiple items" = 1,
                     "Partial information curve for multiple items" = 2,
                     "Total information (TIC) for all items" = 0),
                   width = 600,
                   selected = 1)
    } else if (myinputs$selectedPlot == "Survival curves") {
      if (is.null(myinputs$selectedSubPlot)) {

      } else if (myinputs$selectedSubPlot == "Estimated baseline survival curve" || myinputs$selectedSubPlot == "Estimated log cumulative baseline curve") {
        varlist <- mplus.list.survival.variables(myinputs$ofile, TRUE)
        if (length(varlist) > 1) {
          radioButtons("numItems", "Select what to plot:",
                       c("Single variable" = 1,
                         "Multiple variables, connected sequentially" = 2),
                       selected = 1)
        }
      }
    } else if (myinputs$selectedPlot == "Observed individual values" ||
               myinputs$selectedPlot == "Estimated individual values" ||
               myinputs$selectedPlot == "Estimated means and observed individual values" ||
               myinputs$selectedPlot == "Estimated means and estimated individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      if (is.null(myinputs$selectedSubPlot)) {

      } else if (myinputs$selectedSubPlot == "Individually-fitted curves") {
        radioButtons("numItems", "Select type of curve:",
                     c("Linear curve" = 1,
                       "Quadratic curve" = 2),
                     selected = 1)
      }
    }
  })
  output$subPlotOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if ((substr(myinputs$selectedPlot, 1, 10) == "Histograms") || (substr(myinputs$selectedPlot, 1, 24) == "Between-level histograms")) {
      choicelist <- (c("Standard histogram", "Histogram/density plot", "QQ plot"))
      selectInput("selectedHistSubPlot",
                  label = "Choose plot to view",
                  choices = choicelist,
                  width = 500,
                  selected = "Standard histogram")
    } else if (myinputs$selectedPlot == "Estimated means, medians, modes, and percentiles") {
      choicelist <- (c("Estimated means", "Estimated modes", "Estimated medians", "Estimated medians with percentiles"))
      selectInput("selectedSubPlot",
                  label = "Choose plot to view",
                  choices = choicelist,
                  width = 500,
                  selected = "Estimated means")
    } else if (myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities") {
      choicelist <- (c("Sample proportions", "Estimated probabilities", "Sample proportions and estimated probabilities"))
      selectInput("selectedSubPlot",
                  label = "Choose plot to view",
                  choices = choicelist,
                  width = 500,
                  selected = "Estimated probabilities")
    } else if (substr(myinputs$selectedPlot, 1, 17) == "Time series plots") {
      choicelist <- (c("Values for selected subject", "Means over subjects"))
      selectInput("selectedSubPlot",
                  label = "Select values to be plotted",
                  choices = choicelist,
                  width = 500,
                  selected = "Values for selected subject")
    } else if (myinputs$selectedPlot == "Survival curves") {
      choicelist <- (c("Kaplan-Meier curve", "Estimated baseline survival curve",
                       "Sample log cumulative hazard curve", "Estimated log cumulative baseline curve",
                       "Estimated baseline hazard curve",
                       "Kaplan-Meier curve with estimated baseline survival curve",
                       "Sample log cumulative hazard curve with estimated log cumulative baseline curve"))
      selectInput("selectedSubPlot",
                  label = "Choose plot to view",
                  choices = choicelist,
                  width = 500,
                  selected = "Kaplan-Meier curve")
    } else if (myinputs$selectedPlot == "Discrete survival curves") {
      choicelist <- (c("Kaplan-Meier curve", "Estimated baseline survival curve",
                       "Kaplan-Meier curve with estimated baseline survival curve"))
      selectInput("selectedSubPlot",
                  label = "Choose plot to view",
                  choices = choicelist,
                  width = 500,
                  selected = "Kaplan-Meier curve")
    } else if (myinputs$selectedPlot == "Measurement parameter plots") {
      choicelist <- (c("Measurement invariance (green invariance, red non-invariant)", "Histogram"))
      selectInput("selectedSubPlot",
                  label = "Select type of plot",
                  choices = choicelist,
                  width = 500,
                  selected = "Measurement invariance")
    } else if (myinputs$selectedPlot == "Observed individual values" ||
               myinputs$selectedPlot == "Estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values") {
      choicelist <- (c("Individual data", "Individually-fitted curves"))
      selectInput("selectedSubPlot",
                  label = "Select type of plot",
                  choices = choicelist,
                  width = 500,
                  selected = "Individual data")
    }
  })
  output$groupOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Item characteristic curves" || myinputs$selectedPlot == "Information curves") {
      choicelist <- mplus.get.model_group_labels(myinputs$ofile)
      selectInput("selectedGroup",
                  label = "Choose group/class to view",
                  choices = choicelist,
                  width = 500,
                  selected = choicelist[1])
    } else if (myinputs$selectedPlot == "Bayesian autocorrelation plots") {
      nchains <- mplusapp.get.bayesian.chains(myinputs$ofile)
      choicelist <- seq(1,nchains)
      selectInput("selectedGroup",
                  label = "Choose chain to view",
                  choices = choicelist,
                  width = 500,
                  selected = choicelist[1])
    } else if (myinputs$selectedPlot == "Survival curves" || myinputs$selectedPlot == "Discrete survival curves") {
      if (mplus.get.num.model_groups(myinputs$ofile) > 1) {
        choicelist <- mplus.get.model_group_labels(myinputs$ofile)
        choicelist <- append(choicelist, "All", 0)
        selectInput("selectedGroup",
                    label = "Choose group/class to view",
                    choices = choicelist,
                    width = 500,
                    selected = "All")
      }
    } else if (myinputs$selectedPlot == "Estimated means and observed individual values" ||
               myinputs$selectedPlot == "Estimated means and estimated individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      if (mplus.get.num.model_groups(myinputs$ofile) > 1) {
        choicelist <- mplus.get.model_group_labels(myinputs$ofile)
        choicelist <- append(choicelist, "All", 0)
        selectInput("selectedGroup",
                    label = "Choose group/class to view",
                    choices = choicelist,
                    width = 500,
                    selected = "All")
      }
    } else if (is.null(myinputs$selectedPlot)) {
      if (mplus.get.num.model_groups(myinputs$ofile) > 1) {
        choicelist <- mplus.get.model_group_labels(myinputs$ofile)
        choicelist <- append(choicelist, "All", 0)
        selectInput("selectedGroup",
                    label = "Choose group/class to view",
                    choices = choicelist,
                    width = 500,
                    selected = "All")
      }
    }
  })
  output$variableOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (substr(myinputs$selectedPlot, 1, 10) == "Histograms") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplus.list.variables(myinputs$ofile, TRUE)
        selectInput("selectedVar",
                    label = "Choose variable to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (substr(myinputs$selectedPlot, 1, 24) == "Between-level histograms") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplusapp.get.variable.list.between.plots(myinputs$ofile)
        selectInput("selectedVar",
                    label = "Choose variable to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (substr(myinputs$selectedPlot, 1, 12) == "Scatterplots") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplus.list.variables(myinputs$ofile, TRUE)
        selectInput("selectedVar",
                    label = "Choose variable (X) to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (substr(myinputs$selectedPlot, 1, 26) == "Between-level scatterplots") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplusapp.get.variable.list.between.plots(myinputs$ofile)
        selectInput("selectedVar",
                    label = "Choose variable (X) to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities") {
      if (is.null(input$plotSeries)) {

      } else if (!as.logical(input$plotSeries)) {
        if (mplusapp.has.individual_data(myinputs$ofile)) {
          variableList <- mplusapp.get.probability_variables(myinputs$ofile)
          selectInput("selectedVar",
                      label = "Choose variable to plot",
                      choices = variableList,
                      width = 500,
                      selected = variableList[1])
        }
      }
    } else if (myinputs$selectedPlot == "Item characteristic curves") {
      if (is.null(input$numItems)) {

      } else if (input$numItems == 1) {
        variableList <- mplusapp.get.irt_variables(myinputs$ofile)
        selectInput("selectedVar",
                    label = "Choose item to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      } else if (input$numItems == 2) {
        variableList <- mplusapp.get.irt_variables(myinputs$ofile)
        selectInput("selectedVar",
                    label = "Choose item to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1],
                    multiple = TRUE)
      }
    } else if (myinputs$selectedPlot == "Information curves") {
      if (is.null(input$numItems)) {

      } else if (input$numItems == 1 || input$numItems == 2) {
        variableList <- mplusapp.get.irt_variables(myinputs$ofile)
        selectInput("selectedVar",
                    label = "Choose item(s) to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1],
                    multiple = TRUE)
      }
    } else if (substr(myinputs$selectedPlot, 1, 17) == "Time series plots") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplus.list.timeseries.variables(myinputs$ofile, TRUE)
        selectInput("selectedVar",
                    label = "Choose variable to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (myinputs$selectedPlot == "Bayesian posterior parameter distributions" ||
               myinputs$selectedPlot == "Bayesian posterior parameter trace plots" ||
               myinputs$selectedPlot == "Bayesian autocorrelation plots" ||
               myinputs$selectedPlot == "Bayesian prior parameter distributions") {
      variableList <- mplus.list.bayesian.parameters(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Bayesian posterior predictive checking distribution plots" ||
               myinputs$selectedPlot == "Bayesian posterior predictive checking scatterplots") {
      variableList <- mplus.list.bayesian.predictive.labels(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose information to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Bootstrap distributions") {
      variableList <- mplus.list.bootstrap.parameters(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Autoregressive curves") {
      variableList <- mplus.list.autoregressive.curve.parameters(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Loop plots") {
      variableList <- mplus.list.loop.labels(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Moderation plots") {
      variableList <- mplus.list.moderation.labels(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Sensitivity plots") {
      variableList <- mplus.list.sensitivity.labels(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Survival curves" || myinputs$selectedPlot == "Discrete survival curves") {
      variableList <- mplus.list.survival.variables(myinputs$ofile, TRUE)
      if (is.null(myinputs$numItems) || myinputs$numItems == 1) {
        selectInput("selectedVar",
                    label = "Choose variable to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      } else {
        selectInput("selectedVar",
                    label = "Choose starting variable to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (myinputs$selectedPlot == "Measurement parameter plots") {
      variableList <- mplus.list.measurement.parameters(myinputs$ofile, TRUE)
      selectInput("selectedVar",
                  label = "Choose parameter to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    }
  })
  output$variableYOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (substr(myinputs$selectedPlot, 1, 12) == "Scatterplots") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplus.list.variables(myinputs$ofile, TRUE)
        selectInput("selectedVarY",
                    label = "Choose variable (Y) to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (substr(myinputs$selectedPlot, 1, 26) == "Between-level scatterplots") {
      if (mplusapp.has.individual_data(myinputs$ofile)) {
        variableList <- mplusapp.get.variable.list.between.plots(myinputs$ofile)
        selectInput("selectedVarY",
                    label = "Choose variable (Y) to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (myinputs$selectedPlot == "Survival curves") {
      variableList <- mplus.list.survival.variables(myinputs$ofile, TRUE)
      if (!is.null(myinputs$numItems) && myinputs$numItems == 2) {
        selectInput("selectedVarY",
                    label = "Choose ending variable to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[2])
      }
    }
  })
  output$binOptions <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (substr(myinputs$selectedPlot, 1, 10) == "Histograms" || substr(myinputs$selectedPlot, 1, 24) == "Between-level histograms") {
      if (is.null(myinputs$selectedSubPlot) || myinputs$selectedSubPlot != "QQ plot") {
        sliderInput("binCount", label = "Number of bins", min = 2, max = 50, value = 20)
      }
    } else if (myinputs$selectedPlot == "Bayesian posterior parameter distributions" ||
               myinputs$selectedPlot == "Bayesian prior parameter distributions" ||
               myinputs$selectedPlot == "Bayesian posterior predictive checking distribution plots" ||
               myinputs$selectedPlot == "Bootstrap distributions") {
      sliderInput("binCount", label = "Number of bins", min = 20, max = 500, value = 100)
    } else if (myinputs$selectedPlot == "Measurement parameter plots") {
      if (is.null(myinputs$selectedSubPlot)) {

      } else if (myinputs$selectedSubPlot == "Histogram") {
        sliderInput("binCount", label = "Number of bins", min = 2, max = 20, value = 10)
      }
    }
  })
  output$catOptionTypes <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Item characteristic curves") {
      if (is.null(input$numItems)) {

      } else if (input$numItems == 1) {
        radioButtons("catType", "Which category:",
                     c("Use all categories" = 0,
                       "Use specific category" = 1,
                       "Use sum of categories" = 2),
                     selected = 0)
      } else if (input$numItems == 0 || input$numItems == 2) {
        radioButtons("catType", "Which category:",
                     c("Use specific category" = 1,
                       "Use sum of categories" = 2),
                     selected = 1)
      }
    }
  })
  output$catOption1 <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Item characteristic curves") {
      if (is.null(input$numItems)) {

      } else if (input$numItems == 1) {
        if (is.null(input$catType) || is.null(myinputs$selectedVar)) {

        } else if (input$catType == 1) {
          numcat <- mplusapp.get.irt_categories(myinputs$ofile, myinputs$selectedVar)
          categories <- c(1:numcat)
          catlen <- length(categories)
          selectInput("selectedCat1",
                      label = "Choose category",
                      choices = categories,
                      width = 200,
                      selected = categories[catlen])
        } else if (input$catType == 2) {
          numcat <- mplusapp.get.irt_categories(myinputs$ofile, myinputs$selectedVar)
          categories <- c(1:numcat)
          tagList(
            selectInput("selectedCat1",
                        label = "Choose first category x",
                        choices = categories,
                        width = 200,
                        selected = categories[1]),
            selectInput("selectedCat2",
                        label = "Choose second category x",
                        choices = categories,
                        width = 200,
                        selected = categories[numcat])
          )
        }
      } else if (input$numItems == 0 || input$numItems == 2) {
        if (is.null(input$catType)) {

        } else if (input$catType == 1) {
          numcat <- mplusapp.get.irt_categories(myinputs$ofile)
          categories <- c(1:numcat)
          catlen <- length(categories)
          selectInput("selectedCat1",
                      label = "Choose category",
                      choices = categories,
                      width = 200,
                      selected = categories[catlen])
        } else if (input$catType == 2) {
          numcat <- mplusapp.get.irt_categories(myinputs$ofile)
          categories <- c(1:numcat)
          tagList(
          selectInput("selectedCat1",
                      label = "Choose first category",
                      choices = categories,
                      width = 200,
                      selected = categories[1]),
          selectInput("selectedCat2",
                      label = "Choose second category",
                      choices = categories,
                      width = 200,
                      selected = categories[numcat])
          )
        }
      }
    } else if (myinputs$selectedPlot == "Observed individual values" ||
               myinputs$selectedPlot == "Estimated individual values") {
      sliderInput("selectedCat1", label = "Number of curves", min = 1, max = 20, value = 10)
    } else if (myinputs$selectedPlot == "Histograms (sample values, estimated values, residuals)" ||
               myinputs$selectedPlot == "Histograms (sample values, estimated factor scores, estimated values, residuals)" ||
               myinputs$selectedPlot == "Histograms (sample values, estimated factor scores, outliers, estimated values, residuals)") {
      if (is.null(myinputs$selectedVar)) {
        selectedVar <- 1
      } else {
        selectedVar <- myinputs$selectedVar
      }
      if (mplusapp.variable.has.estimated.values(myinputs$ofile,selectedVar)) {
        choicelist <- c("Sample values", "Estimated values", "Residuals")
        selectInput("selectedVarType",
                    label = "Choose values to plot",
                    choices = choicelist,
                    width = 500,
                    selected = choicelist[1])
      }
    } else if (myinputs$selectedPlot == "Scatterplots (sample values, estimated values, residuals)" ||
               myinputs$selectedPlot == "Scatterplots (sample values, estimated factor scores, estimated values, residuals)" ||
               myinputs$selectedPlot == "Scatterplots (sample values, estimated factor scores, outliers, estimated values, residuals)") {
      if (is.null(myinputs$selectedVar)) {
        selectedVar <- 1
      } else {
        selectedVar <- myinputs$selectedVar
      }
      if (mplusapp.variable.has.estimated.values(myinputs$ofile,selectedVar)) {
        choicelist <- c("Sample values", "Estimated values", "Residuals")
        selectInput("selectedVarType",
                    label = "Choose values (X) to plot",
                    choices = choicelist,
                    width = 500,
                    selected = choicelist[1])
      }
    }
  })
  output$catOption2 <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Item characteristic curves") {
      if (is.null(input$numItems)) {

      } else if (input$numItems == 1) {
        if (is.null(input$catType) || is.null(myinputs$selectedVar)) {

        } else if (input$catType == 2) {
          numcat <- mplusapp.get.irt_categories(myinputs$ofile, myinputs$selectedVar)
          categories <- c(1:numcat)
          selectInput("selectedCat2",
                      label = "Choose second category",
                      choices = categories,
                      width = 200,
                      selected = categories[numcat])
        }
      } else if (input$numItems == 0 || input$numItems == 2) {
        if (is.null(input$catType)) {

        } else if (input$catType == 2) {
          numcat <- mplusapp.get.irt_categories(myinputs$ofile)
          categories <- c(1:numcat)
          selectInput("selectedCat2",
                      label = "Choose second category",
                      choices = categories,
                      width = 200,
                      selected = categories[numcat])
        }
      }
    } else if (myinputs$selectedPlot == "Scatterplots (sample values, estimated values, residuals)" ||
               myinputs$selectedPlot == "Scatterplots (sample values, estimated factor scores, estimated values, residuals)" ||
               myinputs$selectedPlot == "Scatterplots (sample values, estimated factor scores, outliers, estimated values, residuals)") {
      if (is.null(myinputs$selectedVarY)) {
        selectedVarY <- 1
      } else {
        selectedVarY <- myinputs$selectedVarY
      }
      if (mplusapp.variable.has.estimated.values(myinputs$ofile,selectedVarY)) {
        choicelist <- c("Sample values", "Estimated values", "Residuals")
        selectInput("selectedVarYType",
                    label = "Choose values (Y) to plot",
                    choices = choicelist,
                    width = 500,
                    selected = choicelist[1])
      }
    }
  })
  output$idOption <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (substr(myinputs$selectedPlot, 1, 17) == "Time series plots") {
      if (is.null(myinputs$selectedSubPlot) || myinputs$selectedSubPlot == "Values for selected subject") {
        variableList <- mplus.list.timeseries.idnums(myinputs$ofile, TRUE)
        selectInput("selectedID",
                    label = "Choose subject to plot",
                    choices = variableList,
                    width = 500,
                    selected = variableList[1])
      }
    } else if (myinputs$selectedPlot == "Time interval plots") {
      variableList <- mplus.list.timeseries.idnums(myinputs$ofile, TRUE)
      selectInput("selectedID",
                  label = "Choose subject to plot",
                  choices = variableList,
                  width = 500,
                  selected = variableList[1])
    } else if (myinputs$selectedPlot == "Observed individual values" ||
               myinputs$selectedPlot == "Estimated individual values") {
      datasize <- mplusapp.get.individual.data.size(file)
      numericInput("selectedID", "Starting observation number", value=1, min=1, max=datasize)
    }
  })
  output$covariateVars <- renderUI({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Adjusted estimated means" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      if (is.null(myCovariateDF)) {
        sliderInput("selectedTest", label = "Number of x", min = 2, max = 50, value = 20)
      } else {
        names <- rownames(myCovariateDF)
        selectInput("selectedX",
                    label = "Choose covariate",
                    choices = names,
                    width = 200,
                    selected = names[1])
      }
    }
  })

  ###############################################
  #
  # Render the plot
  #
  ###############################################
  myPlotData <- reactiveVal()
  myCovariateDF <- reactiveValues()

  # CovariatePlots <- reactive({
  #   if (myinputs$selectedPlot == "Adjusted estimated means") {
  #   } else if (!is.null(myinputs$row) && !is.null(myinputs$column) && !is.null(myinputs$newvalue)) {
  #     myCovariateDF$Data[myinputs$row,myinputs$column] <- as.numeric(myinputs$newvalue)
  #     mplus.plot.adjusted_estimated_means(myinputs$ofile, myCovariateDF$Data)
  #   }
  # })
  output$distPlot <- renderPlot({
    myPlotData.temp <- NULL
    myPlotData(myPlotData.temp)
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (substr(myinputs$selectedPlot, 1, 10) == "Histograms") {
      if (!mplusapp.has.individual_data(myinputs$ofile)) {
        # show nothing
      } else {
        if (is.null(myinputs$selectedVar)) {
          selectedVar <- 1
        } else {
          selectedVar <- as.character(myinputs$selectedVar)
        }
        if (is.null(myinputs$selectedVarType)) {
          isEstimated <- FALSE
          isResidual <- FALSE
        } else if (myinputs$selectedVarType == "Sample values") {
          isEstimated <- FALSE
          isResidual <- FALSE
        } else if (myinputs$selectedVarType == "Estimated values") {
          isEstimated <- TRUE
          isResidual <- FALSE
        } else if (myinputs$selectedVarType == "Residuals") {
          isEstimated <- FALSE
          isResidual <- TRUE
        }
        if (is.null(myinputs$binCount)) {
          binCount <- 20
        } else {
          binCount <- as.numeric(myinputs$binCount)
        }
        if (is.null(myinputs$selectedHistSubPlot) || myinputs$selectedHistSubPlot == "Standard histogram") {
          mplus.plot.histogram(myinputs$ofile, selectedVar, binCount, estimated=isEstimated, residual=isResidual)
        } else if (myinputs$selectedHistSubPlot == "Histogram/density plot") {
          mplus.plot.densityplot(myinputs$ofile, selectedVar, binCount, estimated=isEstimated, residual=isResidual)
        } else {
          mplus.plot.qqplot(myinputs$ofile, selectedVar, estimated=isEstimated, residual=isResidual)
        }
      }
    } else if (substr(myinputs$selectedPlot, 1, 24) == "Between-level histograms") {
      if (!mplusapp.has.individual_data(myinputs$ofile)) {
        # show nothing
      } else {
        if (is.null(myinputs$selectedVar)) {
          selectedVar <- 1
        } else {
          selectedVar <- as.character(myinputs$selectedVar)
        }
        if (is.null(myinputs$binCount)) {
          binCount <- 20
        } else {
          binCount <- as.numeric(myinputs$binCount)
        }
        if (is.null(myinputs$selectedHistSubPlot) || myinputs$selectedHistSubPlot == "Standard histogram") {
          mplus.plot.histogram(myinputs$ofile, selectedVar, binCount, between=TRUE)
        } else if (myinputs$selectedHistSubPlot == "Histogram/density plot") {
          mplus.plot.densityplot(myinputs$ofile, selectedVar, binCount, between=TRUE)
        } else {
          mplus.plot.qqnorm(myinputs$ofile, selectedVar, between=TRUE)
        }
      }
    } else if (substr(myinputs$selectedPlot, 1, 12) == "Scatterplots") {
      if (is.null(myinputs$selectedVar)) {
        xVar <- 1
      } else {
        xVar <- as.character(myinputs$selectedVar)
      }
      if (is.null(myinputs$selectedVarY)) {
        yVar <- 1
      } else {
        yVar <- as.character(myinputs$selectedVarY)
      }
      if (is.null(myinputs$selectedVarType)) {
        xEstimated <- FALSE
        xResidual <- FALSE
      } else if (myinputs$selectedVarType == "Sample values") {
        xEstimated <- FALSE
        xResidual <- FALSE
      } else if (myinputs$selectedVarType == "Estimated values") {
        xEstimated <- TRUE
        xResidual <- FALSE
      } else if (myinputs$selectedVarType == "Residuals") {
        xEstimated <- FALSE
        xResidual <- TRUE
      }
      if (is.null(myinputs$selectedVarYType)) {
        yEstimated <- FALSE
        yResidual <- FALSE
      } else if (myinputs$selectedVarYType == "Sample values") {
        yEstimated <- FALSE
        yResidual <- FALSE
      } else if (myinputs$selectedVarYType == "Estimated values") {
        yEstimated <- TRUE
        yResidual <- FALSE
      } else if (myinputs$selectedVarYType == "Residuals") {
        yEstimated <- FALSE
        yResidual <- TRUE
      }

      myPlotData.temp <- mplus.plot.scatterplot(myinputs$ofile, xVar, yVar, getdf=TRUE, xestimated=xEstimated, yestimated=yEstimated, xresidual=xResidual, yresidual=yResidual)
      myPlotData(myPlotData.temp)
    } else if (substr(myinputs$selectedPlot, 1, 26) == "Between-level scatterplots") {
      if (is.null(myinputs$selectedVar)) {
        xVar <- 1
      } else {
        xVar <- as.character(myinputs$selectedVar)
      }
      if (is.null(myinputs$selectedVarY)) {
        yVar <- 1
      } else {
        yVar <- as.character(myinputs$selectedVarY)
      }

      myPlotData.temp <- mplus.plot.scatterplot(myinputs$ofile, xVar, yVar, between=TRUE, getdf=TRUE)
      myPlotData(myPlotData.temp)
    } else if (myinputs$selectedPlot == "Dropout means") {
      if (is.null(myinputs$selectedProcess)) {
        selectedProcess <- 1
      } else {
        selectedProcess <- as.numeric(myinputs$selectedProcess)
      }
      myPlotData.temp <- mplus.plot.dropout_means(myinputs$ofile,selectedProcess,getdf=TRUE)
      myPlotData(myPlotData.temp)
    } else if (myinputs$selectedPlot == "Sample means") {
      if (is.null(myinputs$selectedProcess)) {
        selectedProcess <- 1
      } else {
        selectedProcess <- as.numeric(myinputs$selectedProcess)
      }
      myPlotData.temp <- mplus.plot.sample_means(myinputs$ofile,selectedProcess,getdf=TRUE)
      myPlotData(myPlotData.temp)
    } else if (myinputs$selectedPlot == "Estimated means") {
      if (is.null(myinputs$selectedProcess)) {
        selectedProcess <- 1
      } else {
        selectedProcess <- as.numeric(myinputs$selectedProcess)
      }
      myPlotData.temp <- mplus.plot.estimated_means(myinputs$ofile,selectedProcess,getdf=TRUE)
      myPlotData(myPlotData.temp)
    } else if (myinputs$selectedPlot == "Sample and estimated means") {
      if (is.null(myinputs$selectedProcess)) {
        selectedProcess <- 1
      } else {
        selectedProcess <- as.numeric(myinputs$selectedProcess)
      }
      myPlotData.temp <- mplus.plot.sample_and_estimated_means(myinputs$ofile,selectedProcess,getdf=TRUE)
      myPlotData(myPlotData.temp)
    } else if (myinputs$selectedPlot == "Estimated means, medians, modes, and percentiles") {
      if (is.null(myinputs$selectedSubPlot)) {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        myPlotData.temp <- mplus.plot.estimated_means(myinputs$ofile,selectedProcess,getdf=TRUE)
      } else if (myinputs$selectedSubPlot == "Estimated means") {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        myPlotData.temp <- mplus.plot.estimated_means(myinputs$ofile,selectedProcess,getdf=TRUE)
      } else if (myinputs$selectedSubPlot == "Estimated modes") {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        myPlotData.temp <- mplus.plot.estimated_modes(myinputs$ofile,selectedProcess,getdf=TRUE)
      } else if (myinputs$selectedSubPlot == "Estimated medians") {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        myPlotData.temp <- mplus.plot.estimated_medians(myinputs$ofile,selectedProcess,getdf=TRUE)
      } else { #if (myinputs$selectedSubPlot == "Estimated medians with percentiles") {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        myPlotData.temp <- mplus.plot.estimated_medians_with_percentiles(myinputs$ofile,selectedProcess,getdf=TRUE)
      }
      if (!is.null(myPlotData.temp)) {
        myPlotData(myPlotData.temp)
      }
    } else if (myinputs$selectedPlot == "Adjusted estimated means") {
      if (is.null(myCovariateDF$Data)) {
        # show nothing
      } else {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        xd <- myCovariateDF$Data
        xd[myinputs$row,myinputs$column] <- as.numeric(myinputs$newvalue)
        mplus.plot.adjusted.estimated.means(myinputs$ofile, myCovariateDF$Data, selectedProcess)
      }
    } else if (myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities") {
      if (is.null(myinputs$selectedSubPlot)) {
        # show nothing
      } else if (myinputs$selectedSubPlot == "Sample proportions") {
        if (is.null(input$plotSeries)) {
          plotSeries <- FALSE
        } else {
          plotSeries <- input$plotSeries
        }
        if (plotSeries) {
          if (is.null(myinputs$selectedProcess)) {
            selectedVarProcess <- 1
          } else {
            selectedVarProcess <- as.numeric(myinputs$selectedProcess)
          }
        } else {
          if (is.null(myinputs$selectedVar)) {
            selectedVarProcess <- 1
          } else {
            selectedVarProcess <- as.character(myinputs$selectedVar)
          }
        }
        mplus.plot.sample_proportions(myinputs$ofile,selectedVarProcess,series=plotSeries)
      } else if (myinputs$selectedSubPlot == "Estimated probabilities") {
        if (is.null(input$plotSeries)) {
          plotSeries <- FALSE
        } else {
          plotSeries <- input$plotSeries
        }
        if (plotSeries) {
          if (is.null(myinputs$selectedProcess)) {
            selectedVarProcess <- 1
          } else {
            selectedVarProcess <- as.numeric(myinputs$selectedProcess)
          }
        } else {
          if (is.null(myinputs$selectedVar)) {
            selectedVarProcess <- 1
          } else {
            selectedVarProcess <- as.character(myinputs$selectedVar)
          }
        }
        mplus.plot.estimated_probabilities(myinputs$ofile,selectedVarProcess,series=plotSeries)
      } else if (myinputs$selectedSubPlot == "Sample proportions and estimated probabilities") {
        if (is.null(input$plotSeries)) {
          plotSeries <- FALSE
        } else {
          plotSeries <- input$plotSeries
        }
        if (plotSeries) {
          if (is.null(myinputs$selectedProcess)) {
            selectedVarProcess <- 1
          } else {
            selectedVarProcess <- as.numeric(myinputs$selectedProcess)
          }
        } else {
          if (is.null(myinputs$selectedVar)) {
            selectedVarProcess <- 1
          } else {
            selectedVarProcess <- as.character(myinputs$selectedVar)
          }
        }
        mplus.plot.sample_proportions_and_estimated_probabilities(myinputs$ofile,selectedVarProcess,series=plotSeries)
      }
    } else if (myinputs$selectedPlot == "Item characteristic curves") {
      myPlotData.temp <- NULL
      if (is.null(myinput$numItems)) {

      } else if (myinput$numItems == 1) {
        if (is.null(myinput$catType)) {

        } else if (myinput$catType == 0) {
          if (is.null(myinput$selectedGroup)) {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,uvar=myinputs$selectedVar,getdf=TRUE)
          } else if (!(is.null(myinputs$selectedVar))) {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(myinput$selectedGroup),uvar=myinputs$selectedVar,getdf=TRUE)
          }
        } else if (input$catType == 1) {
          if (is.null(input$selectedCat1)) {

          } else {
            if (is.null(input$selectedGroup)) {
              myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),getdf=TRUE)
            } else {
              myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(input$selectedGroup),uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),getdf=TRUE)
            }
          }
        } else {
          if (is.null(input$selectedCat1) || is.null(input$selectedCat2)) {

          } else {
            if (is.null(input$selectedGroup)) {
              myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),cat2=as.numeric(input$selectedCat2),getdf=TRUE)
            } else {
              myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(input$selectedGroup),uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),cat2=as.numeric(input$selectedCat2),getdf=TRUE)
            }
          }
        }
      } else if (input$numItems == 2) {
        if (is.null(input$selectedCat1)) {

        } else if (is.null(input$selectedCat2)) {
          if (is.null(input$selectedGroup)) {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),getdf=TRUE)
          } else {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(input$selectedGroup),uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),getdf=TRUE)
          }
        } else {
          if (is.null(input$selectedGroup)) {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),cat2=as.numeric(input$selectedCat2),getdf=TRUE)
          } else {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(input$selectedGroup),uvar=myinputs$selectedVar,cat=as.numeric(input$selectedCat1),cat2=as.numeric(input$selectedCat2),getdf=TRUE)
          }
        }
      } else if (input$numItems == 0) {
        if (is.null(input$selectedCat1)) {

        } else if (is.null(input$selectedCat2)) {
          if (is.null(input$selectedGroup)) {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,cat=as.numeric(input$selectedCat1),getdf=TRUE)
          } else {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(input$selectedGroup),cat=as.numeric(input$selectedCat1),getdf=TRUE)
          }
        } else {
          if (is.null(input$selectedGroup)) {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,cat=as.numeric(input$selectedCat1),cat2=as.numeric(input$selectedCat2),getdf=TRUE)
          } else {
            myPlotData.temp <- mplus.plot.irt.icc(myinputs$ofile,group=as.character(input$selectedGroup),cat=as.numeric(input$selectedCat1),cat2=as.numeric(input$selectedCat2),getdf=TRUE)
          }
        }
      }
      if (!is.null(myPlotData.temp)) {
        myPlotData(myPlotData.temp)
      }
    } else if (myinputs$selectedPlot == "Information curves") {
      if (is.null(input$numItems)) {

      } else if (input$numItems == 1) {
        if (is.null(myinputs$selectedVar)) {

        } else if (is.null(input$selectedGroup)) {
          mplus.plot.irt.iic(myinputs$ofile,uvar=myinputs$selectedVar)
        } else {
          mplus.plot.irt.iic(myinputs$ofile,group=as.character(input$selectedGroup),uvar=as.character(myinputs$selectedVar))
        }
      } else if (input$numItems == 2) {
        if (is.null(input$selectedGroup)) {
          mplus.plot.irt.tic(myinputs$ofile,uvar=myinputs$selectedVar)
        } else {
          mplus.plot.irt.tic(myinputs$ofile,group=as.character(input$selectedGroup),uvar=myinputs$selectedVar)
        }
      } else if (input$numItems == 0) {
        if (is.null(input$selectedGroup)) {
          mplus.plot.irt.tic(myinputs$ofile)
        } else {
          mplus.plot.irt.tic(myinputs$ofile,group=as.character(input$selectedGroup))
        }
      }
    } else if (substr(myinputs$selectedPlot, 1, 17) == "Time series plots") {
      if (is.null(myinputs$selectedVar)) {
        selectedVar <- 1
      } else {
        selectedVar <- as.character(myinputs$selectedVar)
      }
      if (is.null(myinputs$selectedID)) {
        selectedID <- 1
      } else {
        selectedID <- as.integer(myinputs$selectedID)
      }

      if (myinputs$selectedSubPlot == "Means over subjects") {
        mplus.plot.timeseries.observed(myinputs$ofile, selectedVar)
      } else {
        mplus.plot.timeseries.observed(myinputs$ofile, selectedVar, selectedID)
      }
    } else if (myinputs$selectedPlot == "Histogram of subjects per time point") {
      mplus.plot.histogram.timepoints(myinputs$ofile)
    } else if (myinputs$selectedPlot == "Time interval plots") {
      if (is.null(myinputs$selectedID)) {
      } else {
        mplus.plot.time.intervals(myinputs$ofile, as.integer(myinputs$selectedID))
      }
    } else if (myinputs$selectedPlot == "Bayesian posterior parameter distributions") {
      if (is.null(myinputs$selectedVar)) {
      } else if (is.null(myinputs$binCount)) {
        mplus.plot.bayesian.distribution(myinputs$ofile, as.character(myinputs$selectedVar))
      } else {
        mplus.plot.bayesian.distribution(myinputs$ofile, as.character(myinputs$selectedVar), as.numeric(myinputs$binCount))
      }
    } else if (myinputs$selectedPlot == "Bayesian posterior parameter trace plots") {
      if (is.null(myinputs$selectedVar)) {
      } else {
        mplus.plot.bayesian.traceplot(myinputs$ofile, as.character(myinputs$selectedVar))
      }
    } else if (myinputs$selectedPlot == "Bayesian autocorrelation plots") {
      if (is.null(myinputs$selectedVar)) {
      } else if (is.null(myinputs$selectedGroup)) {
        mplus.plot.bayesian.autocorrelation(myinputs$ofile, as.character(myinputs$selectedVar))
      } else {
        mplus.plot.bayesian.autocorrelation(myinputs$ofile, as.character(myinputs$selectedVar), as.numeric(myinputs$selectedGroup))
      }
    } else if (myinputs$selectedPlot == "Bayesian prior parameter distributions") {
      if (is.null(myinputs$selectedVar)) {
      } else if (is.null(myinputs$binCount)) {
        mplus.plot.bayesian.prior.distribution(myinputs$ofile, as.character(myinputs$selectedVar))
      } else {
        mplus.plot.bayesian.prior.distribution(myinputs$ofile, as.character(myinputs$selectedVar), as.numeric(myinputs$binCount))
      }
    } else if (myinputs$selectedPlot == "Bayesian posterior predictive checking distribution plots") {
      if (is.null(myinputs$selectedVar)) {
        selectedVar <- 1
      } else {
        selectedVar <- as.character(myinputs$selectedVar)
      }
      if (is.null(myinputs$binCount)) {
        binCount <- 100
      } else {
        binCount <- as.numeric(myinputs$binCount)
      }
      mplus.plot.bayesian.predictive.distribution(myinputs$ofile, selectedVar, binCount)
    } else if (myinputs$selectedPlot == "Bayesian posterior predictive checking scatterplots") {
      if (is.null(myinputs$selectedVar)) {
        selectedVar <- 1
      } else {
        selectedVar <- as.character(myinputs$selectedVar)
      }
      mplus.plot.bayesian.predictive.scatterplot(myinputs$ofile, selectedVar)
    } else if (myinputs$selectedPlot == "Bootstrap distributions") {
      if (is.null(myinputs$selectedVar)) {
      } else if (is.null(myinputs$binCount)) {
        mplus.plot.bootstrap.distribution(myinputs$ofile, as.character(myinputs$selectedVar))
      } else {
        mplus.plot.bootstrap.distribution(myinputs$ofile, as.character(myinputs$selectedVar), as.numeric(myinputs$binCount))
      }
    } else if (myinputs$selectedPlot == "Autoregressive curves") {
      if (is.null(myinputs$selectedVar)) {
      } else {
        mplus.plot.autoregressive.curve(myinputs$ofile, as.character(myinputs$selectedVar))
      }
    } else if (myinputs$selectedPlot == "Loop plots") {
      if (is.null(myinputs$selectedVar)) {
      } else {
        mplus.plot.loop(myinputs$ofile, as.character(myinputs$selectedVar))
      }
    } else if (myinputs$selectedPlot == "Moderation plots") {
      if (is.null(myinputs$selectedVar)) {
      } else {
        mplus.plot.moderation(myinputs$ofile, as.character(myinputs$selectedVar))
      }
    } else if (myinputs$selectedPlot == "Sensitivity plots") {
      if (is.null(myinputs$selectedVar)) {
      } else {
        mplus.plot.sensitivity(myinputs$ofile, as.character(myinputs$selectedVar))
      }
    } else if (myinputs$selectedPlot == "Survival curves" || myinputs$selectedPlot == "Discrete survival curves") {
      if (is.null(myinputs$selectedSubPlot)) {
        # show nothing
      } else if (is.null(myinputs$selectedVar)) {
        #print("selectedVar is NULL, plot is...")
        #print(myinputs$selectedSubPlot)
        if (myinputs$selectedSubPlot == "Kaplan-Meier curve") {
          mplus.plot.survival.kaplanmeier(myinputs$ofile)
        } else if (myinputs$selectedSubPlot == "Estimated baseline survival curve") {
          if (is.null(myinputs$selectedVarY)) {
            mplus.plot.survival.baseline(myinputs$ofile)
          } else {
            mplus.plot.survival.baseline(myinputs$ofile,endvar=as.character(myinputs$selectedVarY))
          }
        } else if (myinputs$selectedSubPlot == "Sample log cumulative hazard curve") {
          mplus.plot.survival.sample.logcumulative(myinputs$ofile)
        } else if (myinputs$selectedSubPlot == "Estimated log cumulative baseline curve") {
          if (is.null(myinputs$selectedVarY)) {
            mplus.plot.survival.estimated.logcumulative(myinputs$ofile)
          } else {
            mplus.plot.survival.estimated.logcumulative(myinputs$ofile,endvar=as.character(myinputs$selectedVarY))
          }
        } else if (myinputs$selectedSubPlot == "Estimated baseline hazard curve") {
          mplus.plot.survival.basehazard(myinputs$ofile)
        } else if (myinputs$selectedSubPlot == "Kaplan-Meier curve with estimated baseline survival curve") {
          mplus.plot.survival.kaplanmeier.vs.baseline(myinputs$ofile)
        } else if (myinputs$selectedSubPlot == "Sample log cumulative hazard curve with estimated log cumulative baseline curve") {
          mplus.plot.survival.sample.vs.estimated.logcumulative(myinputs$ofile)
        }
      } else if (is.null(myinputs$selectedGroup)) {
        #print("selectedGroup is NULL, plot is...")
        #print(myinputs$selectedSubPlot)
        if (myinputs$selectedSubPlot == "Kaplan-Meier curve") {
          mplus.plot.survival.kaplanmeier(myinputs$ofile, as.character(myinputs$selectedVar))
        } else if (myinputs$selectedSubPlot == "Estimated baseline survival curve") {
          if (is.null(myinputs$selectedVarY)) {
            mplus.plot.survival.baseline(myinputs$ofile, as.character(myinputs$selectedVar))
          } else {
            mplus.plot.survival.baseline(myinputs$ofile, as.character(myinputs$selectedVar),endvar=as.character(myinputs$selectedVarY))
          }
        } else if (myinputs$selectedSubPlot == "Sample log cumulative hazard curve") {
          mplus.plot.survival.sample.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar))
        } else if (myinputs$selectedSubPlot == "Estimated log cumulative baseline curve") {
          if (is.null(myinputs$selectedVarY)) {
            mplus.plot.survival.estimated.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar))
          } else {
            mplus.plot.survival.estimated.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar),endvar=as.character(myinputs$selectedVarY))
          }
        } else if (myinputs$selectedSubPlot == "Kaplan-Meier curve with estimated baseline survival curve") {
          mplus.plot.survival.kaplanmeier.vs.baseline(myinputs$ofile, as.character(myinputs$selectedVar))
        } else if (myinputs$selectedSubPlot == "Sample log cumulative hazard curve with estimated log cumulative baseline curve") {
          mplus.plot.survival.sample.vs.estimated.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar))
        }
      } else if (myinputs$selectedSubPlot == "Kaplan-Meier curve") {
        mplus.plot.survival.kaplanmeier(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup))
      } else if (myinputs$selectedSubPlot == "Estimated baseline survival curve") {
        if (is.null(myinputs$numItems) || is.null(myinputs$selectedVarY)) {
          mplus.plot.survival.baseline(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup))
        } else if (!is.null(myinputs$selectedVarY)) {
          mplus.plot.survival.baseline(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup), endvar=as.character(myinputs$selectedVarY))
        }
      } else if (myinputs$selectedSubPlot == "Sample log cumulative hazard curve") {
        mplus.plot.survival.sample.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup))
      } else if (myinputs$selectedSubPlot == "Estimated log cumulative baseline curve") {
        if (is.null(myinputs$numItems) || is.null(myinputs$selectedVarY)) {
          mplus.plot.survival.estimated.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup))
        } else if (!is.null(myinputs$selectedVarY)) {
          mplus.plot.survival.estimated.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup), endvar=as.character(myinputs$selectedVarY))
        }
      } else if (myinputs$selectedSubPlot == "Estimated baseline hazard curve") {
        mplus.plot.survival.basehazard(myinputs$ofile, as.character(myinputs$selectedVar), as.numeric(myinputs$selectedGroup))
      } else if (myinputs$selectedSubPlot == "Kaplan-Meier curve with estimated baseline survival curve") {
        mplus.plot.survival.kaplanmeier.vs.baseline(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup))
      } else if (myinputs$selectedSubPlot == "Sample log cumulative hazard curve with estimated log cumulative hazard curve") {
        mplus.plot.survival.sample.vs.estimated.logcumulative(myinputs$ofile, as.character(myinputs$selectedVar), classnum=as.numeric(myinputs$selectedGroup))
      } else {
        print("Can't find plot")
      }
    } else if (myinputs$selectedPlot == "Measurement parameter plots") {
      if (is.null(myinputs$selectedSubPlot)) {

      } else if (myinputs$selectedSubPlot == "Histogram") {
        if (is.null(myinputs$selectedVar)) {
          if (is.null(myinputs$binCount)) {
            mplus.plot.measurement.parameter(myinputs$ofile, showhist=TRUE)
          } else {
            mplus.plot.measurement.parameter(myinputs$ofile, showhist=TRUE, bins=as.integer(myinputs$binCount))
          }
        } else {
          if (is.null(myinputs$binCount)) {
            mplus.plot.measurement.parameter(myinputs$ofile, as.character(myinputs$selectedVar), showhist=TRUE)
          } else {
            mplus.plot.measurement.parameter(myinputs$ofile, as.character(myinputs$selectedVar), showhist=TRUE, bins=as.integer(myinputs$binCount))
          }
        }
      } else {
        if (is.null(myinputs$selectedVar)) {
          mplus.plot.measurement.parameter(myinputs$ofile)
        } else {
          mplus.plot.measurement.parameter(myinputs$ofile, as.character(myinputs$selectedVar))
        }
      }
    } else if (myinputs$selectedPlot == "Observed individual values") {
      if (is.null(myinputs$selectedID)) {
        startidx <- 1
      } else {
        startidx <- as.numeric(myinputs$selectedID)
      }
      if (is.null(myinputs$selectedCat1)) {
        ccount <- 10
      } else {
        ccount <- as.numeric(myinputs$selectedCat1)
      }
      if (is.null(myinputs$selectedProcess)) {
        process <- 1
      } else {
        process <- as.numeric(myinputs$selectedProcess)
      }
      if (is.null(myinputs$numItems)) {
        curvetype <- 0
      } else {
        curvetype <- as.numeric(myinputs$numItems)
      }
      mplus.plot.individual.values(myinputs$ofile,process,curve=curvetype,start=startidx,count=ccount)
    } else if (myinputs$selectedPlot == "Estimated individual values") {
      if (is.null(myinputs$selectedID)) {
        startidx <- 1
      } else {
        startidx <- as.numeric(myinputs$selectedID)
      }
      if (is.null(myinputs$selectedCat1)) {
        ccount <- 10
      } else {
        ccount <- as.numeric(myinputs$selectedCat1)
      }
      if (is.null(myinputs$selectedProcess)) {
        process <- 1
      } else {
        process <- as.numeric(myinputs$selectedProcess)
      }
      mplus.plot.individual.values(myinputs$ofile,process,start=startidx,count=ccount,estimated=TRUE)
    } else if (myinputs$selectedPlot == "Estimated means and observed individual values") {
      if (is.null(myinputs$selectedID)) {
        startidx <- 1
      } else {
        startidx <- as.numeric(myinputs$selectedID)
      }
      if (is.null(myinputs$selectedCat1)) {
        ccount <- 10
      } else {
        ccount <- as.numeric(myinputs$selectedCat1)
      }
      if (is.null(myinputs$selectedProcess)) {
        process <- 1
      } else {
        process <- as.numeric(myinputs$selectedProcess)
      }
      if (is.null(myinputs$selectedGroup)) {
        group <- 1
      } else {
        if (myinputs$selectedGroup == "All") {
          group <- 0
        } else {
          group <- as.character(myinputs$selectedGroup)
        }
      }
      if (is.null(myinputs$numItems)) {
        curvetype <- 0
      } else {
        curvetype <- as.numeric(myinputs$numItems)
      }
      mplus.plot.individual.values(myinputs$ofile,process,curve=curvetype,start=startidx,count=ccount,showmean=TRUE,classidx=group)
    } else if (myinputs$selectedPlot == "Estimated means and estimated individual values") {
      if (is.null(myinputs$selectedID)) {
        startidx <- 1
      } else {
        startidx <- as.numeric(myinputs$selectedID)
      }
      if (is.null(myinputs$selectedCat1)) {
        ccount <- 10
      } else {
        ccount <- as.numeric(myinputs$selectedCat1)
      }
      if (is.null(myinputs$selectedProcess)) {
        process <- 1
      } else {
        process <- as.numeric(myinputs$selectedProcess)
      }
      if (is.null(myinputs$selectedGroup)) {
        group <- 1
      } else {
        if (myinputs$selectedGroup == "All") {
          group <- 0
        } else {
          group <- as.character(myinputs$selectedGroup)
        }
      }
      mplus.plot.individual.values(myinputs$ofile,process,start=startidx,count=ccount,estimated=TRUE,showmean=TRUE,classidx=group)
    } else if (myinputs$selectedPlot == "Adjusted estimated means and observed individual values") {
      if (is.null(myinputs$selectedID)) {
        startidx <- 1
      } else {
        startidx <- as.numeric(myinputs$selectedID)
      }
      if (is.null(myinputs$selectedCat1)) {
        ccount <- 10
      } else {
        ccount <- as.numeric(myinputs$selectedCat1)
      }
      if (is.null(myinputs$selectedProcess)) {
        process <- 1
      } else {
        process <- as.numeric(myinputs$selectedProcess)
      }
      if (is.null(myinputs$selectedGroup)) {
        group <- 1
      } else {
        if (myinputs$selectedGroup == "All") {
          group <- 0
        } else {
          group <- as.character(myinputs$selectedGroup)
        }
      }
      if (is.null(myinputs$numItems)) {
        curvetype <- 0
      } else {
        curvetype <- as.numeric(myinputs$numItems)
      }
      if (is.null(myCovariateDF$Data)) {
        # show nothing
      } else {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        xd <- myCovariateDF$Data
        xd[myinputs$row,myinputs$column] <- as.numeric(myinputs$newvalue)
        mplus.plot.observed.individual.values(myinputs$ofile,process,curve=curvetype,start=startidx,count=ccount,showmean=TRUE,classidx=group,showadjusted=TRUE,dfcovariates=xd)
      }
    } else if (myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      if (is.null(myinputs$selectedID)) {
        startidx <- 1
      } else {
        startidx <- as.numeric(myinputs$selectedID)
      }
      if (is.null(myinputs$selectedCat1)) {
        ccount <- 10
      } else {
        ccount <- as.numeric(myinputs$selectedCat1)
      }
      if (is.null(myinputs$selectedProcess)) {
        process <- 1
      } else {
        process <- as.numeric(myinputs$selectedProcess)
      }
      if (is.null(myinputs$selectedGroup)) {
        group <- 1
      } else {
        if (myinputs$selectedGroup == "All") {
          group <- 0
        } else {
          group <- as.character(myinputs$selectedGroup)
        }
      }
      if (is.null(myCovariateDF$Data)) {
        # show nothing
      } else {
        if (is.null(myinputs$selectedProcess)) {
          selectedProcess <- 1
        } else {
          selectedProcess <- as.numeric(myinputs$selectedProcess)
        }
        xd <- myCovariateDF$Data
        xd[myinputs$row,myinputs$column] <- as.numeric(myinputs$newvalue)
        mplus.plot.individual.values(myinputs$ofile,process,start=startidx,count=ccount,estimated=TRUE,showmean=TRUE,classidx=group,showadjusted=TRUE,dfcovariates=xd)
      }
    } else {
      # something selected but this plot is not available yet.
    }
  }, width=reactive(myinputs$plotWidth), height=reactive(myinputs$plotWidth*0.8))

  output$covariatesdf <- renderDataTable({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (myinputs$selectedPlot == "Adjusted estimated means" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
      if (is.null(myCovariateDF$Data)) {
        if (is.null(myinputs$numXsets)) {
          nset <- 1
        } else {
          nset <- as.integer(myinputs$numXsets)
        }
        myCovariateDF$Data <- mplus.get.covariates.for.adjusted.estimated.means(myinputs$ofile,sets=nset)
      }
    }
    if (!is.null(myCovariateDF$Data)) {
      datatable(myCovariateDF$Data, selection = 'single', escape=F, editable = TRUE, options = list(dom = 't', pageLength = 15, lengthChange = FALSE))
    }
  })


  ###############################################
  #
  # Render the plot data table
  #
  ###############################################
  output$plotValues <- renderTable({
    if (is.null(myinputs$cleardb)) {
      print("Click the 'Show plot data' button under the Plot tab.")
    } else if (!is.null(myPlotData)) {
      myPlotData()
    } else {
    }
  }, digits = reactive(myinputs$decimals), rownames = TRUE)


  ###############################################
  #
  # Render the text output
  #
  ###############################################
  output$textMessage <- renderText({
    if (is.null(myinputs$selectedPlot)) {
      # show nothing
    } else if (substr(myinputs$selectedPlot, 1, 10) == "Histograms" ||
               substr(myinputs$selectedPlot, 1, 24) == "Between-level histograms" ||
               substr(myinputs$selectedPlot, 1, 12) == "Scatterplots" ||
               substr(myinputs$selectedPlot, 1, 26) == "Between-level scatterplots" ||
               myinputs$selectedPlot == "Dropout means" ||
               myinputs$selectedPlot == "Sample means" ||
               myinputs$selectedPlot == "Estimated means" ||
               myinputs$selectedPlot == "Sample and estimated means" ||
               myinputs$selectedPlot == "Estimated means, medians, modes, and percentiles" ||
               myinputs$selectedPlot == "Adjusted estimated means"
               #myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities"
               ) {
      # show nothing
    } else if (myinputs$selectedPlot == "Sample proportions, estimated and conditional estimated probabilities") {
      if (is.null(myinputs$selectedSubPlot)) {
        "Sample proportions, estimated and conditional estimated probabilities: selectedSubPlot is NULL"
      } else if (myinputs$selectedSubPlot == "Sample proportions") {
        if (is.null(myinputs$selectedProcess) && is.null(myinputs$selectedVar) && is.null(input$plotSeries)) {
          "Sample proportions, estimated and conditional estimated probabilities, selectedSubPlot is Sample: selectedProcess, selectedVar, and plotSeries are NULL"
        } else if (input$plotSeries) {
        } else {
        }
      } else if (myinputs$selectedSubPlot == "Estimated probabilities") {
        if (is.null(myinputs$selectedProcess) && is.null(myinputs$selectedVar) && is.null(input$plotSeries)) {
          "Sample proportions, estimated and conditional estimated probabilities, selectedSubPlot is Estimated: selectedProcess, selectedVar, and plotSeries are NULL"
        } else if (input$plotSeries) {
        } else {
        }
      } else if (myinputs$selectedSubPlot == "Sample proportions and estimated probabilities") {
        if (is.null(myinputs$selectedProcess) && is.null(myinputs$selectedVar) && is.null(input$plotSeries)) {
          "Sample proportions, estimated and conditional estimated probabilities, selectedSubPlot is Sample/Estimated: selectedProcess, selectedVar, and plotSeries are NULL"
        } else if (input$plotSeries) {
        } else {
        }
      }
    } else if (myinputs$selectedPlot == "Item characteristic curves" || myinputs$selectedPlot == "Information curves") {
    } else if (substr(myinputs$selectedPlot, 1, 17) == "Time series plots" || myinputs$selectedPlot == "Histogram of subjects per time point" || myinputs$selectedPlot == "Time interval plots") {
    } else if (myinputs$selectedPlot == "Bayesian posterior parameter distributions" ||
               myinputs$selectedPlot == "Bayesian posterior parameter trace plots" ||
               myinputs$selectedPlot == "Bayesian autocorrelation plots" ||
               myinputs$selectedPlot == "Bayesian prior parameter distributions" ||
               myinputs$selectedPlot == "Bayesian posterior predictive checking distribution plots" ||
               myinputs$selectedPlot == "Bayesian posterior predictive checking scatterplots") {
    } else if (myinputs$selectedPlot == "Bootstrap distributions" ||
               myinputs$selectedPlot == "Autoregressive curves" ||
               myinputs$selectedPlot == "Loop plots" ||
               myinputs$selectedPlot == "Moderation plots" ||
               myinputs$selectedPlot == "Measurement parameter plots" ||
               myinputs$selectedPlot == "Sensitivity plots" ||
               myinputs$selectedPlot == "Survival curves" ||
               myinputs$selectedPlot == "Discrete survival curves") {
    } else if (myinputs$selectedPlot == "Observed individual values" ||
               myinputs$selectedPlot == "Estimated individual values" ||
               myinputs$selectedPlot == "Estimated means and observed individual values" ||
               myinputs$selectedPlot == "Estimated means and estimated individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and observed individual values" ||
               myinputs$selectedPlot == "Adjusted estimated means and estimated individual values") {
    } else {
      "This plot is currently not yet developed in the app.  Refer to the Mplus R Tutorial to see if a function in R has been developed for this plot.  If so, use this function in the R console."
    }
  })
}

# Run the application
if (require(rhdf5,quietly=TRUE)) {
  print("Loaded rhdf5 package")
} else {
  print("trying to install rhdf5 from bioconductor.org")
  rversion <- R.Version()
  if (as.numeric(rversion$major) < 4 || (as.numeric(rversion$major) == 3 && as.numeric(rversion$minor) < 6)) {
    source("https://bioconductor.org/biocLite.R")
    biocLite("rhdf5")
  } else {
    if (!require("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    BiocManager::install(c('rhdf5'))
  }
  if (require(rhdf5)) {
    print("Loaded missing rhdf5 package ")
  } else {
    stop("could not install rhdf5")
  }
}
if (!require("DT", quietly = TRUE)) {
  install.packages("DT")
}

shinyApp(ui = ui, server = server)
