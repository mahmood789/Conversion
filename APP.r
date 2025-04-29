library(shiny)
library(bs4Dash)
library(esc)
library(dmetar)
library(DT)

# Create a reactiveValues object to store conversion history
convHistory <- reactiveVal(data.frame(
  Time = character(), 
  ConversionType = character(), 
  Inputs = character(), 
  Result = character(), 
  stringsAsFactors = FALSE
))

# Function to generate sample CSV data for each conversion type
generateSampleData <- function(convType) {
  switch(convType,
         "Mean & Standard Error" = data.frame(
           grp1m = 8.5,
           grp1se = 1.5,
           grp1n = 50,
           grp2m = 11,
           grp2se = 1.8,
           grp2n = 60,
           esType_ms = "d"
         ),
         "Unstandardized Regression Coefficient" = data.frame(
           b = 3.3,
           sdy = 5,
           grp1n_reg = 100,
           grp2n_reg = 150,
           esType_B = "d"
         ),
         "Standardized Regression Coefficient" = data.frame(
           beta = 0.32,
           sdy_beta = 5,
           grp1n_beta = 100,
           grp2n_beta = 150,
           esType_beta = "d"
         ),
         "Point-biserial Correlation" = data.frame(
           rpb = 0.25,
           grp1n_rpb = 99,
           grp2n_rpb = 120,
           esType_rpb = "d"
         ),
         "One-Way ANOVA F-value" = data.frame(
           f = 5.04,
           grp1n_f = 519,
           grp2n_f = 528,
           esType_f = "g"
         ),
         "Two-Sample t-Test" = data.frame(
           t = 3.3,
           grp1n_t = 100,
           grp2n_t = 150,
           esType_t = "d"
         ),
         "p-value to SE" = data.frame(
           effect_size_p = 0.71,
           p = 0.013,
           N = 71,
           effectSizeType_p = "difference"
         ),
         "Chi-squared Test" = data.frame(
           chisq = 7.9,
           totaln = 100,
           esType_chisq = "cox.or"
         ),
         "Pool Groups" = data.frame(
           n1 = 50,
           n2 = 50,
           m1 = 3.5,
           m2 = 4,
           sd1 = 3,
           sd2 = 3.8
         ),
         "Number Needed to Treat" = data.frame(
           nnt_d = 0.245,
           nnt_CER = 0.35
         ),
         data.frame()  # default: empty
  )
}

# List of all conversion types
allConvTypes <- c("Mean & Standard Error",
                  "Unstandardized Regression Coefficient",
                  "Standardized Regression Coefficient",
                  "Point-biserial Correlation",
                  "One-Way ANOVA F-value",
                  "Two-Sample t-Test",
                  "p-value to SE",
                  "Chi-squared Test",
                  "Pool Groups",
                  "Number Needed to Treat")

ui <- bs4DashPage(
  title = "Meta-Analysis Effect Size Calculator",
  header = bs4DashNavbar(
    title = "Effect Size Calculator",
    skin = "light"
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    sidebarMenu(
      menuItem("Single Conversion", tabName = "single", icon = icon("calculator")),
      menuItem("Batch Conversion", tabName = "batch", icon = icon("upload")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("History", tabName = "history", icon = icon("history")),
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  body = bs4DashBody(
    tabItems(
      # Single Conversion Tab
      tabItem(
        tabName = "single",
        fluidRow(
          bs4Card(
            title = "Single Conversion Input",
            status = "primary",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            selectInput("conversionType", "Select Conversion Type:",
                        choices = allConvTypes,
                        selected = "Mean & Standard Error"),
            uiOutput("dynamicInputs"),
            br(),
            actionButton("calculate", "Calculate", icon = icon("play-circle"), class = "btn-success")
          )
        )
      ),
      # Batch Conversion Tab
      tabItem(
        tabName = "batch",
        fluidRow(
          bs4Card(
            title = "Batch Conversion Input",
            status = "warning",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            fileInput("batchFile", "Upload CSV File:",
                      accept = c(".csv")),
            selectInput("batchConversionType", "Select Conversion Type for Batch:",
                        choices = allConvTypes,
                        selected = "Mean & Standard Error"),
            # Button to download a sample CSV for the selected conversion type
            downloadButton("downloadSample", "Download Sample CSV", class = "btn-info"),
            br(), br(),
            # Button to download all sample CSV files in a zip archive
            downloadButton("downloadAllSamples", "Download All Sample CSV Files", class = "btn-warning"),
            br(), br(),
            actionButton("processBatch", "Process Batch Conversion", icon = icon("cogs"), class = "btn-primary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Uploaded Data Preview",
            status = "info",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            DTOutput("uploadedTable")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Batch Conversion Results",
            status = "success",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            DTOutput("batchResultsTable"),
            br(),
            downloadButton("downloadBatch", "Download Batch Results", class = "btn-info")
          )
        )
      ),
      # Results Tab (for Single Conversion)
      tabItem(
        tabName = "results",
        fluidRow(
          bs4Card(
            title = "Single Conversion Results",
            status = "success",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            verbatimTextOutput("resultOutput"),
            br(),
            actionButton("copyResult", "Copy to Clipboard", icon = icon("copy"))
          )
        )
      ),
      # History Tab
      tabItem(
        tabName = "history",
        fluidRow(
          bs4Card(
            title = "Conversion History",
            status = "secondary",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            DTOutput("historyTable"),
            br(),
            actionButton("clearHistory", "Clear History", icon = icon("trash"), class = "btn-danger")
          )
        )
      ),
      # Instructions Tab
      tabItem(
        tabName = "instructions",
        fluidRow(
          bs4Card(
            title = "Instructions",
            status = "info",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            HTML("
              <h4>How to Use the Effect Size Calculator</h4>
              <p>This application converts various reported statistics into effect sizes suitable for meta-analysis. It supports both single-item conversions and batch conversions via CSV file upload.</p>
              <h5>Single Conversion</h5>
              <ul>
                <li>Select the desired conversion type from the dropdown.</li>
                <li>Enter the required parameters in the input fields that appear.</li>
                <li>Click <strong>Calculate</strong> to view the conversion result on the Results tab.</li>
                <li>The conversion details and a summary of your input are stored in the History tab.</li>
              </ul>
              <h5>Batch Conversion</h5>
              <ul>
                <li>Upload a CSV file containing one row per conversion. Each row must have column names that match the parameter names required for the selected conversion type.</li>
                <li>For example, for <strong>Mean & Standard Error</strong>, the CSV should include: <code>grp1m, grp1se, grp1n, grp2m, grp2se, grp2n, esType_ms</code>.</li>
                <li>You can download a sample CSV for the currently selected conversion type using the <strong>Download Sample CSV</strong> button.</li>
                <li>You can also download a ZIP archive containing sample CSV files for <strong>all</strong> conversion types using the <strong>Download All Sample CSV Files</strong> button.</li>
                <li>After processing, the batch results are shown in a table and can be downloaded as a CSV file.</li>
              </ul>
              <h5>History & Advanced Options</h5>
              <ul>
                <li>The History tab displays a log of all single conversions with timestamp, conversion type, and a summary of inputs and results.</li>
                <li>You can clear the history if needed.</li>
              </ul>
              <p>The calculator uses functions from the <code>esc</code> and <code>dmetar</code> packages to compute effect sizes, standard errors, confidence intervals, and related statistics.</p>
            ")
          )
        )
      ),
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          bs4Card(
            title = "About This App",
            status = "secondary",
            width = 12,
            collapsible = TRUE,
            solidHeader = TRUE,
            HTML("
              <h4>About the Meta-Analysis Effect Size Calculator</h4>
              <p>This app was developed to help researchers and practitioners convert various reported statistics (e.g., t-values, F-values, regression coefficients) into effect sizes (e.g., Cohen’s d, Hedges’ g, correlation r) that can be used in meta-analysis.</p>
              <p>Built with R Shiny and the bs4Dash framework, it leverages functions from the <code>esc</code> and <code>dmetar</code> packages.</p>
              <p><strong>Author:</strong> [Your Name]</p>
              <p><strong>Version:</strong> 2.0</p>
            ")
          )
        )
      )
    )
  ),
  controlbar = bs4DashControlbar(), 
  footer = bs4DashFooter()
)

server <- function(input, output, session) {
  
  #### SINGLE CONVERSION SECTION ####
  
  # Dynamic UI for single conversion input parameters
  output$dynamicInputs <- renderUI({
    conv <- input$conversionType
    switch(conv,
           "Mean & Standard Error" = tagList(
             numericInput("grp1m", "Group 1 Mean:", value = 8.5),
             numericInput("grp1se", "Group 1 SE:", value = 1.5),
             numericInput("grp1n", "Group 1 n:", value = 50, min = 1),
             numericInput("grp2m", "Group 2 Mean:", value = 11),
             numericInput("grp2se", "Group 2 SE:", value = 1.8),
             numericInput("grp2n", "Group 2 n:", value = 60, min = 1),
             selectInput("esType_ms", "Effect Size Type:", choices = c("d", "g"), selected = "d")
           ),
           "Unstandardized Regression Coefficient" = tagList(
             numericInput("b", "Regression Coefficient (b):", value = 3.3),
             numericInput("sdy", "SD of y:", value = 5),
             numericInput("grp1n_reg", "Group 1 n:", value = 100, min = 1),
             numericInput("grp2n_reg", "Group 2 n:", value = 150, min = 1),
             selectInput("esType_B", "Effect Size Type:", choices = c("d", "r"), selected = "d")
           ),
           "Standardized Regression Coefficient" = tagList(
             numericInput("beta", "Standardized Beta:", value = 0.32),
             numericInput("sdy_beta", "SD of y:", value = 5),
             numericInput("grp1n_beta", "Group 1 n:", value = 100, min = 1),
             numericInput("grp2n_beta", "Group 2 n:", value = 150, min = 1),
             selectInput("esType_beta", "Effect Size Type:", choices = c("d", "r"), selected = "d")
           ),
           "Point-biserial Correlation" = tagList(
             numericInput("rpb", "Point-Biserial r:", value = 0.25),
             numericInput("grp1n_rpb", "Group 1 n:", value = 99, min = 1),
             numericInput("grp2n_rpb", "Group 2 n:", value = 120, min = 1),
             selectInput("esType_rpb", "Effect Size Type:", choices = c("d", "r"), selected = "d")
           ),
           "One-Way ANOVA F-value" = tagList(
             numericInput("f", "F-value:", value = 5.04),
             numericInput("grp1n_f", "Group 1 n:", value = 519, min = 1),
             numericInput("grp2n_f", "Group 2 n:", value = 528, min = 1),
             selectInput("esType_f", "Effect Size Type:", choices = c("d", "g"), selected = "g")
           ),
           "Two-Sample t-Test" = tagList(
             numericInput("t", "t-value:", value = 3.3),
             numericInput("grp1n_t", "Group 1 n:", value = 100, min = 1),
             numericInput("grp2n_t", "Group 2 n:", value = 150, min = 1),
             selectInput("esType_t", "Effect Size Type:", choices = c("d", "g"), selected = "d")
           ),
           "p-value to SE" = tagList(
             numericInput("effect_size_p", "Effect Size:", value = 0.71),
             numericInput("p", "p-value:", value = 0.013, min = 0, max = 1),
             numericInput("N", "Total N:", value = 71, min = 1),
             selectInput("effectSizeType_p", "Effect Size Type:", choices = c("difference", "ratio"), selected = "difference")
           ),
           "Chi-squared Test" = tagList(
             numericInput("chisq", "Chi-squared value:", value = 7.9),
             numericInput("totaln", "Total N:", value = 100, min = 1),
             selectInput("esType_chisq", "Effect Size Type:", choices = c("cox.or"), selected = "cox.or")
           ),
           "Pool Groups" = tagList(
             numericInput("n1", "Group 1 n:", value = 50, min = 1),
             numericInput("n2", "Group 2 n:", value = 50, min = 1),
             numericInput("m1", "Group 1 Mean:", value = 3.5),
             numericInput("m2", "Group 2 Mean:", value = 4),
             numericInput("sd1", "Group 1 SD:", value = 3),
             numericInput("sd2", "Group 2 SD:", value = 3.8)
           ),
           "Number Needed to Treat" = tagList(
             numericInput("nnt_d", "Effect Size (d):", value = 0.245),
             checkboxInput("nnt_useCER", "Specify CER?", value = FALSE),
             conditionalPanel(
               condition = "input.nnt_useCER == true",
               numericInput("nnt_CER", "Control Event Rate (CER):", value = 0.35, min = 0, max = 1)
             )
           )
    )
  })
  
  # Single Conversion: Compute result when Calculate is clicked
  singleResultText <- eventReactive(input$calculate, {
    conv <- input$conversionType
    res <- tryCatch({
      switch(conv,
             "Mean & Standard Error" = {
               out <- esc_mean_se(grp1m = input$grp1m,
                                  grp1se = input$grp1se,
                                  grp1n = input$grp1n,
                                  grp2m = input$grp2m,
                                  grp2se = input$grp2se,
                                  grp2n = input$grp2n,
                                  es.type = input$esType_ms)
               paste(capture.output(out), collapse = "\n")
             },
             "Unstandardized Regression Coefficient" = {
               out <- esc_B(b = input$b,
                            sdy = input$sdy,
                            grp1n = input$grp1n_reg,
                            grp2n = input$grp2n_reg,
                            es.type = input$esType_B)
               paste(capture.output(out), collapse = "\n")
             },
             "Standardized Regression Coefficient" = {
               out <- esc_beta(beta = input$beta,
                               sdy = input$sdy_beta,
                               grp1n = input$grp1n_beta,
                               grp2n = input$grp2n_beta,
                               es.type = input$esType_beta)
               paste(capture.output(out), collapse = "\n")
             },
             "Point-biserial Correlation" = {
               out <- esc_rpb(r = input$rpb,
                              grp1n = input$grp1n_rpb,
                              grp2n = input$grp2n_rpb,
                              es.type = input$esType_rpb)
               paste(capture.output(out), collapse = "\n")
             },
             "One-Way ANOVA F-value" = {
               out <- esc_f(f = input$f,
                            grp1n = input$grp1n_f,
                            grp2n = input$grp2n_f,
                            es.type = input$esType_f)
               paste(capture.output(out), collapse = "\n")
             },
             "Two-Sample t-Test" = {
               out <- esc_t(t = input$t,
                            grp1n = input$grp1n_t,
                            grp2n = input$grp2n_t,
                            es.type = input$esType_t)
               paste(capture.output(out), collapse = "\n")
             },
             "p-value to SE" = {
               out <- se.from.p(effect_size = input$effect_size_p,
                                p = input$p,
                                N = input$N,
                                effect.size.type = input$effectSizeType_p)
               paste(capture.output(out), collapse = "\n")
             },
             "Chi-squared Test" = {
               out <- esc_chisq(chisq = input$chisq,
                                totaln = input$totaln,
                                es.type = input$esType_chisq)
               paste(capture.output(out), collapse = "\n")
             },
             "Pool Groups" = {
               out <- pool.groups(n1 = input$n1,
                                  n2 = input$n2,
                                  m1 = input$m1,
                                  m2 = input$m2,
                                  sd1 = input$sd1,
                                  sd2 = input$sd2)
               paste(capture.output(out), collapse = "\n")
             },
             "Number Needed to Treat" = {
               if (input$nnt_useCER) {
                 out <- NNT(d = input$nnt_d, CER = input$nnt_CER)
               } else {
                 out <- NNT(d = input$nnt_d)
               }
               paste(capture.output(out), collapse = "\n")
             }
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      paste("Error:", e$message)
    })
    
    # Append conversion to history
    isolate({
      history <- convHistory()
      inputList <- reactiveValuesToList(input)
      inputSummary <- paste(names(inputList)[1:5], "=", unlist(inputList)[1:5], collapse = ", ")
      newRow <- data.frame(
        Time = as.character(Sys.time()),
        ConversionType = input$conversionType,
        Inputs = inputSummary,
        Result = res,
        stringsAsFactors = FALSE
      )
      convHistory(rbind(history, newRow))
    })
    res
  })
  
  output$resultOutput <- renderText({
    req(singleResultText())
    singleResultText()
  })
  
  # Simulated copy-to-clipboard action
  observeEvent(input$copyResult, {
    showNotification("Result copied to clipboard (simulated)", type = "message")
  })
  
  #### BATCH CONVERSION SECTION ####
  
  # Reactive expression to store the uploaded CSV as a data frame
  batchData <- reactive({
    req(input$batchFile)
    tryCatch({
      read.csv(input$batchFile$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error reading CSV file.", type = "error")
      NULL
    })
  })
  
  # Display a preview of the uploaded CSV
  output$uploadedTable <- renderDT({
    req(batchData())
    datatable(batchData(), options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Process batch conversion and store results in a data frame
  batchResults <- eventReactive(input$processBatch, {
    req(batchData())
    df <- batchData()
    conv <- input$batchConversionType
    results <- apply(df, 1, function(row) {
      tryCatch({
        if (conv == "Mean & Standard Error") {
          out <- esc_mean_se(grp1m = as.numeric(row["grp1m"]),
                             grp1se = as.numeric(row["grp1se"]),
                             grp1n = as.numeric(row["grp1n"]),
                             grp2m = as.numeric(row["grp2m"]),
                             grp2se = as.numeric(row["grp2se"]),
                             grp2n = as.numeric(row["grp2n"]),
                             es.type = row["esType_ms"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Unstandardized Regression Coefficient") {
          out <- esc_B(b = as.numeric(row["b"]),
                       sdy = as.numeric(row["sdy"]),
                       grp1n = as.numeric(row["grp1n_reg"]),
                       grp2n = as.numeric(row["grp2n_reg"]),
                       es.type = row["esType_B"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Standardized Regression Coefficient") {
          out <- esc_beta(beta = as.numeric(row["beta"]),
                          sdy = as.numeric(row["sdy_beta"]),
                          grp1n = as.numeric(row["grp1n_beta"]),
                          grp2n = as.numeric(row["grp2n_beta"]),
                          es.type = row["esType_beta"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Point-biserial Correlation") {
          out <- esc_rpb(r = as.numeric(row["rpb"]),
                         grp1n = as.numeric(row["grp1n_rpb"]),
                         grp2n = as.numeric(row["grp2n_rpb"]),
                         es.type = row["esType_rpb"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "One-Way ANOVA F-value") {
          out <- esc_f(f = as.numeric(row["f"]),
                       grp1n = as.numeric(row["grp1n_f"]),
                       grp2n = as.numeric(row["grp2n_f"]),
                       es.type = row["esType_f"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Two-Sample t-Test") {
          out <- esc_t(t = as.numeric(row["t"]),
                       grp1n = as.numeric(row["grp1n_t"]),
                       grp2n = as.numeric(row["grp2n_t"]),
                       es.type = row["esType_t"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "p-value to SE") {
          out <- se.from.p(effect_size = as.numeric(row["effect_size_p"]),
                           p = as.numeric(row["p"]),
                           N = as.numeric(row["N"]),
                           effect.size.type = row["effectSizeType_p"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Chi-squared Test") {
          out <- esc_chisq(chisq = as.numeric(row["chisq"]),
                           totaln = as.numeric(row["totaln"]),
                           es.type = row["esType_chisq"])
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Pool Groups") {
          out <- pool.groups(n1 = as.numeric(row["n1"]),
                             n2 = as.numeric(row["n2"]),
                             m1 = as.numeric(row["m1"]),
                             m2 = as.numeric(row["m2"]),
                             sd1 = as.numeric(row["sd1"]),
                             sd2 = as.numeric(row["sd2"]))
          paste(capture.output(out), collapse = "\n")
        } else if (conv == "Number Needed to Treat") {
          if (!is.na(row["nnt_CER"]) && row["nnt_CER"] != "") {
            out <- NNT(d = as.numeric(row["nnt_d"]), CER = as.numeric(row["nnt_CER"]))
          } else {
            out <- NNT(d = as.numeric(row["nnt_d"]))
          }
          paste(capture.output(out), collapse = "\n")
        } else {
          "Unsupported conversion type"
        }
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })
    df$result <- results
    df
  })
  
  output$batchResultsTable <- renderDT({
    req(batchResults())
    datatable(batchResults(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$downloadBatch <- downloadHandler(
    filename = function() {
      paste("batch_conversion_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(batchResults(), file, row.names = FALSE)
    }
  )
  
  #### DOWNLOAD SAMPLE CSV FOR CURRENTLY SELECTED CONVERSION TYPE ####
  
  output$downloadSample <- downloadHandler(
    filename = function() {
      paste("sample_", gsub(" ", "_", tolower(input$batchConversionType)), ".csv", sep = "")
    },
    content = function(file) {
      sampleData <- generateSampleData(input$batchConversionType)
      write.csv(sampleData, file, row.names = FALSE)
    }
  )
  
  #### DOWNLOAD ALL SAMPLE CSV FILES AS ZIP ####
  
  output$downloadAllSamples <- downloadHandler(
    filename = function() {
      paste("all_sample_csv_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory
      tmpDir <- tempdir()
      # For each conversion type, generate a CSV file
      for(conv in allConvTypes){
        sampleData <- generateSampleData(conv)
        csvFile <- file.path(tmpDir, paste0(gsub(" ", "_", tolower(conv)), ".csv"))
        write.csv(sampleData, csvFile, row.names = FALSE)
      }
      # List the CSV files created
      csvFiles <- list.files(tmpDir, pattern = "\\.csv$", full.names = TRUE)
      # Create a zip archive containing the CSV files
      zip::zipr(zipfile = file, files = csvFiles)
    },
    contentType = "application/zip"
  )
  
  #### HISTORY SECTION ####
  
  output$historyTable <- renderDT({
    datatable(convHistory(), options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  observeEvent(input$clearHistory, {
    convHistory(data.frame(
      Time = character(), 
      ConversionType = character(), 
      Inputs = character(), 
      Result = character(), 
      stringsAsFactors = FALSE
    ))
    showNotification("History cleared", type = "message")
  })
}

shinyApp(ui, server)
