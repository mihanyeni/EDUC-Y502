# ==============================================================================

# Course:        EDUC-Y 502 Intermediate statistics applied to education
# File:          Lecture notes B.1 - Tabular & graphical summary representations
#                of frequencies
# Author:        Justin Wild, Ph.D.
# Date modified: 20.08.2023
# Purpose:       Create Interactive Figure B.1.1

# Load packages ================================================================

library(dplyr)       # version 1.1.2
library(DT)          # version 0.28
library(haven)       # version 2.5.2
library(openxlsx)    # version 4.2.5.2
library(shiny)       # version 1.7.4
library(shinythemes) # version 1.2.0
library(schoolmath)  # version 0.4.1

# Load data ====================================================================

# filepath <- "/Users/wildden/Wildman/Work/IU Teaching/Y502 - Intermediate Statistics Applied to Education/Lecture notes/Interactive figures/data/"
# setsdata <- openxlsx::read.xlsx(xlsxFile = paste0(filepath,
#                                                   "setup_y502.xlsx"),
#                                 sheet = "datasets", startRow = 1, colNames = T,
#                                 rowNames = F, rows = 1:6, cols = 1:4)
# datarow1 <- 2
# datayear <- setsdata[datarow1, 2]
# datapart <- "STU"
# var_achv <- setsdata[datarow1, 3]

var_achv <- "CIL"

# dataset1 <- list.files(filepath, setsdata[datarow1, 1])
# dataset1 <- grep(pattern = datayear, x = dataset1, value = T)
# dataset1 <- grep(pattern = "Rdata",  x = dataset1, value = T)
# dataset1 <- grep(pattern = datapart, x = dataset1, value = T)
# 
# df1 <- get(load(paste0(filepath, dataset1)))

df1 <- get(load("/Users/wildden/Wildman/Work/IU Teaching/Y502 - Intermediate Statistics Applied to Education/Lecture notes/Interactive figures/data/ICILS_2018_USA_SCH_STU.Rdata"))

# Create function to create table then use in server ===========================

group.freq.table <- function(dcm, min, max, iwd, dfr, vrb) {

  # Create empty grouped frequency table with column names

  gf_data <- as.data.frame(matrix(data = 0, nrow = ((max - min) / iwd) + 1, ncol = 7))

  colnames(gf_data) <- c("apparent limits", "real limits", "f",
                         "rel f (proportion)", "rel f (percentage)",
                         "cumulative f", "cumulative f (percentage)")

  # Fill empty grouped frequency table based on user input

  for (i in 0:((max - min) / iwd)) {

    # Get a character string of apparent limits
    
    if (min + iwd*(i-1) + 1 < 0) {

      gf_data[i + 1,1] <- paste0(0, " - ", min + iwd*i)

    } else {

      gf_data[i + 1,1] <- paste0(min + iwd*(i-1) + 1, " - ", min + iwd*i)
    
    }

    # Get a character string of real limits
    
    if (min + iwd*(i-1) + 1 < 0) {
      
      if (dcm != 2) {

        gf_data[i + 1,2] <- paste0("0.0 - ", min + iwd*i + 0.5)
      
      } else {
        
        gf_data[i + 1,2] <- paste0("0.00 - ", min + iwd*i + 0.5)
        
      }

    } else {
      
      gf_data[i + 1,2] <- paste0(min + iwd*(i-1) + 0.5, " - ", min + iwd*i + 0.5)
      
    }

    # Get bounds for frequency counts
    
    if (min + iwd*(i-1) + 1 < 0) {

      lo_bound <- 0
      hi_bound <- min + iwd*i     + 0.5

    } else {
      
      lo_bound <- min + iwd*(i-1) + 0.5
      hi_bound <- min + iwd*i     + 0.5
      
    }

    # Get frequency count

    gf_data[i + 1,3] <- sum(dfr[,vrb] >= lo_bound & dfr[,vrb] < hi_bound)

    # Get frequency as a relative proportion

    gf_data[i + 1,4] <-
      format(round(gf_data[i + 1,3] / nrow(dfr), digits = 3), nsmall = 3)

    # Get frequency as a percentage proportion

    gf_data[i + 1,5] <-
      paste0(format(round((gf_data[i + 1,3] / nrow(dfr))*100, digits = 1),
                    nsmall = 1), " %")

    # Get cumulative frequency count

    gf_data[i + 1,6] <- if (i != 0) {

      gf_data[i + 1,3] + gf_data[i,6]

    } else {

      gf_data[i + 1,3]

    }

    # Get cumulative frequency as a percentage proportion

    gf_data[i + 1,7] <-
      paste0(format(round((gf_data[i + 1,6] / nrow(dfr)) * 100, digits = 1),
                    nsmall = 1), " %")
    
    # if (i == ((max - min) / iwd) & gf_data[1,2] == 0) gf_data <- gf_data[-1,]

  }

  as.data.frame(gf_data)

}

# ==============================================================================
# ===========================   USER INTERFACE   ===============================
# ==============================================================================

ui <- fluidPage(theme = shinytheme("cosmo"),
  
  # Title
  
  tags$h2("Interactive Figure B.1.1 from Lecture notes B.1 - Tabular & graphical summary representations of frequencies"),
  tags$h3("author: Justin Wild, Ph.D."),
  tags$h3("course: EDUC-Y 502, Fall 2023, section #1725"),
  
  # Text
  
  tags$h4("This interactive figure allows you to choose the minimum, maximum, interval width, and decimal places for a grouped frequency table of the student achievement variable ", em(strong("Computer and Information Literacy (CIL)")), " from the ICILS 2018 study. The theoretical minimum for this variable is 0 and its theoretical maximum is 100. You will get warnings if you choose values that are not possible, or try to construct a grouped frequency table that goes against the recommendations of the course's textbook. However, you can still create your table even if you recieve warnings. Have fun!"),
  
  # 01. Get input for grouped frequency table ==============
  
  tags$h2("Submit information for group frequency table"),
  
  wellPanel(
    
    numericInput(inputId = "minimum",  label = "Choose the minimum value",
                 value = 0,   min = 0, max = 100, step = 1),
    
    numericInput(inputId = "maximum",  label = "Choose the maximum value",
                 value = 100, min = 0, max = 100, step = 1),
    
    numericInput(inputId = "intwidth", label = "Choose the interval width",
                 value = 10,  min = 1, max = 20,  step = 1),
    
    actionButton(inputId = "checkInputs",
                 label = "Check minimum, maximum, and interval width")
    
  ),
  
  # 02. Create feedback for inputs =========================
  
  conditionalPanel(
    
    condition = "input.checkInputs",
    
    wellPanel(
      
      # 02a. Create warning about minimum
      
      uiOutput(outputId = "warningMinimum"),
      
      # 02b. Create warning about maximum
      
      uiOutput(outputId = "warningMaximum"),
      
      # 02c. Create warning about interval
      
      uiOutput(outputId = "warningInterval"),
      
      actionButton(inputId = "submitInputs",
                   label = "Submit grouped frequency inputs")
      
    )
    
  ),
  
  # 03. Create radio button for decimal places =============
  
  conditionalPanel(

    condition = "input.submitInputs",

    wellPanel(
      
      radioButtons(inputId = "decimal",
                   label = "Choose the number of decimal places",
                   choices = c(0,1,2), selected = 0, inline = T),

      actionButton(inputId = "submitDecimals", label = "Submit decimal places")

    )

  ),
  
  # 04. Choose decimal place values and create table =======

  conditionalPanel(

    condition = "input.submitDecimals",
    
    wellPanel(
      
      uiOutput(outputId = "decimalValues"),
      
      actionButton(inputId = "createTable",
                   label = "Create grouped frequency table")
      
    )
      
  ),
  
  # 05. Create table =======================================

  DT::dataTableOutput(outputId = "table")
    
)

#===============================================================================
#============================       SERVER       ===============================
#===============================================================================

server <- function(input, output, session){
  
  # 02a. Create warning about minimum ======================

  textWarningMinimum <- eventReactive(input$checkInputs, {
    
    if (input$minimum < 0) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The minimum is below zero, which is not theoretically possible for this variable. This will result in meaningless intervals. Please select a minimum value at or above zero and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$minimum > 100) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The ', em('minimum'), ' is above 100, which is above the theoretical ', em(strong('maximum')), ' for this variable. This will result in meaningless intervals. Please select a minimum value below 100 and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$minimum > input$maximum) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The minimum is ', em(strong('greater')), ' than the maximum. This will result in meaningless intervals. Please select a minimum value below the maximum or a maximum value above the minimum and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else {
      
      tags$h5('Minimum value is ok.')
      
    }
    
  })
  
  output$warningMinimum <- renderUI({textWarningMinimum()})

  # 02b. Create warning about maximum ======================
  
  textWarningMaximum <- eventReactive(input$checkInputs, {
    
    if (input$maximum > 100) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The maximum is above 100, which is not theoretically possible for this variable. This will result in meaningless intervals. Please select a maximum value at or below 100 and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$maximum < 0) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The ', em('maximum'), ' is below 0, which is below the theoretical ', em(strong('minimum')), ' for this variable. This will result in meaningless intervals. Please select a maximum value above 0 and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$minimum > input$maximum) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The maximum is ', em(strong('less than')), ' the minimum. This will result in meaningless intervals. Please select a maximum value above the minimum or a minimum value below the maximum and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else {
      
      tags$h5('Maximum value is ok.')
      
    }
    
  })

  output$warningMaximum <- renderUI({textWarningMaximum()})
  
  # 02c. Create warning about interval =====================
  
  textWarningInterval <- eventReactive(input$checkInputs, {
    
    if (!is.decimal((input$maximum - input$minimum) / input$intwidth)) {
      
      tags$h5('Interval width is ok.')
      
    } else {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"),
              ": Range is not evenly divisible by the interval width. A minimum of ",
              input$minimum, " subtracted from a maximum of ",
              input$maximum, " results in a range of ",
              input$maximum - input$minimum, " that, when divided by an interval width of ",
              tags$span(input$intwidth, .noWS = "after"), ", results in ",
              tags$span(format(
                round((input$maximum - input$minimum) / input$intwidth,
                      digits = 2), nsmall = 2), .noWS = "after"),
              ', or a non-evenly divisible result (i.e., there are values after the decimal). Please select an evenly divisible interval wdith and re-click the "Check minimum, maximum, and interval width" button.')
      
    }
    
  })
  
  output$warningInterval <- renderUI({textWarningInterval()})
  
  
  # 04. Choose decimal place values and create table =======
  
  output$decimalValues <- renderUI({
    
    if (input$decimal == 0) {
      
      tags$h5("No more input needed! Click the button below :o)")
      
    } else if (input$decimal == 1) {
      
      radioButtons(inputId = "dcm10",
                   label = "Choose the tenth place value after the decimal point, then click the button below.",
                   choices = c(0,1,2,3,4,5,6,7,8,9), selected = 0, inline = T)
      
    } else if (input$decimal == 2) {
      
      tagList(
        
        radioButtons(inputId = "dcm10",
                     label = "Choose the tenth place value after the decimal point.",
                     choices = c(0,1,2,3,4,5,6,7,8,9), selected = 0, inline = T),
        
        radioButtons(inputId = "dcm100",
                     label = "Choose the hundredth place value after the decimal point, then click the button below.",
                     choices = c(0,1,2,3,4,5,6,7,8,9), selected = 0, inline = T)
        
      )
      
    }
    
  })
  
  # 05. Create table =======================================

  gfTable <- shiny::eventReactive(input$createTable, {

    if (input$decimal == 0) {

      group.freq.table(input$decimal, input$minimum, input$maximum,
                       input$intwidth, df1, var_achv)

    } else if (input$decimal == 1) {

      group.freq.table(input$decimal,
                       as.numeric(paste0(input$minimum,".",input$dcm10)),
                       as.numeric(paste0(input$maximum,".",input$dcm10)),
                       input$intwidth, df1, var_achv)

    } else if (input$decimal == 2) {

      group.freq.table(input$decimal,
                       as.numeric(paste0(input$minimum,".",input$dcm10,input$dcm100)),
                       as.numeric(paste0(input$maximum,".",input$dcm10,input$dcm100)),
                       input$intwidth, df1, var_achv)

    }

  })

  output$table <- DT::renderDataTable(
    gfTable(),
    options = list(
      columnDefs = list(list(className = "dt-left",  targets = c(1:2)),
                        list(className = "dt-right", targets = c(3:7))),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20))
  )
  
}

#===============================================================================
#============================        RUN         ===============================
#===============================================================================

shinyApp(ui = ui, server = server)

# END ==========================================================================
