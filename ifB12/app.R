# ==============================================================================

# Course:        EDUC-Y 502 Intermediate statistics applied to education
# File:          Lecture notes B.1 - Tabular & graphical summary representations
#                of frequencies
# Author:        Justin Wild, Ph.D.
# Date modified: 21.08.2023
# Purpose:       Create Interactive Figure B.1.2

# Load packages ================================================================

library(dplyr)       # version 1.1.2
library(DT)          # version 0.28
library(haven)       # version 2.5.2
library(openxlsx)    # version 4.2.5.2
library(shiny)       # version 1.7.4
library(shinythemes) # version 1.2.0
library(schoolmath)  # version 0.4.1

# Load data ====================================================================

vbl <- "S_NISB"

df1 <- get(load("/Users/wildden/Wildman/Work/IU Teaching/Y502 - Intermediate Statistics Applied to Education/Data/ICILS_2018_USA_Sch_Stu.Rdata"))

# Create function to create table then use in server ===========================

group.freq.table <- function(min, max, iwd, dfr, vrb) {
  
  # Create empty grouped frequency table with column names
  
  gf_data <- as.data.frame(matrix(data = 0, nrow = 1, ncol = 6))
  
  colnames(gf_data) <- c("apparent limits", "real limits", "f",
                         "rel f (percentage)", "cumulative f",
                         "cumulative f (percentage)")
  
  # Fill empty grouped frequency table based on user input
  
  for (i in 0:((max - min) / iwd) - 1) {
    
    # Get a character string of apparent limits
    
    gf_data[i + 1,1] <- paste0(format(round(min + iwd*i + 0.01, digits = 2),
                                      nsmall = 2),
                               " - ",
                               format(round(min + iwd*i + iwd, digits = 2),
                                      nsmall = 2))
    
    # Get a character string of real limits
    
    gf_data[i + 1,2] <- paste0(format(round(min + iwd*i + 0.01 - 0.005,
                                            digits = 3),
                                      nsmall = 3),
                               " - ",
                               format(round(min + iwd*i + iwd + 0.005,
                                            digits = 3),
                                      nsmall = 3))
    
    # Get bounds for frequency counts
    
    lo_bound <- min + iwd*i + 0.01 - 0.005
    hi_bound <- min + iwd*i + iwd + 0.005
    
    # Get frequency count
    
    gf_data[i + 1,3] <- sum(dfr[,vrb] >= lo_bound &
                              dfr[,vrb] < hi_bound, na.rm = T)
    
    # Get frequency as a percentage proportion
    
    gf_data[i + 1,4] <-
      paste0(format(round((gf_data[i + 1,3] / nrow(dfr))*100, digits = 1),
                    nsmall = 1), " %")
    
    # Get cumulative frequency count
    
    gf_data[i + 1,5] <- if (i != 0) {
      
      gf_data[i + 1,3] + gf_data[i,5]
      
    } else {
      
      gf_data[i + 1,3]
      
    }
    
    # Get cumulative frequency as a percentage proportion
    
    gf_data[i + 1,6] <-
      paste0(format(round((gf_data[i + 1,5] / nrow(dfr)) * 100, digits = 1),
                    nsmall = 1), " %")
    
  }
  
  as.data.frame(gf_data)
  
}

# ==============================================================================
# ===========================   USER INTERFACE   ===============================
# ==============================================================================

ui <- fluidPage(
  
  theme = shinytheme("cosmo"),
  
  # Title
  
  tags$h2("Interactive Figure B.1.2 from Lecture notes B.1 - Tabular & graphical summary representations of frequencies"),
  tags$h3("author: Justin Wild, Ph.D."),
  tags$h3("course: EDUC-Y 502, Fall 2023, section #1725"),
  
  # Text
  
  tags$h4("This interactive figure allows you to choose the minimum, maximum, interval width, and decimal places for a grouped frequency table of the student scale (i.e., aggregate) variable ", em(strong("National index of students’ socioeconomic background (S_NISB)")), " from the ICILS 2018 study. The theoretical minimum for this variable is -3 and its theoretical maximum is 3. We will look at this variable round to two decimal places. You will get warnings if you choose values that are not possible, or try to construct a grouped frequency table that goes against the recommendations of the course's textbook. However, you can still create your table even if you recieve warnings. Have fun!"),
  
  # 01. Get input for grouped frequency table ==============
  
  tags$h2("Submit information for group frequency table"),
  
  wellPanel(
    
    numericInput(inputId = "minimum",  label = "Choose the minimum value",
                 value = -3,   min = -3, max = 3, step = 1),
    
    numericInput(inputId = "maximum",  label = "Choose the maximum value",
                 value = 3, min = -3, max = 3, step = 1),
    
    numericInput(inputId = "intwidth", label = "Choose the interval width",
                 value = 0.5,  min = 0.25, max = 2,  step = 0.25),
    
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
      
      actionButton(inputId = "createTable",
                   label = "Create grouped frequency table")
      
    )
    
  ),
  
  # 03. Create table =======================================
  
  DT::dataTableOutput(outputId = "table")
  
)

#===============================================================================
#============================       SERVER       ===============================
#===============================================================================

server <- function(input, output, session){
  
  # 02a. Create warning about minimum ======================
  
  textWarningMinimum <- eventReactive(input$checkInputs, {
    
    if (input$minimum < -3) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The minimum is below -3, which is not theoretically possible for this variable. This will result in meaningless intervals. Please select a minimum value at or above -3 and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$minimum > 3) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The ', em('minimum'), ' is above 3, which is above the theoretical ', em(strong('maximum')), ' for this variable. This will result in meaningless intervals. Please select a minimum value below 3 and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$minimum > input$maximum) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The minimum is ', em(strong('greater')), ' than the maximum. This will result in meaningless intervals. Please select a minimum value below the maximum or a maximum value above the minimum and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else {
      
      tags$h5('Minimum value is ok.')
      
    }
    
  })
  
  output$warningMinimum <- renderUI({textWarningMinimum()})
  
  # 02b. Create warning about maximum ======================
  
  textWarningMaximum <- eventReactive(input$checkInputs, {
    
    if (input$maximum > 3) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The maximum is above 3, which is not theoretically possible for this variable. This will result in meaningless intervals. Please select a maximum value at or below 3 and re-click the "Check minimum, maximum, and interval width" button.')
      
    } else if (input$maximum < -3) {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The ', em('maximum'), ' is below -3, which is below the theoretical ', em(strong('minimum')), ' for this variable. This will result in meaningless intervals. Please select a maximum value above -3 and re-click the "Check minimum, maximum, and interval width" button.')
      
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
  
  # 03. Create table =======================================
  
  gfTable <- shiny::eventReactive(input$createTable, {
    
    group.freq.table(input$minimum, input$maximum, input$intwidth, df1,
                     vbl)
    
  })
  
  output$table <- DT::renderDataTable(
    gfTable(),
    caption = paste0("S_NISB - ",
                     attr(df1$S_NISB, "label"),
                     ", n = ", length(na.omit(df1$S_NISB))),
    options = list(
      columnDefs = list(list(className = "dt-left",  targets = c(1:2)),
                        list(className = "dt-right", targets = c(3:6))),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20))
  )
  
}

#===============================================================================
#============================        RUN         ===============================
#===============================================================================

shinyApp(ui = ui, server = server)

# END ==========================================================================
