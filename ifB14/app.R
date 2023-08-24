# ==============================================================================

# Course:        EDUC-Y 502 Intermediate statistics applied to education
# File:          Lecture notes B.1 - Tabular & graphical summary representations
#                of frequencies
# Author:        Justin Wild, Ph.D.
# Date modified: 24.08.2023
# Purpose:       Create Interactive Figure B.1.4

# Load packages ================================================================

library(dplyr)       # version 1.1.2
library(DT)          # version 0.28
library(haven)       # version 2.5.2
library(shiny)       # version 1.7.4
library(shinythemes) # version 1.2.0

# Load data ====================================================================

df1 <- get(load("data/ICILS_2018_USA_Sch_Stu.Rdata"))

# Load objects =================================================================

decimal  <- 0
minimum  <- 30
maximum  <- 100
vbl      <- "CIL"

wrapper <- function(x, ...) {
  
  paste(strwrap(x, ...), collapse = "\n")
  
}

# Create function to create table then use in server ===========================

group.freq.table <- function(dcm, min, max, iwd, dfr, vrb) {
  
  # Create empty grouped frequency table with column names
  
  gf_data <- as.data.frame(matrix(data = 0, nrow = 1, ncol = 6))
  
  colnames(gf_data) <- c("apparent limits", "real limits", "f",
                         "rel f (percentage)", "cumulative f",
                         "cumulative f (percentage)")
  
  # Fill empty grouped frequency table based on user input
  
  for (i in 0:((max - min) / as.numeric(iwd)) - 1) {
    
    # Get a character string of apparent limits and real limits, and bounds for
    # frequency counts
    
    # Real limits
    
    gf_data[i + 1,1] <- paste0(format(round(min + as.numeric(iwd)*i + 1, digits = 0),
                                      nsmall = 0),
                               " - ",
                               format(round(min + as.numeric(iwd)*i + as.numeric(iwd), digits = 0),
                                      nsmall = 0))
    
    # Apparent limits
    
    gf_data[i + 1,2] <- paste0(format(round(min + as.numeric(iwd)*i + 1 - 0.5, digits = 1),
                                      nsmall = 1),
                               " - ", 
                               format(round(min + as.numeric(iwd)*i + as.numeric(iwd) + 0.5, digits = 1),
                                      nsmall = 1))
    
    # Bounds
    
    lo_bound <- min + as.numeric(iwd)*i + 1 - 0.5
    hi_bound <- min + as.numeric(iwd)*i + as.numeric(iwd) + 0.5
    
    # Get frequency count
    
    gf_data[i + 1,3] <- sum(dfr[,vrb] >= lo_bound & dfr[,vrb] < hi_bound)
    
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
  
  gf_data <- as.data.frame(apply(X = gf_data, MARGIN = 2, FUN = rev))
  
  rownames(gf_data) <- NULL
  
  as.data.frame(gf_data)
  
}

# ==============================================================================
# ===========================   USER INTERFACE   ===============================
# ==============================================================================

ui <- fluidPage(theme = shinytheme("cosmo"),
  
  # Title
  
  tags$h2("Interactive Figure B.1.4 from Lecture notes B.1 - Tabular & graphical summary representations of frequencies"),
  tags$h3("author: Justin Wild, Ph.D."),
  tags$h3("course: EDUC-Y 502, Fall 2023, section #1725"),
  
  # Text
  
  tags$h4("This interactive figure allows you to choose the interval width for a grouped frequency table and histogram of the student achievement variable ", em(strong("Computer and Information Literacy (CIL)")), " from the ICILS 2018 study. Have fun!"),
  
  # 01. Get input for grouped frequency table ==============
  
  tags$h2("Choose an interval width"),
  
  wellPanel(
    
    radioButtons(inputId = "intwidth", label = "Choose the interval width",
                 choices = c(1,2,5,10,35), selected = 10, inline = T),
    
    actionButton(inputId = "submitInputs",
                 label = "Submit interval width")
    
  ),
  
  # 02. Get table and graph ================================
  
  conditionalPanel(
    
    condition = "input.submitInputs",
    
    fluidRow(
      
      # 02a. Grouped frequency table ===
      
      column(width = 6,
             
             wellPanel(
               
               DT::dataTableOutput(outputId = "table")
               
             )
             
      ),
      
      # 02b. Histogram =================
      
      column(width = 6,
             
             wellPanel(
               
               plotOutput(outputId = "histogram")
               
             )
             
      )
      
    )
    
  )
  
)

#===============================================================================
#============================       SERVER       ===============================
#===============================================================================

server <- function(input, output, session){
  
  # 02a. Create table ======================================

  gfTable <- shiny::eventReactive(input$submitInputs, {

    group.freq.table(decimal, minimum, maximum, input$intwidth, df1, vbl)

  })

  output$table <- DT::renderDataTable(
    gfTable(),
    caption = paste0("CIL - ", attr(df1$CIL, "label"),
                     ", n = ", length(stats::na.omit(df1$CIL))),
    options = list(
      columnDefs = list(list(className = "dt-left",  targets = c(1:2)),
                        list(className = "dt-right", targets = c(3:6))),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20))
  )
  
  hgPlot <- shiny::eventReactive(input$submitInputs, {
    
    ggplot2::ggplot(df1, ggplot2::aes(CIL)) +
      ggplot2::geom_histogram(boundary = 30, color = "#000000", fill = "#56B4E9",
                              binwidth = as.numeric(input$intwidth), na.rm = T) +
      ggplot2::labs(title = wrapper(paste0("CIL - ", attr(df1$CIL, "label"),
                                           ", n = ",
                                           length(stats::na.omit(df1$CIL))))) +
      ggplot2::xlab(attr(df1$CIL, "label")) +
      ggplot2::ylab("n")
    
  })
  
  output$histogram <- renderPlot({hgPlot()})
  
}

#===============================================================================
#============================        RUN         ===============================
#===============================================================================

shinyApp(ui = ui, server = server)

# END ==========================================================================
