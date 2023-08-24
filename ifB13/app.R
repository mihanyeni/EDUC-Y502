# ==============================================================================

# Course:        EDUC-Y 502 Intermediate statistics applied to education
# File:          Lecture notes B.1 - Tabular & graphical summary representations
#                of frequencies
# Author:        Justin Wild, Ph.D.
# Date modified: 24.08.2023
# Purpose:       Create Interactive Figure B.1.3

# Load packages ================================================================

library(dplyr)       # version 1.1.2
library(DT)          # version 0.28
library(haven)       # version 2.5.2
library(shiny)       # version 1.7.4
library(shinythemes) # version 1.2.0

# Load data ====================================================================

df1 <- get(load("/Users/wildden/Wildman/Work/IU Teaching/Y502 - Intermediate Statistics Applied to Education/Data/ICILS_2018_USA_Sch_Stu.Rdata"))

# Load objects =================================================================

decimal  <- 0
minimum  <- 30
maximum  <- 100
intwidth <- 10
vbl      <- "CIL"

# Create function to create table then use in server ===========================

group.freq.table <- function(dcm, min, max, iwd, dfr, vrb) {

  # Create empty grouped frequency table with column names

  gf_data <- as.data.frame(matrix(data = 0, nrow = 1, ncol = 6))

  colnames(gf_data) <- c("apparent limits", "real limits", "f",
                         "rel f (percentage)", "cumulative f",
                         "cumulative f (percentage)")

  # Fill empty grouped frequency table based on user input

  for (i in 0:((max - min) / iwd) - 1) {

    # Get a character string of apparent limits and real limits, and bounds for
    # frequency counts
    
    # Real limits
    
    gf_data[i + 1,1] <- paste0(format(round(min + iwd*i + 1, digits = 0),
                                      nsmall = 0),
                               " - ",
                               format(round(min + iwd*i + iwd, digits = 0),
                                      nsmall = 0))
    
    # Apparent limits
    
    gf_data[i + 1,2] <- paste0(format(round(min + iwd*i + 1 - 0.5, digits = 1),
                                      nsmall = 1),
                               " - ", 
                               format(round(min + iwd*i + iwd + 0.5, digits = 1),
                                      nsmall = 1))
    
    # Bounds
    
    lo_bound <- min + iwd*i + 1 - 0.5
    hi_bound <- min + iwd*i + iwd + 0.5
    
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
  
  tags$h2("Interactive Figure B.1.3 from Lecture notes B.1 - Tabular & graphical summary representations of frequencies"),
  tags$h3("author: Justin Wild, Ph.D."),
  tags$h3("course: EDUC-Y 502, Fall 2023, section #1725"),
  
  # Text
  
  tags$h4("This interactive figure allows you to choose the needed values to compute a percentile (point) or percentile rank for the student achievement variable ", em(strong("Computer and Information Literacy (CIL)")), " from the ICILS 2018 study. You will be given a grouped frequency table with an interval width of 10 and definitions of the values you need to find. Once you select whether you want to find a percentile (point) or percentile rank, you will be given a random value below the table of definitions, along with areas to input values. If you make a mistake(s) you will receive a warning(s) and an explanation(s), followed by the answer. Have fun!"),
  
  tags$h5("Of note, you may need to widen the browser window to be able to see the tables well."),
  
  # 01. Get input for percentile or percentile rank ========
  
  wellPanel(
    
    radioButtons(inputId = "percentile",
                 label = "Do you want to find a percentile or percentile rank?",
                 choices = c("Percentile" = 0,
                             "Percentile rank" = 1),
                 selected = 0,
                 inline = T),
    
    actionButton(inputId = "submitChoice", label = "Submit your choice")
    
  ),
  
  # 02. Create tables and inputs ===========================
  
  conditionalPanel(
    
    condition = "input.submitChoice",
    
    fluidRow(
      
      # 02a. Grouped frequency table ===
      
      column(width = 6,
             
             wellPanel(
               
               DT::dataTableOutput(outputId = "gfTable")
               
             )
             
      ),
      
      column(width = 6,
             
             # 02bi. Symbol table ======
             
             wellPanel(
               
               DT::dataTableOutput(outputId = "inputTable")
               
             ),
             
             # 02bii. What to find =====
             
             wellPanel(
               
               uiOutput(outputId = "find")
               
             )
      )
      
    ),
    
    wellPanel(
      
      fluidRow(
        
        tags$h4("Given the information in the grouped frequency table above, please input the requested values below.")
        
      ),
      
      fluidRow(
        
        # 02c. Input 1 =================
        
        column(width = 4,
               
               uiOutput(outputId = "input1")
               
        ),
        
        # 02d. Input 2 =================
        
        column(width = 4,
               
               uiOutput(outputId = "input2")
               
        ),
        
        # 02e. Input 3 =================
        
        column(width = 4,
               
               uiOutput(outputId = "input3")
               
        )
        
      ),
      
      fluidRow(
        
        # 02f. Input 4 =================
        
        column(width = 4,
               
               uiOutput(outputId = "input4")
               
        ),
        
        # 02g. Input 5 =================
        
        column(width = 4,
               
               uiOutput(outputId = "input5")
               
        ),
        
        # 02h. Input 6 =================
        
        column(width = 4,
               
               uiOutput(outputId = "input6")
               
        )
        
      )
      
    ),
    
    wellPanel(
      
      actionButton(inputId = "submitValues", label = "Submit values")
      
    )
    
  ),
  
  # 03. Warnings ===========================================
  
  conditionalPanel(
    
    condition = "input.submitValues",
    
    wellPanel(
      
      # 03a. Create warning about Input1
      
      uiOutput(outputId = "warningInput1"),
      
      # 03b. Create warning about Input2
      
      uiOutput(outputId = "warningInput2"),
      
      # 03c. Create warning about Input3
      
      uiOutput(outputId = "warningInput3"),
      
      # 03d. Create warning about Input4
      
      uiOutput(outputId = "warningInput4"),
      
      # 03e. Create warning about Input5
      
      uiOutput(outputId = "warningInput5"),
      
      # 03f. Create warning about Input6
      
      uiOutput(outputId = "warningInput6"),
      
      # 03g. Answer ====================
      
      uiOutput(outputId = "answer")
      
    )
    
  )
  
)

#===============================================================================
#============================       SERVER       ===============================
#===============================================================================

server <- function(input, output, session){
  
  # 02. Create tables and inputs ===========================
  
  # 02a. Grouped frequency table =======
  
  gfTable <- shiny::eventReactive(input$submitChoice, {
    
    group.freq.table(decimal, minimum, maximum, intwidth, df1, vbl)
    
  })
  
  output$gfTable <- DT::renderDataTable(
    gfTable(),
    caption = paste0("CIL - ", attr(df1$CIL, "label"),
                     ", n = ", length(stats::na.omit(df1$CIL))),
    options = list(
      columnDefs = list(list(className = "dt-left",  targets = c(1:2)),
                        list(className = "dt-right", targets = c(3:6))),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20))
  )
  
  # 02bi. Symbol table =================
  
  syTable <- shiny::eventReactive(input$submitChoice, {
    
    if (input$percentile == 0) {
      
      df_sy <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 2))
      
      colnames(df_sy) <- c("Symbol", "Meaning")
      
      df_sy[1,1] <- "LL"
      df_sy[1,2] <- "Lower real limit of class interval containing the percentile"
      
      df_sy[2,1] <- "i"
      df_sy[2,2] <- "Width of class interval"
      
      df_sy[3,1] <- "cum f percentile"
      df_sy[3,2] <- "Number of scores lying below the percentile"
      
      df_sy[4,1] <- "cum f below"
      df_sy[4,2] <- "Number of scores lying below LL"
      
      df_sy[5,1] <- "f"
      df_sy[5,2] <- "Frequency of scores in the interval containing the percentile"
      
      as.data.frame(df_sy)
      
    } else if (input$percentile == 1) {
      
      df_sy <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 2))
      
      colnames(df_sy) <- c("Symbol", "Meaning")
      
      df_sy[1,1] <- "cum f below"
      df_sy[1,2] <- "Number of scores lying below LL"
      
      df_sy[2,1] <- "x"
      df_sy[2,2] <- "Score"
      
      df_sy[3,1] <- "LL"
      df_sy[3,2] <- "Lower real limit of class interval containing the percentile"
      
      df_sy[4,1] <- "i"
      df_sy[4,2] <- "Width of class interval"
      
      df_sy[5,1] <- "f"
      df_sy[5,2] <- "Frequency of scores in the interval containing the percentile"
      
      df_sy[6,1] <- "N"
      df_sy[6,2] <- "Number of scores in the distribution"
      
      as.data.frame(df_sy)
      
    }
    
  })
  
  output$inputTable <- DT::renderDataTable(
    syTable(),
    caption = "Symbols and their meaning",
    options = list(
      columnDefs = list(list(className = "dt-left", targets = c(1:2))),
      pageLength = 20,
      lengthMenu = c(5, 10, 15, 20))
  )
  
  # 02bii. What to find ================
  
  valueP <- eventReactive(input$submitChoice, {
    
    if (input$percentile == 0) {
      
      findP <- sample(x = 1:100, size = 1)
      
    } else if (input$percentile == 1) {
      
      findP <- sample(x = 30:100, size = 1)
      
    } 
    
    as.numeric(findP)
    
  })
  
  charP <- eventReactive(input$submitChoice, {
    
    if (input$percentile == 0) {
      
      last_digit <- valueP() %% 10
      
      if (last_digit == 1) {
        
        P <- paste0(valueP(),"st")
        
      } else if (last_digit == 2) {
        
        P <- paste0(valueP(),"nd")
        
      } else if (last_digit == 3) {
        
        P <- paste0(valueP(),"rd")
        
      } else {
        
        P <- paste0(valueP(),"th")
        
      }
      
      as.character(P)
      
    }
    
  })

  output$find <- renderUI({
    
    if (input$percentile == 0) {
      
      tags$h4("Find the",
              tags$span(style = "color:red", charP()),
              "percentile")
      
    } else if (input$percentile == 1) {
      
      tags$h4("Find the percentile rank for the score",
              tags$span(style = "color:red", valueP()))
      
    }
    
  })
  
  # 02c. Input 1 =======================
  
  output$input1 <- renderUI({
    
    if (input$percentile == 0) {
      
      textInput(inputId = "inputLLp", label = "LL:", value="")
      
    } else if (input$percentile == 1) {
      
      textInput(inputId = "inputcumfBelowpr", label = "cum f below:", value="")
      
    } 
    
  })
  
  # 02d. Input 2 =======================
  
  output$input2 <- renderUI({
    
    if (input$percentile == 0) {
      
      textInput(inputId = "inputip", label = "i:", value="")
      
    } else if (input$percentile == 1) {
      
      textInput(inputId = "inputxpr", label = "x:", value="")
      
    } 
    
  })
  
  # 02e. Input 3 =======================
  
  output$input3 <- renderUI({
    
    if (input$percentile == 0) {
      
      textInput(inputId = "inputcumfPercentilep", label = "cum f percentile:",
                value="")
      
    } else if (input$percentile == 1) {
      
      textInput(inputId = "inputLLpr", label = "LL:", value="")
      
    } 
    
  })
  
  # 02f. Input 4 =======================
  
  output$input4 <- renderUI({
    
    if (input$percentile == 0) {
      
      textInput(inputId = "inputcumfBelowp", label = "cum f below:", value="")
      
    } else if (input$percentile == 1) {
      
      textInput(inputId = "inputipr", label = "i:", value="")
      
    } 
    
  })
  
  # 02g. Input 5 =======================
  
  output$input5 <- renderUI({
    
    if (input$percentile == 0) {
    
      textInput(inputId = "inputfp", label = "f:", value="")
      
    } else if (input$percentile == 1) {
      
      textInput(inputId = "inputfpr", label = "f:", value="")
      
    }
    
  })
  
  # 02h. Input 6 =======================
  
  output$input6 <- renderUI({
    
    if (input$percentile == 0) {
      
      tags$h5("")
      
    } else if (input$percentile == 1) {
      
      textInput(inputId = "inputNpr", label = "N:", value="")
      
    } 
    
  })
  
  # 03. Warnings ===========================================
  
  # Get values =========================
  
  valueRow <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      for (i in 2:nrow(gfTable())) {
        
        if (valueP() < as.numeric(strsplit(x = gfTable()[i,6], split = " ")[[1]][1])) {
          
          next
          
        } else {
          
          work_row <- i - 1
          
          break
          
        }
        
      }
      
      as.numeric(work_row)
      
    } else if (input$percentile == 1) {
      
      for (i in 2:nrow(gfTable())) {
        
        lrl <- as.numeric(strsplit(x = gfTable()[i,2], split = " ")[[1]][1])
        url <- as.numeric(strsplit(x = gfTable()[i,2], split = " ")[[1]][3])
        
        if (valueP() > lrl & valueP() < url) {
          
          work_row <- i
          
          break
          
        } else {
          
          next
          
        }
        
      }
      
      as.numeric(work_row)
      
    }
    
  })
  
  nameRow <- eventReactive(input$submitValues, {
    
    if (valueRow() == 1) {
      
      name_work_row <- "1st"
      
    } else if (valueRow() == 2) {
      
      name_work_row <- "2nd"
      
    } else if (valueRow() == 3) {
      
      name_work_row <- "3rd"
      
    } else {
      
      name_work_row <- paste0(valueRow(), "th")
      
    }
    
    as.character(name_work_row)
    
  })
  
  valueLL <- eventReactive(input$submitValues, {
    
    as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][1])
    
  })
  
  # valuei = 10
  
  valuecumfPercentile <- eventReactive(input$submitValues, {
    
    as.numeric(valueP()/100 * 500)
    
  })
  
  valuecumfBelow <- eventReactive(input$submitValues, {
    
    as.numeric(gfTable()[valueRow() + 1,5])
    
  })
  
  valuef <- eventReactive(input$submitValues, {
    
    as.numeric(gfTable()[valueRow(),3])
    
  })
  
  # valuex = findP
  
  # valueN = 500
  
  # 03a. Create warning about Input1 ===

  textWarningInput1 <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      if (as.numeric(input$inputLLp) == valueLL()) {
        
        tags$h5("'LL' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(charP()), ' percentile must be in the ', nameRow(), ' row as it is below ', gfTable()[valueRow(),6], ' and above  ', gfTable()[valueRow() + 1,6], '. Therefore, the lower real limit, or ', em('LL'), ', is ', valueLL(), '.')
        
      }
      
    } else if (input$percentile == 1) {
      
      if (as.numeric(input$inputcumfBelowpr) == valuecumfBelow()) {
        
        tags$h5("'cum f below' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(valueP()), 'is in the ', nameRow(), ' row as it is between ', as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][1]), ' and  ', as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][3]), '. Therefore, the cumulative frequency below this row is ', valuecumfBelow(), '.')
        
      }
      
    }
    
  })

  output$warningInput1 <- renderUI({textWarningInput1()})
  
  # 03b. Create warning about Input2 ===
  
  textWarningInput2 <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      if (as.numeric(input$inputip) == 10) {
        
        tags$h5("'i' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The interval width is 10. Take the difference between any upper and lower real limits. For example, from the second row 90.5 - 80.5 = 10.')
        
      }
      
    } else if (input$percentile == 1) {
      
      if (as.numeric(input$inputxpr) == valueP()) {
        
        tags$h5("x is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The score you are trying to find the percentile rank for is ', valueP(), '.')
        
      }
      
    }
    
  })
  
  output$warningInput2 <- renderUI({textWarningInput2()})
  
  # 03c. Create warning about Input3 ===
  
  textWarningInput3 <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      if (as.numeric(input$inputcumfPercentilep) == valuecumfPercentile()) {
        
        tags$h5("'cum f percentile' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(charP()), ' percentile must be in the ', nameRow(), ' row as it is below ', gfTable()[valueRow(),6], ' and above  ', gfTable()[valueRow() + 1,6], '. Therefore, the cumulative frequency below the ', em(charP()), ' percentile is ', em(valueP()),  '% multiplied by the total number of observations, 500, or ', valuecumfPercentile(), ', which is between the the cumulative frequency of the row below, ', gfTable()[valueRow() + 1,5], ' and the cumulative frequency of the ', nameRow(), ' row,', gfTable()[valueRow(),5], '.')
        
      }
      
    } else if (input$percentile == 1) {
     
      if (as.numeric(input$inputLLpr) == valueLL()) {
        
        tags$h5("'LL' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(valueP()), 'is in the ', nameRow(), ' row as it is between ', as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][1]), ' and  ', as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][3]), '. Therefore, the lower real limit, or ', em('LL'), ', is ', valueLL(), '.')
        
      }
       
    }
    
  })
  
  output$warningInput3 <- renderUI({textWarningInput3()})
  
  # 03d. Create warning about Input4 ===
  
  textWarningInput4 <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      if (as.numeric(input$inputcumfBelowp) == valuecumfBelow()) {
        
        tags$h5("'cum f below' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(charP()), ' percentile must be in the ', nameRow(), ' row as it is below ', gfTable()[valueRow(),6], ' and above  ', gfTable()[valueRow() + 1,6], '. Therefore, the cumulative frequency below this row is ', valuecumfBelow(), '.')
        
      }
      
    } else if (input$percentile == 1) {
      
      if (as.numeric(input$inputipr) == 10) {
        
        tags$h5("'i' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The interval width is 10. Take the difference between any upper and lower real limits. For example, from the second row 90.5 - 80.5 = 10.')
        
      }
      
    }
    
  })
  
  output$warningInput4 <- renderUI({textWarningInput4()})
  
  # 03e. Create warning about Input5 ===
  
  textWarningInput5 <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
    
      if (as.numeric(input$inputfp) == valuef()) {
      
      tags$h5("'f' is ok!")
      
    } else {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(charP()), ' percentile must be in the ', nameRow(), ' row as it is below ', gfTable()[valueRow(),6], ' and above  ', gfTable()[valueRow() + 1,6], '. Therefore, the frequency of this row is ', valuef(), '.')
      
    }
      
    } else if (input$percentile == 1) {
      
      if (as.numeric(input$inputfpr) == valuef()) {
        
        tags$h5("'f' is ok!")
        
      } else {
      
      tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': ', em(valueP()), 'is in the ', nameRow(), ' row as it is between ', as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][1]), ' and  ', as.numeric(strsplit(x = gfTable()[valueRow(),2], split = " ")[[1]][3]), '. Therefore, the frequency of this row is ', valuef(), '.')
        
      }
      
    }
    
  })
  
  output$warningInput5 <- renderUI({textWarningInput5()})
  
  # 03f. Create warning about Input6 ===
  
  textWarningInput6 <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      as.character("")
      
    } else if (input$percentile == 1) {
      
      if (as.numeric(input$inputNpr) == 500) {
        
        tags$h5("'N' is ok!")
        
      } else {
        
        tags$h4(tags$span(style = "color:red", "WARNING", .noWS = "after"), ': The total number of observations is 500.')
        
      }
      
    }
    
  })
  
  output$warningInput6 <- renderUI({textWarningInput6()})
  
  # 03g. Get answer ====================
  
  textAnswer <- eventReactive(input$submitValues, {
    
    if (input$percentile == 0) {
      
      perc <- valueLL() + (10 * 
        ((valuecumfPercentile() - valuecumfBelow()) / valuef()))
      
      perc <- format(round(perc, digits = 2), nsmall = 2)
      
      tags$h4("The ", charP(), " percentile is ", perc)
      
    } else  if (input$percentile == 1) {
      
      perc <- 100 * ((valuecumfBelow() +
                        (((valueP() - valueLL()) / 10) * valuef())) / 
                        500)
      
      perc <- format(round(perc, digits = 2), nsmall = 2)
      
      tags$h4("The percentile rank of ", valueP(), " is ", perc)
      
    }
    
  })
  
  output$answer <- renderUI({textAnswer()})
  
}

#===============================================================================
#============================        RUN         ===============================
#===============================================================================

shinyApp(ui = ui, server = server)

# END ==========================================================================
