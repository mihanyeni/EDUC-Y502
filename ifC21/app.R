# ==============================================================================

# Course:        EDUC-Y 502 Intermediate statistics applied to education
# File:          Lecture notes C.2 Sampling & probability
# Author:        Justin Wild, Ph.D.
# Date modified: 12.09.2023
# Purpose:       Create Interactive Figure C.2.1

# Load packages ================================================================

library(dplyr)   # version 1.1.2
library(ggplot2) # version 3.4.2
library(shiny)   # version 1.7.4
library(stats)   # version 4.3.1

# Load data ====================================================================

load("data/ICCS_2016_FIN_Sch_Stu.Rdata")

# Load function ================================================================

wrapper <- function(x, ...) {
  
  paste(strwrap(x, ...), collapse = "\n")
  
}

# ==============================================================================
# ===========================   USER INTERFACE   ===============================
# ==============================================================================

ui <- fluidPage(
  
  # Title
  
  tags$h2("Interactive Figure C.2.1 from ", em("Lecture notes C.2 Sampling & probability")),
  tags$h5("author: Justin Wild, Ph.D."),
  tags$h5("course: EDUC-Y 502, Fall 2023, section #1725"),
  
  # Text
  
  tags$span("This interactive figure allows you to choose the sample size, number of samples, and whether to sample with or without replacement to view the sample means distribution of the variable ", em(strong("Students’ civic participation in the wider community (S_COMPART)")), " from the ICCS 2016 study. Theoretically, as this data set has a size of 500 observations, you should not go over 500 for your sample size. However, you can...what will happen if you do? Have fun!"),
  
  # 01. Show density plot of variable ======================
  
  wellPanel(
  
    plotOutput(outputId = "density"),
  
  ),
  
  fluidRow(
    
    # 02. Input sample size ================================
    
    column(width = 4,
           
           wellPanel(
             
             numericInput(inputId = "sampsize",
                          label = "Input your sample size:",
                          value = 100)
             
           )
           
    ),
    
    # 03. Input number of samples ==========================
    
    column(width = 4,
           
           wellPanel(
             
             radioButtons(inputId = "sampnumb",
                          label = "How many samples would you like?",
                          choices = c(5,10,20,50,100),
                          selected = 100, inline = T)
             
           )
           
    ),
    
    # 04. Input with or without replacement ================
    
    column(width = 4,
           
           wellPanel(
             
             radioButtons(inputId = "replace",
                          label = "Do you want to sample with or without replacement?",
                          choices = c("With replacement" = 0,
                                      "Without replacement" = 1),
                          selected = 0,
                          inline = F),
             
           )
           
    )
    
  ),
  
  fluidRow(
    
    column(width = 4,
           
           wellPanel(
             
             actionButton(inputId = "submitChoice", label = "Submit your choice")
             
           )
           
    ),
    
    column(width = 8,
           
           wellPanel(
           
             paste0('Without changing the inputs above, click the "Submit your choice" button again. The results will change as new samples of the same size will be taken. This shows that not all sampling distributions are the same, even when taking the same number of samples and the same size of samples!')
             
           )
           
    )
    
  ),
  
  conditionalPanel(
    
    condition = "input.submitChoice",
    
    # 05. Get sample density plot ==========================
    
    wellPanel(
      
      uiOutput(outputId = "sampstats")
      
    ),
    
    # 05. Get text about stats =============================
    
    wellPanel(
      
      plotOutput(outputId = "sampdensity")
      
    )
    
  )
  
)

#===============================================================================
#============================       SERVER       ===============================
#===============================================================================

server <- function(input, output, session){
  
  # 01. Show density plot of variable ======================

  output$density <- renderPlot({
    
    ggplot2::ggplot(ICCS_2016_FIN_SCH_STU, ggplot2::aes(x = S_COMPART)) + 
      ggplot2::geom_density(na.rm = TRUE) +
      ggplot2::labs(title =
        wrapper(paste0("S_COMPART - students’ civic participation in the wider community, n = ",
                       length(stats::na.omit(ICCS_2016_FIN_SCH_STU$S_COMPART))),
                width = 100)) +
      ggplot2::xlab("Students’ civic participation in the wider community (S_COMPART)")
    
  })
  
  # 05. Get sample density plot ============================
  
  sample_means <- eventReactive(input$submitChoice, {
    
    if (input$replace == 0) {
      
      log_replace <- TRUE
      
    } else if (input$replace == 1) {
      
      log_replace <- FALSE
      
    }
    
    df_vector <- stats::na.omit(ICCS_2016_FIN_SCH_STU$S_COMPART)
    
    sample_mean <- c()

    for (i in 1:input$sampnumb) {

      df_samp <-
        sample(df_vector, size = input$sampsize, replace = log_replace)
      
      sample_mean[i] <- mean(df_samp)

    }
    
    as.vector(sample_mean)
    
  })
  
  sampDens <- eventReactive(input$submitChoice, {
    
    if (input$sampnumb == 5) {
      
      hist_int <- 5
      
    } else if (input$sampnumb == 10 | input$sampnumb == 20) {
      
      hist_int <- 10
      
    } else if (input$sampnumb == 50) {
      
      hist_int <- 25
      
    } else if (input$sampnumb == 100) {
      
      hist_int <- 25
      
    }
    
    df <- dplyr::bind_cols(1:input$sampnumb, sample_means())
    
    colnames(df) <- c("sample","sample_mean")
    
    ggplot2::ggplot(df, ggplot2::aes(x = sample_mean)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              color = "#000000", fill = "#56B4E9",# boundary = 45
                              bins = hist_int, na.rm = T) +
      ggplot2::geom_density(na.rm = TRUE) +
      ggplot2::labs(title =
        wrapper(paste0(input$sampnumb,
                       " sample means of the variable S_COMPART from sample sizes of ",
                       input$sampsize, "."),
                width = 100)) +
      ggplot2::xlab("Students’ participation in the wider community (S_COMPART)")
    
  })
  
  output$sampdensity <- renderPlot({sampDens()})
  
  # 05. Get text about stats ===============================
  
  textSampStats <- eventReactive(input$submitChoice, {
    
    if (input$replace == 0) {
      
      text_replace <- " (with replacement) "
      
    } else if (input$replace == 1) {
      
      text_replace <- " (without replacement) "
      
    }
    
    paste0("The population mean for S_COMPART is 42.71. The mean of sample means from ",
           input$sampnumb, " samples of size ", input$sampsize,
           " observations", text_replace, "is ",
           format(round(mean(sample_means()), digits = 2), nsmall = 2),
           " with a standard error of ",
           format(round(stats::sd(sample_means())/sqrt(input$sampsize),
                        digits = 2),
                  nsmall = 2), ".")
    
  })
  
  output$sampstats <- renderUI({textSampStats()})
  
}

#===============================================================================
#============================        RUN         ===============================
#===============================================================================

shinyApp(ui = ui, server = server)

# END ==========================================================================
