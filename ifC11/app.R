# ==============================================================================

# Course:        EDUC-Y 502 Intermediate statistics applied to education
# File:          Lecture notes C.1 The Normal Curve
# Author:        Justin Wild, Ph.D.
# Date modified: 12.09.2023
# Purpose:       Create Interactive Figure C.1.1

# Load packages ================================================================

library(graphics) # version 4.3.1
library(shiny)    # version 1.7.4

# Create function to create a shaded normal curve ==============================

shadenorm <- function(below     = NULL,            # Sets lower endpoint
                      above     = NULL,            # Sets upper endpoint
                      pcts      = c(0.025, 0.975), # Sets alpha?
                      mu        = 0,               # Sets mean
                      sigma     = 1,               # Sets standard deviation
                      numpts    = 500,             # Sets number of points in
                                                   #   plotting sequence
                      color     = "gray",          # Sets color
                      dens      = 40,              # Sets color density
                      justabove = FALSE,           # Sets shading as just above
                      justbelow = FALSE,           # Sets shading as just below
                      lines     = FALSE,           # If "TRUE" plots on top of
                                                   #   previous plot
                      between   = NULL,            # Colors between points
                      outside   = NULL) {          # Colors outside of points
  
  if (is.null(between)) {
    
    below = ifelse(is.null(below), qnorm(pcts[1], mu, sigma), below)
    above = ifelse(is.null(above), qnorm(pcts[2], mu, sigma), above)
    
  }
  
  if (is.null(outside) == FALSE) {
    
    below = min(outside)
    above = max(outside)
    
  }
  
  lowlim = mu - 4 * sigma
  upplim = mu + 4 * sigma
  
  x.grid   = seq(lowlim, upplim, length = numpts)
  dens.all = dnorm(x.grid, mean = mu, sd = sigma)
  
  if (lines == FALSE) {
    
    plot(x.grid, dens.all, type = "l", xlab = "x", ylab = "Density")
    
  }
  
  if (lines == TRUE) {
    
    lines(x.grid, dens.all)
    
  }
  
  if (justabove == FALSE) {
    
    x.below    = x.grid[x.grid < below]
    dens.below = dens.all[x.grid < below]
    polygon(c(x.below, rev(x.below)),
            c(rep(0, length(x.below)), rev(dens.below)),
            col = color, density = dens)
    
  }
  
  if (justbelow == FALSE) {
    
    x.above    = x.grid[x.grid > above]
    dens.above = dens.all[x.grid > above]
    polygon(c(x.above, rev(x.above)),
            c(rep(0, length(x.above)), rev(dens.above)),
            col = color, density = dens)
    
  }
  
  if (is.null(between) == FALSE) {
    
    from = min(between)
    to   = max(between)
    x.between    = x.grid[x.grid > from & x.grid < to]
    dens.between = dens.all[x.grid > from & x.grid < to]
    polygon(c(x.between, rev(x.between)),
            c(rep(0, length(x.between)), rev(dens.between)),
            col = color, density = dens)
    
  }
  
}

# ==============================================================================
# ===========================   USER INTERFACE   ===============================
# ==============================================================================

ui <- fluidPage(
  
  # Title
  
  tags$h2("Interactive Figure C.1.1 from ", em("Lecture notes C.1 The Normal Curve")),
  tags$h5("author: Justin Wild, Ph.D."),
  tags$h5("course: EDUC-Y 502, Fall 2023, section #1725"),
  
  # Text
  
  tags$span("This interactive figure allows you to randomly generate a standard deviation above or below the mean and state the percentage of observations shown under the normal curve. Have fun!"),
  
  # 00. Review proportions =================================
  
  fluidRow(
    
    column(width = 6,
           
           wellPanel(
             
             paste0("Let's review the proportions under a normal curve.")
             
           ),
           
           wellPanel(
             
             radioButtons(inputId = "proportion",
                          label = "What proportion do you want to view?",
                          choices = c("Full (100%)" = 0,
                                      "Between -1 and +1" = 1,
                                      "Between -2 and +2" = 2,
                                      "Between -3 and +3" = 3),
                          selected = 0, inline = F)
             
           )
           
    ),
    
    column(width = 6,
           
           wellPanel(
             
             plotOutput(outputId = "proportionCurve")
             
           )
           
    ),
    
    
  ),
  
  # 01. Randomly generate a standard deviation =============
  
  wellPanel(
    
    actionButton(inputId = "stddev",
                 label = "Randomly generate a standard deviation")
    
  ),
  
  # 02. Get value and graphical representation of area =====
  
  conditionalPanel(
    
    condition = "input.stddev",
    
    fluidRow(
      
      column(width = 6,
             
             # 02a. State value ========
             
             wellPanel(
               
               uiOutput(outputId = "find")
               
             ),
             
             # 02c. Provide input ======
             
             wellPanel(
               
               uiOutput(outputId = "input")
               
             ),
             
             # 02d. Submit input =======
             
             wellPanel(
               
               actionButton(inputId = "submitValue", label = "Submit value")
               
             )
             
      ),
      
      # 02b. Show shaded curve =========
      
      column(width = 6,
             
             wellPanel(
               
               plotOutput(outputId = "shadedCurve")
               
             )
             
      )
      
    )
    
  ),
  
  # 03. Warning ============================================
  
  conditionalPanel(
    
    condition = "input.submitValue",
    
    wellPanel(
      
      uiOutput(outputId = "warningInput")
      
    )
    
  )
  
)

#===============================================================================
#============================       SERVER       ===============================
#===============================================================================

server <- function(input, output, session){
  
  # 00. Review proportions =================================
  
  output$proportionCurve <- renderPlot({
    
    if (input$proportion == 0) {
      
      shadenorm(between = c(-4, 4), color = "red")
      
      graphics::text(x = 0, y = 0.15,
                     labels = "100 %",
                     cex = 2, col = "black", font = 2)
      
    } else if (input$proportion == 1) {
      
      shadenorm(between = c(-1, 1), color = "red")
      
      graphics::text(x = 0, y = 0.15,
                     labels = "68 %",
                     cex = 2, col = "black", font = 2)
      
    } else if (input$proportion == 2) {
      
      shadenorm(between = c(-2, 2), color = "red")
      
      graphics::text(x = 0, y = 0.15,
                     labels = "95 %",
                     cex = 2, col = "black", font = 2)
      
    } else {
      
      shadenorm(between = c(-3, 3), color = "red")
      
      graphics::text(x = 0, y = 0.15,
                     labels = "99.7 %",
                     cex = 2, col = "black", font = 2)
      
    }
    
  })
  
  # 01a. Randomly generate a standard deviation ============
  
  valueSD <- eventReactive(input$stddev, {
    
    findSD <- sample(c(-3,-2,-1,1,2,3), size = 1)
    
    as.numeric(findSD)
    
  })
  
  # 01b. Randomly generate above or below ==================
  
  ab_numr <- eventReactive(input$stddev, {
    
    if (sample(c(1,2), size = 1) == 1) {
      
      ab_n <- 1
      
    } else {
      
      ab_n <- 2
      
    }
    
    as.numeric(ab_n)
    
  })
  
  # 02a. State value =======================================

  stateSD <- eventReactive(input$stddev, {
    
    if (valueSD() < 0) {
      
      charSD <- valueSD()
      
    } else {
      
      charSD <- paste0("+", valueSD())
      
    }
    
    as.character(charSD)
    
  })
  
  ab_char <- eventReactive(input$stddev, {
    
    if (ab_numr() == 1) {
      
      ab_c <- "above"
      
    } else {
      
      ab_c <- "below"
      
    }
    
    as.character(ab_c)
    
  })
  
  output$find <- renderUI({
    
    tags$h4("Find the percentage of values ", ab_char(),
            "the standard deviation value of ", stateSD(), ".")
    
  })
  
  # 02b. Show shaded curve =================================
  
  sCurve <- shiny::eventReactive(input$stddev, {
    
    if (ab_numr() == 1 & valueSD() < 0) {
      
      shadenorm(between = c(valueSD(), 4), color = "red")
      
    } else if (ab_numr() == 1 & valueSD() > 0) {
      
      shadenorm(above = valueSD(), justabove = T, color = "red")
      
    } else if (ab_numr() == 2 & valueSD() < 0) {
      
      shadenorm(below = valueSD(), justbelow = T, color = "red")
      
    } else {
      
      shadenorm(between = c(-4, valueSD()), color = "red")
      
    }
    
  })
  
  output$shadedCurve <- renderPlot({sCurve()})
  
  # 02c. Provide input =====================================
  
  output$input <- renderUI({
    
    numericInput(inputId = "guess",
                 label = "What percentage of values is in the shaded area?",
                 value = 0)
    
  })
  
  # 03. Warning ============================================
  
  otherSD <- eventReactive(input$submitValue, {
    
    if (valueSD() == -3) {
      
      other_sd <- paste0("+", 3)
      
    } else if (valueSD() == -2) {
      
      other_sd <- paste0("+", 2)
      
    } else if (valueSD() == -1) {
      
      other_sd <- paste0("+", 1)
      
    } else if (valueSD() == 1) {
      
      other_sd <- -1
      
    } else if (valueSD() == 2) {
      
      other_sd <- -2
      
    } else {
      
      other_sd <- -3
      
    }
    
    as.character(other_sd)
    
  })
  
  midPerc <- eventReactive(input$submitValue, {
    
    if (valueSD() == -1 | valueSD() == 1) {
     
      mid_perc <- "68%"
       
    } else if (valueSD() == -2 | valueSD() == 2) {
      
      mid_perc <- "95%"
      
    } else {
      
      mid_perc <- "99.7%"
      
    }
    
    as.character(mid_perc)
    
  })
 
  halfMid <- eventReactive(input$submitValue, {
    
    if (valueSD() == -1 | valueSD() == 1) {
      
      half_mid <- paste0(68/2,"%")
      
    } else if (valueSD() == -2 | valueSD() == 2) {
      
      half_mid <- paste0(95/2,"%")
      
    } else {
      
      half_mid <- paste0(99.7/2,"%")
      
    }
    
    as.character(half_mid)
    
  })
   
  answer <- eventReactive(input$submitValue, {
    
    if (ab_numr() == 1 & valueSD() < 0) {
      
      if (valueSD() == -1) {
        
        shade_perc <- (0.5 + 0.68/2) * 100
        
      } else if (valueSD() == -2) {
        
        shade_perc <- (0.5 + 0.95/2) * 100
        
      } else {
        
        shade_perc <- (0.5 + 0.997/2) * 100
        
      }
      
    } else if (ab_numr() == 1 & valueSD() > 0) {
      
      if (valueSD() == 1) {
        
        shade_perc <- ((1 - 0.68)/2) * 100
        
      } else if (valueSD() == 2) {
        
        shade_perc <- ((1 - 0.95)/2) * 100
        
      } else {
        
        shade_perc <- ((1 - 0.997)/2) * 100
        
      }
      
    } else if (ab_numr() == 2 & valueSD() < 0) {
      
      if (valueSD() == -1) {
        
        shade_perc <- ((1 - 0.68)/2) * 100
        
      } else if (valueSD() == -2) {
        
        shade_perc <- ((1 - 0.95)/2) * 100
        
      } else {
        
        shade_perc <- ((1 - 0.997)/2) * 100
        
      }
      
    } else {
      
      if (valueSD() == 1) {
        
        shade_perc <- (0.5 + 0.68/2) * 100
        
      } else if (valueSD() == 2) {
        
        shade_perc <- (0.5 + 0.95/2) * 100
        
      } else {
        
        shade_perc <- (0.5 + 0.997/2) * 100
        
      }
      
    }
      
    as.numeric(shade_perc)
    
  })
  
  finalPC <- eventReactive(input$submitValue, {as.character(paste0(answer(),"%"))})
  
  textWarningInput <- eventReactive(input$submitValue, {
    
    if (input$guess == round(answer(), digits = 2) & ab_numr() == 1 & valueSD() < 0) {
      
      tags$h5("Correct! Good work!", ' We are looking for values ', em('above'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is negative we are looking at value under the curve to the right ', em('toward the mean'), ' which means the percentage of values must be greater than 50%, or the right half of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we take half that value, or ', halfMid(), ', and add it to 50% to get ', finalPC(), '.')
      
    } else if (input$guess == round(answer(), digits = 2) & ab_numr() == 1 & valueSD() > 0) {
      
      tags$h5("Correct! Good work!", ' We are looking for values ', em('above'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is positive we are looking at value under the curve to the right ', em('away from the mean'), ' which means the percentage of values must be less than 50%, toward a tail of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we subtract that value from 100% and take half that value (i.e., one tail), to get ', finalPC(), '.')
      
    } else if (input$guess == round(answer(), digits = 2) & ab_numr() == 2 & valueSD() < 0) {
      
      tags$h5("Correct! Good work!", ' We are looking for values ', em('below'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is negative we are looking at value under the curve to the left ', em('away from the mean'), ' which means the percentage of values must be less than 50%, toward a tail of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we subtract that value from 100% and take half that value (i.e., one tail), to get ', finalPC(), '.')
      
    } else if (input$guess == round(answer(), digits = 2) & ab_numr() == 2 & valueSD() > 0) {
      
      tags$h5("Correct! Good work!", ' We are looking for values ', em('below'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is positive we are looking at value under the curve to the left ', em('toward the mean'), ' which means the percentage of values must be greater than 50%, or the left half of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we take half that value, or ', halfMid(), ', and add it to 50% to get ', finalPC(), '.')
      
    } else if (ab_numr() == 1 & valueSD() < 0) {
      
      tags$h4(tags$span(style = "color:red", "Incorrect", .noWS = "after"), ': We are looking for values ', em('above'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is negative we are looking at value under the curve to the right ', em('toward the mean'), ' which means the percentage of values must be greater than 50%, or the right half of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we take half that value, or ', halfMid(), ', and add it to 50% to get ', finalPC(), '.')
      
    } else if (ab_numr() == 1 & valueSD() > 0) {
      
      tags$h4(tags$span(style = "color:red", "Incorrect", .noWS = "after"), ': We are looking for values ', em('above'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is positive we are looking at value under the curve to the right ', em('away from the mean'), ' which means the percentage of values must be less than 50%, toward a tail of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we subtract that value from 100% and take half that value (i.e., one tail), to get ', finalPC(), '.')
      
    } else if (ab_numr() == 2 & valueSD() < 0) {
      
      tags$h4(tags$span(style = "color:red", "Incorrect", .noWS = "after"), ': We are looking for values ', em('below'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is negative we are looking at value under the curve to the left ', em('away from the mean'), ' which means the percentage of values must be less than 50%, toward a tail of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we subtract that value from 100% and take half that value (i.e., one tail), to get ', finalPC(), '.')
      
    } else {
      
      tags$h4(tags$span(style = "color:red", "Incorrect", .noWS = "after"), ': We are looking for values ', em('below'), ' the standard deviation of ', stateSD(), '. As ', stateSD(), ' is positive we are looking at value under the curve to the left ', em('toward the mean'), ' which means the percentage of values must be greater than 50%, or the left half of the curve. We know the area between ', stateSD(), ' and ', otherSD(), ' is ', midPerc(), ', therefore we take half that value, or ', halfMid(), ', and add it to 50% to get ', finalPC(), '.')
      
    }

  })
  
  output$warningInput <- renderUI({textWarningInput()})
  
}

#===============================================================================
#============================        RUN         ===============================
#===============================================================================

shinyApp(ui = ui, server = server)

# END ==========================================================================
