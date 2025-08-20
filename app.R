#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(dplyr)
library(ggplot2)

library(shiny)
library(bslib)


source("tabs/variable-types.R")
source("tabs/population-sampling.R")
source("tabs/distributions.R")
source("tabs/descriptives.R")


ui <- 
  page_navbar(title = "Statistikker!", id="page",
    nav_panel("Start",
      h2("Welcome to Statistikker! "),
      p("This application is intended to help students gain a deeper understanding of statistics through interactive demonstrations of core concepts." ),
      p("Click on the tabs above to access the statistics topics." ),
    ),
    nav_panel("Variables
",
                variable_types_panel()
             ),
    nav_panel("Distributions", value = "distributions",
                distributions_panel()
              ),
    nav_panel("Descriptives", value = "descriptives",
                descriptives_panel()
              ),
#    nav_panel("Populations", value = "population",
#                population_sampling_panel()
#              ),
#    nav_panel("Sampling"),
#    nav_panel("Testing")
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  pop_size <- 10000
  
  rv <- reactiveValues( population = generate_population(pop_size),
                        ct_sample = sample(1:9, 5, replace=T) )
  
  output$population_vars <- renderUI({
    req(rv$population)
    selectInput(inputId = "selectVar", label = "Highlight Variable:", 
                choices = attr(rv$population, "variable.labels") |> as.vector() |> setdiff("ID number"), 
                width = "200px")
  })
  
  output$population_plot <- renderPlot({
    req(input$selectVar)
    rowwidth <- 100
    
    varname <- names(rv$population)[ which(input$selectVar == attr(rv$population, "variable.labels")) ]
    message("selectVar: ", varname)
    
    sortVar <- ifelse(input$sortVar, varname, "idnum")
    message("sortVar: ", varname)
    
    plotData <- rv$population |> arrange( .data[[sortVar]] ) |> mutate( sort_id = 1:n() )
    if( is.numeric(plotData[[varname]]) && all( plotData[[varname]] %% 1 == 0 ) ) { plotData[[varname]] <- as.factor(plotData[[varname]])}
    
    
    # Base Plot
    g <- ggplot(data = plotData, aes(y = (sort_id-1) %/% rowwidth, x = (sort_id-1) %% rowwidth)) +
      geom_point(aes(color = .data[[varname]]), size=1) + 
      theme_void()
    
    if(plotData[[varname]] |> is.numeric()){
        g <- g + scale_color_gradientn(colours = c("yellow", "red", "black")) # Continuous scale
    } else if ( rv$population[[varname]] |> is.factor() ){
      
    }
    
    return(g)
    
  })
  
  observe({
    message("New sample of size ", input$sample_size, " from a population of ", pop_size)
    rv$sample <- rv$population |> sample_n(size = as.integer(input$sample_size))
  }) |> bindEvent(input$new_sample, input$sample_size, rv$population)
  
  histvar <- "age"
  
  output$histogram <- renderPlot({
    req(rv$sample)
    make_histogram_plot(rv$sample[[histvar]], as.integer(input$histogram_bars))
  })
  
  output$population_histogram <- renderPlot({
    req(rv$population)
    req(input$selectVar)
    varname <- names(rv$population)[ which(input$selectVar == attr(rv$population, "variable.labels")) ]
    
    hist(rv$population[[varname]], main=input$selectVar, xlab="Value")
  })
  
  output$density_plot <- renderPlot({
    req(rv$sample)
    plot(density(rv$sample[[histvar]], input$density_bandwidth), main="Density Plot of Sample Values")
  })
  
  output$histogram_table <- renderTable({
    req(rv$sample)
    make_histogram_table(rv$sample[[histvar]], as.integer(input$histogram_bars))
    
  })
  
  output$distribution_plot <- renderPlot({
    make_dist_plot(input$dist_a, input$dist_b)
  })
  
  ## Descriptives
 
  output$mean_and_sd_plot <- renderPlot({
    s <- rnorm(500, input$adjust_mean, input$adjust_sd)
    hist(s, xlim=c(0, 100), ylim = c(0, 280), breaks = seq(-1000,2000, 2),
         main="Histogram of Sample Values", xlab="Value")
    segments(x0=input$adjust_mean, x1 = input$adjust_mean,  
             y0=200, y1=265,
             col="red", lty=2)
    text(x = input$adjust_mean, y = 280, labels="Mean", cex=1.5, col="red")
    text(x = input$adjust_mean + input$adjust_sd+1, y = 260, labels="+/- 1 SD", cex=1.5, col="blue", adj=0)
    segments(x0=input$adjust_mean - 1, x1 = input$adjust_mean - input$adjust_sd,  
             y0=255, y1=255,
             col="blue", lty=2)
    segments(x0=input$adjust_mean + 1, x1 = input$adjust_mean + input$adjust_sd,  
             y0=255, y1=255,
             col="blue", lty=2)
    segments(x0=input$adjust_mean - input$adjust_sd, x1 = input$adjust_mean - input$adjust_sd,  
             y0=250, y1=260,
             col="blue", lty=1)
    segments(x0=input$adjust_mean + input$adjust_sd, x1 = input$adjust_mean + input$adjust_sd,  
             y0=250, y1=260,
             col="blue", lty=1)
  }) 
  
  output$boxplot <- renderPlot({
    skew <- c(-15, -14, -12, -10, -5, 0, 5, 10, 12, 14, 15)[input$adjust_skew + 6]
    s <- rbeta(25e3, 16 - skew , 16 + skew) * 100
    bx <- boxplot(s, plot=F)
    hist(s, ylim=c(0, 8500), xlim = c(bx$stats[1], bx$stats[5]), breaks = 50,
         main = "Boxplot and Histogram")
    
    adjust <- ifelse(mean(s) > median(s), 0, 1)
    
    segments(x0 = bx$stats[2], x1= bx$stats[2], 
             y0 = 0, y1 = 4500, col="red", lty=2)
    text(x = bx$stats[2], y = 4750, labels="Q1")
    
    segments(x0 = bx$stats[3], x1= bx$stats[3], 
             y0 = 0, y1 = 4500, col="red", lty=2)
    text(x = bx$stats[3], y = 4750, labels="Q2")
    
    segments(x0 = bx$stats[4], x1= bx$stats[4], 
             y0 = 0, y1 = 4500, col="red", lty=2)
    text(x = bx$stats[4], y = 4750, labels="Q3")
    
    segments(x0 = bx$stats[3], x1= bx$stats[3], 
             y0 = 6500, y1 = 7750, col="black", lty=2)
    text(x = bx$stats[3], y = 8150, labels="Median")
    
    segments(x0 = mean(s), x1= mean(s),
             y0 = 6500, y1 = 7500, col="blue", lty=1)
    text(x = mean(s), y = 7750, labels="Mean", adj=adjust)
    
    boxplot(s, add=T, at = c(6000), horizontal=T, boxwex=3000, col="lightblue", outline=F)
    
  })
  
  # Mean
  observe({
    message("New mean sample of size ", input$ct_sample_size)
    if(input$ct_outlier){
      rv$ct_sample <- c(sample(1:9, input$ct_sample_size-1, replace=T), sample(100:200, 1))
    } else {
      rv$ct_sample <- sample(1:9, input$ct_sample_size, replace=T) 
    }
  }) |> bindEvent(input$new_ct_sample, input$ct_sample_size, input$ct_outlier)
  
  output$mean_example <- renderUI({
    textString <- paste0("$$ \\bar{x} = \\frac{", paste0(rv$ct_sample, collapse="+") ,"}{",length(rv$ct_sample),"} = ",
                         "\\frac{",sum(rv$ct_sample),"}{",length(rv$ct_sample),"}"," = ", mean(rv$ct_sample) |> round(3),"$$")
    str(textString)
    withMathJax(p(textString))
  })
  
  output$median_example <- renderUI({
    if(length(rv$ct_sample) %% 2 == 0){
      textString <- paste0("$$ n\\textrm{ is even:} $$ $$ \\textrm{median}(x) = 
                           \\frac{x_{",length(rv$ct_sample),"/2} + x_{(", length(rv$ct_sample) ,"/2)+1} }{2} =
                           \\frac{x_{",length(rv$ct_sample)/2,"} + x_{", (length(rv$ct_sample)/2)+1, "} }{2} =
                           \\frac{",sort(rv$ct_sample)[length(rv$ct_sample)/2]," + ", sort(rv$ct_sample)[(length(rv$ct_sample)/2)+1], " }{2} =
                           \\frac{",sort(rv$ct_sample)[length(rv$ct_sample)/2] + sort(rv$ct_sample)[(length(rv$ct_sample)/2)+1], "}{2} =",
                           median(rv$ct_sample) |> round(3),"$$")
    } else {
      textString <- paste0("$$ n\\textrm{ is odd:} $$ $$ \\textrm{median}(x) = x_{(",length(rv$ct_sample),"+1)/2}","= x_{", 
                           (length(rv$ct_sample) + 1) / 2 ,"} = ", median(rv$ct_sample) |> round(3),"$$")
    }
    str(textString)
    withMathJax(p("Ordered values: ", paste0(rv$ct_sample |> sort(), collapse=", ")),p(textString))
  })
  
  output$mode_example <- renderTable({
    x <- rv$ct_sample |> table()
    m <- matrix(c("Frequency", x), nrow=1, byrow = T) 
    colnames(m) <- c("Value", names(x))
    mode_idx <- which(x == max(x))
    modes <- rep("no", length(x))
    modes[mode_idx] <- "yes"
    rbind(m, c("Mode", modes))
  })
  
  output$ct_sample_list <- output$ct_sample_list2 <- renderText({
    paste0(rv$ct_sample, collapse=", ")
  })
  
  output$mean_deviation <- renderUI({
    n <- length(rv$ct_sample)
    ts1 <- paste0("$$ \\color{teal}{\\textrm{Sample size:} \\quad n } = \\color{teal}{",n,"} $$
    
                  $$ \\color{red}{\\textrm{Mean: } \\quad \\bar{x}} = ",
            "\\frac{", paste0( sprintf("x_{%d}", 1:n), collapse=" + "), "}{\\color{teal}{n}}  $$"
           )
    ts2 <- paste0("$$ = \\frac{", paste0( rv$ct_sample, collapse=" + "), "}{\\color{teal}{",n,"}} = \\color{red}{", sprintf("%.5g",mean(rv$ct_sample)),"} $$"
           )
    ts3 <- paste0("$$
            \\textrm{Mean deviation: } \\quad \\bar{d} = ",
            "\\frac{", paste0( sprintf("(x_{%d} - \\color{red}{\\bar{x}})", 1:n), collapse=" + "), "}{\\color{teal}{n}}  $$" )
    ts4 <- paste0("$$
            = \\frac{ ",paste0(sprintf("(%d - \\color{red}{%.5g})", rv$ct_sample, mean(rv$ct_sample)),collapse=" + ")," }{\\color{teal}{",n,"}} $$")
    ts5 <- paste0("$$
            = \\frac{ ",paste0(sprintf("(%.5g)", rv$ct_sample-mean(rv$ct_sample) ),collapse=" + ")," }{\\color{teal}{",n,"}} 
            = \\frac{ ",mean( rv$ct_sample - mean(rv$ct_sample) ) |> round(9) ," }{\\color{teal}{",n,"}}
            = ", mean( rv$ct_sample - mean(rv$ct_sample) ) |> round(9),"$$")
    div(
      p(ts1) |> withMathJax(),
      p(ts2),
      p(ts3),
      p(ts4),
      p(ts5)
    )
  })
  
  output$variance <- renderUI({
    n <- length(rv$ct_sample)
    x <- rv$ct_sample
    xbar <- mean(rv$ct_sample)
    ts3 <- paste0("$$
            \\color{magenta}{s^2} = ",
            "\\frac{", paste0( sprintf("(x_{%d} - \\color{red}{\\bar{x}})^2", 1:n), collapse=" + "), "}{\\color{teal}{n}-1}  $$" )
    ts4 <- paste0("$$
            = \\frac{ ",paste0(sprintf("(%d - \\color{red}{%.3g})^2", x, xbar),collapse=" + ")," }{\\color{teal}{",n,"}-1} $$")
    ts5 <- paste0("$$
            = \\frac{ ",paste0(sprintf("(%.3g)^2", x-xbar ),collapse=" + ")," }{\\color{teal}{",n,"}-1} $$")
    ts6 <- paste0("$$
            = \\frac{ ",paste0(sprintf("%.5g", (x-xbar)^2 ),collapse=" + ")," }{\\color{teal}{",n,"}-1}  $$")
    ts7 <- paste0("$$
            = \\frac{",sprintf("%.5g",sum((x - xbar)^2)),"}{",n-1,"} = \\color{magenta}{", sprintf("%.5g",(sum((x - xbar)^2) / (n-1)) |> round(9)),"}$$")
    div(
      p("Sample Variance:"),
      p(ts3) |> withMathJax(),
      p(ts4), p(ts5), p(ts6), p(ts7) )
              
  }) 
  
  output$stddev <- renderUI({
    ts2 <- paste0("Sample standard deviation (from variance):$$
            \\color{blue}{s} = \\sqrt{\\color{magenta}{s^2}} = \\sqrt{\\color{magenta}{", sprintf("%.5g",var(rv$ct_sample)),"}} = \\color{blue}{", sprintf("%.5g",sd(rv$ct_sample)), "}$$")
    
    n <- length(rv$ct_sample)
    x <- rv$ct_sample
    xbar <- mean(rv$ct_sample)
    
    ts3 <- paste0("Sample standard deviation (direct calculation): $$
            \\color{blue}{s} = ",
            "\\sqrt{\\frac{", paste0( sprintf("(x_{%d} - \\color{red}{\\bar{x}})^2", 1:n), collapse=" + "), "}{\\color{teal}{",n,"}-1} } $$" )
    ts4 <- paste0("$$
            = \\sqrt{\\frac{ ",paste0(sprintf("(%d - \\color{red}{%.3g})^2", x, xbar),collapse=" + ")," }{\\color{teal}{",n,"}-1} } $$")
    ts5 <- paste0("$$
            = \\sqrt{\\frac{ ",paste0(sprintf("(%.3g)^2", x-xbar ),collapse=" + ")," }{\\color{teal}{",n,"}-1 }} $$")
    ts6 <- paste0("$$
            = \\sqrt{\\frac{ ",paste0(sprintf("%.5g", (x-xbar)^2 ),collapse=" + ")," }{\\color{teal}{",n,"}-1 }}  $$")
    ts7 <- paste0("$$
            = \\sqrt{\\frac{",sprintf("%.5g",sum((x - xbar)^2)),"}{",n-1,"} } = \\color{blue}{", sprintf("%.5g", sqrt(sum((x - xbar)^2) / (n-1)) |> round(9)),"} $$")
    div(
      p(ts2) |> withMathJax(),
      p(ts3), p(ts4), p(ts5), p(ts6), p(ts7) 
    )
    
              
  }) 

}

# Run the application 
shinyApp(ui = ui, server = server)
