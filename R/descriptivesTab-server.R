library(shiny)

descriptivesTabServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
          
      rv <- reactiveValues()

      
      output$mean_and_sd_plot <- renderPlot({
        s <- rnorm(500, input$adjust_mean, input$adjust_sd)
        hist(s, xlim=c(0, 100), ylim = c(0, 280), breaks = seq(-1000,2000, 2),
             main="Histogram of Sample Values", xlab="Value")
        segments(x0=input$adjust_mean, x1 = input$adjust_mean,  
                 y0=200, y1=265,
                 col="red", lty=2)
        text(x = input$adjust_mean, y = 280, labels="Mean", cex=1.5, col="red")
        text(x = input$adjust_mean + input$adjust_sd+1, y = 255, labels="+/- 1 SD", cex=1.5, col="blue", adj=0)
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
      
      observe({
        dist <- list(mean = 50, sd = 10)
        skew <- c(-15, -14, -12, -10, -5, 0, 5, 10, 12, 14, 15)[input$adjust_skew + 6]
        message("New sample of size ", input$sample_size, " from N(",dist$mean,",",dist$sd,")")
        rv$sample <- (rbeta(25e3, 16 - skew , 16 + skew) * 100) |> round(2)
      }) |> bindEvent(input$adjust_skew)
      
      output$boxplot <- renderPlot({
        req(rv$sample)
        s <- rv$sample

        bx <- boxplot(s, plot=F)
        hist(s, ylim=c(0, 8500), xlim = c(bx$stats[1], bx$stats[5]), breaks = 50,
             main = "Boxplot and Histogram", xlab="Value")
        
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
                 y0 = 0, y1 = 7500, col="blue", lty=1)
        text(x = mean(s), y = 7750, labels="Mean", adj=adjust)
        
        boxplot(s, add=T, at = c(6000), horizontal=T, boxwex=3000, col="lightblue", outline=F)
        
      })
      
      output$quartile_values <- renderTable({
        req(rv$sample)
        
        d <- data.frame(
          Name = c("Mean", "Minimum", "Q1", "Q2 (Median)", "Q3", "Maximum"),
          Percentile = c("-", "0th", "25th", "50th", "75th", "100th"),
          Value=c(mean(rv$sample), quantile(rv$sample))
        ) |> arrange(Value)

        return(d)
        
      })
      
      #### Central Tendency ####
      

      # Mean
      observe({
        message("New mean sample of size ", input$ct_sample_size)
        if(input$ct_outlier){
          rv$ct_sample <- c(sample(1:9, input$ct_sample_size-1, replace=T), sample(100:200, 1))
        } else {
          rv$ct_sample <- sample(1:9, input$ct_sample_size, replace=T) 
        }
      }) |> bindEvent(input$ct_new_sample, input$ct_sample_size, input$ct_outlier)
      

      output$ct_sample_list <- output$ct_sample_list2 <- renderText({
        paste0(rv$ct_sample, collapse=", ")
      })
      
      output$ct_mean_example <- renderUI({
        textString <- paste0("$$ \\bar{x} = \\frac{", paste0(rv$ct_sample, collapse="+") ,"}{",length(rv$ct_sample),"} = ",
                             "\\frac{",sum(rv$ct_sample),"}{",length(rv$ct_sample),"}"," = ", mean(rv$ct_sample) |> round(3),"$$")
        str(textString)
        withMathJax(p(textString))
      })
      
      # Median
      output$ct_median_example <- renderUI({
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
  
      #### Dispersion ####
      
      observe({
        message("New mean sample of size ", input$ds_sample_size)
        if(input$ds_outlier){
          rv$ds_sample <- c(sample(1:9, input$ds_sample_size-1, replace=T), sample(100:200, 1))
        } else {
          rv$ds_sample <- sample(1:9, input$ds_sample_size, replace=T) 
        }
      }) |> bindEvent(input$ds_new_sample, input$ds_sample_size, input$ds_outlier)
      

      output$ds_sample_list <- output$ds_sample_list2 <- renderText({
        paste0(rv$ds_sample, collapse=", ")
      })

      # Mean deviation
      output$ds_mean_deviation <- renderUI({
        n <- length(rv$ds_sample)
        ts1 <- paste0("$$ \\color{teal}{\\textrm{Sample size:} \\quad n } = \\color{teal}{",n,"} $$
        
                      $$ \\color{red}{\\textrm{Mean: } \\quad \\bar{x}} = ",
                "\\frac{", paste0( sprintf("x_{%d}", 1:n), collapse=" + "), "}{\\color{teal}{n}}  $$"
               )
        ts2 <- paste0("$$ = \\frac{", paste0( rv$ds_sample, collapse=" + "), "}{\\color{teal}{",n,"}} = \\color{red}{", sprintf("%.5g",mean(rv$ds_sample)),"} $$"
               )
        ts3 <- paste0("$$
                \\textrm{Mean deviation: } \\quad \\bar{d} = ",
                "\\frac{", paste0( sprintf("(x_{%d} - \\color{red}{\\bar{x}})", 1:n), collapse=" + "), "}{\\color{teal}{n}}  $$" )
        ts4 <- paste0("$$
                = \\frac{ ",paste0(sprintf("(%d - \\color{red}{%.5g})", rv$ds_sample, mean(rv$ds_sample)),collapse=" + ")," }{\\color{teal}{",n,"}} $$")
        ts5 <- paste0("$$
                = \\frac{ ",paste0(sprintf("(%.5g)", rv$ds_sample-mean(rv$ds_sample) ),collapse=" + ")," }{\\color{teal}{",n,"}} 
                = \\frac{ ",mean( rv$ds_sample - mean(rv$ds_sample) ) |> round(9) ," }{\\color{teal}{",n,"}}
                = ", mean( rv$ds_sample - mean(rv$ds_sample) ) |> round(9),"$$")
        div(
          p(ts1) |> withMathJax(),
          p(ts2),
          p(ts3),
          p(ts4),
          p(ts5)
        )
      })

      # Variance
        
      output$ds_variance <- renderUI({
        n <- length(rv$ds_sample)
        x <- rv$ds_sample
        xbar <- mean(rv$ds_sample)
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
        
      # Standard Deviation

      output$ds_stddev <- renderUI({
        ts2 <- paste0("Sample standard deviation (from variance):$$
                \\color{blue}{s} = \\sqrt{\\color{magenta}{s^2}} = \\sqrt{\\color{magenta}{", sprintf("%.5g",var(rv$ds_sample)),"}} = \\color{blue}{", sprintf("%.5g",sd(rv$ds_sample)), "}$$")
        
        n <- length(rv$ds_sample)
        x <- rv$ds_sample
        xbar <- mean(rv$ds_sample)
        
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
  )
}