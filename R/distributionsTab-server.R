library(dplyr)
library(shiny)

calculate_skew <- function(x, digits=2) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  skew <- ((sum((x - mean_x)^3) / n) / (sd_x^3)) |> round(digits)
  return(skew)
}

make_histogram_plot <- function(s, num_bars){
  
  message("Making histogram plot")
  
  breakpoints_func <- function(x, n){seq(from=min(x), to = max(x), length.out = n + 1)}
  
  message("Breaks:", breakpoints_func(s, num_bars) |> round(2) |> paste0(collapse=", "))
  
  hist(s, main = "Histogram of Sample Values", xlab = "Sample Value", col = "lightblue", breaks = breakpoints_func(s, num_bars))
  
}

make_histogram_table <- function(s, num_bars){
  
  breakpoints_func <- function(x, n){seq(from=min(x), to = max(x), length.out = n + 1)}
  
  h <- hist(s, plot=FALSE, breaks = breakpoints_func(s, num_bars))
  str(h)

  data.frame(
    value = s,
    Range = s |> cut(breaks=h$breaks, include.lowest=T) 
  ) |> group_by(Range, .drop=FALSE) |>
    summarise(
      Count = n(),
      Values = paste0(value, collapse=", ")
    ) |> 
    mutate(`Bin Number` = 1:n()) |>
    select(`Bin Number`, everything())
  
}
  
make_dist_plot <- function(a,b){
  s = rbeta(10e3, a, b)
  hist(x = s, 
       xlim=c(0,1),
       ylim=c(0,7), 
       freq=FALSE, main = "Distribution", xlab="Value", 
       breaks=50)
  text(x=0.5, y = 5, paste0("Skewness: ", calculate_skew(s)), cex=2 )
}


distributionsTabServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
          
      rv <- reactiveValues()

      observe({
        dist <- list(mean = 50, sd = 10)
        message("New sample of size ", input$sample_size, " from N(",dist$mean,",",dist$sd,")")
        rv$sample <- rnorm(as.integer(input$sample_size), dist$mean, sd=dist$sd) |> round(2)
      }) |> bindEvent(input$new_sample, input$sample_size)

      output$sample_table <- renderTable({
        req(rv$sample)
        data.frame(`Observation Number`=1:length(rv$sample), Value=rv$sample)
      })

      output$sample_plot <- renderPlot({
        req(rv$sample)
        d <- data.frame(Number=1:length(rv$sample), Value=rv$sample)
        plot(d$Value, d$Number, ylab="Observation number", xlab="Value", pch=20)
        text(d$Value, d$Number, labels=d$Value, pos=4)
      })
      
      output$histogram <- renderPlot({
        req(rv$sample)
        make_histogram_plot( s = rv$sample, num_bars = as.integer(input$num_bars))
      })

      output$frequency_table <- renderTable({
        req(rv$sample)
        make_histogram_table(s = rv$sample, as.integer(input$num_bins))
      })
      
      output$density_plot <- renderPlot({
        req(rv$sample)
        plot(density(rv$sample, input$density_bandwidth), main="Density Plot of Sample Values")
      })
      
      output$distribution_plot <- renderPlot({
        make_dist_plot(input$dist_a, input$dist_b)
      })
      
      observe({
        updateSliderInput(session, "dist_a", value = 10)
        updateSliderInput(session, "dist_b", value = 10)
      }) |> bindEvent(input$normal_dist)

      observe({
        updateSliderInput(session, "dist_a", value = 1)
        updateSliderInput(session, "dist_b", value = 10)
      }) |> bindEvent(input$rightskew_dist)

      observe({
        updateSliderInput(session, "dist_a", value = 10)
        updateSliderInput(session, "dist_b", value = 1)
      }) |> bindEvent(input$leftskew_dist)

      observe({
        updateSliderInput(session, "dist_a", value = 1)
        updateSliderInput(session, "dist_b", value = 1)
      }) |> bindEvent(input$uniform_dist)

    }
  )
}