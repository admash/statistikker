distributions_panel <- function() {
  
  tabsetPanel(
    tabPanel(title = "Histograms and Density Curves",
      layout_columns(
        card(
          h3("How values are distributed:"),
          p("When working with a continuous variables in statistics it is often
             important to understand how the values of a particular variable are distributed. 
             Statisticians are often interested in answering questions like:"),
          tags$ul(
            tags$li("Are the values symmetrically distributed about a point?"),
            tags$li("Are most values relatively low with a few high values?"),
            tags$li("Are the values evenly distributed across a range?")
          ),
          p("One of the most common ways of understanding how the values of a variable are distributed 
             is visually -- using a ",tags$strong("histogram"),". First, a ",tags$strong("frequency table")," is contstructed by counting the number of values that 
             fall within a series of equally-sized number ranges. Second, a histogram is drawn using a bar graph where the
             height of each bar is equal to the frequencies in each range. An example is shown 
             on the right."),
          p("A second common way of showing how values are distributed is with a ",tags$strong("density curve"),". Around
             values where more observations are clustered, the density curve is higher. This can be seen
             on the right, under the Density Curve tab.")
        ), # End card 1
        card(
          layout_columns(
            card(
              sliderInput("sample_size", label = "Sample size:", min = 10, max = 100, ticks = FALSE, value = 30, step = 1, round = TRUE)
            ), 
            card(
              actionButton("new_sample", label = "New sample")
            )
          ),
          tabsetPanel(
            tabPanel("Histogram",
              plotOutput("histogram"),
              sliderInput("histogram_bars", label="Number of bars:", min=5, max = 50, value = 10, step=1, round = TRUE),
              h4("Frequency Table:"),
              tableOutput("histogram_table")
            ),
            tabPanel("Density Curve",
              plotOutput("density_plot"),
              sliderInput("density_bandwidth", label="Bandwidth:", min=1, max = 20, value = 10, step=1, round = TRUE),
            )
          ),
          
        )  # end card 2
      ) # end layout_columns
    ), #end tabPanel
    tabPanel(title = "Distribution Shapes",
      layout_columns(
        card(
          h4("Gaussian/Normal curve"),
          p("When plotting distributions of variables, one commonly found shape
             is the 'bell curve', which is a characteristic of the Normal or Gaussian
             distribution. The shape symmetric, with a peak in the middle, and tails on
            each side whose height approaches zero the further they are from the peak. "),
          h4("Skew"),
          p("It is also very common to see a distribution that looks like a Normal
            distribution that has been stretched to either side. This is described as a
            \"skewed distribution\". Skew can be to either the right skew (positive) or 
            left skew (negative)."),
          h4("Uniform"),
          p("A third distribution shape is a uniform distribution. When plotted, this distribution
            appears flat, with each value occuring with approximately the same frequency.")
        ), # End card #1
        card(
          plotOutput("distribution_plot"),
          layout_columns(
            card(p("Adjust A and B to show:"), 
              tags$ul(
                tags$li(tags$strong("normal"), "(A=10, B=10)"),
                tags$li(tags$strong("right-skewed"), "(A=1, B=10)"),
                tags$li(tags$strong("left-skewed"), "(A=10, B=1)"),
                tags$li(tags$strong("uniform"), "(A=1, B=1)")
              )
            ),
            card(
              sliderInput("dist_a", label = "A", min = 1, max=10, step=0.1, value = 3, ticks=FALSE),
              sliderInput("dist_b", label = "B", min = 1, max=10, step=0.1, value = 3, ticks=FALSE)
            )
          )
        ) # End card #2
      )
    )
  ) # end tabsetPanel
  
}

calculate_skew <- function(x, digits=2) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  skew <- ((sum((x - mean_x)^3) / n) / (sd_x^3)) |> round(digits)
  return(skew)
}

make_histogram_plot <- function(s, num_bars){
  
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
    Range =  s |> cut(breaks=h$breaks, include.lowest=T) 
  ) |> group_by(Range, .drop=FALSE) |>
    summarise(
      Count = n(),
      Values = paste0(value, collapse=", ")
    )
  
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