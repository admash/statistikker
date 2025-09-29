
library(shiny)
library(bslib)

distributionsTab <- function(id) {
  ns <- NS(id)
  
  nav_panel("Distributions",
    tabsetPanel(
      tabPanel(title = "Histograms and Density Curves",
        layout_columns(
          card(
            h3("How values are distributed:"),
            p(HTML("When working with a continuous variables in statistics it is often
               important to understand how the values of a particular variable are distributed. 
               Statisticians are often interested in answering questions like:
            <ul>
              <li>Are the values symmetrically distributed about a point?</li>
              <li>Are most values relatively low with a few high values?</li>
              <li>Are the values evenly distributed across a range?</li>
            </ul>"
            )),
            p(HTML("One of the most common ways of understanding how the values of a variable are distributed 
               is visually -- using a <strong>histogram</strong>. First, a <strong>frequency table</strong> is contstructed by counting the number of values that 
               fall within a series of equally-sized number ranges. Second, a histogram is drawn using a bar graph where the
               height of each bar is equal to the frequencies in each range. An example is shown 
               on the right.")),
            p("A second common way of showing how values are distributed is with a ",tags$strong("density curve"),". Around
               values where more observations are clustered, the density curve is higher. This can be seen
               on the right, under the Density Curve tab.")
          ), # End card 1
          card(
            layout_columns(
              card(
                sliderInput(ns("sample_size"), label = "Sample size:", min = 10, max = 100, ticks = FALSE, value = 30, step = 1, round = TRUE),
              ), 
              card(
                actionButton(ns("new_sample"), label = "New sample")
              )
            ),
            tabsetPanel(
              tabPanel("Sample Values",
                plotOutput(ns("sample_plot")),
                tableOutput(ns("sample_table"))
              ),
              tabPanel("Frequency Table",
                sliderInput(ns("num_bins"), label="Number of bins:", min=5, max = 50, value = 10, step=1, round = TRUE),
                tableOutput(ns("frequency_table"))
              ),
              tabPanel("Histogram",
                sliderInput(ns("num_bars"), label="Number of bars:", min=5, max = 50, value = 10, step=1, round = TRUE),
                plotOutput(ns("histogram"))
              ),
              tabPanel("Density Curve",
                sliderInput(ns("density_bandwidth"), label="Bandwidth:", min=1, max = 20, value = 10, step=1, round = TRUE),
                plotOutput(ns("density_plot"))
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
            plotOutput(ns("distribution_plot")),
            layout_columns(
              card(p("You can adjust A and B to show:"), 
                   actionButton(ns("normal_dist"), label = "Normal - A=10, B=10"),
                   actionButton(ns("rightskew_dist"), label = "Right Skew - A=1, B=10"),
                   actionButton(ns("leftskew_dist"), label = "Left Skew - A=10, B=1"),
                   actionButton(ns("uniform_dist"), label = "Uniform - A=10, B=10")
              ),
              card(
                sliderInput(ns("dist_a"), label = "A", min = 1, max=10, step=0.1, value = 3, ticks=FALSE),
                sliderInput(ns("dist_b"), label = "B", min = 1, max=10, step=0.1, value = 3, ticks=FALSE)
              )
            )
          ) # End card #2
        )
      ) # end tabPanel "Distribution Shapes"
    ) # end tabsetPanel
  ) # end navPanel
  
}