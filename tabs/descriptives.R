
descriptives_panel <- function(){
    tabsetPanel(
      tabPanel("Sample Description",
        layout_columns(
          card(
            card_header("How to describe sample variable?"),
            p("Given a sample variable that may contain anywhere from tens to thousands of 
               values, how do we describe those values as simply and accurately 
               as possible?")
          ), 
          br()
          
        ), #end layout_columns, row 1
        layout_columns(
          card(
            card_header("The simplest way: 2 numbers"),
            p("The simplest way is based upon the assumption that the values are
              normally distributed (i.e. come from a Gaussian/Normal distribution),
              which comes with the expectation that the sample distribution is symmetric. 
              If we make this assumption, we can then describe the values using two
              numbers: ", tags$strong("mean"), "and", tags$stron("standard deviation"), "."),
            p("The first of these, the mean, is a measurement of ",
              tags$em("central tendency"), "meaning that it describes the location 
              that the values tend to center around.  Other useful measures of 
              central are the median and the mode."),
            p("The second descriptive value, standard deviation, is a measurement
              of ", tags$em("dispersion"), " which describes how \"dispersed\" 
              or spread out from the center the sample values are."),
            p(tags$strong("Explore:"), "Adjust the sliders for mean and standard
              deviation to see how it affects the sample histogram.")
            
          ), # end card "The Simplest Way"
          card(
            card_header("Mean and Standard Deviation"),
            plotOutput("mean_and_sd_plot"),
            layout_column_wrap(
              sliderInput("adjust_mean", label = "Population Mean: ", 
                          min = 30, max=70, value=50, step = 1, ticks=FALSE),
              sliderInput("adjust_sd", label = "Population Std. Dev.:", 
                          min = 2, max=20, value=10, step = 1, ticks=FALSE)
            )
          )
        ), # end layout_columns # row 2
        layout_columns(
              card(
                card_header("The more detailed way: several numbers"),
                p("It is not always true that the values of a sample variable are
                  normally distributed. Sometimes the shape is distorted and/or asymmetric. When this is the case, we need to use more
                  numbers to describe the shape of the data. The typical way to 
                  do this is by using ", tags$strong("percentiles"),"."),
                p("Percentiles describe the value at which a given percentage of 
                  the sample values are less than or equal to. For example,  if 
                  the 37th percentile is 7.5, then 37% of the sample values are less than or 
                  equal to 7.5. Typically, percentiles which are multiples of 25 are 
                  used, called ",tags$strong("quartiles"),", though others are common as well. 
                  The 0th and 100th percentiles are equivalent to the minimum 
                  and maximum values. The table below shows the most commonly 
                  used percentiles:"),
                tags$table(
                  tags$thead(
                    tags$tr(
                      tags$th("Percentile"), tags$th("Name"), tags$th("Description")
                    ),
                    tags$tr(tags$td("0"), tags$td("minimum"), tags$td("Smallest value")),
                    tags$tr(tags$td("25"), tags$td("Q1"), tags$td("1st Quartile")),
                    tags$tr(tags$td("50"), tags$td("Q2"), tags$td("2nd Quartile / median")),
                    tags$tr(tags$td("75"), tags$td("Q3"), tags$td("3rd Quartile")),
                    tags$tr(tags$td("100"), tags$td("maximum"), tags$td("Largest value")),
                  ),
                ),
                p("Quartiles are often visualized through the use of box plots, as shown in the plot to the right. "),
                p(tags$strong("Explore:"), "Adjust the skew slider to see how it affects the quartiles. Note 
                  the differing effect on the median and the mean.")
              ), # end card "The more detailed way"
              card(
                card_header("Quartiles"),
                plotOutput("boxplot"),
                sliderInput("adjust_skew", label="Skew:", min=-5, max=5, value=0, step=1, ticks = FALSE,
                            animate=animationOptions(interval=750, loop=TRUE))
              )
        ) # end layout_columns, row 2
      ),
      tabPanel("Central Tendency",
        layout_columns(
          card(
           p("To describe the distribution of a sample using numbers, we 
             need a way to specify where the middle of distribution is located. 
             This is called ", tags$strong("central tendency"),"."),
           p("There are several ways to measure this, with the most common being the ",
             tags$em("mean"),", ", 
             tags$em("median"),", and ", 
             tags$em("mode"),"."), 
           p("Use the controls on the right to adjust/update the sample used in the examples below.")
          ),
          card(
            layout_columns(
              card(
                sliderInput("ct_sample_size", "Sample size: ", min=3, max=10, step=1, value=5),
                checkboxInput("ct_outlier", label = "Include outlier (extreme value)"),
              ),
              card(
                actionButton("new_ct_sample", "New Sample"),
                p(tags$strong("Sample Values: ")), 
                textOutput("ct_sample_list")
              )
            )
          )
        ),
        layout_columns(
          card(
            h3("Mean"),
            withMathJax(p("The most common measure of central tendancy is the mean or average. 
                 It is calculated by summing all the values in a sample and 
                 dividing by the number of values . In mathematical notation:
               $$\\frac{\\sum_{i=1}^{n} x_i}{n} =
               \\frac{x_1 + x_2 + x_3 + ... +x_n}{n} = \\bar{x}
                          $$"))
          ), # end card 1
          card(
            h3("Example Calculation: Mean"),
            uiOutput("mean_example")
          ) # end card 2
        ), # end mean layout_columns
        
        layout_columns(
          card(
            h3("Median"),
            withMathJax(p("The next most common measure of central tendancy is the median or 50th percentile. 
                 If the number of values is odd, then median is the value in the middle of the ordered values in the sample. 
                 If the number of values is even, the median is calculated as the mean of the two values in on either side of the middle.
                 In mathematical notation:
               $$n\\textrm{ is odd: median}(x)= x_{(n+1)/2}$$",
              "$$n\\textrm{ is even: median}(x) =  \\frac{x_{n/2} + x_{(n/2)+1} }{2} $$",
              ))
          ), # end card 1
          card(
            h3("Example Calculation: Median"),
            uiOutput("median_example")
          ) # end card 2
        ), # end median layout_columns
        
        layout_columns(
          card(
            h3("Mode"),
            withMathJax(p("The last common measure of central tendancy is the mode. 
                 It is simply defined as the most frequent value in the sample. 
                 Represented graphically, it is the highest point on the distribution.
                 It is possible for there to be more than one mode,  if more than 
                 one value has the highest frequency, though this is uncommon 
                 in larger samples."
            ))
          ), # end card 1
          card(
            h3("Example Calculation: Mode"),
            tableOutput("mode_example")
          ) # end card 2
        ) # end mean layout_columns
        
          ), # end central tendency tab
      tabPanel("Dispersion",
        layout_columns( # Row 1
          card(
            card_header("How spread out are the values?"),
            p("We often need to describe how spread out values are from the middle. This characteristic is called ", tags$strong("dispersion") ,"."),
            p("There are several ways to measure dispersion. The most useful of these are", 
              tags$em("variance"), "and", tags$em("standard deviation"),"." )
          ),
          card(
            layout_columns(
              card(
                sliderInput("ct_sample_size", "Sample size: ", min=3, max=10, step=1, value=5),
                checkboxInput("ct_outlier", label = "Include outlier (extreme value)"),
              ),
              card(
                actionButton("new_ct_sample", "New Sample"),
                p(tags$strong("Sample Values: ")), textOutput("ct_sample_list2")
              )
            )
          )
        ), # End row 1
        
        layout_columns(
          card(
            card_header("Mean deviation"),
            p("Why not measure dispersion by subtracting the mean from each sample value and calculating the average? (This is called the 'mean deviation'.)"),
            withMathJax(p("$$ 
            \\color{red}{ \\textrm{Mean: } \\quad \\bar{x} } = \\frac{\\sum_{i=1}^{ \\color{teal}{n} } x_i}{ \\color{teal}{n} } =
               \\frac{x_1 + x_2 + x_3 + ... +x_{ \\color{teal}{n} } }{ \\color{teal}{n} }
                          $$")),
            p("$$  \\textrm{Deviation: } \\quad d_i = x_i - \\color{red}{\\bar{x}} $$"),
            p("Since the mean is, by definition, in the center of the sample values, 
              the average difference from it is zero, as all the positive and negative differences cancel out."),
            withMathJax(p("$$ 
            \\textrm{Mean deviation: } \\quad \\bar{d} = \\frac{\\sum_{i=1}^{\\color{teal}{n}} d_i}{\\color{teal}{n}} = \\frac{\\sum_{i=1}^{\\color{teal}{n}} (x_i-\\color{red}{\\bar{x}})}{\\color{teal}{n}} =
               \\frac{ (x_1 - \\color{red}{\\bar{x}}) + (x_2 - \\color{red}{\\bar{x}}) + (x_3 - \\color{red}{\\bar{x}}) + ... +(x_{ \\color{teal}{n} } - \\color{red}{\\bar{x}}) }{\\color{teal}{n}}
                          $$"))
          ),
          card(
            card_header(h4("Example: mean deviation")),
            uiOutput("mean_deviation")
          ) # end card Example: mean deviation
        ), # end row 2
        
        layout_columns(
          card(
            card_header("Variance"),
            p("To prevent the positive and negative deviations from cancelling out, 
               we can square them first. This results in a statistic called the ", 
               tags$strong("variance"),"."),
            p("Note, because we are using the ", 
               tags$em("sample mean"),"the mean deviation will be smaller, on average, than if we 
               used the ",tags$em("population mean"), ". This causes the sample 
               variance to be too small also, so we correct for it by dividing by",
               tags$em("n-1"), "instead, ."),
            withMathJax(
              
              p("Sample variance:$$ 
                   \\color{magenta}{s^2} = \\frac{\\sum_{i=1}^{ \\color{teal}{n} } d_i^2}{ \\color{teal}{n} -1} = \\frac{\\sum_{i=1}^{ \\color{teal}{n} } (x_i-\\color{red}{\\bar{x}})^2}{ \\color{teal}{n} -1} =
                   \\frac{ (x_1 - \\color{red}{\\bar{x}})^2 + (x_2 - \\color{red}{\\bar{x}})^2 + (x_3 - \\color{red}{\\bar{x}})^2 + ... +(x_{ \\color{teal}{n} } - \\color{red}{\\bar{x}})^2 }{ \\color{teal}{n} -1} 
                 $$")
            ),
            p(tags$strong("Explore:"),"Adjust the sample size using the sliders and test how the presence of an outlier (extreme value) affects the variance.")
          ),
          card(
            card_header(h4("Example: sample variance")),
            uiOutput("variance")
          ) # end card Example: mean deviation
        ), # end row 3
        
        layout_columns(
          card(
            card_header("Standard Deviation"),
            p("Using the variance directly is difficult and inconvenient for several reasons including not having the same units as the sample (squared) and having comparatively large values relative to the range of the sample. For these, and other more mathematical reasons, it is much more useful to use the square root of the variance, which is called the ", tags$strong("standard deviation"),"."),
            withMathJax(p("Sample Standard Deviation: $$ 
             \\color{blue}{s} = \\sqrt{\\color{magenta}{s^2}} = \\sqrt{\\frac{\\sum_{i=1}^{ \\color{teal}{n} } d_i^2}{ \\color{teal}{n} -1}} = \\sqrt{\\frac{\\sum_{i=1}^{ \\color{teal}{n} } (x_i-\\color{red}{\\bar{x}})^2}{  \\color{teal}{n} -1}} $$",
               "$$ = \\sqrt{\\frac{ (x_1 - \\color{red}{\\bar{x}})^2 + (x_2 - \\color{red}{\\bar{x}})^2 + (x_3 - \\color{red}{\\bar{x}})^2 + ... +(x_{ \\color{teal}{n} } - \\color{red}{\\bar{x}})^2 }{ \\color{teal}{n} -1}}
                          $$"))
          ),
          card(
            card_header(h4("Example: standard deviation")),
            uiOutput("stddev")
          ) # end card Example: mean deviation
        ) # end row 3
               
      ), # end of Dispersion tabPanel
      # tabPanel("Tabulation",
      #   layout_columns(
      #     card(),
      #     card()
      #   ) 
      # )
    ) # end tabsetPanel
}