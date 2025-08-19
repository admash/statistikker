
# This function returns the UI content for the variables and types panel.

variable_types_panel <- function() {
  
tabsetPanel(
  tabPanel(title = tags$strong("Variables"),
           h3("What are variables?"),
           wellPanel(
             p("Variables are characteristics or qualities whose values can be observed and measured. 
                Variables are grouped into different types based on the nature of the observed characteristic. 
               In general, the two main types of variables are numeric and categorical, each of which are divided into the subtypes shown below:
               "),
             tags$ul(
               tags$li("Numerical",
                 tags$ul(
                   tags$li("Continuous"),
                   tags$li("Discrete")
                 ),
               ),
               tags$li("Categorical",
                 tags$ul(
                   tags$li("Binary/Dichotomous"),
                   tags$li("Unordered"),
                   tags$li("Ordered"),
                 ),
               )
             ),
           ),
  ),
  tabPanel(title=tags$strong("Variable Types"),
           br(),
             p("Click on the tabs above to view explanations and examples of each variable type."),
           tabsetPanel(
    tabPanel(title = "Continous",
           wellPanel(
           h3("Continous Variables"),
             p("Continuous variables measure numerical values which ", tags$strong("can include fractional/decimal values"),", 
                such as 0.4, 22.14, and -4.5. Depending on what characteric is being measured, 
                they may have additional restrictions, such as only positive values or a limited range."),
             p("Examples:"),
             tags$ul(
               tags$li(tags$strong("Age"),
                 tags$ul(
                   tags$li("Units: years"),
                   tags$li("Example values: 24.3, 99.0, 0.4, 12.5")
                 ),
               ),
               tags$li(tags$strong("Body Fat"),
                 tags$ul(
                   tags$li("Units: proportion (%) "),
                   tags$li("Example values: 4.5, 19.1, 12.8, 8.2")
                 ),
               ),
               tags$li(tags$strong("Height"),
                 tags$ul(
                   tags$li("Units: centimeters (cm)"),
                   tags$li("Example values: 165.2, 184.9, 177.4, 169.7")
                 ),
               ),
             ) # end ul()
           ) # end wellPanel()
           ),
  tabPanel(title = "Discrete",
           wellPanel(
           h3("Discrete Variables"),
             p("Discrete variables measure numerical values which ", tags$strong("do not include fractional values"),", such as 34, 0, 400, and -3. 
                Variables may have additional restrictions such as only positive values, or a limited range."),
             p("Examples:"),
             tags$ul(
               tags$li(tags$strong("Number of children"),
                 tags$ul(
                   tags$li("Units: count"),
                   tags$li("Example values: 5, 0, 2, 1")
                 ),
               ),
               tags$li(tags$strong("Years of Education Completed"),
                 tags$ul(
                   tags$li("Units: years"),
                   tags$li("Example values: 6, 21, 12, 15")
                 ),
               ),
               tags$li(tags$strong("Number of jobs held"),
                 tags$ul(
                   tags$li("Units: count"),
                   tags$li("Example values: 1, 8, 0, 3")
                 ),
               ),
             ) # end ul()
           )
           ),
    tabPanel(title = "Ordered",
           wellPanel(
           h3("Ordered Categorical Variables"),
             p("Ordered categorical variables measure non-numeric values in which ", tags$strong("order does matter"),". Examples include educational, preference, and attainment variables. Numerical variables are often converted into ordered categorical variables."),
             p("Examples:"),
             tags$ul(
               tags$li(tags$strong("Educational Level"),
                 tags$ul(
                   tags$li("Example values: Primary < Secondary < Bachelor < Master < Doctor")
                 ),
               ),
               tags$li(tags$strong("Language Level"),
                 tags$ul(
                   tags$li("Example values: A1 < A2 < B1 < B2 < C1 < C2")
                 ),
               ),
               tags$li(tags$strong("Preference < Likert Scale"),
                 tags$ul(
                   tags$li("Example values: Strongly Disagree < Somewhat Disagree < Neither < Agree < Strongly Agree")
                 ),
               ),
             ) # end ul()
           )
           ),
  tabPanel(title = "Binary",
           wellPanel(
           h3("Binary Variables"),
             p("Binary variables are categorical variables which have ", tags$strong("only two possible values"),", such as TRUE/FALSE, 0/1, yes/no, A/B. Both numerical variables and ordered categorical variables are sometimes converted into binary categorical variables."),
             p("Examples:"),
             tags$ul(
               tags$li(tags$strong("Employed"),
                 tags$ul(
                   tags$li("Example values: Yes/No")
                 ),
               ),
               tags$li(tags$strong("Marital Status"),
                 tags$ul(
                   tags$li("Example values: Married/Not married")
                 ),
               ),
               tags$li(tags$strong("Male"),
                 tags$ul(
                   tags$li("Example values: 1/0")
                 ),
               ),
             ) # end ul()
           )
           ),
  tabPanel(title = "Unordered",
           wellPanel(
           h3("Unordered Categorical Variables"),
             p("Unordered categorical variables measure non-numeric values in which ", tags$strong("order does NOT matter"),". Examples include grouping, geographic, and demographic variables."),
             p("Examples:"),
             tags$ul(
               tags$li(tags$strong("Blood Type"),
                 tags$ul(
                   tags$li("Example values: A/B/O")
                 ),
               ),
               tags$li(tags$strong("Natural Hair Color"),
                 tags$ul(
                   tags$li("Example values: Blond/Brown/Black/Red/White")
                 ),
               ),
               tags$li(tags$strong("Religious Affiliation"),
                 tags$ul(
                   tags$li("Example values: Atheist/Christian/Muslim/Buddhist/Other")
                 ),
               ),
             ) # end ul()
           )
           ),

           ) # end of tabsetPanel(
           ), # end of VariableTypes tabPanel

  tabPanel(title = tags$strong("Practice"),
           h3("Determining Variable Type - Practice"),
           wellPanel(
             p("Listed below are a variety of variables of different types. 
                As an exercise, you can attempt to classify each variable into its type, then click on the variable to check if you are correct."),
             p("Variables:"),
             accordion(multiple=TRUE, open = FALSE,
               accordion_panel(title="Dominant Hand: Left/Right", 
                               p(tags$strong("Binary/Dichotomous Variable"))),
               accordion_panel(title="Body Mass Index (BMI): 18.2, 29.1, 20.3",
                               p(tags$strong("Continuous Variable"))),
               accordion_panel(title="Household Size: 6, 1, 3",
                               p(tags$strong("Discrete Variable"))),
               accordion_panel(title="Self-reported health status: Good, Bad, Terrible, Ok, Excellent",
                               p(tags$strong("Ordered Categorical Variable"))),
               accordion_panel(title="Ever Smoked: Yes, No",
                               p(tags$strong("Binary/Dichotomous Variable"))),
               accordion_panel(title="Age at death: 78, 92.1, 88.6, 39",
                               p(tags$strong("Continuous Variable"))),
               accordion_panel(title="Birth Continent: Africa, North America, Asia, Europe",
                               p(tags$strong("Unordered Categorical Variable"))),
               accordion_panel(title="Weight: 67.2, 55.1, 73.8, 66.4",
                               p(tags$strong("Continuous Variable"))),
               accordion_panel(title="Marital Status: Married, Divorced, Widowed, Single",
                               p(tags$strong("Unordered Categorical Variable")))
             )
           )
           ),
)
  
} # End variable_types_panel() 

