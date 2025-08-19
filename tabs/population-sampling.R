

##Population

generate_population <- function(n = 10000) {
  

# Age

age_gen <- function(n = 10000){
  # mild right skew 
  popvals  <- rnorm(1000, mean = 30, sd=30)   + rnorm(10000, mean=10, sd=10)
  # Limit to (0,100)
  popvals <- (subset(popvals, popvals > 0 & popvals < 100))
  # Make KDE
  kde <- popvals |> density(adjust=1.5, from = 0, to=100) 
  # Sample n values
  sample(kde$x, size=n, replace=TRUE, prob=kde$y) |> round(1)
}

# Body Fat Percentage
bodyfat_gen <- function(n=10000){
  # centered around 20 with small right skew
  popvals  <- rnorm(1000, mean = 7, sd=6)^1.1
  popvals <- (subset(popvals, popvals > 0 & popvals < 100)) + 10
  # Make KDE
  kde <- popvals |> density(adjust=1.5) #, from = 4, to=40) 
  # Sample n values
  sample(kde$x, size=n, replace=TRUE, prob=kde$y) |> round(1)
}

# Height 
height_gen <- function(n=10000){
  # Centered around 177
  popvals <- rnorm(2000, mean = 170, sd=9)
  popvals <- (subset(popvals, popvals > 140 & popvals < 200))
  kde <- popvals |> density(adjust=1.5) #, from = 4, to=40) 
  sample(kde$x, size=n, replace=TRUE, prob=kde$y) |> round(1)
}

# Oxygen saturation percentage
oxygen_gen <- function(n=10000){
  (rbeta(n, 30, 2.5)*100) |> round(1)
}

# Num Children
numchild_gen <- function(n=10000){
  rpois(n, 2)
}

# Employed
employment_gen <- function(n=10000){
  rbinom(n,size=1, prob=0.5) |> factor(levels =0:1, labels = c("Unemployed", "Employed"))
}

# Marital Status
marital_gen <- function(n=10000){
  rbinom(n,size=1, prob=0.60) |> factor(levels=0:1, labels = c("Single", "Married"))
}

# Sex
sex_gen <- function(n=10000){
  rbinom(n,size=1, prob=0.50) |> factor(levels = c(0,1), labels = c("Male", "Female"))
}

# Blood type
bloodtype_gen <- function(n=10000){
  # O, A, B, AB
  rbinom(n, 3, prob=c(0.39, .48, 0.08, 0.05)) |> factor(levels = 0:3, labels = c("O", "A", "B", "AB"))
}

# Education
education_gen <- function(n=10000){
  popdist <- c(rep(0, 253), rep(1, 370), rep(2, 346)) |> factor(levels=0:2, labels = c("Primary", "Secondary", "University"))
  sample(popdist, n, replace = TRUE)
}

# Population size
n <- 10000

population <- data.frame(
  idnum = 1:n,
  age = age_gen(n),
  sex = sex_gen(n),
  bloodtype = bloodtype_gen(n),
  spo2 = oxygen_gen(n),
  bodyfat = bodyfat_gen(n),
  education = education_gen(n),
  employed = employment_gen(n),
  height = height_gen(n),
  married = marital_gen(n),
  numchild = numchild_gen(n)
)

# Rules

population[ population$age < 18, ]$married <- "Single"
population[ population$age < 18, ]$numchild <- 0
population[ population$age < 16, ]$employed <- "Employed"
population[ population$sex == "Female", ]$height <- population[ population$sex == "Female", ]$height  - 5

var_labels <- c(idnum = "ID number", age = "Age", sex = "Sex", bloodtype = "Blood Type", spo2 = "SpO2", bodyfat="Body Fat %", education = "Attained Education",
                           employed = "Employment Status", height="Height", married = "Marital Status", numchild = "Number of Children")
attr(population, "variable.labels") <- var_labels

return(population)

} # End generate_population()


population_sampling_panel <- function() {
  
tabsetPanel(
  tabPanel(title = tags$strong("Population"),
           layout_columns( col_widths = c(4,8),
            card(
             uiOutput("population_vars"),
             checkboxInput("sortVar", "Sort subjects", value = FALSE),
            ),
            card(
             plotOutput(outputId = "population_plot"),
             plotOutput(outputId = "population_histogram")
            )
           )
  )
)
  
} # End population_sampling_panel()

