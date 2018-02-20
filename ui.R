
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("yeti"),
  # Application title
  titlePanel("Simulation App for the Central Limit Theorem"),
  # Sidebar with a slider input for type of distributionT and slider input.
  # FOR number of bins
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(8,
                      radioButtons("dist", "Distribution type:",
                                   c("Normal" = "Normal",
                                     "Binomial" = "Binomial",
                                     "Uniform" = "Uniform",
                                     "Lognormal" = "Lognormal",
                                     "Exponential" = "Exponential")))),
      fluidRow(column(8,
                      numericInput("nobs",
                                  "Number of samples:",
                                  min = 10,
                                  max = 50000,
                                  value = 500))),
      fluidRow(column(8,
                      numericInput("sampsize",
                                   "Sample size:",
                                   min = 2,
                                   max = 5000,
                                   value = 2))),
      fluidRow(column(8,
                      submitButton(text = "Run Simulation", icon = NULL, width = NULL)))),
    # Show a plot of the generated distribution
    
    
    
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        tabPanel(h5(strong("Directions, Notes, and Guide Questions")), 
                 h4("Use the panel on the left to:"),
                 p("1. choose the distribution type"),
                 p("2. indicate the number of independent samples from the distribution. The minimum is 10 and the maximum is 50,000."),
                 p("3. set the sample size of each random sample from the distribution. The minimum is 2 and the maximum is 5000."),
                 h4("Notes"),
                 p("1. Hit the", span(strong("Run Simulation")), "button to apply the changes and run the simulation."),
                 p("2. Changing the sample size for the same number of samples will only change the plot of the sample means."),
                 p("3. The binomial distribution is set at \\(p=0.7\\)."),
                 h4("Guide Questions"),
                 withMathJax(),
                 p("1. Suppose we have the random sample \\(X\\) from the chosen distribution, what does the histogram of \\(Z=(X-\\bar{X}) / sd(X) \\) look like as we increase the number of random samples?"),
                 p("2. What does the histogram of \\(Z\\) look like as we increase the size of the samples?"),
                 p("3. How fast do the histogram of \\(Z\\) approach a normal curve if we increase the sample size? (Try increments of number samples.)"),
                 p("4. What do you think are the mean and standard deviaton of \\(Z\\)?")
                 ), 
        tabPanel(h5(strong("Plot")), plotOutput("distPlot")),
        tabPanel(h5(strong("Estimates")), tableOutput("estimates"))
      )
              
             
             
    )
  )	 
)
)
