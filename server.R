
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  nobs <- reactive({
    input$nobs
  })
  
  sampsize <- reactive({
    input$sampsize
  })
  
  disttype <- reactive({
    input$dist
  })
  
  dist <- reactive({switch(disttype(),
           Normal = rnorm,
           Binomial = rbinom,
           Uniform = runif,
           Lognormal = rlnorm,
           Exponential = rexp,
           rnorm
           )})
  
  rnos <- reactive({
    dist <- dist()
    if (disttype()=="Binomial"){
      rbinom(nobs(), size=sampsize(), prob=0.7)
    } else{
    dist(nobs())}
    })


  df <- reactive({
    rnos <- rnos()
    set.seed(nobs())
    # generate nobs random numbers from the chosen distribution
    
    # create vector of sample means from the nobs random numbers
    sampleMeans <- c(rep(0, times=nobs()))
    for (i in 1:nobs()){
      sampleMeans[i] <- mean(sample(rnos(), sampsize(), replace = TRUE))
    }
    
    meanSamp <- mean(sampleMeans)
    z_means <- (sampleMeans - meanSamp) / sd(sampleMeans)
    
    # create panel for two side-by-side plots
    # of the nobs observations from the distribution
    # and the nobs sample means of sampsize size
    
    
    data.frame(
      rnos ,
      sampleMeans,
      z_means) 
  })
  
    output$distPlot <- renderPlot({
    
    df <- df()
    df %>%
      gather(samp, value, c(rnos,z_means)) %>%
      mutate(
        samp = factor(samp, 
                      labels = c(
                        paste(nobs(), "Random Numbers from\n", disttype(), "Distribution"), 
                        paste(nobs(), "Z's for sample size", sampsize(), "\nfrom", disttype(), "Distribution")))) %>%
      ggplot(aes(value, color = samp)) + 
      geom_histogram(fill = "white", bins = min(60, nobs() / sampsize())) + 
      facet_wrap(~samp, scale = "free") +
      theme_bw() + 
      xlab("") + 
      ylab("") + 
      theme(legend.title=element_blank())
    ##dist size, parameter from slider
  })
  
    output$estimates <- renderTable({
      
      df <- df()

      data.frame(
        Statistics = c("Mean", "Standard deviation"),
        Estimates = c(round(mean(df$z_means),4), round(sd(df$z_means) / sqrt(sampsize()),4))
      )
    }, hover = TRUE, digits = 8)
    
    


}
)
