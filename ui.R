library(shiny)

shinyUI(fluidPage(
    
    titlePanel('Play around with Binomial Bayesian inference'),
    
    sidebarLayout(
        sidebarPanel(
                    sliderInput('data_alpha', 'data shape 1:', min=1, max=50, value=6, step=1),
                    sliderInput('data_beta', 'data shape 2:',  min=1, max=50, value=21, step=1),
                    sliderInput('prior_alpha', 'prior shape 1:', min=0.1, max=100, value=10, step=1),
                    sliderInput('prior_beta', 'prior shape 2:',  min=0.1, max=100, value=10, step=1)
            ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("pure density", plotOutput('pdf')),
                        tabPanel("joyplot", plotOutput('joy', height = "800px"))
            #textOutput('header'),
            )
        )
    )
    
))

