library(shiny)
library(ggplot2)


shinyServer(function(input, output) {

    output$pdf <- renderPlot({
        x <- seq(0, 1, length = 100)
        density <- c(dbeta(x, shape1 = input$data_alpha, shape2 = input$data_beta),
                     dbeta(x, shape1 = input$prior_alpha, shape2 = input$prior_beta),
                     dbeta(x, shape1 = input$data_alpha + input$prior_alpha, shape2 = input$data_beta + input$prior_beta))
        params <- rep(paste(c("data ", "prior ", "posterior "),
                            "α =", c(input$data_alpha, input$prior_alpha, input$data_alpha+input$prior_alpha), 
                            "β =", c(input$data_beta, input$prior_beta, input$data_beta+input$prior_beta)),
                      each = 100)
        betas <- data_frame(density, params, id = rep(x, 3))
        
        betas %>% 
            ggplot(aes(id, density, fill = params))+
            geom_polygon(alpha = 0.8)+
            theme_bw()
    })
})

