library(shiny)
suppressPackageStartupMessages(library(tidyverse))

shinyServer(function(input, output) {
    x <- seq(0, 1, length = 100)
    
    prop_model <- function(data = c(), prior_prop = c(1, 1), n_draws = 10000) {
        data <- as.logical(data)

        # dens_curves will be a data frame with the x & y coordinates for the 
        # denities to plot where x = proportion_success and y = probability
        proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
        dens_curves <- map_dfr(c(0, seq_along(data)), function(i) {
            value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", "Failure"))
            label <- paste0("n=", i)
            probability <- dbeta(proportion_success,
                                 prior_prop[1] + sum(data[seq_len(i)]),
                                 prior_prop[2] + sum(!data[seq_len(i)]))
            probability <- probability / max(probability)
            tibble(value, label, proportion_success, probability)
        })
        # Turning label and value into factors with the right ordering for the plot
        dens_curves$label <- fct_rev(factor(dens_curves$label, levels =  paste0("n=", c(0, seq_along(data)))))
        dens_curves$value <- factor(dens_curves$value, levels = c("Prior", "Success", "Failure"))
        
        p <- ggplot(dens_curves, aes(x = proportion_success, y = label,
                                     height = probability, fill = value)) +
            ggridges::geom_density_ridges(stat="identity", color = "white", alpha = 0.8,
                                          panel_scaling = TRUE) +
            scale_y_discrete("", expand = c(0.01, 0)) +
            scale_x_continuous("Underlying proportion of success") +
            scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), name = "", drop = FALSE,
                              labels =  c("Prior   ", "Success   ", "Failure   ")) +
            ggtitle(paste0(
                "Binomial model - Data: ", sum(data),  " successes, " , sum(!data), " failures")) +
            theme_light() +
            theme(legend.position = "bottom")+
            labs(caption = "based on Rasmus Bååth post")
        print(p)
        
        # Returning a sample from the posterior distribution that can be further 
        # manipulated and inspected
        posterior_sample <- rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + sum(!data))
        invisible(posterior_sample)
    }
    
    
    
    output$pdf <- renderPlot({
        density <- c(dbeta(x, shape1 = input$data_alpha, shape2 = input$data_beta),
                     dbeta(x, shape1 = input$prior_alpha, shape2 = input$prior_beta),
                     dbeta(x, shape1 = input$data_alpha + input$prior_alpha, shape2 = input$data_beta + input$prior_beta))
        params <- rep(paste(c("data ", "prior ", "posterior "),
                            "α =", c(input$data_alpha, input$prior_alpha, input$data_alpha+input$prior_alpha), 
                            "β =", c(input$data_beta, input$prior_beta, input$data_beta+input$prior_beta)),
                      each = 100)
        betas <- data.frame(density, params, id = rep(x, 3))
        
        betas %>% 
            ggplot(aes(id, density, fill = params))+
            geom_polygon(alpha = 0.8)+
            theme_light() +
            theme(legend.position = "bottom")+
            labs(title = paste0("Binomial model - Data: ", input$data_alpha,  " successes, " , input$data_beta, " failures"))
    })
    
    output$joy <- renderPlot({
        data <- sample(c(rep(TRUE, input$data_alpha), rep(FALSE, input$data_beta)))
        prop_model(data, prior_prop = c(input$prior_alpha, input$prior_beta))
    })    
})

