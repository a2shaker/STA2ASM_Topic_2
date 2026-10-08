library(shiny)
library(plotly)

# Helper for formatting numbers
fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

ui <- fluidPage(
  titlePanel("Expectation and variance properties"),
  
  tabsetPanel(
    
    # ============================================================
    # Tab 1: Transform a random variable
    # ============================================================
    
    tabPanel(
      "Transform a random variable",
      br(),
      p(
        "Let X be a continuous random variable. Use the sliders to see what happens when X is transformed to aX + b."
      ),
      
      sidebarLayout(
        sidebarPanel(
          
          sliderInput(
            "a", "a", min = -2.5, max = 2.5, value = 1,
            step = 0.1
          ),
          
          sliderInput(
            "b", "b", min = -10, max = 10, value = 0,
            step = 0.5
          ),
          
          p(
            strong("Current transformation: "),
            uiOutput("transformationFormula", inline = TRUE)
          ),
          
          tags$hr(),
          
          strong("Properties"),
          p("E(aX + b) = aE(X) + b"),
          p("Var(aX + b) = a²Var(X)")
        ),
        
        mainPanel(
          
          plotOutput("transformPlot", height = "470px"),
          
          fluidRow(
            
            column(
              4,
              wellPanel(
                h4("Original X"),
                p(
                  strong("E(X): "),
                  textOutput("origMean", inline = TRUE)
                ),
                p(
                  strong("Var(X): "),
                  textOutput("origVar", inline = TRUE)
                ),
                p(
                  strong("sd(X): "),
                  textOutput("origSD", inline = TRUE)
                )
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Transformed aX + b"),
                p(
                  strong("E(aX + b): "),
                  textOutput("newMean", inline = TRUE)
                ),
                p(
                  strong("Var(aX + b): "),
                  textOutput("newVar", inline = TRUE)
                ),
                p(
                  strong("sd(aX + b): "),
                  textOutput("newSD", inline = TRUE)
                )
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("What changed?"),
                textOutput("transformMessage")
              )
            )
          )
        )
      )
    ),
    
    
    # ============================================================
    # Tab 2: Combine X and Y
    # ============================================================
    
    tabPanel(
      "Combine X and Y",
      br(),
      p(
        "Explore how the means, variances, covariance and correlation of two random variables affect linear combinations and products."
      ),
      
      sidebarLayout(
        
        sidebarPanel(
          
          h4("Distribution of X"),
          
          sliderInput(
            "mu_x", "Mean of X", min = 0, max = 20, value = 10,
            step = 0.5
          ),
          
          sliderInput(
            "var_x", "Variance of X", min = 0.25, max = 16, value = 4,
            step = 0.25
          ),
          
          h4("Distribution of Y"),
          
          sliderInput(
            "mu_y", "Mean of Y", min = 0, max = 20, value = 10,
            step = 0.5
          ),
          
          sliderInput(
            "var_y", "Variance of Y", min = 0.25, max = 16, value = 4,
            step = 0.25
          ),
          
          h4("Relationship between X and Y"),
          
          sliderInput(
            "rho",
            tags$span("Correlation ", tags$i("\u03c1")),
            min = -0.9, max = 0.9, value = 0,
            step = 0.05
          ),
          
          p(
            em(
              "In this bivariate normal example, \u03c1 = 0 corresponds to independence. Other values indicate dependence."
            )
          ),
          
          h4("Linear combination"),
          
          sliderInput(
            "a2", "a", min = -2.5, max = 2.5, value = 1,
            step = 0.1
          ),
          
          sliderInput(
            "b2", "b", min = -2.5, max = 2.5, value = 1,
            step = 0.1
          )
        ),
        
        mainPanel(
          
          # Joint distribution of X and Y
          plotlyOutput("jointPlot3d", height = "560px"),
          
          br(),
          
          # Distribution of aX + bY
          h4("Distribution of the linear combination aX + bY"),
          
          plotlyOutput("linearDensityPlot", height = "360px"),
          
          br(),
          
          fluidRow(
            
            column(
              4,
              wellPanel(
                h4("Linear combination: aX + bY"),
                p(
                  strong("E(aX + bY): "),
                  textOutput("linearMean", inline = TRUE)
                ),
                p(
                  strong("Var(aX + bY): "),
                  textOutput("linearVar", inline = TRUE)
                ),
                p(
                  strong("sd(aX + bY): "),
                  textOutput("linearSD", inline = TRUE)
                )
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Product: XY"),
                p(
                  strong("E(XY): "),
                  textOutput("productMean", inline = TRUE)
                ),
                p(
                  strong("E(X)E(Y): "),
                  textOutput("productOfMeans", inline = TRUE)
                ),
                p(
                  strong("Cov(X, Y): "),
                  textOutput("covariance", inline = TRUE)
                )
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("What to notice"),
                textOutput("combineMessage")
              )
            )
          )
        )
      )
    )
  )
)


# ================================================================
# Server
# ================================================================

server <- function(input, output, session) {
  
  
  # ================================================================
  # Transform tab
  # ================================================================
  
  mu_x_1 <- 10
  sd_x_1 <- 2
  var_x_1 <- sd_x_1^2
  
  
  # Current transformation shown below the sliders
  output$transformationFormula <- renderUI({
    
    a <- input$a
    b <- input$b
    
    # Format the coefficient of X
    if (a == 1) {
      x_part <- "X"
    } else if (a == -1) {
      x_part <- "-X"
    } else {
      x_part <- paste0(fmt(a, 1), "X")
    }
    
    # Format the constant term
    if (b > 0) {
      formula <- paste0(
        x_part,
        " + ",
        fmt(b, 1)
      )
    } else if (b < 0) {
      formula <- paste0(
        x_part,
        " - ",
        fmt(abs(b), 1)
      )
    } else {
      formula <- x_part
    }
    
    tags$strong(formula)
  })
  
  
  # Calculate transformed quantities
  transformed <- reactive({
    
    a <- input$a
    b <- input$b
    
    list(
      mean = a * mu_x_1 + b,
      variance = a^2 * var_x_1,
      sd = abs(a) * sd_x_1
    )
  })
  
  
  # Original X
  output$origMean <- renderText(
    fmt(mu_x_1)
  )
  
  output$origVar <- renderText(
    fmt(var_x_1)
  )
  
  output$origSD <- renderText(
    fmt(sd_x_1)
  )
  
  
  # Transformed X
  output$newMean <- renderText(
    fmt(transformed()$mean)
  )
  
  output$newVar <- renderText(
    fmt(transformed()$variance)
  )
  
  output$newSD <- renderText(
    fmt(transformed()$sd)
  )
  
  
  # Explanation
  output$transformMessage <- renderText({
    
    a <- input$a
    b <- input$b
    
    if (b == 0 && a == 1) {
      return("Nothing changes.")
    }
    
    parts <- character(0)
    
    if (b != 0) {
      parts <- c(
        parts,
        "Changing b shifts the distribution and changes the mean, but it does not change the variance."
      )
    }
    
    if (a != 1) {
      parts <- c(
        parts,
        paste0(
          "Changing a rescales the distribution. For example, when a = ",
          fmt(a, 1),
          ", distances from the mean are multiplied by ",
          fmt(abs(a), 1),
          ", so the variance is multiplied by a² = ",
          fmt(a^2),
          "."
        )
      )
    }
    
    paste(
      parts,
      collapse = " "
    )
  })
  
  
  # Plot original and transformed distribution
  output$transformPlot <- renderPlot({
    
    a <- input$a
    b <- input$b
    
    x <- seq(
      mu_x_1 - 4 * sd_x_1,
      mu_x_1 + 4 * sd_x_1,
      length.out = 600
    )
    
    fx <- dnorm(
      x,
      mean = mu_x_1,
      sd = sd_x_1
    )
    
    y <- a * x + b
    
    
    # ------------------------------------------------------------
    # Special case: a = 0
    # ------------------------------------------------------------
    
    if (a == 0) {
      
      plot(
        x, fx,
        type = "l",
        lwd = 3,
        col = "#174F8A",
        xlab = "Value",
        ylab = "Density",
        las = 1,
        main = "Distribution before and after transformation"
      )
      
      abline(
        v = mu_x_1,
        lty = 2,
        col = "#174F8A"
      )
      
      abline(
        v = b,
        lty = 2,
        col = "#C0504D"
      )
      
      legend(
        "topright",
        legend = c(
          "X",
          "aX + b (constant)",
          "mean of X",
          "value of aX + b"
        ),
        col = c(
          "#174F8A",
          "#C0504D",
          "#174F8A",
          "#C0504D"
        ),
        lty = c(1, 2, 2, 2),
        lwd = c(3, 2, 1, 1),
        bty = "n"
      )
      
      return()
    }
    
    
    # ------------------------------------------------------------
    # Transformed density
    # ------------------------------------------------------------
    
    fy <- fx / abs(a)
    
    xlim <- range(
      c(x, y)
    )
    
    ylim <- c(
      0,
      max(c(fx, fy)) * 1.35
    )
    
    
    plot(
      x, fx,
      type = "l",
      lwd = 3,
      col = "#174F8A",
      xlim = xlim,
      ylim = ylim,
      xlab = "Value",
      ylab = "Density",
      las = 1,
      main = "Distribution before and after transformation"
    )
    
    lines(
      y, fy,
      lwd = 3,
      col = "#C0504D"
    )
    
    
    # ------------------------------------------------------------
    # Means
    # ------------------------------------------------------------
    
    mu_new <- a * mu_x_1 + b
    
    abline(
      v = mu_x_1,
      lty = 2,
      col = "#174F8A"
    )
    
    abline(
      v = mu_new,
      lty = 2,
      col = "#C0504D"
    )
    
    
    # ------------------------------------------------------------
    # Three reference values:
    # mean - SD, mean, mean + SD
    # ------------------------------------------------------------
    
    ref_x <- c(
      mu_x_1 - sd_x_1,
      mu_x_1,
      mu_x_1 + sd_x_1
    )
    
    ref_y <- a * ref_x + b
    
    
    # Corresponding density heights
    ref_fx <- dnorm(
      ref_x,
      mean = mu_x_1,
      sd = sd_x_1
    )
    
    ref_fy <- dnorm(
      ref_y,
      mean = mu_new,
      sd = abs(a) * sd_x_1
    )
    
    
    # Mark the three reference values
    points(
      ref_x,
      ref_fx,
      pch = 21,
      bg = "white",
      col = "#174F8A",
      cex = 1.1
    )
    
    points(
      ref_y,
      ref_fy,
      pch = 21,
      bg = "white",
      col = "#C0504D",
      cex = 1.1
    )
    
    
    # Display the actual values
    text(
      ref_x,
      ref_fx + max(c(fx, fy)) * 0.055,
      labels = fmt(ref_x, 1),
      col = "#174F8A",
      font = 2,
      cex = 0.8
    )
    
    text(
      ref_y,
      ref_fy + max(c(fx, fy)) * 0.055,
      labels = fmt(ref_y, 1),
      col = "#C0504D",
      font = 2,
      cex = 0.8
    )
    
    
    # ------------------------------------------------------------
    # Show distances from the mean when the scale changes
    # ------------------------------------------------------------
    
    if (a != 1 && a != -1) {
      
      # Original distance from the mean
      original_y <- max(fx) * 0.72
      
      arrows(
        ref_x[1], original_y,
        ref_x[2], original_y,
        code = 3,
        angle = 20,
        length = 0.06,
        col = "#174F8A",
        lwd = 1.5
      )
      
      text(
        x = mean(c(ref_x[1], ref_x[2])),
        y = original_y + max(fx) * 0.07,
        labels = paste0(
          "Distance from mean of X = ",
          fmt(sd_x_1, 1)
        ),
        col = "#174F8A",
        font = 2,
        cex = 0.82
      )
      
      
      # Transformed distance from the mean
      transformed_left <- min(ref_y[1], ref_y[2])
      transformed_right <- max(ref_y[1], ref_y[2])
      
      transformed_y <- max(fy) * 0.72
      
      arrows(
        transformed_left, transformed_y,
        transformed_right, transformed_y,
        code = 3,
        angle = 20,
        length = 0.06,
        col = "#C0504D",
        lwd = 1.5
      )
      
      text(
        x = mean(c(transformed_left, transformed_right)),
        y = transformed_y + max(fy) * 0.07,
        labels = paste0(
          "Distance from mean of aX + b = ",
          fmt(abs(a) * sd_x_1, 1)
        ),
        col = "#C0504D",
        font = 2,
        cex = 0.82
      )
    }
    
    
    # Legend
    legend(
      "topright",
      legend = c(
        "X",
        "aX + b",
        "mean of X",
        "mean of aX + b"
      ),
      col = c(
        "#174F8A",
        "#C0504D",
        "#174F8A",
        "#C0504D"
      ),
      lty = c(1, 1, 2, 2),
      lwd = c(3, 3, 1, 1),
      bty = "n"
    )
  })
  
  
  # ================================================================
  # Combine tab
  # ================================================================
  
  combine_data <- reactive({
    
    mu_x <- input$mu_x
    mu_y <- input$mu_y
    
    sd_x <- sqrt(input$var_x)
    sd_y <- sqrt(input$var_y)
    
    rho <- input$rho
    
    a <- input$a2
    b <- input$b2
    
    cov_xy <- rho * sd_x * sd_y
    
    list(
      mu_x = mu_x,
      mu_y = mu_y,
      sd_x = sd_x,
      sd_y = sd_y,
      var_x = input$var_x,
      var_y = input$var_y,
      rho = rho,
      cov = cov_xy,
      
      linear_mean =
        a * mu_x +
        b * mu_y,
      
      linear_var =
        a^2 * input$var_x +
        b^2 * input$var_y +
        2 * a * b * cov_xy,
      
      product_mean =
        mu_x * mu_y +
        cov_xy,
      
      product_means =
        mu_x * mu_y
    )
  })
  
  
  # Linear combination values
  output$linearMean <- renderText(
    fmt(combine_data()$linear_mean)
  )
  
  output$linearVar <- renderText(
    fmt(combine_data()$linear_var)
  )
  
  output$linearSD <- renderText(
    fmt(sqrt(combine_data()$linear_var))
  )
  
  
  # Product values
  output$productMean <- renderText(
    fmt(combine_data()$product_mean)
  )
  
  output$productOfMeans <- renderText(
    fmt(combine_data()$product_means)
  )
  
  output$covariance <- renderText(
    fmt(combine_data()$cov)
  )
  
  
  # Explanation
  output$combineMessage <- renderText({
    
    d <- combine_data()
    
    if (abs(d$rho) < 1e-10) {
      
      paste0(
        "Here X and Y are independent (for this bivariate normal model). ",
        "The covariance is 0, so the variance of aX + bY contains no covariance term, ",
        "and E(XY) = E(X)E(Y)."
      )
      
    } else {
      
      direction <- if (d$rho > 0) {
        "positive"
      } else {
        "negative"
      }
      
      paste0(
        "Here X and Y are dependent, with ",
        direction,
        " correlation. ",
        "The covariance contributes to both Var(aX + bY) and E(XY): ",
        "E(XY) = E(X)E(Y) + Cov(X, Y)."
      )
    }
  })
  
  
  # ================================================================
  # Density of aX + bY
  # ================================================================
  
  output$linearDensityPlot <- renderPlotly({
    
    d <- combine_data()
    
    # For a bivariate normal distribution,
    # aX + bY is also normally distributed
    mu_z <- d$linear_mean
    var_z <- d$linear_var
    sd_z <- sqrt(var_z)
    
    z_seq <- seq(
      mu_z - 4 * sd_z,
      mu_z + 4 * sd_z,
      length.out = 500
    )
    
    density_z <- dnorm(
      z_seq,
      mean = mu_z,
      sd = sd_z
    )
    
    
    plot_ly(
      x = z_seq,
      y = density_z,
      type = "scatter",
      mode = "lines",
      
      line = list(
        color = "#174F8A",
        width = 3
      ),
      
      hovertemplate = paste(
        "z = %{x:.2f}",
        "<br>Density = %{y:.3f}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        
        title = list(
          text = paste0(
            "Distribution of Z = aX + bY",
            " &nbsp; | &nbsp; ",
            "a = ", fmt(input$a2),
            ", b = ", fmt(input$b2)
          )
        ),
        
        xaxis = list(
          title = "Z = aX + bY"
        ),
        
        yaxis = list(
          title = "Density"
        ),
        
        shapes = list(
          list(
            type = "line",
            x0 = mu_z,
            x1 = mu_z,
            y0 = 0,
            y1 = max(density_z),
            
            line = list(
              dash = "dash",
              color = "grey50"
            )
          )
        ),
        
        annotations = list(
          list(
            x = mu_z,
            y = max(density_z) * 0.92,
            
            text = paste0(
              "Mean = ", fmt(mu_z),
              "<br>SD = ", fmt(sd_z)
            ),
            
            showarrow = FALSE,
            yanchor = "top"
          )
        ),
        
        margin = list(
          l = 60,
          r = 20,
          b = 55,
          t = 65
        )
      )
  })
  
  
  # ================================================================
  # Bivariate normal joint density
  # ================================================================
  
  output$jointPlot3d <- renderPlotly({
    
    d <- combine_data()
    
    # Grid for the bivariate normal joint density
    nx <- 70
    ny <- 70
    
    x_seq <- seq(
      d$mu_x - 4 * d$sd_x,
      d$mu_x + 4 * d$sd_x,
      length.out = nx
    )
    
    y_seq <- seq(
      d$mu_y - 4 * d$sd_y,
      d$mu_y + 4 * d$sd_y,
      length.out = ny
    )
    
    
    # Covariance matrix
    Sigma <- matrix(
      c(
        d$var_x, d$cov,
        d$cov, d$var_y
      ),
      nrow = 2,
      byrow = TRUE
    )
    
    det_Sigma <- det(Sigma)
    inv_Sigma <- solve(Sigma)
    
    
    # Evaluate the bivariate normal density
    z <- outer(
      x_seq,
      y_seq,
      function(x, y) {
        
        dx <- x - d$mu_x
        dy <- y - d$mu_y
        
        quad <-
          inv_Sigma[1, 1] * dx^2 +
          2 * inv_Sigma[1, 2] * dx * dy +
          inv_Sigma[2, 2] * dy^2
        
        exp(-0.5 * quad) /
          (2 * pi * sqrt(det_Sigma))
      }
    )
    
    
    # Label the linear combination in the title
    z_label <- paste0(
      "aX + bY = ",
      fmt(input$a2),
      "X + ",
      fmt(input$b2),
      "Y"
    )
    
    
    plot_ly(
      x = x_seq,
      y = y_seq,
      z = z,
      type = "surface",
      colorscale = "Viridis",
      showscale = TRUE,
      
      colorbar = list(
        title = "Joint density"
      )
    ) %>%
      layout(
        
        title = list(
          text = paste0(
            "Bivariate normal joint distribution",
            " &nbsp; | &nbsp; ",
            z_label,
            " &nbsp; | &nbsp; ",
            "\u03c1 = ",
            fmt(d$rho)
          )
        ),
        
        scene = list(
          
          xaxis = list(
            title = paste0(
              "X (mean = ",
              fmt(d$mu_x),
              ", variance = ",
              fmt(d$var_x),
              ")"
            )
          ),
          
          yaxis = list(
            title = paste0(
              "Y (mean = ",
              fmt(d$mu_y),
              ", variance = ",
              fmt(d$var_y),
              ")"
            )
          ),
          
          zaxis = list(
            title = "Joint density"
          )
        ),
        
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 70
        )
      )
  })
}

shinyApp(ui, server)