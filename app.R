library(shiny)

# Helper for formatting numbers
fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

# Colours used for the original and transformed distributions
original_col <- "#426F70"      # slate teal
transformed_col <- "#B94F64"   # raspberry

ui <- fluidPage(
  withMathJax(),
  titlePanel("Expectation and variance properties"),
  fluidRow(
    column(4,
           wellPanel(
             sliderInput("a", "a", min=-2.5, max=2.5, value=1, step=0.1),
             sliderInput("b", "b", min=-10, max=10, value=0, step=0.5),
             p(strong("Current transformation: "), uiOutput("transformationFormula", inline=TRUE)),
             tags$hr(),
             strong("Properties"),
             div(HTML("\\[\\operatorname{E}(aX+b)=a\\operatorname{E}(X)+b\\]")),
             div(HTML("\\[\\operatorname{Var}(aX+b)=a^2\\operatorname{Var}(X)\\]"))
           ),
           wellPanel(h4("What changed?"), uiOutput("transformMessageMath")),
           wellPanel(h4("Remember"), uiOutput("rememberMath"))
    ),
    column(8,
           h3(HTML("Transforming \\(X\\) to \\(aX+b\\)")),
           fluidRow(
             column(6, wellPanel(h4("Original distribution"), uiOutput("originalDistribution"))),
             column(6, wellPanel(h4("Transformed distribution"), uiOutput("transformedDistribution")))
           ),
           plotOutput("transformPlot", height="500px")
    )
  )
)

server <- function(input, output, session) {
  
  # Fixed original distribution: X ~ N(10, 4),
  # where the second parameter is the variance.
  mu_x <- 10
  sigma_x <- 2
  sigma2_x <- sigma_x^2
  
  # ------------------------------------------------------------
  # Distribution labels
  # ------------------------------------------------------------
  
  output$originalDistribution <- renderUI({
    withMathJax(
      HTML(
        paste0(
          "<div style='font-size:1.15em;'>",
          "\\[X\\sim N(10,4)\\]",
          "\\[\\mu=\\operatorname{E}(X)=10\\]",
          "\\[\\sigma=\\operatorname{sd}(X)=2\\]",
          "\\[\\sigma^2=\\operatorname{Var}(X)=4\\]",
          "</div>"
        )
      )
    )
  })
  
  output$transformedDistribution <- renderUI({
    a <- input$a
    b <- input$b
    mu_new <- a * mu_x + b
    sigma_new <- abs(a) * sigma_x
    sigma2_new <- a^2 * sigma2_x
    
    if (a == 0) {
      math_text <- paste0(
        "\\[aX+b=", fmt(b, 1), "\\quad\\text{(constant)}\\]",
        "\\[\\mu=\\operatorname{E}(aX+b)=", fmt(mu_new, 1), "\\]",
        "\\[\\sigma=\\operatorname{sd}(aX+b)=0\\]",
        "\\[\\sigma^2=\\operatorname{Var}(aX+b)=0\\]"
      )
    } else {
      math_text <- paste0(
        "\\[aX+b\\sim N\\left(", fmt(mu_new, 1), ",\\,", fmt(sigma2_new, 1), "\\right)\\]",
        "\\[\\mu=\\operatorname{E}(aX+b)=", fmt(mu_new, 1), "\\]",
        "\\[\\sigma=\\operatorname{sd}(aX+b)=", fmt(sigma_new, 1), "\\]",
        "\\[\\sigma^2=\\operatorname{Var}(aX+b)=", fmt(sigma2_new, 1), "\\]"
      )
    }
    withMathJax(HTML(paste0("<div style='font-size:1.15em;'>", math_text, "</div>")))
  })
  
  # ------------------------------------------------------------
  # Current transformation shown below the sliders
  # ------------------------------------------------------------
  
  output$transformationFormula <- renderUI({
    a <- input$a
    b <- input$b
    
    if (a == 1) x_part <- "X"
    else if (a == -1) x_part <- "-X"
    else x_part <- paste0(fmt(a, 1), "X")
    
    if (b > 0) formula <- paste0(x_part, "+", fmt(b, 1))
    else if (b < 0) formula <- paste0(x_part, "-", fmt(abs(b), 1))
    else formula <- x_part
    
    withMathJax(HTML(paste0("\\(", formula, "\\)")))
  })
  
  # ------------------------------------------------------------
  # Original values
  # ------------------------------------------------------------
  
  output$origMean <- renderText(fmt(mu_x, 1))
  output$origSD <- renderText(fmt(sigma_x, 1))
  output$origVar <- renderText(fmt(sigma2_x, 1))
  
  # ------------------------------------------------------------
  # Transformed values
  # ------------------------------------------------------------
  
  transformed <- reactive({
    a <- input$a
    b <- input$b
    
    list(
      mean = a * mu_x + b,
      sd = abs(a) * sigma_x,
      variance = a^2 * sigma2_x
    )
  })
  
  output$newMean <- renderText(
    fmt(transformed()$mean, 1)
  )
  
  output$newSD <- renderText(
    fmt(transformed()$sd, 1)
  )
  
  output$newVar <- renderText(
    fmt(transformed()$variance, 1)
  )
  
  # ------------------------------------------------------------
  # Explanation
  # ------------------------------------------------------------
  
  output$transformMessageMath <- renderUI({
    a <- input$a
    b <- input$b
    if (a == 1 && b == 0)
      return(withMathJax(HTML("Nothing changes: the transformed distribution is the same as the original distribution.")))
    parts <- character(0)
    if (b != 0)
      parts <- c(parts, "Changing \\(b\\) shifts the distribution and changes the mean, but it does not change the variance.")
    if (a != 1)
      parts <- c(parts, paste0("Changing \\(a\\) rescales the distribution. When \\(a=", fmt(a,1),
                               "\\), distances from the mean are multiplied by \\(", fmt(abs(a),1),
                               "\\), so the variance is multiplied by \\(a^2=", fmt(a^2,2), "\\)."))
    withMathJax(HTML(paste(parts, collapse=" ")))
  })
  
  output$rememberMath <- renderUI({
    withMathJax(HTML("Changing \\(b\\) shifts the distribution. Changing \\(a\\) changes its scale (and reflects it when \\(a<0\\))."))
  })
  
  # ------------------------------------------------------------
  # Plot original and transformed distributions
  # ------------------------------------------------------------
  
  output$transformPlot <- renderPlot({
    a <- input$a
    b <- input$b
    
    x <- seq(
      mu_x - 4 * sigma_x,
      mu_x + 4 * sigma_x,
      length.out = 600
    )
    
    fx <- dnorm(
      x,
      mean = mu_x,
      sd = sigma_x
    )
    
    y <- a * x + b
    
    # ----------------------------------------------------------
    # Special case a = 0
    # ----------------------------------------------------------
    
    if (a == 0) {
      
      plot(
        x, fx,
        type = "l",
        lwd = 3,
        col = original_col,
        xlab = "Value",
        ylab = "Density",
        las = 1,
        main = "Distribution before and after transformation"
      )
      
      abline(
        v = mu_x,
        lty = 2,
        col = original_col
      )
      
      abline(
        v = b,
        lty = 2,
        col = transformed_col
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
          original_col,
          transformed_col,
          original_col,
          transformed_col
        ),
        lty = c(1, 2, 2, 2),
        lwd = c(3, 2, 1, 1),
        bty = "n"
      )
      
      return()
    }
    
    # ----------------------------------------------------------
    # Transformed density
    # ----------------------------------------------------------
    
    fy <- fx / abs(a)
    
    xlim <- range(c(x, y))
    
    ylim <- c(
      0,
      max(c(fx, fy)) * 1.35
    )
    
    plot(
      x, fx,
      type = "l",
      lwd = 3,
      col = original_col,
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
      col = transformed_col
    )
    
    # ----------------------------------------------------------
    # Means
    # ----------------------------------------------------------
    
    mu_new <- a * mu_x + b
    
    abline(
      v = mu_x,
      lty = 2,
      col = original_col
    )
    
    abline(
      v = mu_new,
      lty = 2,
      col = transformed_col
    )
    
    # ----------------------------------------------------------
    # Three reference values:
    # mean - SD, mean, mean + SD
    # ----------------------------------------------------------
    
    ref_x <- c(
      mu_x - sigma_x,
      mu_x,
      mu_x + sigma_x
    )
    
    ref_y <- a * ref_x + b
    
    ref_fx <- dnorm(
      ref_x,
      mean = mu_x,
      sd = sigma_x
    )
    
    ref_fy <- dnorm(
      ref_y,
      mean = mu_new,
      sd = abs(a) * sigma_x
    )
    
    points(
      ref_x,
      ref_fx,
      pch = 21,
      bg = "white",
      col = original_col,
      cex = 1.1
    )
    
    points(
      ref_y,
      ref_fy,
      pch = 21,
      bg = "white",
      col = transformed_col,
      cex = 1.1
    )
    
    # Actual reference values
    text(
      ref_x,
      ref_fx + max(c(fx, fy)) * 0.055,
      labels = fmt(ref_x, 1),
      col = original_col,
      font = 2,
      cex = 0.8
    )
    
    text(
      ref_y,
      ref_fy + max(c(fx, fy)) * 0.055,
      labels = fmt(ref_y, 1),
      col = transformed_col,
      font = 2,
      cex = 0.8
    )
    
    # ----------------------------------------------------------
    # Distance annotations
    # ----------------------------------------------------------
    
    if (a != 1 && a != -1) {
      
      original_y <- max(fx) * 0.72
      
      arrows(
        ref_x[1], original_y,
        ref_x[2], original_y,
        code = 3,
        angle = 20,
        length = 0.06,
        col = original_col,
        lwd = 1.5
      )
      
      text(
        x = mean(c(ref_x[1], ref_x[2])),
        y = original_y + max(fx) * 0.07,
        labels = paste0(
          "Distance from mean of X = ",
          fmt(sigma_x, 1)
        ),
        col = original_col,
        font = 2,
        cex = 0.82
      )
      
      transformed_left <- min(ref_y[1], ref_y[2])
      transformed_right <- max(ref_y[1], ref_y[2])
      
      transformed_y <- max(fy) * 0.72
      
      arrows(
        transformed_left, transformed_y,
        transformed_right, transformed_y,
        code = 3,
        angle = 20,
        length = 0.06,
        col = transformed_col,
        lwd = 1.5
      )
      
      text(
        x = mean(c(transformed_left, transformed_right)),
        y = transformed_y + max(fy) * 0.07,
        labels = paste0(
          "Distance from mean of aX + b = ",
          fmt(abs(a) * sigma_x, 1)
        ),
        col = transformed_col,
        font = 2,
        cex = 0.82
      )
    }
    
    legend(
      "topright",
      legend = c(
        "X",
        "aX + b",
        "mean of X",
        "mean of aX + b"
      ),
      col = c(
        original_col,
        transformed_col,
        original_col,
        transformed_col
      ),
      lty = c(1, 1, 2, 2),
      lwd = c(3, 3, 1, 1),
      bty = "n"
    )
  })
}

shinyApp(ui, server)
