outlier_ui <- function(id) {
  # Application title

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("outlier_method", "Outlier Reduction Method", 
                  choices = c("c-tf-idf",
                              "embeddings",
                              "tokenset similarity")),
      sliderInput("outlier_threshold",
                  "Threshold:",
                  min = 0,
                  max = 1,
                  value = 0.3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outlier_plot")
    )
  )
}
