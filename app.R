# ui.R
library(shiny)
ui <- fluidPage(
  titlePanel("Housing Price Analysis (Ames & California)"),
  sidebarLayout(
    sidebarPanel(
      h4("Summary"),
      verbatimTextOutput("summaryText"),
      h4("Model RMSE"),
      verbatimTextOutput("modelMetrics")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sale Price by Neighborhood", plotOutput("barPlot")),
        tabPanel("Sale vs Liv Area", plotOutput("scatterPlot")),
        tabPanel("Boxplot by Bedrooms", plotOutput("bedroomBoxPlot")),
        tabPanel("Liv Area by Neighborhood", plotOutput("areaBoxPlot")),
        tabPanel("Feature Importance", plotOutput("importancePlot")),
        tabPanel("Correlation Heatmap", plotOutput("correlationHeatmap"))
      )
    )
  )
)
