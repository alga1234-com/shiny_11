# server.R

server <- function(input, output) {

  output$summaryText <- renderPrint({
    summary(combined_data)
  })

  output$modelMetrics <- renderPrint({
    list(
      "Linear Model RMSE" = lm_rmse,
      "XGBoost RMSE" = xgb_rmse
    )
  })

  output$barPlot <- renderPlot({
    ggplot(combined_summary, aes(x = reorder(Neighborhood, -SalePrice), y = SalePrice, fill = sourcename)) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
      labs(title = "SalePrice by Neighborhood", x = "Neighborhood", y = "SalePrice") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

  output$scatterPlot <- renderPlot({
    ggplot(combined_data, aes(x = LivArea, y = SalePrice)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Sale Price vs Living Area", x = "Living Area", y = "Sale Price")
  })

  output$bedroomBoxPlot <- renderPlot({
    ggplot(combined_data, aes(x = as.factor(Total_bedrooms), y = SalePrice)) +
      geom_boxplot(fill = "green") +
      labs(title = "Sale Price by Bedroom Count", x = "Bedrooms", y = "Sale Price")
  })

  output$areaBoxPlot <- renderPlot({
    ggplot(combined_data, aes(x = Neighborhood, y = LivArea)) +
      geom_boxplot(fill = "wheat") +
      labs(title = "Living Area by Neighborhood", x = "Neighborhood", y = "Living Area") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$importancePlot <- renderPlot({
    xgb.plot.importance(importance, top_n = 10)
  })

  output$correlationHeatmap <- renderPlot({
    numeric_data <- combined_data %>% select_if(is.numeric)
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    melted_cor <- melt(cor_matrix)

    ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
      coord_fixed()
  })
}
