library(shiny)
library(shinydashboard)
library(ggplot2)
library(cluster)
library(factoextra)
library(plotly)
library(dplyr)

# Cargar dataset de calidad del vino tinto
wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")

# UI con shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Calidad del Vino"),
  dashboardSidebar(
    checkboxGroupInput("selected_vars", "Seleccionar Variables:",
                       choices = names(wine),
                       selected = c("alcohol", "density")),
    sliderInput("num_clusters", "Número de Clusters (K):", min = 2, max = 6, value = 3),
    actionButton("run", "Ejecutar Agrupamiento")
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("scatterPlot"), width = 6),
      box(plotlyOutput("clusterPlot"), width = 6)
    ),
    fluidRow(
      box(tableOutput("clusterTable"), width = 6),
      box(plotlyOutput("histPlot"), width = 6)
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  observeEvent(input$run, {
    selected_vars <- input$selected_vars
    wine_numeric <- wine %>% select(all_of(selected_vars))
    wine_scaled <- as.data.frame(scale(wine_numeric))
    
    set.seed(123)
    kmeans_result <- kmeans(wine_scaled, centers = input$num_clusters, nstart = 25)
    
    output$clusterPlot <- renderPlotly({
      p <- fviz_cluster(kmeans_result, data = wine_scaled, geom = "point")
      ggplotly(p)
    })
    
    output$scatterPlot <- renderPlotly({
      p <- ggplot(wine, aes_string(x = selected_vars[1], y = selected_vars[2])) +
        geom_point(aes(color = factor(kmeans_result$cluster)), size = 3) +
        labs(title = "Scatter Plot con Clusters", color = "Cluster")
      ggplotly(p)
    })
    
    output$histPlot <- renderPlotly({
      p <- ggplot(wine, aes_string(x = selected_vars[1])) +
        geom_histogram(fill = "blue", bins = 20, alpha = 0.7) +
        labs(title = "Histograma de Variable Seleccionada")
      ggplotly(p)
    })
    
    output$clusterTable <- renderTable({
      table(kmeans_result$cluster)
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
