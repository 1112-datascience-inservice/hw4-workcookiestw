library(shiny)
library(ggplot2)
library(ggbiplot)
library(MASS)
library(FactoMineR)
library(factoextra)
library(ca)

# UI
ui <- fluidPage(
  
  titlePanel("NCCU_DS2023_hw4_110971012"),
  
  sidebarLayout(
    sidebarPanel(
      h5("NCCU CS"),
      h5("110971012 YUN HAO"),
      h5("\n\n"),
      h4("PCA"),
      checkboxInput("pca_check", "Show PCA plot", value = TRUE),
      h5("PCA result"),
      verbatimTextOutput("pca_result"),
      h4("CA"),
      checkboxInput("ca_check", "Show CA plot", value = TRUE),
      h5(""),
      h5("原始數據呈現請將網頁往下拉"),
    ),
    mainPanel(
      plotOutput("pca_plot"),
      plotOutput("ca_plot"),
      tableOutput("iris_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # PCA analysis
  pca <- reactive({
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    
    # PCA result
    pca_result <- paste("PC1 variance ratio:", round(summary(ir.pca)$importance[2,1]*100, 2), "%",
                        "| PC2 variance ratio:", round(summary(ir.pca)$importance[2,2]*100, 2), "%")
    
    return(list(g, pca_result))
  })
  
  # CA analysis
  ca <- reactive({
    iris.ca <- CA(iris[, 1:4], graph = FALSE)
    g <- fviz_ca(iris.ca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                 repel = TRUE, labelsize = 4, ggtheme = theme_gray())
    # CA result
    ca_result <- paste("Factor 1 variance ratio:", round(iris.ca$eig[1,2]*100, 2), "%",
                       "| Factor 2 variance ratio:", round(iris.ca$eig[2,2]*100, 2), "%")
    
    return(list(g, ca_result))
  })
  
  # PCA plot
  output$pca_plot <- renderPlot({
    if (input$pca_check) {
      pca()[[1]]
    }
  })
  
  # PCA result
  output$pca_result <- renderPrint({
    if (input$pca_check) {
      pca()[[2]]
    }
  })
  
  # CA plot
  output$ca_plot <- renderPlot({
    if (input$ca_check) {
      ca()
    }
  })
  
  
  
  # Iris table
  output$iris_table <- renderTable({
    iris
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
