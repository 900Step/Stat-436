library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(embed)

url <- "https://raw.githubusercontent.com/900Step/Stat-436/main/cancer.csv"

cancer <- read_csv(url)
ui<-fluidPage(
  titlePanel("The UMAP plot of the breast cance dataset"),
  numericInput("input_n", "n_neighbors: ", value = 50, min = 5, max = 200),
  plotOutput("umap")
)


server <- function(input, output){
  
  umap_prep <- reactive({
    recipe(~., data = cancer) %>%
      update_role(id, diagnosis, new_role = "id") %>%
      step_umap(all_predictors(), learn_rate = 0.1, neighbors = input$input_n)%>%
      prep()
    
  })
  umap_embeddings <- reactive({
    bake(umap_prep(), new_data = cancer) %>%
      left_join(cancer)
    
  })
  
  output$umap <- renderPlot({
    ggplot(umap_embeddings())+
      geom_point(aes(UMAP1, UMAP2, group = diagnosis, col = diagnosis), alpha = 0.4) +
      scale_color_brewer(palette = "Set2")
  })
}

shinyApp(ui, server)