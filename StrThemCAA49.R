library(shiny)
library(plotly)
library(dplyr)
library(magrittr)
library(RColorBrewer)

# Charger les données avant le lancement de l'app
entries_df <- read.csv("/Users/c.ca/Downloads/ANA49.csv", stringsAsFactors = FALSE)
labels_unique <- unique(entries_df$label)

ui <- fluidPage(
  titlePanel("Visualisation spirale 3D avec contrôle de l'échelle et sélection/btn Tout"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("radius_scale",
                  "Échelle du rayon:",
                  min = 1,
                  max = 10,
                  value = 5,
                  step = 0.1),
      
      # Boutons pour tout sélectionner / désélectionner
      actionButton("select_all", "Tout sélectionner"),
      actionButton("deselect_all", "Tout désélectionner"),
      
      selectInput("selected_labels",
                  "Sélectionnez les entrées à afficher :",
                  choices = labels_unique,
                  selected = labels_unique,
                  multiple = TRUE,
                  selectize = TRUE)
    ),
    
    mainPanel(
      plotlyOutput("spiralPlot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  # Observateurs pour gérer les boutons Tout sélectionner / Tout désélectionner
  observeEvent(input$select_all, {
    updateSelectInput(session, "selected_labels", selected = labels_unique)
  })
  
  observeEvent(input$deselect_all, {
    updateSelectInput(session, "selected_labels", selected = character(0))
  })
  
  output$spiralPlot <- renderPlotly({
    req(input$selected_labels)  # S'assurer qu'il y a une sélection
    
    # Filtrage des données selon la sélection utilisateur
    df_filtered <- entries_df %>%
      filter(label %in% input$selected_labels)
    
    # Calcul des fréquences des labels dans la sélection
    freq_df <- df_filtered %>%
      group_by(label) %>%
      summarise(freq = n()) %>%
      ungroup()
    
    df <- df_filtered %>%
      left_join(freq_df, by = "label")
    
    # Calcul du rayon inverse pondéré par la fréquence
    rayon_inverse_freq <- 1 / df$freq
    rayon_normalized <- (rayon_inverse_freq - min(rayon_inverse_freq)) / (max(rayon_inverse_freq) - min(rayon_inverse_freq))
    
    # Application de l'échelle dynamique définie par l'utilisateur
    rayon_final <- 1 + input$radius_scale * rayon_normalized
    
    n <- nrow(df)
    theta <- seq(0, 12 * pi, length.out = n)
    z <- seq(0, 10, length.out = n)
    
    x <- rayon_final * cos(theta)
    y <- rayon_final * sin(theta)
    
    spirale_df <- data.frame(
      x = x,
      y = y,
      z = z,
      type = df$label
    )
    
    type_list <- unique(spirale_df$type)
    n_types <- length(type_list)
    base_colors <- brewer.pal(min(n_types, 12), "Set3")
    extended_palette <- colorRampPalette(base_colors)(n_types)
    names(extended_palette) <- type_list
    
    plot_ly(
      spirale_df,
      x = ~x,
      y = ~y,
      z = ~z,
      type = "scatter3d",
      mode = "lines+markers",
      color = ~type,
      colors = extended_palette,
      marker = list(size = 5),
      line = list(width = 2)
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          zaxis = list(title = "")
        ),
        legend = list(
          title = list(text = "structure thématique du poème"),
          font = list(size = 11, color = "black"),
          orientation = "v",
          x = 1.02,
          y = 1,
          width = 0.15,
          bgcolor = "rgba(255,255,255,0.8)"
        )
      )
  })
}

shinyApp(ui, server)
