# ==============================================================================
# PROYECTO FINAL: ESTADÍSTICA DESCRIPTIVA - DASHBOARD INTERACTIVO (SHINY)
# Autor: Kevin Quiñonez - Desarrollo de Software
# ==============================================================================

# 1. CARGA DE LIBRERÍAS
# Si te falta alguna, recuerda instalarla en consola con install.packages("nombre")
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(bslib) # Librería extra para darle un tema moderno (Bootstrap) al frontend

# 2. CARGA DE DATOS (Se hace fuera del UI y Server para que cargue solo una vez)
mi_ruta <- "C:/Users/kadri/Desktop/Escritorio/Proyecto Estadistica/data/PF estadistica Descriptiva.csv"
datos_app <- read_csv(mi_ruta, show_col_types = FALSE)

# ==============================================================================
# FRONTEND (USER INTERFACE - UI)
# ==============================================================================
ui <- fluidPage(
  
  # Tema visual moderno usando bslib (similar a Bootstrap en web dev normal)
  theme = bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
  
  # Título de la App Web
  titlePanel("📊 Dashboard Interactivo: Ventas de Contratos"),
  
  # Estructura de barra lateral (Filtros) y panel principal (Gráficos)
  sidebarLayout(
    
    # --- PANEL LATERAL (FILTROS) ---
    sidebarPanel(
      h4("Filtros Interactivos"),
      p("Ajusta los controles para actualizar los datos en tiempo real."),
      hr(), # Línea divisoria
      
      # Filtro 1: Dropdown para Sucursal
      selectInput(inputId = "filtro_sucursal", 
                  label = "🏢 Seleccione Sucursal:",
                  choices = c("Todas", unique(datos_app$Sucursal)),
                  selected = "Todas"),
      
      # Filtro 2: Dropdown para Financiamiento
      selectInput(inputId = "filtro_financia", 
                  label = "💳 Tipo de Financiamiento:",
                  choices = c("Todos", unique(datos_app$Financiamiento)),
                  selected = "Todos"),
      
      # Filtro 3: Slider de Rango para Monto de Contrato
      sliderInput(inputId = "filtro_monto", 
                  label = "💰 Rango Total de Contrato ($):",
                  min = min(datos_app$`Total Contrato`, na.rm = TRUE),
                  max = max(datos_app$`Total Contrato`, na.rm = TRUE),
                  value = c(0, max(datos_app$`Total Contrato`, na.rm = TRUE)), # Rango por defecto
                  pre = "$", sep = ",")
    ),
    
    # --- PANEL PRINCIPAL (RESULTADOS) ---
    mainPanel(
      # Creamos pestañas (Tabs) para organizar la información
      tabsetPanel(
        
        # Pestaña 1: Visualizaciones visuales
        tabPanel("📈 Visualización Interactiva", 
                 br(),
                 plotOutput("grafico_histograma"),
                 hr(),
                 plotOutput("grafico_boxplot")
        ),
        
        # Pestaña 2: Números y resúmenes calculados en vivo
        tabPanel("🧮 Resumen Estadístico", 
                 br(),
                 h4("Medidas Estadísticas de los datos filtrados:"),
                 verbatimTextOutput("texto_estadisticas")
        ),
        
        # Pestaña 3: Tabla de datos crudos filtrados
        tabPanel("📋 Base de Datos", 
                 br(),
                 dataTableOutput("tabla_datos")
        )
      )
    )
  )
)

# ==============================================================================
# BACKEND (SERVER LÓGICA)
# ==============================================================================
server <- function(input, output) {
  
  # REACTIVIDAD: Creamos un "DataFrame" dinámico que escucha a los filtros del UI
  # Cada vez que el usuario mueve un filtro, este bloque se re-ejecuta.
  datos_filtrados <- reactive({
    
    df <- datos_app
    
    # Lógica de Filtrado: Sucursal
    if (input$filtro_sucursal != "Todas") {
      df <- df %>% filter(Sucursal == input$filtro_sucursal)
    }
    
    # Lógica de Filtrado: Financiamiento
    if (input$filtro_financia != "Todos") {
      df <- df %>% filter(Financiamiento == input$filtro_financia)
    }
    
    # Lógica de Filtrado: Rango de Montos
    df <- df %>% filter(`Total Contrato` >= input$filtro_monto[1] & 
                          `Total Contrato` <= input$filtro_monto[2])
    
    return(df)
  })
  
  # OUTPUT 1: Renderizar Histograma
  output$grafico_histograma <- renderPlot({
    # Validamos que haya datos, si está vacío por los filtros, no da error
    req(nrow(datos_filtrados()) > 0) 
    
    ggplot(datos_filtrados(), aes(x = `Total Contrato`)) +
      geom_histogram(fill = "#3498db", color = "white", bins = 20) +
      labs(title = paste("Distribución de Contratos (N =", nrow(datos_filtrados()), ")"), 
           x = "Monto ($)", y = "Frecuencia") +
      theme_minimal(base_size = 14)
  })
  
  # OUTPUT 2: Renderizar Boxplot
  output$grafico_boxplot <- renderPlot({
    req(nrow(datos_filtrados()) > 0)
    
    ggplot(datos_filtrados(), aes(x = Financiamiento, y = `Total Contrato`, fill = Financiamiento)) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "Detección de Outliers según Filtros Aplicados", 
           x = "Tipo de Financiamiento", y = "Monto ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  # OUTPUT 3: Renderizar Estadísticas en Texto
  output$texto_estadisticas <- renderPrint({
    df <- datos_filtrados()
    
    if(nrow(df) == 0){
      cat("No hay datos en este rango de filtros.")
    } else {
      cat("Total de Contratos Mostrados: ", nrow(df), "\n\n")
      
      cat("--- TENDENCIA CENTRAL ---\n")
      cat("Media (Promedio)  : $", round(mean(df$`Total Contrato`, na.rm = TRUE), 2), "\n")
      cat("Mediana (Centro)  : $", round(median(df$`Total Contrato`, na.rm = TRUE), 2), "\n\n")
      
      cat("--- DISPERSIÓN ---\n")
      cat("Desviación Est.   : $", round(sd(df$`Total Contrato`, na.rm = TRUE), 2), "\n")
      media_val <- mean(df$`Total Contrato`, na.rm = TRUE)
      sd_val <- sd(df$`Total Contrato`, na.rm = TRUE)
      cat("Coef. de Variación: ", round((sd_val / media_val) * 100, 2), "%\n")
    }
  })
  
  # OUTPUT 4: Renderizar Tabla Dinámica
  output$tabla_datos <- renderDataTable({
    datos_filtrados()
  }, options = list(pageLength = 10, scrollX = TRUE)) # Configuración para que sea amigable a la vista
}

# ==============================================================================
# EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
shinyApp(ui = ui, server = server)