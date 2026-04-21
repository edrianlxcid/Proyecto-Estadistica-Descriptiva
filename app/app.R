# ==============================================================================
# PROYECTO FINAL: DASHBOARD DE AUDITORÍA Y PREDICCIÓN
# Autores: Kevin Quiñonez, Michael Chilan, Steven Gaspata, Luis Tarabata
# ==============================================================================

# --- BLOQUE DE SEGURIDAD: INSTALACIÓN Y CARGA ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, ggplot2, readr, bslib, shinyWidgets)

# 1. CARGA DE DATOS
# Asegúrate de que la ruta sea correcta en tu PC
mi_ruta <- "C:/Users/kadri/Desktop/Escritorio/Proyecto Estadistica/data/PF estadistica Descriptiva.csv"

# Verificamos si el archivo existe antes de cargar para evitar crashes
if (!file.exists(mi_ruta)) {
  stop("¡Error! No se encontró el archivo CSV en la ruta especificada.")
}

datos_app <- read_csv(mi_ruta, show_col_types = FALSE)

# --- UI (INTERFAZ DE USUARIO) ---
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "superhero", primary = "#e74c3c"),
  title = "📊 Inteligencia de Negocios: Plan vs Contado",
  
  sidebar = sidebar(
    title = "Panel de Control",
    bg = "#2c3e50",
    
    # BUSCADORES ACTIVOS (Requieren shinyWidgets)
    pickerInput("sucursal_select", "📍 Buscador de Sucursal:", 
                choices = c("Todas", unique(datos_app$Sucursal)), 
                selected = "Todas", 
                options = list(`live-search` = TRUE)),
    
    checkboxGroupInput("pago_select", "💳 Modalidad de Pago:", 
                       choices = unique(datos_app$Financiamiento), 
                       selected = unique(datos_app$Financiamiento)),
    
    sliderInput("monto_range", "💰 Rango de Contrato ($):", 
                min = 0, 
                max = max(datos_app$`Total Contrato`, na.rm = TRUE),
                value = c(0, max(datos_app$`Total Contrato`, na.rm = TRUE)),
                pre = "$")
  ),
  
  # KPIs DINÁMICOS
  layout_column_wrap(
    width = 1/3,
    value_box("Ventas en Filtro", textOutput("kpi_total"), showcase = icon("cash-register"), theme = "primary"),
    value_box("Proyección Próximo Mes", textOutput("kpi_proy"), showcase = icon("chart-line"), theme = "warning"),
    value_box("Contratos en Riesgo", textOutput("kpi_riesgo"), showcase = icon("triangle-exclamation"), theme = "danger")
  ),
  
  navset_card_pill(
    nav_panel("📈 Estadística Descriptiva", 
              layout_column_wrap(
                width = 1/2,
                card(card_header("Histograma: Plan vs Contado (Apilado)"), plotOutput("hist_plot")),
                card(card_header("Caja y Bigotes (Comparativa de Grupos)"), plotOutput("box_plot"))
              )
    ),
    nav_panel("🔮 Proyecciones a Futuro", 
              card(
                card_header("Tendencia y Regresión Lineal"),
                plotOutput("trend_plot"),
                card_footer("La línea amarilla indica la dirección esperada de las ventas basada en el modelo lineal.")
              )
    ),
    nav_panel("📉 Auditoría de Pérdidas", 
              card(card_header("Sucursales con Menor Rendimiento"), plotOutput("low_plot"))
    ),
    nav_panel("📋 Reporte de Datos", card(dataTableOutput("tabla_final")))
  )
)

# --- SERVER (LÓGICA) ---
server <- function(input, output) {
  
  # FILTRO MAESTRO
  data_ready <- reactive({
    res <- datos_app
    
    if (input$sucursal_select != "Todas") {
      res <- res %>% filter(Sucursal == input$sucursal_select)
    }
    
    res <- res %>% filter(
      Financiamiento %in% input$pago_select,
      `Total Contrato` >= input$monto_range[1],
      `Total Contrato` <= input$monto_range[2]
    )
    return(res)
  })
  
  # LÓGICA DE KPIs
  output$kpi_total <- renderText({ 
    paste0("$", format(round(sum(data_ready()$`Total Contrato`, na.rm = TRUE), 0), big.mark = ",")) 
  })
  
  output$kpi_proy <- renderText({ 
    total <- sum(data_ready()$`Total Contrato`, na.rm = TRUE)
    paste0("$", format(round(total * 1.15, 0), big.mark = ",")) # Proyección simple del +15%
  })
  
  output$kpi_riesgo <- renderText({
    umbral <- quantile(datos_app$`Total Contrato`, 0.10, na.rm = TRUE)
    riesgo <- data_ready() %>% filter(`Total Contrato` <= umbral) %>% count()
    paste0(riesgo$n, " Contratos")
  })
  
  # GRÁFICO 1: HISTOGRAMA APILADO
  output$hist_plot <- renderPlot({
    req(nrow(data_ready()) > 0)
    ggplot(data_ready(), aes(x = `Total Contrato`, fill = Financiamiento)) +
      geom_histogram(color = "white", bins = 20, position = "stack") +
      scale_fill_manual(values = c("PLAN" = "#e74c3c", "CONTADO" = "#2ecc71")) +
      theme_minimal() + 
      labs(x = "Monto del Contrato ($)", y = "Frecuencia", fill = "Método:")
  })
  
  # GRÁFICO 2: BOXPLOT
  output$box_plot <- renderPlot({
    req(nrow(data_ready()) > 0)
    ggplot(data_ready(), aes(x = Financiamiento, y = `Total Contrato`, fill = Financiamiento)) +
      geom_boxplot(outlier.color = "yellow", outlier.size = 3) + 
      scale_fill_manual(values = c("PLAN" = "#e74c3c", "CONTADO" = "#2ecc71")) +
      theme_minimal() + 
      labs(x = "Modalidad de Pago", y = "Monto ($)")
  })
  
  # GRÁFICO 3: TENDENCIA (REGRESIÓN)
  output$trend_plot <- renderPlot({
    req(nrow(data_ready()) > 2)
    df_trend <- data_ready() %>% mutate(orden = row_number())
    ggplot(df_trend, aes(x = orden, y = `Total Contrato`)) +
      geom_point(color = "#ecf0f1", alpha = 0.6) +
      geom_smooth(method = "lm", color = "yellow", fill = "yellow", alpha = 0.2) +
      theme_dark() + 
      labs(x = "Secuencia de Transacciones", y = "Monto ($)")
  })
  
  # GRÁFICO 4: AUDITORÍA DE SUCURSALES
  output$low_plot <- renderPlot({
    req(nrow(data_ready()) > 0)
    data_ready() %>% 
      group_by(Sucursal) %>% 
      summarise(Total = sum(`Total Contrato`, na.rm = TRUE)) %>%
      arrange(Total) %>%
      ggplot(aes(x = reorder(Sucursal, Total), y = Total, fill = Total)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      scale_fill_gradient(low = "#c0392b", high = "#27ae60") +
      theme_minimal() + 
      labs(x = "Sucursal", y = "Ingresos Totales ($)")
  })
  
  output$tabla_final <- renderDataTable({ data_ready() })
}

# LANZAR APP
shinyApp(ui, server)