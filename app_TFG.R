library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(mapSpain)
library(leaflet)
library(leaflet.extras)
library(DT)
library(plotly)
library(scales)
library(classInt)

# ------ Paletas y constantes

COLORES_CLUSTER <- c(
  "Municipios en expansión"  = "#1D9E75",
  "Municipios consolidados"  = "#3B8BD4",
  "Municipios residenciales" = "#E8593C"
)

COLORES_CUADRANTE <- c(
  "Alta expansión\nAlto déficit"  = "#d7191c",
  "Baja expansión\nAlto déficit"  = "#fdae61",
  "Alta expansión\nBuen servicio" = "#1a9641",
  "Baja expansión\nBuen servicio" = "#92c5de"
)

ETIQUETAS_SERVICIO <- c(
  infantil      = "Ed. infantil",
  primaria      = "Ed. primaria",
  secundaria    = "Ed. secundaria",
  centros_salud = "Centros salud",
  hospitales    = "Hospitales",
  farmacias     = "Farmacias",
  tren          = "Tren",
  autobus       = "Autobús",
  lineas        = "Líneas bus",
  interurbanas  = "Interurbanas"
)

COLORES_CATEGORIA <- c(
  "Educación"  = "#3B8BD4",
  "Sanidad"    = "#E8593C",
  "Transporte" = "#1D9E75"
)

# ----- Carga de datos 

exp_data  <- readRDS("expansion_export.rds")
pred_data <- readRDS("resultados_prediccion.rds")

df_pca           <- exp_data$df_pca
resultados_pred_n <- pred_data

# Tabla base integrada
df_base <- df_pca |>
  select(municipio, LAU_CODE, PC1, PC2, cluster_label,
         tasa_crec_pob, tasa_crec_empresas, tasa_crec_ocupados,
         tasa_crec_renta_imp) |>
  mutate(LAU_CODE = as.character(LAU_CODE)) |>
  left_join(
    resultados_pred_n |> mutate(LAU_CODE = as.character(LAU_CODE)),
    by = c("municipio", "LAU_CODE")
  )

# Tabla larga déficit
df_largo <- df_base |>
  pivot_longer(
    cols      = starts_with("n_deficit_"),
    names_to  = "servicio",
    values_to = "deficit"
  ) |>
  mutate(
    servicio       = str_remove(servicio, "^n_deficit_"),
    servicio_label = ETIQUETAS_SERVICIO[servicio],
    categoria      = case_when(
      servicio %in% c("infantil", "primaria", "secundaria")          ~ "Educación",
      servicio %in% c("centros_salud", "hospitales", "farmacias")    ~ "Sanidad",
      servicio %in% c("tren", "autobus", "lineas", "interurbanas")   ~ "Transporte"
    )
  )

# Claves de servicios disponibles
SERVICIOS_COLS <- names(ETIQUETAS_SERVICIO)[
  names(ETIQUETAS_SERVICIO) %in% unique(df_largo$servicio)
]

# Score de inversión por municipio
resumen_mun <- df_largo |>
  group_by(
    municipio,
    LAU_CODE,
    PC1,
    PC2,
    cluster_label,
    poblacion_total
  ) |>
  summarise(
    n_servicios_deficit   = sum(deficit < 0, na.rm = TRUE),
    n_servicios_cubiertos = sum(deficit >= 0, na.rm = TRUE),
    balance_total         = sum(deficit, na.rm = TRUE),
    
    # magnitud total del déficit (en negativo)
    deficit_puro = sum(pmin(deficit, 0), na.rm = TRUE),
    
    # magnitud total del superávit (en positivo)
    superavit_puro = sum(pmax(deficit, 0), na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  mutate(
    
    # CUADRANTES
    
    cuadrante = case_when(
      PC1 >= 0 & balance_total <  0 ~ "Alta expansión\nAlto déficit",
      PC1 <  0 & balance_total <  0 ~ "Baja expansión\nAlto déficit",
      PC1 >= 0 & balance_total >= 0 ~ "Alta expansión\nBuen servicio",
      PC1 <  0 & balance_total >= 0 ~ "Baja expansión\nBuen servicio"
    ),
    
    # NECESIDAD
    
    intensidad =
      abs(deficit_puro) / log1p(poblacion_total),
    
    amplitud =
      n_servicios_deficit /
      (n_servicios_deficit + n_servicios_cubiertos),
    
    score_intensidad =
      (intensidad - min(intensidad)) /
      (max(intensidad) - min(intensidad)) * 100,
    
    score_amplitud =
      (amplitud - min(amplitud)) /
      (max(amplitud) - min(amplitud)) * 100,
    
    # media
    score_necesidad =
      0.5 * score_intensidad +
      0.5 * score_amplitud,
    
    # EXPANSIÓN
    
    score_expansion =
      (PC1 - min(PC1)) /
      (max(PC1) - min(PC1)) * 100,
    
    superavit_rel =
      superavit_puro / log1p(poblacion_total),
    
    superavit_norm =
      (superavit_rel - min(superavit_rel)) /
      (max(superavit_rel) - min(superavit_rel)),
    
    # ajuste por superávit
    # mucho superávit = menor urgencia
    score_expansion_adj =
      score_expansion *
      (1 - superavit_norm),
    
    # SCORE FINAL
    
    # media
    score_inversion =
      0.5 * score_necesidad +
      0.5 * score_expansion_adj
  ) |>
  arrange(desc(score_inversion))

# Score por categoría
score_cat <- df_largo |>
  group_by(municipio, LAU_CODE, categoria) |>
  summarise(
    balance_cat         = sum(deficit, na.rm = TRUE),
    n_deficit_cat       = sum(deficit < 0, na.rm = TRUE),
    n_total_cat         = n(),
    
    # déficit y superávit de la categoría
    deficit_puro_cat    = sum(pmin(deficit, 0), na.rm = TRUE),
    superavit_puro_cat  = sum(pmax(deficit, 0), na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  left_join(
    resumen_mun |>
      select(municipio, PC1, poblacion_total),
    by = "municipio"
  ) |>
  group_by(categoria) |>
  mutate(
    
    # EXPANSIÓN
    
    score_expansion =
      (PC1 - min(PC1, na.rm = TRUE)) /
      (max(PC1, na.rm = TRUE) -
         min(PC1, na.rm = TRUE)) * 100,
    
    superavit_rel_cat =
      superavit_puro_cat /
      log1p(poblacion_total),
    
    superavit_norm_cat =
      (superavit_rel_cat -
         min(superavit_rel_cat, na.rm = TRUE)) /
      (max(superavit_rel_cat, na.rm = TRUE) -
         min(superavit_rel_cat, na.rm = TRUE)),
    
    # el superávit reduce expansión
    score_expansion_adj = score_expansion * (1 - superavit_norm_cat),
    
    # NECESIDAD
    
    intensidad_cat =
      abs(deficit_puro_cat) /
      log1p(poblacion_total),
    
    amplitud_cat =
      n_deficit_cat / n_total_cat,
    
    score_intensidad_cat =
      (intensidad_cat -
         min(intensidad_cat, na.rm = TRUE)) /
      (max(intensidad_cat, na.rm = TRUE) -
         min(intensidad_cat, na.rm = TRUE)) * 100,
    
    score_amplitud_cat =
      (amplitud_cat -
         min(amplitud_cat, na.rm = TRUE)) /
      (max(amplitud_cat, na.rm = TRUE) -
         min(amplitud_cat, na.rm = TRUE)) * 100,
    
    score_necesidad =
      0.5 * score_intensidad_cat +
      0.5 * score_amplitud_cat,
    
    # SCORE FINAL
    
    score_cat =
      0.5 * score_expansion_adj +
      0.5 * score_necesidad
  ) |>
  ungroup()

# Geometrías
municipios_sf <- esp_get_munic(region = "Madrid") |>
  st_transform(crs = 4326) |>
  mutate(LAU_CODE = as.character(LAU_CODE)) |>
  filter(LAU_CODE %in% resumen_mun$LAU_CODE) |>
  left_join(resumen_mun, by = "LAU_CODE")

# ------ UI

ui <- page_navbar(
  title = div(
    span("Municipios en Expansión", style = "font-weight:700; font-size:1.1rem;"),
    span(" · Comunidad de Madrid", style = "color:#adb5bd; font-size:0.9rem;")
  ),
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#3B8BD4",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  bg      = "#1a1a2e",
  inverse = TRUE,
  
  # Pestaña 1: Déficit por servicio 
  nav_panel(
    title = "Déficit por servicio",
    icon  = icon("map"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        title = "Filtros",
        
        selectInput(
          "servicio_sel",
          "Tipo de servicio",
          choices  = setNames(SERVICIOS_COLS, ETIQUETAS_SERVICIO[SERVICIOS_COLS]),
          selected = SERVICIOS_COLS[1]
        ),
        
        hr(),
        p("El déficit mide cuántas unidades de servicio faltan o sobran respecto a lo esperado.",
          style = "font-size:0.82rem; color:#6c757d;"),
        p(HTML("<b style='color:#d7191c'>Rojo</b> = faltan servicios<br>
                <b style='color:#2166ac'>Azul</b> = superávit de servicios"),
          style = "font-size:0.82rem;"),
        
        hr(),
        uiOutput("stats_servicio")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Mapa de déficit"),
        leafletOutput("mapa_deficit", height = "520px")
      )
    )
  ),
  
  # Pestaña 2: Prioridad de inversión 
  nav_panel(
    title = "Prioridad de inversión",
    icon  = icon("chart-line"),
    
    layout_columns(
      col_widths = c(8, 4),
      
      card(
        full_screen = TRUE,
        card_header("Score de prioridad de inversión (0–100)"),
        leafletOutput("mapa_inversion", height = "480px")
      ),
      
      layout_columns(
        col_widths = 12,
        
        card(
          card_header("Expansión vs balance de servicios"),
          plotlyOutput("scatter_inversion", height = "340px")
        ),
        card(
          card_header("Distribución por cuadrante"),
          plotlyOutput("barras_cuadrante", height = "120px")
        )
      )
    )
  ),
  
  # Pestaña 3: Score por categoría
  nav_panel(
    title = "Por categoría",
    icon  = icon("layer-group"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        title = "Opciones",
        selectInput(
          "categoria_sel",
          "Categoría",
          choices  = c("Educación", "Sanidad", "Transporte"),
          selected = "Educación"
        ),
        radioButtons(
          "ranking_tipo",
          "Mostrar",
          choices  = c("Más urgentes" = "top", "Menos urgentes" = "bottom"),
          selected = "top"
        ),
        sliderInput(
          "ranking_n",
          "Nº de municipios",
          min = 5, max = 30, value = 10, step = 1
        ),
        hr(),
        p("Los rankings muestran los municipios con mayor o menor score de urgencia de inversión por categoría.",
          style = "font-size:0.82rem; color:#6c757d;"),
        p(HTML("<b style='color:#d7191c'>Rojo</b> = alta urgencia<br>
                <b style='color:#2c7bb6'>Azul</b> = baja urgencia"),
          style = "font-size:0.82rem;")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(textOutput("titulo_ranking")),
          plotlyOutput("ranking_cat", height = "520px")
        ),
        card(
          card_header(textOutput("titulo_mapa")),
          leafletOutput("mapa_cat", height = "520px")
        )
      )
    )
  ),
  
  # Pestaña 4: Explorador de municipios
  nav_panel(
    title = "Explorador",
    icon  = icon("table"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        title = "Filtrar",
        
        checkboxGroupInput(
          "filtro_cluster",
          "Tipo de municipio",
          choices  = names(COLORES_CLUSTER),
          selected = names(COLORES_CLUSTER)
        ),
        
        checkboxGroupInput(
          "filtro_cuadrante",
          "Cuadrante",
          choices  = names(COLORES_CUADRANTE),
          selected = names(COLORES_CUADRANTE)
        ),
        
        sliderInput(
          "filtro_score",
          "Score inversión",
          min   = 0,
          max   = 100,
          value = c(0, 100),
          step  = 1
        ),
        
        actionButton("reset_filtros", "Resetear filtros",
                     class = "btn-outline-secondary btn-sm w-100")
      ),
      
      card(
        full_screen = TRUE,
        card_header(textOutput("n_municipios_tabla")),
        DTOutput("tabla_municipios")
      )
    )
  ),
  
  # Pestaña 5: Perfil de municipio
  nav_panel(
    title = "Perfil de municipio",
    icon  = icon("city"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        title = "Seleccionar municipio",
        
        selectizeInput(
          "municipio_sel",
          "Municipio",
          choices  = sort(unique(resumen_mun$municipio)),
          selected = "Alcalá de Henares",
          options  = list(placeholder = "Escribe un municipio...")
        ),
        
        hr(),
        uiOutput("ficha_municipio")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        layout_columns(
          col_widths = 12,
          card(
            card_header("Perfil de expansión"),
            plotlyOutput("radar_expansion", height = "300px")
          ),
          card(
            card_header("Posición en el espacio PCA"),
            plotlyOutput("scatter_pca", height = "280px")
          )
        ),
        
        card(
          card_header("Balance por servicio"),
          plotlyOutput("barras_deficit", height = "620px")
        )
      )
    )
  )
)

# Server

server <- function(input, output, session) {
  
  # Municipio seleccionado reactivo (compartido entre pestañas)
  mun_activo <- reactiveVal("Alcalá de Henares")
  
  # Actualizar selector de perfil al hacer clic en mapa
  observeEvent(input$mapa_deficit_shape_click, {
    click <- input$mapa_deficit_shape_click
    if (!is.null(click$id)) {
      mun_activo(click$id)
      updateSelectizeInput(session, "municipio_sel", selected = click$id)
    }
  })
  observeEvent(input$mapa_inversion_shape_click, {
    click <- input$mapa_inversion_shape_click
    if (!is.null(click$id)) {
      mun_activo(click$id)
      updateSelectizeInput(session, "municipio_sel", selected = click$id)
    }
  })
  observeEvent(input$municipio_sel, {
    mun_activo(input$municipio_sel)
  })
  
  #  Mapa déficit 
  
  output$mapa_deficit <- renderLeaflet({
    col_def <- paste0("n_deficit_", input$servicio_sel)
    
    datos_mapa <- municipios_sf |>
      left_join(
        df_base |>
          select(LAU_CODE, all_of(col_def)) |>
          mutate(LAU_CODE = as.character(LAU_CODE)),
        by = "LAU_CODE"
      ) |>
      mutate(valor = .data[[col_def]])
    
    # Excluir Madrid del dominio de color para no romper la escala
    datos_sin_madrid <- datos_mapa |> filter(municipio != "Madrid")
    lim <- max(abs(datos_sin_madrid$valor), na.rm = TRUE)
    pal <- colorNumeric(
      palette  = colorRampPalette(c("#d7191c", "#fafafa", "#2c7bb6"))(100),
      domain   = c(-lim, lim),
      na.color = "#e0e0e0"
    )
    COLOR_MADRID <- "#9b59b6"  # color propio para Madrid (morado)
    
    popup_txt <- paste0(
      "<b>", datos_mapa$municipio, "</b><br>",
      "Balance: <b>", round(datos_mapa$valor, 1), " unidades</b><br>",
      "Cluster: ", datos_mapa$cluster_label, "<br>",
      "PC1: ", round(datos_mapa$PC1, 2),
      ifelse(datos_mapa$municipio == "Madrid",
             "<br><i>(Madrid no se incluye en la escala de color)</i>", "")
    )
    
    # Asignar color: Madrid en morado, resto según paleta
    fill_colors <- ifelse(
      datos_mapa$municipio == "Madrid",
      COLOR_MADRID,
      pal(datos_mapa$valor)
    )
    
    leaflet(datos_mapa) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        layerId      = ~municipio,
        fillColor    = fill_colors,
        fillOpacity  = 0.95,
        color        = "white",
        weight       = 0.5,
        popup        = popup_txt,
        highlight    = highlightOptions(
          weight       = 2,
          color        = "#333",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal      = pal,
        values   = datos_sin_madrid$valor,
        title    = "Balance<br>(unidades)",
        position = "bottomright",
        labFormat = labelFormat(digits = 0)
      ) |>
      addLegend(
        colors   = COLOR_MADRID,
        labels   = "Madrid (escala propia)",
        position = "bottomright",
        opacity  = 0.95
      )
  })
  
  output$stats_servicio <- renderUI({
    col_def <- paste0("n_deficit_", input$servicio_sel)
    vals    <- df_base[[col_def]]
    n_def   <- sum(vals < 0, na.rm = TRUE)
    n_sob   <- sum(vals >= 0, na.rm = TRUE)
    tagList(
      tags$b(ETIQUETAS_SERVICIO[input$servicio_sel]),
      tags$p(
        sprintf("%d municipios con déficit · %d con superávit o equilibrado", n_def, n_sob),
        style = "font-size:0.82rem; color:#6c757d; margin-top:4px;"
      )
    )
  })
  
  #  Mapa inversión 
  
  output$mapa_inversion <- renderLeaflet({
    pal_inv <- colorNumeric(
      palette  = c("#1a9641", "#ffffbf", "#d7191c"),
      domain   = range(municipios_sf$score_inversion, na.rm = TRUE),
      na.color = "#e8e8e8"
    )
    
    popup_txt <- paste0(
      "<b>", municipios_sf$municipio, "</b><br>",
      "Score inversión: <b>", round(municipios_sf$score_inversion, 1), "/100</b><br>",
      "Balance total: ", round(municipios_sf$balance_total, 0), " unidades<br>",
      "Servicios con déficit: ", municipios_sf$n_servicios_deficit, "/", length(SERVICIOS_COLS), "<br>",
      "Cuadrante: <b>", gsub("\n", " - ", municipios_sf$cuadrante), "</b>"
    )
    
    leaflet(municipios_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        layerId      = ~municipio,
        fillColor    = ~pal_inv(score_inversion),
        fillOpacity  = 0.85,
        color        = "white",
        weight       = 0.5,
        popup        = popup_txt,
        highlight    = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
      ) |>
      addLegend(
        pal      = pal_inv,
        values   = ~score_inversion,
        title    = "Score inversión<br>(0–100)",
        position = "bottomright",
        na.label = "Sin datos"
      )
  })
  
  output$scatter_inversion <- renderPlotly({
    p <- resumen_mun |>
      filter(municipio != "Madrid") |>
      mutate(
        cuadrante_1l = gsub("\n", " - ", cuadrante),
        texto = paste0(
          municipio, "<br>PC1: ", round(PC1, 2),
          "<br>Balance: ", round(balance_total, 0),
          "<br>Score: ", round(score_inversion, 1)
        )
      ) |>
      ggplot(aes(x = PC1, y = balance_total, color = cuadrante_1l,
                 text = texto, size = log1p(poblacion_total))) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
      geom_point(alpha = 0.7) +
      scale_color_manual(
        values = setNames(
          COLORES_CUADRANTE,
          gsub("\n", " - ", names(COLORES_CUADRANTE))
        ),
        name = NULL
      ) +
      scale_size_continuous(range = c(1, 5), guide = "none") +
      labs(x = "PC1 (expansión)", y = "Balance de servicios") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") |>
      layout(showlegend = FALSE)
  })
  
  output$barras_cuadrante <- renderPlotly({
    p <- resumen_mun |>
      count(cuadrante) |>
      mutate(
        cuadrante_1l = gsub("\n", " - ", cuadrante),
        pct = n / sum(n) * 100
      ) |>
      ggplot(aes(x = reorder(cuadrante_1l, pct), y = pct,
                 fill = cuadrante_1l,
                 text = paste0(cuadrante_1l, ": ", round(pct, 1), "% (", n, " municipios)"))) +
      geom_col(width = 0.7) +
      scale_fill_manual(
        values = setNames(
          COLORES_CUADRANTE,
          gsub("\n", " - ", names(COLORES_CUADRANTE))
        ),
        guide = "none"
      ) +
      scale_y_continuous(labels = label_number(suffix = "%")) +
      coord_flip() +
      labs(x = NULL, y = "% municipios") +
      theme_minimal(base_size = 10)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Score por categoría
  
  # Función auxiliar: ranking horizontal con barras de color
  hacer_ranking <- function(categoria_sel, tipo, n) {
    datos <- score_cat |>
      filter(categoria == categoria_sel, !is.na(score_cat))
    
    datos <- if (tipo == "top") {
      slice_max(datos, order_by = score_cat, n = n)
    } else {
      slice_min(datos, order_by = score_cat, n = n)
    }
    
    datos <- datos |>
      mutate(
        municipio = reorder(municipio, score_cat),
        texto = paste0(
          "<b>", municipio, "</b>",
          "<br>Score: ",    round(score_cat, 1),
          "<br>Balance: ",  round(balance_cat, 0),
          "<br>Déficit: ",  round(deficit_puro_cat, 0)
        )
      )
    
    color_barra <- if (tipo == "top") "#d7191c" else "#2c7bb6"
    
    plot_ly(
      datos,
      x         = ~score_cat,
      y         = ~municipio,
      type      = "bar",
      orientation = "h",
      marker    = list(
        color   = color_barra,
        opacity = 0.82,
        line    = list(color = "white", width = 0.4)
      ),
      hovertext = ~texto,
      hoverinfo = "text"
    ) |>
      layout(
        xaxis  = list(title = "Score (0–100)", range = c(0, 100),
                      showgrid = TRUE, gridcolor = "#e8e8e8"),
        yaxis  = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 5, r = 10, t = 5, b = 30),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = FALSE)
  }
  
  # Función auxiliar: mapa coroplético por categoría
  rango_global_cat <- range(score_cat$score_cat, na.rm = TRUE)
  
  hacer_mapa_cat <- function(categoria_sel) {
    datos_cat <- score_cat |>
      filter(categoria == categoria_sel) |>
      select(LAU_CODE, score_cat)
    
    mapa <- municipios_sf |>
      left_join(datos_cat, by = "LAU_CODE")
    
    # Cortes Fisher sobre los valores válidos de esta categoría
    vals <- mapa$score_cat[!is.na(mapa$score_cat)]
    k    <- min(5, length(unique(vals)) - 1)
    brks <- classIntervals(vals, n = k, style = "fisher")$brks
    brks[1]            <- floor(brks[1])      # evitar que el mínimo quede fuera
    brks[length(brks)] <- ceiling(brks[length(brks)])
    
    paleta_urgencia <- c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
    pal <- colorBin(
      palette       = paleta_urgencia[1:k],
      bins          = brks,
      na.color      = "#e0e0e0",
      pretty        = FALSE
    )
    
    popup_txt <- paste0(
      "<b>", mapa$municipio, "</b><br>",
      "Score ", categoria_sel, ": <b>",
      round(mapa$score_cat, 1), "</b>"
    )
    
    leaflet(mapa, options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor   = ~pal(score_cat),
        fillOpacity = 0.85,
        color       = "white",
        weight      = 0.5,
        popup       = popup_txt,
        highlight   = highlightOptions(
          weight       = 2,
          color        = "#333",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal      = pal,
        values   = ~score_cat,
        title    = "Score",
        position = "bottomright",
        labFormat = labelFormat(digits = 1)
      )
  }
  
  # Outputs pestaña 3
  output$titulo_ranking <- renderText(paste(input$categoria_sel, "- Ranking"))
  output$titulo_mapa    <- renderText(paste("Mapa -", input$categoria_sel))
  
  output$ranking_cat <- renderPlotly(
    hacer_ranking(input$categoria_sel, input$ranking_tipo, input$ranking_n)
  )
  output$mapa_cat <- renderLeaflet(
    hacer_mapa_cat(input$categoria_sel)
  )
  
  # Tabla explorador
  
  datos_tabla <- reactive({
    resumen_mun |>
      filter(
        cluster_label %in% input$filtro_cluster,
        cuadrante     %in% input$filtro_cuadrante,
        score_inversion >= input$filtro_score[1],
        score_inversion <= input$filtro_score[2]
      ) |>
      select(municipio, cluster_label, cuadrante, PC1,
             poblacion_total, n_servicios_deficit, deficit_puro, superavit_puro, score_inversion) |>
      mutate(
        cuadrante = gsub("\n", " - ", cuadrante),
        across(where(is.numeric), ~round(., 1))
      ) |>
      rename(
        Municipio              = municipio,
        Tipo                   = cluster_label,
        Cuadrante              = cuadrante,
        `Índice expansión`     = PC1,
        Población              = poblacion_total,
        `Servicios c/ déficit` = n_servicios_deficit,
        `Déficit`              = deficit_puro,
        `Superávit`            = superavit_puro,
        `Score inversión`      = score_inversion
      )
  })
  
  observeEvent(input$reset_filtros, {
    updateCheckboxGroupInput(session, "filtro_cluster",
                             selected = names(COLORES_CLUSTER))
    updateCheckboxGroupInput(session, "filtro_cuadrante",
                             selected = names(COLORES_CUADRANTE))
    updateSliderInput(session, "filtro_score", value = c(0, 100))
  })
  
  output$n_municipios_tabla <- renderText({
    paste0(nrow(datos_tabla()), " municipios")
  })
  
  output$tabla_municipios <- renderDT({
    datatable(
      datos_tabla(),
      selection  = "single",
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        pageLength = 15,
        dom        = "Bfrtip",
        buttons    = list("csv", "excel"),
        scrollX    = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    ) |>
      formatStyle(
        "Score inversión",
        background = styleColorBar(c(0, 100), "#d7191c55"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle(
        "Índice expansión",
        background = styleColorBar(range(resumen_mun$PC1, na.rm = TRUE), "#3B8BD455"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  # Al hacer clic en la tabla, actualizar municipio activo
  observeEvent(input$tabla_municipios_rows_selected, {
    fila <- input$tabla_municipios_rows_selected
    if (length(fila) > 0) {
      mun <- datos_tabla()$Municipio[fila]
      mun_activo(mun)
      updateSelectizeInput(session, "municipio_sel", selected = mun)
    }
  })
  
  # Perfil de municipio
  
  datos_mun <- reactive({
    resumen_mun |> filter(municipio == mun_activo())
  })
  
  output$ficha_municipio <- renderUI({
    d <- datos_mun()
    if (nrow(d) == 0) return(NULL)
    
    cuad_label <- gsub("\n", " - ", d$cuadrante)
    color_cuad <- COLORES_CUADRANTE[d$cuadrante]
    
    tagList(
      tags$div(
        style = paste0("background:", color_cuad, "22; border-left: 4px solid ",
                       color_cuad, "; padding: 8px 12px; border-radius: 4px; margin-bottom:8px;"),
        tags$b(cuad_label)
      ),
      tags$table(
        style = "width:100%; font-size:0.85rem;",
        tags$tr(tags$td("Población"),
                tags$td(tags$b(format(d$poblacion_total, big.mark = ".")))),
        tags$tr(tags$td("PC1"),
                tags$td(tags$b(round(d$PC1, 3)))),
        tags$tr(tags$td("PC2"),
                tags$td(tags$b(round(d$PC2, 3)))),
        tags$tr(tags$td("Balance total"),
                tags$td(tags$b(round(d$balance_total, 0), " unidades"))),
        tags$tr(tags$td("Déficit"),
                tags$td(tags$b(round(d$deficit_puro, 0), " unidades"))),
        tags$tr(tags$td("Superávit"),
                tags$td(tags$b(round(d$superavit_puro, 0), " unidades"))),
        tags$tr(tags$td("Servicios con déficit"),
                tags$td(tags$b(paste0(d$n_servicios_deficit, " / ", length(SERVICIOS_COLS))))),
        tags$tr(tags$td("Score inversión"),
                tags$td(tags$b(round(d$score_inversion, 1), " / 100")))
      )
    )
  })
  
  output$radar_expansion <- renderPlotly({
    d <- df_pca |> filter(municipio == mun_activo())
    if (nrow(d) == 0) return(NULL)
    
    vars_radar <- c("tasa_crec_pob", "tasa_crec_empresas",
                    "tasa_crec_ocupados", "tasa_crec_renta_imp")
    labs_radar <- c("Crec. población", "Crec. empresas",
                    "Crec. ocupados", "Crec. renta")
    
    vals_norm <- map_dbl(vars_radar, function(v) {
      vals_todos <- df_pca[[v]]
      val <- as.numeric(d[[v]][1])
      mn  <- min(vals_todos, na.rm = TRUE)
      mx  <- max(vals_todos, na.rm = TRUE)
      if (anyNA(c(val, mn, mx)) || mx == mn) return(0.5)
      (val - mn) / (mx - mn)
    })
    
    plot_ly(
      type      = "scatterpolar",
      r         = c(vals_norm, vals_norm[1]),
      theta     = c(labs_radar, labs_radar[1]),
      fill      = "toself",
      fillcolor = "#3B8BD433",
      line      = list(color = "#3B8BD4"),
      name      = d$municipio[1]
    ) |>
      layout(
        polar  = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
        margin = list(t = 30, b = 30)
      )
  })
  
  output$barras_deficit <- renderPlotly({
    orden_servicios <- ETIQUETAS_SERVICIO[SERVICIOS_COLS]  # mismo orden que el selector
    
    d_deficit <- df_largo |>
      filter(municipio == mun_activo()) |>
      mutate(
        color          = ifelse(deficit < 0, "#d7191c", "#1a9641"),
        label          = paste0(servicio_label, ": ", round(deficit, 0), " unidades"),
        servicio_label = factor(servicio_label, levels = orden_servicios)
      ) |>
      arrange(servicio_label)
    
    plot_ly(d_deficit,
            x = ~deficit, y = ~servicio_label,
            type = "bar", orientation = "h",
            marker = list(color = ~color),
            text = ~round(deficit, 0), textposition = "outside",
            hovertext = ~label, hoverinfo = "text") |>
      layout(
        xaxis  = list(title = "Balance (+ = superávit, − = déficit)", zeroline = TRUE),
        yaxis  = list(title = ""),
        margin = list(l = 120, t = 10, b = 40),
        shapes = list(list(type = "line", x0 = 0, x1 = 0,
                           y0 = -0.5, y1 = length(SERVICIOS_COLS) - 0.5,
                           line = list(color = "grey50", dash = "dot")))
      )
  })
  
  output$scatter_pca <- renderPlotly({
    mun_sel <- mun_activo()
    
    p <- resumen_mun |>
      mutate(
        es_sel = municipio == mun_sel,
        texto  = paste0(municipio, "<br>PC1: ", round(PC1, 2), "  PC2: ", round(PC2, 2),
                        "<br>", cluster_label)
      ) |>
      ggplot(aes(x = PC1, y = PC2, color = cluster_label, text = texto)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
      geom_point(aes(size = es_sel), alpha = 0.6) +
      geom_point(data = ~filter(., municipio == mun_sel),
                 color = "black", size = 4, shape = 21, stroke = 2) +
      scale_color_manual(values = COLORES_CLUSTER, name = NULL) +
      scale_size_manual(values = c("FALSE" = 1.5, "TRUE" = 4), guide = "none") +
      labs(x = "PC1 - expansión global", y = "PC2 - tipo de expansión") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") |>
      layout(legend = list(orientation = "h", y = -0.25))
  })
}

# Lanzar

shinyApp(ui = ui, server = server)