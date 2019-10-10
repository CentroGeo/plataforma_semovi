library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

library(chron)
library(readxl)

# ===== OPERACIONES INICIALES =====

cdmx <- read_sf(dsn = "data/", layer = "cdmx")

# ===== PÁGINA WEB =====

ui <- fluidPage(
  fluidRow(
    column(6 , leafletOutput(outputId = 'mapa', width = '100%' , height = 900)),
    column(6 , tabsetPanel(id = 'TabSetPanel' ,
      tabPanel(title = 'Datos' , value = 1 ,
               radioButtons(inputId = 'incidente' , label = 'Seleccione el tipo de incidente...' ,
                            choiceNames = c('Fallecidos' , 'Lesionados') , choiceValues = c(2 , 3),
                            selected = 2),
               checkboxGroupInput(inputId = 'bases_elegidas' , label = 'Seleccione las bases de datos a utilizar...' ,
                                  choiceNames = c('PGJ' , 'SSC') , choiceValues = c('PGJ' , 'SSC') ,
                                  selected = c('PGJ' , 'SSC')),
               fileInput(inputId = 'upload_usuario' , label = 'Subir un archivo local' ,
                         accept = c('text/csv' , 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' , '.csv' , '.xlsx')),
               verbatimTextOutput(outputId = 'warningLog_TabA')
               #,actionButton(inputId = 'boton_tab1' , label = 'Iniciar con Vinculación')
               ),
      tabPanel(title = 'Vinculación' , value = 2 , fluidRow(
        fluidRow(
          column(4, sliderInput(inputId = 'muestras', label = 'Seleccione el porcentaje a muestrear', min = 5 , max = 100 , value = 30 , step = 5, post = '%'), htmlOutput(outputId = 'texto_muestras')),
          column(2 , tags$div(style = 'height:75px' ,
                              actionButton(inputId = 'boton_default' , label = 'Utilizar Parámetros por Defecto' ,
                                           style = 'position: absolute; bottom:0;')))
        ), actionButton(inputId= 'boton_inicio', label = 'Iniciar')
      ),tags$div(class = 'area_vinculacion' , fluidRow(
        tags$p(id = 'NinjaA'),
        tags$code('Aprieta el botón para iniciar con la vinculación', class = 'texto_inicial'),
        tableOutput(outputId = 'tabla_pgj'),
        textOutput(outputId = 'remanentes'),
        tags$p(id = 'NinjaB'),
        tags$p(id = 'NinjaC')
      ),fluidRow(
        tags$p(id = 'NinjaD'),
        tableOutput(outputId = 'tabla_ssc')
      )), fluidRow(
        tags$p(id = 'NinjaE'),
        dataTableOutput(outputId = 'tabla_vinculos'),
        tags$p(id = 'NinjaF')
      )),
      tabPanel(title = 'Resultados' , value = 3 , fluidRow(
        tags$p(id = 'NinjaG'),
        tags$div(id = 'div_bmostrar' , actionButton(inputId = 'boton_mostrar' , label = 'Mostrar Tabla de Vínculos Final')),
        verbatimTextOutput(outputId = 'texto_resultado'),
        plotOutput(outputId = 'grafica_a' , click = 'grafica_a_click'),
        fluidRow(
          column(6, plotOutput(outputId = 'grafica_b')),
          column(6, verbatimTextOutput(outputId = 'texto_grafica_b'))
        )
      ))
    ))
  )
)

# ===== SERVIDOR =====

server <- function(input, output , session) {
  # ===== REACTIVE VARIABLES =====
  # Para Parte 1
  bases_de_datos <- reactiveValues(pgj = NULL , ssc = NULL , upload = NULL , unificada = NULL , unificada_sf = NULL)
  logs <- reactiveValues(log_TabA = c('ADVERTENCIAS\n'))
  # Para Parte 2
  muestra_pgj <- reactiveValues(data = NULL)
  candidato_ssc <- reactiveValues(data = NULL)
  resultado <- reactiveValues(data = NULL , diametros = c() , tiempos = c() , final = NULL)
  loop <- reactiveValues(i = NULL , k = NULL)
  
  # ===== MAPA INICIAL =====
  output$mapa <- renderLeaflet({
    leaflet(data = cdmx) %>%
      addTiles(urlTemplate = '//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      setView(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
      addPolygons(fillColor = '#DCDCDC' , fillOpacity = 0.5 , color = '#2F4F4F' , opacity = 0.75)
  })
  mapa_proxy <- leafletProxy('mapa')
  
  # ===== PARTE 1 - STRING MAGIC =====
  observeEvent(c(input$incidente , input$bases_elegidas , input$upload_usuario) , {
    base_unificada <- data.frame(id_original = as.integer() , base_original = as.character() , lat = as.numeric() , lon = as.numeric() , timestamp = chron())
    # ¡Importante! Corregir que código posterior (Mostrar Tablas) también funcione con tablas de Excel
    # ===
    if ('PGJ' %in% input$bases_elegidas) {
      if (!file.exists('data/PGJ.csv') & !file.exists('data/PGJ.xlsx')) {
        logs$log_TabA <- append(logs$log_TabA , 'No puede encontrarse el archivo "PGJ.csv" o "PGJ.xlsx"; favor de colocarlo en la carpeta correspondiente.\n')
      } else {
        if (file.exists('data/PGJ.csv')) {
          bases_de_datos$pgj <- read.csv("data/PGJ.csv")
          if (input$incidente == 2) {
            bases_de_datos$pgj <- filter(bases_de_datos$pgj , calidad_juridica == 'CADAVER' | calidad_juridica == 'FALLECIDO')
          } else if (input$incidente == 3) {
            bases_de_datos$pgj <- filter(bases_de_datos$pgj , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
          }
          id_pgj <- bases_de_datos$pgj$X
        } else {
          bases_de_datos$pgj <- read_excel('data/PGJ.xlsx')
          if (input$incidente == 2) {
            bases_de_datos$pgj <- filter(bases_de_datos$pgj , calidad_juridica == 'CADAVER' | calidad_juridica == 'FALLECIDO')
          } else if (input$incidente == 3) {
            bases_de_datos$pgj <- filter(bases_de_datos$pgj , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
          }
          id_pgj <- as.integer(bases_de_datos$pgj$...1)
        }
        # =
        bases_de_datos$pgj['hora_de_hechos'] <- times(paste0(bases_de_datos$pgj$hora_de_hechos,':00'))
        bases_de_datos$pgj['fecha_de_hechos'] <- dates(as.character(bases_de_datos$pgj$fecha_de_hechos) , format = c(dates = 'y-m-d'))
        bases_de_datos$pgj['timestamp'] <- chron(bases_de_datos$pgj$fecha_de_hechos , bases_de_datos$pgj$hora_de_hechos)
        # =
        tmp <- data.frame(id_original = id_pgj,
                          base_original = replicate(nrow(bases_de_datos$pgj) , 'PGJ'),
                          lat = bases_de_datos$pgj$latitud ,
                          lon = bases_de_datos$pgj$longitud ,
                          timestamp = bases_de_datos$pgj$timestamp)
        tmp <- tmp[complete.cases(tmp),]
        # =
        rango_pgj <- range(tmp$timestamp)
        base_unificada <- rbind(base_unificada , tmp)
      }
    }
    # =
    if ('SSC' %in% input$bases_elegidas) {
      if (!file.exists('data/SSC.csv') & !file.exists('data/SSC.xlsx')) {
        logs$log_TabA <- append(logs$log_TabA , 'No puede encontrarse el archivo "SSC.csv" o "SSC.xlsx"; favor de colocarlo en la carpeta correspondiente.\n')
      } else {
        if (file.exists('data/SSC.csv')) {
          bases_de_datos$ssc <- read.csv("data/SSC.csv")
          if (input$incidente == 2) {
            bases_de_datos$ssc <- filter(bases_de_datos$ssc , total_occisos != 0)
          } else if (input$incidente == 3) {
            bases_de_datos$ssc <- filter(bases_de_datos$ssc , total_lesionados != 0)
          }
          id_ssc <- bases_de_datos$ssc$X
        } else {
          bases_de_datos$ssc <- read_excel('data/SSC.xlsx')
          if (input$incidente == 2) {
            bases_de_datos$ssc <- filter(bases_de_datos$ssc , total_occisos != 0)
          } else if (input$incidente == 3) {
            bases_de_datos$ssc <- filter(bases_de_datos$ssc , total_lesionados != 0)
          }
          id_ssc <- as.integer(bases_de_datos$ssc$...1)
        }
        # =
        bases_de_datos$ssc['hora'] <- times(as.numeric(as.character(bases_de_datos$ssc$hora)))
        bases_de_datos$ssc['fecha_evento'] <- dates(as.character(bases_de_datos$ssc$fecha_evento) , format = c(dates='y-m-d'))
        bases_de_datos$ssc['timestamp'] <- chron(bases_de_datos$ssc$fecha_evento , bases_de_datos$ssc$hora)
        # =
        tmp <- data.frame(id_original = id_ssc,
                          base_original = replicate(nrow(bases_de_datos$ssc) , 'SSC'),
                          lat = bases_de_datos$ssc$latitud ,
                          lon = bases_de_datos$ssc$longitud ,
                          timestamp = bases_de_datos$ssc$timestamp)
        tmp <- tmp[complete.cases(tmp),]
        # =
        rango_ssc <- range(tmp$timestamp)
        base_unificada <- rbind(base_unificada , tmp)
      }
    }
    # =
    if (!is.null(input$upload_usuario)) {
      if (input$upload_usuario$type != 'text/csv' & input$upload_usuario$type != 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') {
        logs$log_TabA <- append(logs$log_TabA , 'El archivo subido no es del formato ".csv" o ".xlsx"; favor de utilizar un archivo en el formato correcto.\n')
      } else {
        if (input$upload_usuario$type == 'text/csv') {
          bases_de_datos$upload <- read.csv(input$upload_usuario$datapath)
        } else {
          bases_de_datos$upload <- read_excel(input$upload_usuario$datapath)
        }
        # ===== Latitud =====
        # Todas las columnas que tienen los caracteres 'lat'
        lat_upload <- select(bases_de_datos$upload , contains('lat'))
        # Si no hay alguno, entonces...
        if (ncol(lat_upload) == 0) {
          # Seleccione todas las columnas que tengan el caracter 'y'
          lat_upload <- select(bases_de_datos$upload , contains('y'))
          # Si no hay alguno, entonces...
          if (ncol(lat_upload) == 0) {
            logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no poseen una columna con la Coordenada "Y" o Latitud; no es posible utilizarlos en el algoritmo\n')
            lat_upload <- NULL
          # Si hay más de uno, entonces...
          } else if (ncol(lat_upload) > 1) {
            # Todas las columnas que son numéricas
            lat_upload <- lat_upload[, sapply(lat_upload , is.numeric)]
            # Si no hay alguno, entonces...
            if (ncol(lat_upload) == 0) {
              logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no poseen una columna con la Coordenada "Y" o Latitud, o ésta posee algún valor erróneo; no es posible utilizarlos en el algoritmo\n')
              lat_upload <- NULL
            # Si hay más de uno, entonces...
            } else if (ncol(lat_upload) > 1) {
              logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no tienen especificada correctamente una columna con la Coordenada "Y" o Latitud; no es posible utilizarlos en el algoritmo\n')
              lat_upload <- NULL
            # Si todo sale bien, entonces...
            } else {
              lat_upload <- lat_upload[, names(lat_upload)]
            }
          # Si todo sale bien, entonces...
          } else {
            lat_upload <- lat_upload[, names(lat_upload)]
          }
        # Si hay más de uno, entonces...
        } else if (ncol(lat_upload) > 1) {
          # Todas las columnas que son numéricas
          lat_upload <- lat_upload[, sapply(lat_upload , is.numeric)]
          # Si no hay alguno, entonces...
          if (ncol(lat_upload) == 0) {
            logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no poseen una columna con la Coordenada "Y" o Latitud, o ésta posee algún valor erróneo; no es posible utilizarlos en el algoritmo\n')
            lat_upload <- NULL
          # Si hay más de una, entonces...
          } else if (ncol(lat_upload) > 1) {
            logs$log_TabA <- append(logs$log_TabA , 'Los datos poseen una columna con nombre similar a la de Latitud, lo cual provoca conflictos; no es posible utilizarlos en el algoritmo\n')
            lat_upload <- NULL
          # Si todo sale bien, entonces...
          } else {
            lat_upload <- lat_upload[, names(lat_upload)]
          }
        # Si todo sale bien, entonces...
        } else {
          lat_upload <- lat_upload[, names(lat_upload)]
        }
        # ===== Longitud =====
        # Todas las columnas que tienen los caracteres 'lon'
        lon_upload <- select(bases_de_datos$upload , contains('lon'))
        # Si no hay alguno, entonces...
        if (ncol(lon_upload) == 0) {
          # Seleccione todas las columnas que tengan el caracter 'x'
          lon_upload <- select(bases_de_datos$upload , contains('x'))
          # Si no hay alguno, entonces...
          if (ncol(lon_upload) == 0) {
            logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no poseen una columna con la Coordenada "X" o Longitud; no es posible utilizarlos en el algoritmo\n')
            lon_upload <- NULL
            # Si hay más de uno, entonces...
          } else if (ncol(lon_upload) > 1) {
            # Todas las columnas que son numéricas
            lon_upload <- lon_upload[, sapply(lon_upload , is.numeric)]
            # Si no hay alguno, entonces...
            if (ncol(lon_upload) == 0) {
              logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no poseen una columna con la Coordenada "X" o Longitud, o ésta posee algún valor erróneo; no es posible utilizarlos en el algoritmo\n')
              lon_upload <- NULL
              # Si hay más de uno, entonces...
            } else if (ncol(lon_upload) > 1) {
              logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no tienen especificada correctamente una columna con la Coordenada "X" o Longitud; no es posible utilizarlos en el algoritmo\n')
              lon_upload <- NULL
              # Si todo sale bien, entonces...
            } else {
              lon_upload <- lon_upload[, names(lon_upload)]
            }
            # Si todo sale bien, entonces...
          } else {
            lon_upload <- lon_upload[, names(lon_upload)]
          }
          # Si hay más de uno, entonces...
        } else if (ncol(lon_upload) > 1) {
          # Todas las columnas que son numéricas
          lon_upload <- lon_upload[, sapply(lon_upload , is.numeric)]
          # Si no hay alguno, entonces...
          if (ncol(lon_upload) == 0) {
            logs$log_TabA <- append(logs$log_TabA , 'Los datos subidos no poseen una columna con la Coordenada "X" o Longitud, o ésta posee algún valor erróneo; no es posible utilizarlos en el algoritmo\n')
            lon_upload <- NULL
            # Si hay más de una, entonces...
          } else if (ncol(lon_upload) > 1) {
            logs$log_TabA <- append(logs$log_TabA , 'Los datos poseen una columna con nombre similar a la de Longitud, lo cual provoca conflictos; no es posible utilizarlos en el algoritmo\n')
            lon_upload <- NULL
            # Si todo sale bien, entonces...
          } else {
            lon_upload <- lon_upload[, names(lon_upload)]
          }
          # Si todo sale bien, entonces...
        } else {
          lon_upload <- lon_upload[, names(lon_upload)]
        }
        # ===== Fecha de Incidente =====
        # Todas las columnas que tienen la palabra 'fecha'
        fecha_upload <- select(bases_de_datos$upload , contains('fecha'))
        # Si no hay alguna, entonces...
        # if (ncol(lat_upload) == 0) {
        #   
        # # Si hay más de una, entonces...
        # } else if (ncol(lat_upload) > 1) {
        #   
        # # Si todo sale bien...
        # } else {
        #   
        # }
      }
    }
    # =
    base_unificada['id_global'] <- seq.int(nrow(base_unificada))
    base_unificada['id_SSC'] <- as.integer(NULL)
    bases_de_datos$unificada <- base_unificada
    bases_de_datos$unificada_sf <- st_transform(st_as_sf(bases_de_datos$unificada , coords = c('lon','lat') , crs = 4326), 32614)
  })
  # ===
  output$warningLog_TabA <- renderText({
    if (length(logs$log_TabA) > 1) {
      logs$log_TabA
    } else {
      'SIN ADVERTENCIAS QUE REPORTAR'
    }
  })
  # ===
  # observeEvent(input$boton_tab1 , {
  #   updateTabsetPanel(session = session , inputId = 'TabSetPanel' , selected = 'Resultados')
  # })
  # ===== PARTE 2 - VINCULACIÓN DE EVENTOS =====
  output$texto_muestras <- renderText({
    tmp <- filter(bases_de_datos$unificada , base_original == 'PGJ')
    paste('<p style="align-self: center">',floor(nrow(tmp)*(input$muestras/100)) , 'muestras de' , nrow(tmp),'</p>')
  })
  # =
  dataModal <- function(){modalDialog(
    tags$strong('¡Importante'), 'Si decide continuar, reiniciará la muestra y deberá comenzar el proceso nuevamente. ¿Continuar?',
    footer = tagList(
      actionButton(inputId = 'boton_restart' , label = 'Reiniciar'),
      modalButton('Cancelar')
    )
  )}
  # =
  observeEvent(input$boton_inicio , {
    if (input$boton_inicio > 1) {
      showModal(dataModal())
    } else if (input$boton_inicio == 1) {
      tmp <- filter(bases_de_datos$unificada , base_original == 'PGJ')
      muestra_pgj$data <- tmp[sample(nrow(tmp), nrow(tmp)*(input$muestras/100)),]
      insertUI(selector = '#NinjaA', where = 'afterEnd',
               tags$h3('Procuraduría General de Justicia (PGJ)'))
      insertUI(selector = '#NinjaB', where = 'afterEnd', 
               fluidRow(
                 column(4, sliderInput(inputId = 'radio' , label = 'Seleccione un radio de búsqueda', min = 100 , max = 1000 , value = 250 , step = 50 , post = ' m')),
                 column(4, sliderInput(inputId = 'tiempo' , label = 'Seleccione un intervalo de búsqueda', min = 30 , max = 180 , value = 60, step = 30 , post = ' min'))
               ))
      insertUI(selector = '#NinjaC', where = 'afterEnd',
               fluidRow(
                 column(2 , actionButton(inputId = 'boton_siguiente' , label = 'Vincular Eventos')),
                 column(2 , actionButton(inputId = 'boton_inutil' , label = 'Evento No Encontrado'))
               ))
      insertUI(selector = '#NinjaD', where = 'afterEnd',
               tags$h3('Secretaría de Seguridad Ciudadana (SSC)'))
      removeUI(selector = '.texto_inicial')
      loop$i <- 1
      longitud <- muestra_pgj$data[loop$i,]$lon
      latitud <- muestra_pgj$data[loop$i,]$lat
      mapa_proxy %>%
        flyTo(lng = longitud , lat = latitud , zoom = 16) %>%
        clearShapes()
    }
  })
  # =
  observeEvent(input$boton_restart , {
    tmp <- filter(bases_de_datos$unificada , base_original == 'PGJ')
    muestra_pgj$data <- tmp[sample(nrow(tmp), nrow(tmp)*(input$muestras/100)),]
    removeModal()
    loop$i <- 1
    longitud <- muestra_pgj$data[loop$i,]$lon
    latitud <- muestra_pgj$data[loop$i,]$lat
    mapa_proxy %>%
      flyTo(lng = longitud , lat = latitud , zoom = 16)
  })
  # = Buffer
  observeEvent(c(input$radio , input$tiempo  , input$boton_siguiente , input$boton_restart , input$boton_inutil), {
    longitud <- muestra_pgj$data[loop$i,]$lon
    latitud <- muestra_pgj$data[loop$i,]$lat
    # =
    buffer <- st_buffer(st_transform(st_sfc(st_point(x = c(longitud,latitud) , dim = 'XY'), crs = 4326), 32614), input$radio)
    tmp_contains <- st_contains(buffer , bases_de_datos$unificada_sf)
    seleccion_ssc <- bases_de_datos$unificada[tmp_contains[[1]],]
    seleccion_ssc <- filter(seleccion_ssc , base_original == 'SSC')
    # =
    tiempo_base <- muestra_pgj$data[loop$i,]$timestamp
    tmp_time <- input$tiempo / 1440
    min_tiempo <- tiempo_base - tmp_time
    max_tiempo <- tiempo_base + tmp_time
    seleccion_ssc <- filter(seleccion_ssc , timestamp >= min_tiempo & timestamp <= max_tiempo)
    # =
    mapa_proxy %>%
      removeShape(layerId = 'circulo') %>%
      clearMarkers() %>%
      addCircles(lng = longitud , lat = latitud , radius = input$radio , layerId = 'circulo') %>%
      addCircleMarkers(lng = longitud , lat = latitud , fillColor = '#FF7E66' , color = '#FF7E66' , opacity = 0.8) %>%
      addCircleMarkers(lng = seleccion_ssc$lon , lat = seleccion_ssc$lat , fillColor = '#009D71' , color = '#009D71' , opacity = 0.8)
  })
  # =
  observeEvent(input$mapa_marker_click , {
    tiempo_base <- muestra_pgj$data[loop$i,]$timestamp
    tmp_time <- input$tiempo / 1440
    min_tiempo <- tiempo_base - tmp_time
    max_tiempo <- tiempo_base + tmp_time
    # =
    candidato_ssc$data <- filter(bases_de_datos$ssc , latitud == input$mapa_marker_click$lat & (timestamp >= min_tiempo & timestamp <= max_tiempo))
  })
  # =
  observeEvent(input$boton_siguiente , {
    removeUI(selector = '.error_vinculacion')
    if (is.null(candidato_ssc$data)) {
      insertUI(selector = '#NinjaD', where = 'afterEnd',
               tags$code('Por favor, seleccione un evento de la SSC para realizar la vinculación' , class = 'error_vinculacion'))
    }
    else if (loop$i < nrow(muestra_pgj$data)) {
      bases_de_datos$unificada[bases_de_datos$unificada$id_global == muestra_pgj$data[loop$i,]$id_global , 'id_SSC'] <- candidato_ssc$data$X
      # =
      tmp_pgj <- filter(bases_de_datos$unificada_sf , id_global == muestra_pgj$data[loop$i,]$id_global)
      tmp_ssc <- filter(bases_de_datos$unificada_sf , id_original == candidato_ssc$data$X & base_original == 'SSC')
      tmp_distance <- st_distance(tmp_pgj , tmp_ssc)
      resultado$diametros <- append(resultado$diametros , as.numeric(tmp_distance[[1]]))
      # =
      time_ssc <- bases_de_datos$ssc[bases_de_datos$ssc$X == candidato_ssc$data$X , 'timestamp']
      time_pgj <- bases_de_datos$pgj[bases_de_datos$pgj$X == muestra_pgj$data[loop$i,]$id_original , 'timestamp']
      resultado$tiempos <- append(resultado$tiempos , as.numeric(abs(time_ssc - time_pgj)))
      # =
      loop$i <- loop$i + 1
      longitud <- muestra_pgj$data[loop$i,]$lon
      latitud <- muestra_pgj$data[loop$i,]$lat
      mapa_proxy %>%
        flyTo(lng = longitud , lat = latitud , zoom = 16)
      candidato_ssc$data <- NULL
    } else {
      bases_de_datos$unificada[bases_de_datos$unificada$id_global == muestra_pgj$data[loop$i,]$id_global , 'id_SSC'] <- candidato_ssc$data$X
      # =
      tmp_pgj <- filter(bases_de_datos$unificada_sf , id_global == muestra_pgj$data[loop$i,]$id_global)
      tmp_ssc <- filter(bases_de_datos$unificada_sf , id_original == candidato_ssc$data$X & base_original == 'SSC')
      tmp_distance <- st_distance(tmp_pgj , tmp_ssc)
      resultado$diametros <- append(resultado$diametros , as.numeric(tmp_distance[[1]]))
      # =
      time_ssc <- bases_de_datos$ssc[bases_de_datos$ssc$X == candidato_ssc$data$X , 'timestamp']
      time_pgj <- bases_de_datos$pgj[bases_de_datos$pgj$X == muestra_pgj$data[loop$i,]$id_original , 'timestamp']
      resultado$tiempos <- append(resultado$tiempos , as.numeric(abs(time_ssc - time_pgj)))
      # =
      removeUI(selector = '.area_vinculacion')
      insertUI(selector = '#NinjaE', where = 'afterEnd',
               tags$h3('Tabla de Vinculación Final'))
      insertUI(selector = '#NinjaF', where = 'afterEnd',
               tags$div(id = 'div_balgoritmo', actionButton(inputId = 'boton_algoritmo' , label = 'Iniciar Algoritmo')))
      # =
      tmp <- bases_de_datos$unificada %>% filter(base_original == 'PGJ') %>% select(id_original , id_SSC)
      tmp <- merge(x = bases_de_datos$pgj , y = tmp , by.x = 'X' , by.y = 'id_original')
      resultado$data <- merge(x = tmp , y = bases_de_datos$ssc , by.x = 'id_SSC' , by.y = 'X')
      # =
      mapa_proxy %>%
        removeShape(layerId = 'circulo') %>%
        addPolygons(data = cdmx , fillColor = '#DCDCDC' , fillOpacity = 0.5 , color = '#2F4F4F' , opacity = 0.75) %>%
        flyTo(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
        clearMarkers()
    }
  })
  # =
  observeEvent(input$boton_inutil , {
    removeUI(selector = '.error_vinculacion')
    if (loop$i < nrow(muestra_pgj$data)) {
      loop$i <- loop$i + 1
      longitud <- muestra_pgj$data[loop$i,]$lon
      latitud <- muestra_pgj$data[loop$i,]$lat
      mapa_proxy %>%
        flyTo(lng = longitud , lat = latitud , zoom = 16)
      candidato_ssc$data <- NULL
    } else {
      removeUI(selector = '.area_vinculacion')
      insertUI(selector = '#NinjaE', where = 'afterEnd',
               tags$h3('Tabla de Vinculación Final'))
      insertUI(selector = '#NinjaF', where = 'afterEnd',
               tags$div(id = 'div_balgoritmo', actionButton(inputId = 'boton_algoritmo' , label = 'Iniciar Algoritmo')))
      # =
      tmp <- bases_de_datos$unificada %>% filter(base_original == 'PGJ') %>% select(id_original , id_SSC)
      tmp <- merge(x = bases_de_datos$pgj , y = tmp , by.x = 'X' , by.y = 'id_original')
      resultado$data <- merge(x = tmp , y = bases_de_datos$ssc , by.x = 'id_SSC' , by.y = 'X')
      # =
      mapa_proxy %>%
        removeShape(layerId = 'circulo') %>%
        addPolygons(data = cdmx , fillColor = '#DCDCDC' , fillOpacity = 0.5 , color = '#2F4F4F' , opacity = 0.75) %>%
        flyTo(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
        clearMarkers()
    }
  })
  # =
  output$tabla_pgj <- renderTable(rownames = TRUE , colnames = FALSE , {
    if (is.null(muestra_pgj$data)) {
      NULL
    } else {
      tmp1 <- filter(bases_de_datos$pgj , X == muestra_pgj$data[loop$i,]$id_original)
      tmp2 <- tmp1 %>% select('fecha_de_hechos','hora_de_hechos','municipio','colonia','delito','calidad_juridica') %>%
        rename('Fecha de Hechos'='fecha_de_hechos', 'Hora de Hechos' = 'hora_de_hechos','Alcaldía'='municipio','Colonia'='colonia','Tipo de Delito'='delito','Calidad Jurídica'='calidad_juridica')
      t(tmp2)
    }
  })
  # =
  output$remanentes <- renderText({
    if (is.null(muestra_pgj$data)) {
      NULL
    } else if (loop$i <= nrow(muestra_pgj$data)) {
      paste('Etiquetando elemento No.',loop$i,'de un total de',nrow(muestra_pgj$data),'muestras')
    }
  })
  # =
  output$tabla_ssc <- renderTable(rownames = TRUE , colnames = FALSE , {
    if (is.null(candidato_ssc$data)) {
      NULL
    } else {
      tmp4 <- candidato_ssc$data %>% select('fecha_evento','hora','alcaldia','colonia','tipo_de_evento','identidad','condicion') %>%
        rename('Fecha de Hechos' = 'fecha_evento' , 'Hora de Hechos' = 'hora' , 'Alcaldía' = 'alcaldia' , 'Colonia' = 'colonia' , 'Tipo de Evento' = 'tipo_de_evento' , 'Víctima' = 'identidad' , 'Condicioń' = 'condicion')
      t(tmp4)
    }
  })
  # =
  output$tabla_vinculos <- renderDataTable({
    if (is.null(resultado$data)) {
      NULL
    } else {
      resultado$data %>% select('fecha_de_hechos','hora_de_hechos','municipio','colonia.x','delito','tipo_de_evento','identidad','calidad_juridica') %>%
        rename('Fecha'='fecha_de_hechos','Hora'='hora_de_hechos','Alcaldia'='municipio','Colonia'='colonia.x','Delito PGJ' = 'delito','Tipo de Evento SSC'='tipo_de_evento','Identidad SSC'='identidad','Calidad Juridica PGJ'='calidad_juridica')
    }
  })
  # =
  observeEvent(input$boton_algoritmo , {
    insertUI(selector = '#div_balgoritmo', where = 'beforeBegin', immediate = TRUE,
             tags$code('Por favor espere a que desaparezca el botón para indicar la ejecución del algoritmo'))
    # =
    distancia_final <- mean(resultado$diametros) + (2*sd(resultado$diametros))
    tiempo_final <- mean(resultado$tiempos) + (2*sd(resultado$tiempos))
    # =
    algoritmo <- function(fila) {
      if (fila['base_original'][[1]] == 'SSC') {
        return(as.integer(fila['id_original']))
      } else if (!is.na(fila['id_SSC'][[1]])) {
        return(as.integer(fila['id_SSC']))
      } else {
        shp <- filter(bases_de_datos$unificada_sf , id_global == as.integer(fila['id_global']))
        # =
        tmp_within <- st_is_within_distance(shp , bases_de_datos$unificada_sf , distancia_final)
        posibles <- bases_de_datos$unificada[tmp_within[[1]],]
        # =
        posibles <- filter(posibles , id_global != as.integer(fila['id_global']))
        posibles <- filter(posibles , base_original == 'SSC')
        # =
        min_tiempo <- shp$timestamp - tiempo_final
        max_tiempo <- shp$timestamp + tiempo_final
        posibles <- filter(posibles , timestamp >= min_tiempo & timestamp <= max_tiempo)
        # =
        if (length(posibles$id_original) > 1) {
          posibles <- posibles[which.min(posibles$timestamp - shp$timestamp) , ]
          return(posibles$id_original)
        } else {
          return(posibles$id_original)
        }
      }
    }
    # =
    bases_de_datos$unificada$id_SSC <- as.numeric(apply(bases_de_datos$unificada , MARGIN = 1 , FUN = algoritmo))
    # =
    tmp <- bases_de_datos$unificada %>% filter(base_original == 'PGJ') %>% select(id_original , id_SSC)
    tmp <- merge(x = bases_de_datos$pgj , y = tmp , by.x = 'X' , by.y = 'id_original')
    resultado$final <- merge(x = tmp , y = bases_de_datos$ssc , by.x = 'id_SSC' , by.y = 'X')
    removeUI(selector='#div_balgoritmo')
    #write.csv(resultado$final , 'data/final_dentro.csv')
  })
  # =
  observeEvent(input$boton_default , {
    distancia_final <- 600    # 600 m
    tiempo_final <- 0.1041667 # 2.5 h
    # =
    # =
    algoritmo <- function(fila) {
      if (fila['base_original'][[1]] == 'SSC') {
        return(as.integer(fila['id_original']))
      } else if (!is.na(fila['id_SSC'][[1]])) {
        return(as.integer(fila['id_SSC']))
      } else {
        shp <- filter(bases_de_datos$unificada_sf , id_global == as.integer(fila['id_global']))
        # =
        tmp_within <- st_is_within_distance(shp , bases_de_datos$unificada_sf , distancia_final)
        posibles <- bases_de_datos$unificada[tmp_within[[1]],]
        # =
        posibles <- filter(posibles , id_global != as.integer(fila['id_global']))
        posibles <- filter(posibles , base_original == 'SSC')
        # =
        min_tiempo <- shp$timestamp - tiempo_final
        max_tiempo <- shp$timestamp + tiempo_final
        posibles <- filter(posibles , timestamp >= min_tiempo & timestamp <= max_tiempo)
        # =
        if (length(posibles$id_original) > 1) {
          posibles <- posibles[which.min(posibles$timestamp - shp$timestamp) , ]
          return(posibles$id_original)
        } else {
          return(posibles$id_original)
        }
      }
    }
    # =
    bases_de_datos$unificada$id_SSC <- as.numeric(apply(bases_de_datos$unificada , MARGIN = 1 , FUN = algoritmo))
    # =
    tmp <- bases_de_datos$unificada %>% filter(base_original == 'PGJ') %>% select(id_original , id_SSC)
    tmp <- merge(x = bases_de_datos$pgj , y = tmp , by.x = 'X' , by.y = 'id_original')
    resultado$final <- merge(x = tmp , y = bases_de_datos$ssc , by.x = 'id_SSC' , by.y = 'X')
    # =
    bases_de_datos$unificada$id_SSC <- replicate(nrow(bases_de_datos$unificada) , as.integer(NA))
  })
  # ===== PARTE 3 - MUESTRA DE RESULTADOS =====
  observeEvent(input$boton_mostrar , {
    insertUI(selector = '#NinjaG' , where = 'afterEnd' ,
             tags$div(id = 'div_bocultar' , fluidRow(
               actionButton(inputId = 'boton_ocultar' , label = 'Ocultar Tabla de Vínculos Final'),
               dataTableOutput(outputId = 'tabla_final')
             )))
    removeUI(selector = '#div_bmostrar')
  })
  # =
  observeEvent(input$boton_ocultar , {
    insertUI(selector = '#NinjaG' , where = 'afterEnd' ,
             tags$div(id = 'div_bmostrar' , actionButton(inputId = 'boton_mostrar' , label = 'Mostrar Tabla de Vínculos Final')))
    removeUI(selector = '#div_bocultar')
  })
  # =
  output$tabla_final <- renderDataTable({
    resultado$final
  })
  # =
  output$texto_resultado <- renderText({
    if (is.null(resultado$final)) {
      NULL
    } else if (input$incidente == 2) {
      paste0('Total de Eventos Ocurridos: ', nrow(bases_de_datos$pgj) , '\n',
             'Número de Vínculos logrados: ', nrow(resultado$final) , '\n',
             'Número de Vínculos no logrados: ', sum(bases_de_datos$ssc$total_occisos) - nrow(resultado$final) , '\n',
             'Número de eventos en PGJ sin registro en SSC: ', nrow(bases_de_datos$pgj) - sum(bases_de_datos$ssc$total_occisos) , '\n')
    } else {
      paste0('Total de Eventos Ocurridos: ', sum(bases_de_datos$ssc$total_lesionados) , '\n',
             'Número de Vínculos logrados: ', nrow(resultado$final) , '\n',
             'Número de Vínculos no logrados: ', nrow(bases_de_datos$pgj) - nrow(resultado$final) , '\n',
             'Número de eventos en SSC sin registro en PGJ: ', sum(bases_de_datos$ssc$total_lesionados) - nrow(bases_de_datos$pgj) , '\n')
    }
  })
  # =
  output$grafica_a <- renderPlot({
    if (is.null(resultado$final)) {
      NULL
    } else if (input$incidente == 2) {
      muertes_ssc <- c(sum(filter(bases_de_datos$ssc , total_occisos != 0 & mes == 'ENERO')$total_occisos) ,
                       sum(filter(bases_de_datos$ssc , total_occisos != 0 & mes == 'FEBRERO')$total_occisos) ,
                       sum(filter(bases_de_datos$ssc , total_occisos != 0 & mes == 'MARZO')$total_occisos))
      muertes_pgj <- c(count(filter(bases_de_datos$pgj , timestamp >= dates('01/01/2018') & timestamp <= dates('01/31/2018')))$n ,
                       count(filter(bases_de_datos$pgj , timestamp >= dates('02/01/2018') & timestamp <= dates('02/28/2018')))$n ,
                       count(filter(bases_de_datos$pgj , timestamp >= dates('03/01/2018') & timestamp <= dates('03/31/2018')))$n)
      muertes_final <- c(count(filter(resultado$final , mes == 'ENERO'))$n ,
                         count(filter(resultado$final , mes == 'FEBRERO'))$n ,
                         count(filter(resultado$final , mes == 'MARZO'))$n)
      muertes <- c(muertes_pgj[1] , muertes_ssc[1] , muertes_final[1] ,
                   muertes_pgj[2] , muertes_ssc[2] , muertes_final[2] ,
                   muertes_pgj[3] , muertes_ssc[3] , muertes_final[3])
      meses <- c(replicate(3 , 'ENERO') , replicate(3 , 'FEBRERO') , replicate(3 , 'MARZO'))
      etiquetas <- c('PGJ' , 'SSC' , 'FINAL' ,'PGJ' , 'SSC' , 'FINAL' ,'PGJ' , 'SSC', 'FINAL')
      tmp <- data.frame(meses , etiquetas , muertes)
      ggplot(data = tmp , aes(x = meses , y = muertes , colour = etiquetas)) +
        geom_col(size = 1.5, position = 'dodge')
      # ggplot(tmp , aes(muertes)) +
      #   geom_freqpoly()
    } else {
      muertes_ssc <- c(sum(filter(bases_de_datos$ssc , total_lesionados != 0 & mes == 'ENERO')$total_lesionados) ,
                       sum(filter(bases_de_datos$ssc , total_lesionados != 0 & mes == 'FEBRERO')$total_lesionados) ,
                       sum(filter(bases_de_datos$ssc , total_lesionados != 0 & mes == 'MARZO')$total_lesionados))
      muertes_pgj <- c(count(filter(bases_de_datos$pgj , timestamp >= dates('01/01/2018') & timestamp <= dates('01/31/2018')))$n ,
                       count(filter(bases_de_datos$pgj , timestamp >= dates('02/01/2018') & timestamp <= dates('02/28/2018')))$n ,
                       count(filter(bases_de_datos$pgj , timestamp >= dates('03/01/2018') & timestamp <= dates('03/31/2018')))$n)
      muertes_final <- c(count(filter(resultado$final , mes == 'ENERO'))$n ,
                         count(filter(resultado$final , mes == 'FEBRERO'))$n ,
                         count(filter(resultado$final , mes == 'MARZO'))$n)
      muertes <- c(muertes_pgj[1] , muertes_ssc[1] , muertes_final[1] ,
                   muertes_pgj[2] , muertes_ssc[2] , muertes_final[2] ,
                   muertes_pgj[3] , muertes_ssc[3] , muertes_final[3])
      meses <- c(replicate(3 , 'ENERO') , replicate(3 , 'FEBRERO') , replicate(3 , 'MARZO'))
      etiquetas <- c('PGJ' , 'SSC' , 'FINAL' ,'PGJ' , 'SSC' , 'FINAL' ,'PGJ' , 'SSC', 'FINAL')
      tmp <- data.frame(meses , etiquetas , muertes)
      ggplot(data = tmp , aes(x = meses , y = muertes , colour = etiquetas)) +
        geom_path()
    }
  })
  # =
  output$grafica_b <- renderPlot({
    if (input$incidente == 2) {
      max_value = 30
    } else {
      max_value = 50
    }
    if (is.null(input$grafica_a_click)) {
      NULL
    } else if (between(input$grafica_a_click$x , 0.5 , 1.5)) {
      tmp <- count(select(filter(resultado$final , mes == 'ENERO') , 'mes' , 'identidad'), mes , identidad)
      ggplot(data = tmp , aes(x = mes , y = n , colour = identidad)) +
        geom_col(size = 1.5, position = 'dodge') +
        ylim(0 , max_value)
    } else if (between(input$grafica_a_click$x , 1.5 , 2.5)) {
      tmp <- count(select(filter(resultado$final , mes == 'FEBRERO') , 'mes' , 'identidad'), mes , identidad)
      ggplot(data = tmp , aes(x = mes , y = n , colour = identidad)) +
        geom_col(size = 1.5, position = 'dodge') +
        ylim(0 , max_value)
    } else if (between(input$grafica_a_click$x , 2.5 , 3.5)) {
      tmp <- count(select(filter(resultado$final , mes == 'MARZO') , 'mes' , 'identidad'), mes , identidad)
      ggplot(data = tmp , aes(x = mes , y = n , colour = identidad)) +
        geom_col(size = 1.5, position = 'dodge') +
        ylim(0 , max_value)
    }
  })
  # =
  output$texto_grafica_b <- renderText({
    if (is.null(input$grafica_a_click)) {
      NULL
    } else if (input$incidente == 2) {
      if (between(input$grafica_a_click$x , 0.5 , 1.5)) {
        string_mes <- 'ENERO'
        muertes_pgj <- count(filter(bases_de_datos$pgj , timestamp >= dates('01/01/2018') & timestamp <= dates('01/31/2018')))$n
        muertes_ssc <- sum(filter(bases_de_datos$ssc , total_occisos != 0 & mes == string_mes)$total_occisos)
        muertes_final <- count(filter(resultado$final , mes == string_mes))$n
      } else if (between(input$grafica_a_click$x , 1.5 , 2.5)) {
        string_mes <- 'FEBRERO'
        muertes_pgj <- count(filter(bases_de_datos$pgj , timestamp >= dates('02/01/2018') & timestamp <= dates('02/28/2018')))$n
        muertes_ssc <- sum(filter(bases_de_datos$ssc , total_occisos != 0 & mes == string_mes)$total_occisos)
        muertes_final <- count(filter(resultado$final , mes == string_mes))$n
      } else if (between(input$grafica_a_click$x , 2.5 , 3.5)) {
        string_mes <- 'MARZO'
        muertes_pgj <- count(filter(bases_de_datos$pgj , timestamp >= dates('03/01/2018') & timestamp <= dates('03/31/2018')))$n
        muertes_ssc <- sum(filter(bases_de_datos$ssc , total_occisos != 0 & mes == string_mes)$total_occisos)
        muertes_final <- count(filter(resultado$final , mes == string_mes))$n
      }
      paste0('Resultados para el mes de ', string_mes , '\n',
             'Total de Eventos Ocurridos: ', muertes_pgj , '\n',
             'Número de Vínculos logrados: ', muertes_final , '\n',
             'Número de Vínculos no logrados: ', muertes_ssc - muertes_final , '\n',
             'Número de eventos en PGJ sin registro en SSC: ', muertes_pgj - muertes_ssc , '\n',
             '===================================\n',
             'Número de Eventos por Naturaleza del Fallecido\n',
             'Ciclista: ', count(filter(resultado$final , mes == string_mes & identidad == 'CICLISTA'))$n, '\n',
             'Conductor: ', count(filter(resultado$final , mes == string_mes & identidad == 'CONDUCTOR'))$n, '\n',
             'Motociclista: ', count(filter(resultado$final , mes == string_mes & identidad == 'MOTOCICLISTA'))$n, '\n',
             'Pasajero: ', count(filter(resultado$final , mes == string_mes & identidad == 'PASAJERO'))$n, '\n',
             'Peatón: ', count(filter(resultado$final , mes == string_mes & identidad == 'PEATON'))$n)
    } else {
      if (between(input$grafica_a_click$x , 0.5 , 1.5)) {
        string_mes <- 'ENERO'
        muertes_pgj <- count(filter(bases_de_datos$pgj , timestamp >= dates('01/01/2018') & timestamp <= dates('01/31/2018')))$n
        muertes_ssc <- sum(filter(bases_de_datos$ssc , total_lesionados != 0 & mes == string_mes)$total_lesionados)
        muertes_final <- count(filter(resultado$final , mes == string_mes))$n
      } else if (between(input$grafica_a_click$x , 1.5 , 2.5)) {
        string_mes <- 'FEBRERO'
        muertes_pgj <- count(filter(bases_de_datos$pgj , timestamp >= dates('02/01/2018') & timestamp <= dates('02/28/2018')))$n
        muertes_ssc <- sum(filter(bases_de_datos$ssc , total_lesionados != 0 & mes == string_mes)$total_lesionados)
        muertes_final <- count(filter(resultado$final , mes == string_mes))$n
      } else if (between(input$grafica_a_click$x , 2.5 , 3.5)) {
        string_mes <- 'MARZO'
        muertes_pgj <- count(filter(bases_de_datos$pgj , timestamp >= dates('03/01/2018') & timestamp <= dates('03/31/2018')))$n
        muertes_ssc <- sum(filter(bases_de_datos$ssc , total_lesionados != 0 & mes == string_mes)$total_lesionados)
        muertes_final <- count(filter(resultado$final , mes == string_mes))$n
      }
      paste0('Resultados para el mes de ', string_mes , '\n',
             'Total de Eventos Ocurridos: ', muertes_ssc , '\n',
             'Número de Vínculos logrados: ', muertes_final , '\n',
             'Número de Vínculos no logrados: ', muertes_pgj - muertes_final , '\n',
             'Número de eventos en SSC sin registro en PGJ: ', muertes_ssc - muertes_pgj , '\n',
             '===================================\n',
             'Número de Eventos por Naturaleza del Lesionado\n',
             'Ciclista: ', count(filter(resultado$final , mes == string_mes & identidad == 'CICLISTA'))$n, '\n',
             'Conductor: ', count(filter(resultado$final , mes == string_mes & identidad == 'CONDUCTOR'))$n, '\n',
             'Motociclista: ', count(filter(resultado$final , mes == string_mes & identidad == 'MOTOCICLISTA'))$n, '\n',
             'Pasajero: ', count(filter(resultado$final , mes == string_mes & identidad == 'PASAJERO'))$n, '\n',
             'Peatón: ', count(filter(resultado$final , mes == string_mes & identidad == 'PEATON'))$n)
    }
  })
}

shinyApp(ui = ui, server = server)