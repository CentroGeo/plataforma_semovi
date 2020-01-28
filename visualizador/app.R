# Visualizador de SEMOVI

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

library(chron)
library(lubridate)
library(readxl)
library(jsonlite)
library(shinycssloaders)

library(shinydashboard)
library(shinyjs)
library(leaflet.extras)
library(htmltools)

# ===== OPERACIONES INICIALES =====
cdmx <- read_sf(dsn = "data/cdmx.shp", layer = "cdmx")
cdmx_sa <- read_sf(dsn = "data/cdmx_sa.shp", layer = "cdmx_sa")
# 
# # = PGJ =
# pgj <- read.csv('data/PGJ.csv' , sep = ';' , encoding = 'UTF-8' , stringsAsFactors = FALSE)
# # =
# pgj['hora_de_hechos'] <- times(paste0(substr(pgj$fecha_hechos , 12 , 16), ':00'))
# pgj['fecha_de_hechos'] <- dates(substr(pgj$fecha_hechos , 1 , 10) , format = 'y-m-d')
# pgj['timestamp'] <- chron(pgj$fecha_de_hechos , pgj$hora_de_hechos)
# pgj <- filter(pgj , timestamp >= dates('2018-01-01' , format = 'y-m-d'))
# # =
# pgj['geopoint'] <- NULL
# pgj <- pgj[order(pgj$timestamp),]
# pgj['id'] <- seq.int(nrow(pgj))
# pgj <- filter(pgj , !is.na(latitud) & !is.na(longitud))
# pgj <- filter(pgj , !is.na(timestamp))
# # =
# pgj <- st_transform(st_as_sf(pgj , coords = c('longitud','latitud') , crs = 4326), 32614)
# 
# # =SSC =
# ssc <- read.csv('data/SSC.csv' , sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE)
# ssc$tmp <- times(substring(ssc$hora , 1 , 8))
# ssc$tmp[str_sub(ssc$hora , -2) == 'PM' &  str_sub(ssc$hora , start = 1 , end = 2) != '12'] <- ssc$tmp[str_sub(ssc$hora , -2) == 'PM' &  str_sub(ssc$hora , start = 1 , end = 2) != '12'] + 0.5
# ssc$tmp[str_sub(ssc$hora , -2) == 'AM' &  str_sub(ssc$hora , start = 1 , end = 2) == '12'] <- ssc$tmp[str_sub(ssc$hora , -2) == 'AM' &  str_sub(ssc$hora , start = 1 , end = 2) == '12'] - 0.5
# ssc$hora <- ssc$tmp
# ssc$tmp <- NULL
# # ssc['hora'] <- times(substr(as.character(strptime(as.character(ssc$hora) , '%I:%M:%S %p')), 12 , 19))
# ssc['fecha_evento'] <- dates(as.character(ssc$fecha_evento) , format = 'd/m/y')
# ssc['timestamp'] <- chron(ssc$fecha_evento , ssc$hora)
# ssc <- filter(ssc , timestamp >= dates('2018-01-01' , format = 'y-m-d'))
# ssc['tipo_vehiculo_1'] <- as.character(ssc$tipo_vehiculo_1)
# ssc['tipo_vehiculo_2'] <- as.character(ssc$tipo_vehiculo_2)
# ssc['tipo_vehiculo_3'] <- as.character(ssc$tipo_vehiculo_3)
# ssc['tipo_vehiculo_4'] <- as.character(ssc$tipo_vehiculo_4)
# ssc['ruta_transporte_publico'] <- as.character(ssc$ruta_transporte_publico)
# ssc['identidad'] <- as.character(ssc$identidad)
# ssc <- filter(ssc , !is.na(coordenada_y) & !is.na(coordenada_x))
# ssc <- filter(ssc , !is.na(timestamp))
# # =
# ssc <- st_transform(st_as_sf(ssc , coords = c('coordenada_y','coordenada_x') , crs = 4326), 32614)
# 
# # = C5 =
# c5 <- read.csv('data/C5.csv' , sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE)
# # =
# c5['hora_creacion'] <- times(c5$hora_creacion)
# c5['fecha_creacion'] <- dates(as.character(c5$fecha_creacion), format = 'd/m/y')
# c5['timestamp'] <- chron(c5$fecha_creacion , c5$hora_creacion)
# c5 <- filter(c5 , timestamp >= dates('2018-01-01' , format = 'y-m-d'))
# c5 <- filter(c5 , timestamp >= dates('2014-01-01' , format = 'y-m-d'))
# # =
# c5['geopoint'] <- NULL
# c5 <- filter(c5 , !is.na(latitud) & !is.na(longitud))
# c5 <- filter(c5 , !is.na(timestamp))
# # =
# c5 <- st_transform(st_as_sf(c5 , coords = c('longitud','latitud') , crs = 4326), 32614)
# 
# # = AXA =
# axa <- read.csv('data/AXA.csv' , sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE)
# # =
# axa['hora'] <- times(paste0(axa$hora, ':01:00'))
# axa['mes'] <- as.character(axa$mes)
# axa$mes[axa$mes == 'ENERO'] <- 1
# axa$mes[axa$mes == 'FEBRERO'] <- 2
# axa$mes[axa$mes == 'MARZO'] <- 3
# axa$mes[axa$mes == 'ABRIL'] <- 4
# axa$mes[axa$mes == 'MAYO'] <- 5
# axa$mes[axa$mes == 'JUNIO'] <- 6
# axa$mes[axa$mes == 'JULIO'] <- 7
# axa$mes[axa$mes == 'AGOSTO'] <- 8
# axa$mes[axa$mes == 'SEPTIEMBRE'] <- 9
# axa$mes[axa$mes == 'OCTUBRE'] <- 10
# axa$mes[axa$mes == 'NOVIEMBRE'] <- 11
# axa$mes[axa$mes == 'DICIEMBRE'] <- 12
# axa['fecha'] <- dates(paste0(axa$dia_numero , '/' , axa$mes , '/' , axa$ao) , format = 'd/m/y')
# axa['timestamp'] <- chron(axa$fecha , axa$hora)
# axa <- filter(axa , timestamp >= dates('2018-01-01' , format = 'y-m-d'))
# # =
# # axa['siniestro'] <- as.character(axa$siniestro)
# # axa$siniestro[axa$siniestro == '\\N'] <- paste0('SinID_', seq(1:nrow(filter(axa , siniestro == '\\N'))))
# axa <- filter(axa , !is.na(latitud) & !is.na(longitud))
# axa <- filter(axa , !is.na(timestamp))
# # =
# axa <- st_transform(st_as_sf(axa , coords = c('longitud','latitud') , crs = 4326), 32614)
# 
# # = Repubikla =
# repubikla <- read.csv('data/repubikla.csv' , sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE)
# # =
# repubikla['hora'] <- times(paste0(as.character(repubikla$hora), ':00'))
# repubikla['fecha'] <- dates(as.character(repubikla$fecha) , format = 'y-m-d')
# repubikla['timestamp'] <- chron(repubikla$fecha , repubikla$hora)
# repubikla <- filter(repubikla , timestamp >= dates('2018-01-01' , format = 'y-m-d'))
# repubikla <- filter(repubikla , !is.na(lat) & !is.na(lon))
# repubikla <- filter(repubikla , !is.na(timestamp))
# # =
# repubikla <- st_transform(st_as_sf(repubikla , coords = c('lon','lat') , crs = 4326), 32614)

# ===== FRONT END =====
ui <- dashboardPage(title = 'Visualizador de Datos de Incidentes Viales - SEMOVI', skin = 'green',
  dashboardHeader(title = 'Incidentes Viales'),
  # ===== SIDEBAR =====
  dashboardSidebar(sidebarMenu(id = 'menu_1',
                               menuItem(text = 'Introducción' , selected = TRUE , icon = icon('door-open') , tabName = 'introduccion'),
                               menuItem(text = 'Bases de Datos' , icon = icon('layer-group') , tabName = 'bd'),
                               menuItem(text = 'Instrucciones' , icon = icon('question-circle') , tabName = 'instrucciones'),
                               menuItem(text = 'Visualizador', icon = icon('globe-americas') , tabName = 'visualizador'))
                   # tags$div(tags$p(strong('Realizado en colaboración por:')),
                   #          tags$table(style = 'width: 100%;',
                   #                     tags$tr(tags$th('') , tags$th('')),
                   #                     tags$tr(tags$td(colspan = 2,
                   #                                     tags$img(src = 'gobcdmx.png' , style = 'display: block; margin: auto; width: 100%; padding-bottom: 15px'))),
                   #                     tags$tr(tags$td(tags$div(style = 'text-align: center; padding-bottom: 15px',
                   #                                              tags$img(src = 'axa.png' , style = 'height: 45px;'))),
                   #                             tags$td(tags$div(style = 'text-align: center; padding-bottom: 15px',
                   #                                              tags$img(src = 'conacyt.png' , style = 'height: 50px;')))),
                   #                     tags$tr(tags$td(tags$div(style = 'text-align: center;',
                   #                                              tags$img(src = 'centrogeo.png' , style = 'height: 50px;'))),
                   #                             tags$td(tags$div(style = 'text-align: center;',
                   #                                              tags$img(src = 'datalab.png' , style = 'width: 80px;'))))),
                   #          style = 'position: absolute; bottom: 0; left: 0; padding: 10px 10px; background-color: white; width: 100%; color: #697070;')
                   ),
  dashboardBody(useShinyjs() , tags$head(tags$link(rel = 'stylesheet' , type = 'text/css' , href = 'custom.css?version=71')), tabItems(
    # ===== TAB INTRODUCCIÓN =====
    tabItem(tabName = 'introduccion' ,
            tags$div(style = 'width: 100%; height: 90vh; text-align: center; padding-top: 10vh;',
                     tags$div(style = 'background-color: white; margin: auto; width: 70%; padding: 30px; border-radius: 10px;',
                              tags$img(src = 'logo_semovi.png' , style = 'height: 100px;'),
                              tags$p(strong('Visualizador de Incidentes Viales') , style = 'font-size: 32pt; color: #848888; padding-bottom: 15px;'),
                              tags$div(style = 'text-align: justify; margin: auto; width: 80%; font-size: 12pt; color: #697070;',
                                       tags$p('Uno de los compromisos de la ', strong('Secretaría de Movilidad de la Ciudad de México (SEMOVI)') ,' es entender las características de los hechos de tránsito que se suscitan en la ciudad, con el objetivo de planear estrategias de seguridad vial con base en evidencia.'),
                                       tags$p('A través de esta herramienta de visualización es posible explorar los datos generados por diferentes instancias gubernamentales relacionados con accidentes viales. Las bases de datos disponibles para este análisis son: ', strong('Secretaría de Seguridad Ciudadana (SSC)' , style = 'color: #043A5F;') ,', la ', strong('Procuraduría General de Justicia (PGJ)' , style = 'color: #952800;') ,', el ', strong('Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México (C5)' , style = 'color: #956F00;') ,', la aseguradora ', strong('AXA' , style = 'color: #5E0061;') ,' y el proyecto colaborativo ', strong('Repubikla' , style = 'color: #3F8500;') ,'. Es importante mencionar que cada una de las bases que se utilizaron fueron generadas con base en una metodología y objetivos distintos, basadas en las necesidades de las instituciones que las generan.'),
                                       tags$p('Para cumplir con el objetivo de la SEMOVI, es importante contar con un panorama general de todos los incidentes viales generados a través de las diferentes fuentes disponibles; es por ello que se creó ')),
                              tags$div(style = 'margin: auto; width: 80%; font-size: 24pt;',
                                       fluidRow(column(6 , actionButton(inputId = 'boton_ver_visualizador' , label = strong('Ir a Visualizador') , icon = icon('globe-americas') , style = 'background-color: #0073B6; color: white; border-color: ; font-size: 14pt;')),
                                                column(6 , actionButton(inputId = 'boton_ver_bd' , label = strong('Descripción de Bases de Datos') , icon = icon('layer-group') , style = 'background-color: #3D9971; color: white; border-color: ; font-size: 14pt;')))),
                              tags$div(style = 'height: 20px; background-color: white;'),
                              tags$div(style = 'margin: auto; width: 90%; text-align: left; color: #848888;',
                                       tags$p(strong('Realizado en colaboración por:'))),
                              tags$div(style = 'margin: auto; width: 90%;',
                                       fluidRow(column(3, tags$img(src = 'logo_semovi.png' , style = 'height: 45px;')),
                                                column(2, tags$img(src = 'axa.png' , style = 'height: 50px;')),
                                                column(2, tags$img(src = 'conacyt.png' , style = 'height: 50px;')),
                                                column(2, tags$img(src = 'centrogeo.png' , style = 'height: 50px;')),
                                                column(3, tags$img(src = 'datalab.png' , style = 'height: 30px;')))))
                     )),
    # ===== TAB BASES DE DATOS =====
    tabItem(tabName = 'bd' , box(width = 12,
                                tags$p(strong('Bases de Datos'), style = 'font-size: 18pt; color: #848888;'),
                                tags$div(style = 'background-color: white; width: 100%; height: 305px;',
                                         navlistPanel(id = 'menu_bd' , selected = 'PGJ', well = FALSE , widths = c(2 , 10) ,
                                                      tabPanel(title = 'PGJ',
                                                               tags$div(style = 'background-color: #AFB1B1; width: 100%; border-radius: 10px; padding: 10px 10px 0px; font-size: 90%;',
                                                                        tags$p(tags$strong('Procuraduría General de Justicia'),
                                                                               tags$img(src = 'pgj.png',
                                                                                        style = 'height: 85px; float: right; overflow: auto; padding-left: 5px;'),
                                                                               style = 'font-size: 18pt;'),
                                                                        tags$p('Carpetas de investigación de delitos a nivel de calle de la Procuraduría General de Justicia de la Ciudad de México, actualizados mensualmente con registros desde enero de 2016.',
                                                                               style = 'text-align: justify;'),
                                                                        fluidRow(column(3 , tags$strong('Campos Importantes'),
                                                                                        selectInput(inputId = 'campos_pgj' , label = NULL , 
                                                                                                    choices = c('Delito' , 'Fiscalía' , 'Agencia' , 'U. Investigación' , 'Fecha de Hechos' , 'Fecha de Inicio'))),
                                                                                 column(9 , tags$div(textOutput(outputId = 'texto_campos_pgj'),
                                                                                                     style = 'background-color: #C0C0C0; width: 100%; border-radius: 9px; padding: 5px 10px; font-size: 9pt; text-align: justify; word-wrap: break-word;'))),
                                                                        fluidRow(column(6 , tags$p(tags$strong('Fuente'),' – ',tags$a('Datos Abiertos de la CDMX' , href = 'https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico/'))),
                                                                                 column(6 , tags$p(strong('Temporalidad'), ' – Enero 2016 a Septiembre 2019*'))),
                                                                        fluidRow(column(4, tags$strong('Uso de la Base de Datos')),
                                                                                 column(8 , radioButtons(inputId = 'bd_pgj' , label = NULL, inline = TRUE,
                                                                                                         choiceNames = c('Local' , 'Remota'),
                                                                                                         choiceValues = c('A' , 'B')))),
                                                                        tags$div(style = 'width: 100%;  padding-bottom: 1%;', tags$p('* Para Base de Datos Local. Versión "Remota" utiliza la base más reciente disponible en "Datos Abiertos CDMX".',
                                                                                                                                     style = 'font-size: 9pt;'))
                                                                        
                                                               )),
                                                      tabPanel(title = 'SSC',
                                                               tags$div(style = 'background-color: #AFB1B1; width: 100%; border-radius: 10px; padding: 10px 10px 0px; font-size: 90%;',
                                                                        tags$p(tags$strong('Secretaría de Seguridad Ciudadana'),
                                                                               tags$img(src = 'ssc.png',
                                                                                        style = 'height: 85px; float: right; overflow: auto; padding-left: 5px;'),
                                                                               style = 'font-size: 18pt;'),
                                                                        tags$p('Reportes de incidentes viales realizados por policías respondientes. Sólo se cuentan con datos de Enero 2018 a Abril 2019, dado que son la única versión pública disponible.',
                                                                               style = 'text-align: justify;'),
                                                                        fluidRow(column(3 , tags$strong('Campos Importantes'),
                                                                                        selectInput(inputId = 'campos_ssc' , label = NULL , 
                                                                                                    choices = c('Tipo de Evento' , 'Intersección' , 'Cuadrante' , 'Vehículo' , 'Ruta T. Público' , 'Condición' , 'Total Occisos' , 'Identidad' , 'U. M. de Apoyo' , 'Observaciones'))),
                                                                                 column(9 , tags$div(textOutput(outputId = 'texto_campos_ssc'),
                                                                                                     style = 'background-color: #C0C0C0; width: 100%; border-radius: 9px; padding: 5px 10px; font-size: 9pt; text-align: justify;  word-wrap: break-word;'))),
                                                                        fluidRow(column(7 , tags$p(tags$strong('Fuente'),' – ',tags$a('Secretaría de Seguridad Ciudadana de la CDMX' , href = 'https://www.ssc.cdmx.gob.mx/'))),
                                                                                 column(5 , tags$p(strong('Temporalidad'), ' – Enero 2018 a Abril 2019')))
                                                                        
                                                               )),
                                                      tabPanel(title = 'C5',
                                                               tags$div(style = 'background-color: #AFB1B1; width: 100%; border-radius: 10px; padding: 10px 10px 0px; font-size: 90%;',
                                                                        tags$p(tags$strong('Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México'),
                                                                               tags$img(src = 'c5.png',
                                                                                        style = 'height: 85px; float: right; overflow: auto; padding-left: 5px;'),
                                                                               style = 'font-size: 16pt;'),
                                                                        tags$p('Incidentes viales reportados por el C5 desde Ene/2014, actualizado mensualmente. Sólo se consideran aquellos clasificados como reales por los códigos internos del centro.',
                                                                               style = 'text-align: justify;'),
                                                                        fluidRow(column(3 , tags$strong('Campos Importantes'),
                                                                                        selectInput(inputId = 'campos_c5' , label = NULL , 
                                                                                                    choices = c('Folio' , 'Fecha Creación' , 'Incidente C4' , 'Código de Cierre' , 'C. con F. Alarma' , 'Tipo de Entrada'))),
                                                                                 column(9 , tags$div(textOutput(outputId = 'texto_campos_c5'),
                                                                                                     style = 'background-color: #C0C0C0; width: 100%; border-radius: 9px; padding: 5px 10px; font-size: 9pt; text-align: justify; word-wrap: break-word;'))),
                                                                        fluidRow(column(6 , tags$p(tags$strong('Fuente'),' – ',tags$a('Datos Abiertos de la CDMX' , href = 'https://datos.cdmx.gob.mx/explore/dataset/incidentes-viales-c5'))),
                                                                                 column(6 , tags$p(strong('Temporalidad'), ' – Enero 2014 a Septiembre 2019*'))),
                                                                        fluidRow(column(4, tags$strong('Uso de la Base de Datos')),
                                                                                 column(8 , radioButtons(inputId = 'bd_c5' , label = NULL, inline = TRUE,
                                                                                                         choiceNames = c('Local' , 'Remota'),
                                                                                                         choiceValues = c('A' , 'B')))),
                                                                        tags$div(style = 'width: 100%;  padding-bottom: 1%;', tags$p('* Para Base de Datos Local. Versión "Remota" utiliza la base más reciente disponible en "Datos Abiertos CDMX".',
                                                                                                                                     style = 'font-size: 9pt;'))
                                                                        
                                                               )),
                                                      tabPanel(title = 'AXA',
                                                               tags$div(style = 'background-color: #AFB1B1; width: 100%; border-radius: 10px; padding: 10px 10px 0px; font-size: 90%;',
                                                                        tags$p(tags$strong('AXA Seguros'),
                                                                               tags$img(src = 'axa.png',
                                                                                        style = 'height: 85px; float: right; overflow: auto; padding-left: 5px;'),
                                                                               style = 'font-size: 18pt;'),
                                                                        tags$p('Datos de percances viales pertenecientes a AXA Seguros, pero aperturados y entregados a la comunidad mediante el Instituto Internacional de Ciencia de Datos.',
                                                                               style = 'text-align: justify;'),
                                                                        fluidRow(column(3 , tags$strong('Campos Importantes'),
                                                                                        selectInput(inputId = 'campos_axa' , label = NULL , 
                                                                                                    choices = c('Siniestro' , 'Causa Siniestro' , 'Vehículo' , 'Nivel de Daño' , 'Punto Impacto' , 'Año de Reporte' , 'T. Lesionados' , 'Rol Lesionado' , 'Nivel de Lesión' , 'Fallecido' , 'Hospitalizado' , 'Var. Binarias'))),
                                                                                 column(9 , tags$div(textOutput(outputId = 'texto_campos_axa'),
                                                                                                     style = 'background-color: #C0C0C0; width: 100%; border-radius: 9px; padding: 5px 10px; font-size: 9pt; text-align: justify; word-wrap: break-word;'))),
                                                                        fluidRow(column(7 , tags$p(tags$strong('Fuente'),' – ',tags$a('Instituto Internacional de Ciencia de Datos' , href = 'http://i2ds.org/datos-abiertos-percances-viales/'))),
                                                                                 column(5 , tags$p(strong('Temporalidad'), ' – Enero 2015 a Julio 2019')))
                                                                        
                                                               )),
                                                      tabPanel(title = 'Repubikla',
                                                               tags$div(style = 'background-color: #AFB1B1; width: 100%; border-radius: 10px; padding: 10px 10px 0px; font-size: 90%;',
                                                                        tags$p(tags$strong('Repubikla'),
                                                                               tags$img(src = 'repubikla2.png',
                                                                                        style = 'height: 85px; float: right; overflow: auto; padding-left: 5px;'),
                                                                               style = 'font-size: 18pt;'),
                                                                        tags$p('Plataforma de mapeo alimentada por ciudadanos que, a través del crowdsourcing, busca generar y centralizar datos sobre movilidad no motorizada.',
                                                                               style = 'text-align: justify;'),
                                                                        fluidRow(column(3 , tags$strong('Campos Importantes'),
                                                                                        selectInput(inputId = 'campos_repubikla' , label = NULL , 
                                                                                                    choices = c('Comentario' , 'Modo' , 'Fuente' , 'Responsable' , 'Gravedad' , 'Seguimiento'))),
                                                                                 column(9 , tags$div(textOutput(outputId = 'texto_campos_repubikla'),
                                                                                                     style = 'background-color: #C0C0C0; width: 100%; border-radius: 9px; padding: 5px 10px; font-size: 9pt; text-align: justify; word-wrap: break-word;'))),
                                                                        fluidRow(column(6 , tags$p(tags$strong('Fuente'),' – ',tags$a('Portal Oficial de Repubikla' , href = 'https://repubikla.herokuapp.com/'))),
                                                                                 column(6 , tags$p(strong('Temporalidad'), ' – Diciembre 2017 a Marzo 2019')))
                                                                        
                                                               )))
                                ))),
    # ===== TAB INSTRUCCIONES =====
    tabItem(tabName = 'instrucciones',
            # ===== CUADRO INSTRUCCIONES =====
            box(width = 12,
                tags$div(tags$img(src = 'gobcdmx.png',
                                  style = 'padding-bottom: 3px; height: 80px;'),
                         tags$br(),
                         strong('Visualizador de Incidentes Viales'),
                         style = 'text-align: center; font-size: 24pt; color: #848888; padding-bottom: 15px;'),
                fluidRow(column(7 , tags$p('Esta aplicación tiene por objetivo ofrecer a la población un medio a través del cual se visualicen los Incidentes Viales ocurridos en la CDMX y registrados por diversas entidades públicas y privadas, así como realizar análisis sencillos sobre su comportamiento temporal y espacial.' ,
                                           style = 'font-size: 11pt; text-align: justify; color: #697070;'),
                                tags$p(strong('Instrucciones'), style = 'font-size: 18pt; color: #848888;'),
                                tags$ol(style = 'font-size: 11pt; text-align: justify; color: #697070;' ,
                                        tags$li('Estudiar las Bases de Datos disponibles y la información proporcionada por cada una utilizando el menú inferior.'),
                                        tags$li('Utilizar el Panel de Navegación del costado izquierdo para acceder al Visualizador de Inciddentes Viales'),
                                        tags$li('Seleccionar las ',strong('Bases de Datos'),' con las que se desee trabajar. Los incidentes correspondientes aparecerán en el mapa y generarán gráficas con información pertinente.'),
                                        tags$li('La aplicación automáticamente arrojará los incidentes ocurridos en 2018; sin embargo, este intervalo puede ser ajustado utilizando el deslizador de ',strong('Periodo de Tiempo'),' presente en la parte inferior del mapa.'),
                                        tags$li('Si se desea, reducir el área de estudio a alguna de las alcaldías de la ciudad utilizando el filtro de ',strong('Área de Análisis'),' presente al costado superior derecho; asimismo, seleccionar entre "Decesos", "Lesionados", "Accidentes" o todos los anteriores utilizando el filtro de ',strong('Tipo de Incidente'),' para mostrar los incidentes correspondientes.'),
                                        tags$li('Explorar los datos más a fondo utilizando las ',strong('Gráficas'),' disponibles en el costado inferior derecho, así como al dar click sobre los eventos en el ',strong('Mapa Interactivo'),'.')),
                                tags$p('Del costado derecho se proporciona un ', strong('Video con Instrucciones'),' más detalladas sobre el uso de la aplicación.' ,
                                       style = 'font-size: 11pt; text-align: justify; color: #697070;')),
                         
                         column(5 , tags$video(src = 'tmp.mp4' , width = '100%' , height = '350px' , type = 'video/mp4' , controls = 'controls')
                                # tags$div('Placeholder' , style = 'background-color: #848888; height: 350px;')
                                ))),
            # ===== CUADRO BD =====
            
            # ===== CUADROS EXTRAS =====
            tags$p(strong('Cifras Importantes de la Base ', textOutput(outputId = 'texto_rndm' , inline = TRUE)), style = 'font-size: 18pt; color: #848888;'),
            fluidRow(valueBoxOutput(outputId = 'extra1', width = 6),
                     valueBoxOutput(outputId = 'extra2', width = 2),
                     valueBoxOutput(outputId = 'extra3', width = 2),
                     valueBoxOutput(outputId = 'extra4', width = 2))
            ),
    # ===== TAB VISUALIZADOR =====
    tabItem(tabName = 'visualizador',
            tags$div(style = 'text-align: center;',
                     fluidRow(column(2 , actionButton(inputId = 'boton_ver_instrucciones' , label = 'Ver Instrucciones' , icon = icon('question-circle') , style = 'background-color: #0073B6; color: white; border-color: ; font-size: 12pt;')),
                              column(2 , actionButton(inputId = 'boton_ver_bd2' , label = 'Descripción de Bases de Datos' , icon = icon('layer-group') , style = 'background-color: #3D9971; color: white; border-color: ; font-size: 12pt;')))),
            tags$div(id = 'berenjena', style = 'display: none; text-align: center;' ,
                     tags$div(style = 'height: 20px;'),
                     box(width = 12 ,
                         fluidRow(column(5 ,
                                         fluidRow(column(2 , actionButton(inputId = 'l_instrucciones' , label = '' , icon = icon('angle-left'))),
                                                  column(8 , tags$p(textOutput(outputId = 'pagina_instrucciones' , inline = TRUE) , '/ 8')),
                                                  column(2 , actionButton(inputId = 'r_instrucciones' , label = '' , icon = icon('angle-right')))),
                                         tags$p(strong(textOutput(outputId = 'titulo_instrucciones' , inline = TRUE)), style = 'font-size: 18pt; color: #848888; text-align: left;'),
                                         tags$p('Hola' , style = 'text-align: left;'),
                                         tags$div(style = 'display: none;',
                                                  numericInput(inputId = 'no_instrucciones' , label = NULL , value = 1 , min = 1 , max = 8))),
                                  column(7 ,
                                         tags$img(src = 'tmp.png' , style = 'width: 100%;'))
                                  
                                  ))),
            tags$div(style = 'height: 20px;'),
            fluidRow(column(6 , withSpinner(leafletOutput(outputId = 'mapa', height = '756px'),
                                            type = 3 , color = '#00A65A' , size = 2, color.background = '#ecf0f5'),
                            tags$div(style = 'width: 100%; height: 20px; background-color: white; opacity: 0;'),
                            fluidRow(box(width = 12,
                                sliderInput(inputId = 'filtro_fecha' , label = 'Periodo de Tiempo' , width = '100%', timeFormat = '%d/%m/%Y',
                                            min = as.Date('2018-01-01',"%Y-%m-%d"),
                                            max = as.Date('2018-12-31',"%Y-%m-%d"),
                                            value = c(as.Date('2018-01-01',"%Y-%m-%d") , as.Date('2018-12-31',"%Y-%m-%d")))))),
                     column(6 , box(width = 12 ,
                                    tags$p('Filtros de Incidentes',
                                           style = 'font-size: 16pt;'),
                                    selectInput(inputId = 'filtro_lugar' , label = 'Área de Análisis',
                                                choices = c('Total Ciudad de México' , 'Álvaro Obregón' , 'Azcapotzalco' , 'Benito Juárez' , 'Coyoacán',
                                                            'Cuajimalpa de Morelos' , 'Cuauhtémoc' , 'Gustavo A. Madero' ,
                                                            'Iztacalco' , 'Iztapalapa' , 'La Magdalena Contreras' , 'Miguel Hidalgo',
                                                            'Milpa Alta' , 'Tlalpan' , 'Tláhuac' , 'Venustiano Carranza', 'Xochimilco')),
                                    fluidRow(column(6,
                                                    radioButtons(inputId = 'filtro_incidente' , label = 'Tipo de Incidente' , inline = TRUE,
                                                                 choices = c('Decesos' , 'Lesionados' , 'Accidentes' , 'Todos')),
                                                    checkboxGroupInput(inputId = 'filtro_bd' , label = 'Base de Datos' , inline = TRUE,
                                                                       choices = c('PGJ' , 'SSC' , 'C5' , 'AXA' , 'Repubikla'))),
                                             column(6)
                                             )),
                            box(width = 12,
                                tabsetPanel(
                                  tabPanel(title = 'Gráficas por Totales',
                                           selectInput(inputId = 'tipo_grafica', label = 'Datos a Graficar', choices = ''),
                                           fluidRow(column(9,
                                                           radioButtons(inputId = 'tiempo_grafica' , label = 'Temporalidad a Graficar', inline = TRUE,
                                                                        choices = c('Por Mes' , 'Por Día') , selected = 'Por Mes')),
                                                    column(2, offset = 1,
                                                           actionButton(inputId = 'boton_zoom_grafica' , label = NULL , icon = icon('search-plus'),
                                                                        style = 'font-size:150%'))),
                                           withSpinner(plotOutput(outputId = 'grafica_sp', height = '350px',
                                                                  click = clickOpts(id = 'plot_click')),
                                                       type = 3 , color = '#00A65A' , size = 1 , color.background = '#FFFFFF'),
                                           uiOutput(outputId = 'click_info'),
                                           tags$div(id = 'div_grafica_a'),
                                           tags$div(tableOutput(outputId = 'tabla_totales'),
                                                    style = 'font-size: 80%; width: 100%; margin: auto;')),
                                  tabPanel(title = 'Gráficas por Día y Hora',
                                           selectInput(inputId = 'tipo_grafica2', label = 'Datos a Graficar', choices = ''),
                                           fluidRow(column(9,
                                                           radioButtons(inputId = 'tiempo_grafica2' , label = 'Temporalidad a Graficar', inline = TRUE,
                                                                        choices = c('Todo el Día' , 'Mañana (6AM - 12PM)' , 'Tarde (1PM - 9PM)' , 'Noche (10PM - 5AM)') , selected = 'Todo el Día')),
                                                    column(2, offset = 1,
                                                           actionButton(inputId = 'boton_zoom_grafica2' , label = NULL , icon = icon('search-plus'),
                                                                        style = 'font-size:150%'))),
                                           withSpinner(plotOutput(outputId = 'grafica_sp2', height = '350px',
                                                                  click = clickOpts(id = 'plot_click2')),
                                                       type = 3 , color = '#00A65A' , size = 1 , color.background = '#FFFFFF'),
                                           uiOutput(outputId = 'click_info2'),
                                           tags$div(id = 'div_grafica_a2'),
                                           fluidRow(column(1 , actionButton(inputId = 'pastel_left' , icon = icon('angle-left') , label = NULL)),
                                                    column(10 , textInput(inputId = 'pastel_texto' , label = NULL , value = '')),
                                                    column(1 , actionButton(inputId = 'pastel_right' , icon = icon('angle-right') , label = NULL))),
                                           plotOutput(outputId = 'grafica_pastel' , height = '50px'))
                                )
                                ))))
  ))
)

# ===== SERVIDOR =====

server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  # ===== REACTIVE VARIABLES =====
  bd <- reactiveValues(tmp_pgj = NULL , tmp_ssc = NULL , tmp_c5 = NULL , tmp_axa = NULL , tmp_repubikla = NULL , df_referencia = NULL)
  # =
  pgj_importada <- reactiveValues(bd = NULL)
  c5_importada <- reactiveValues(bd = NULL)
  key_remote <- reactiveValues(k_pgj = FALSE , k_c5 = FALSE)
  special_bd <- reactiveValues(active_pgj = pgj , active_c5 = c5)
  # =
  hover_h <- reactiveValues(h = NULL , h2 = NULL)
  graf_modal <- reactiveValues(g = NULL , g2 = NULL)
  text_pastel <- reactiveValues(df = NULL , z = 0 , max = 0)
  # =
  filtro_fecha_sp <- reactive(input$filtro_fecha) %>% debounce(1500)
  filtro_bd_sp <- reactive(input$filtro_bd) %>% debounce(1000)
  
  # ===== VENTANA INTRODUCCIÓN =====
  observeEvent(input$boton_ver_visualizador , updateTabItems(session , inputId = 'menu_1' , selected = 'visualizador'))
  
  observeEvent(input$boton_ver_bd , updateTabItems(session , inputId = 'menu_1' , selected = 'bd'))
  
  # ===== BASES DE DATOS REMOTAS - PGJ =====
  observeEvent(input$bd_pgj , {
    if (input$bd_pgj == 'B' & key_remote$k_pgj == FALSE) {
      showModal(modalDialog(title = NULL , footer = NULL,
                            tags$div(id = 'div_modalpgj_a'),
                            tags$div(id = 'div_modalpgj_b',
                                     tags$p(style = 'font-size: 20px;',
                                            strong('Importante')),
                                     tags$p(style = 'text-align: justify',
                                            'Al elegir la Base de Datos Remota, usted permite a la aplicación trabajar con la versión más actualizada disponible en',
                                            tags$a(href = 'https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico' , 'Datos Abiertos de la CDMX'),
                                            ', permitiéndole analizar los sucesos más recientes disponibles en dicha tabla.') ,
                                     tags$p(style = 'text-align: justify',
                                            'Sin embargo, la importación de esta base depende directamente del peso de ésta y de su conexión a internet, razón por la cual puede tardar algunos minutos en finalizarse este proceso.'),
                                     tags$p(style = 'font-size: 25px; text-align: center;',
                                            strong('¿Desea continuar con la importación?')),
                                     fluidRow(column(2, offset = 4,
                                                     actionButton(inputId = 'importar_pgj' , label = strong('Continuar'),
                                                                  style = 'background-color: #00A65B; color: white; border-color: ;')),
                                              column(2,
                                                     actionButton(inputId = 'no_importar_pgj' , label = strong(' Detener '),
                                                                  style = 'background-color: #DD4C39; color: white; border-color: ;')))
                            )))
    }
    else if (input$bd_pgj == 'B' & key_remote$k_pgj == TRUE) special_bd$active_pgj <- pgj_importada$bd
    else if (input$bd_pgj == 'A' & key_remote$k_pgj == TRUE) special_bd$active_pgj <- pgj
  })
  
  observeEvent(input$no_importar_pgj , {
    removeModal()
    updateRadioButtons(session , inputId = 'bd_pgj' , label = NULL, inline = TRUE, selected = 'A' ,
                       choiceNames = c('Local' , 'Remota'),
                       choiceValues = c('A' , 'B'))
  })
  
  observeEvent(input$importar_pgj , {
    removeUI(selector = '#div_modalpgj_b' , immediate = TRUE)
    insertUI(selector = '#div_modalpgj_a' , where = 'afterEnd' ,  immediate = TRUE ,
             tags$div(id = '#div_modalpgj_b',
                      tags$p(style = 'font-size: 20px;', class = 'finalizar_pgj',
                             strong('Importando Base de Datos de PGJ')),
                      tags$p('Espere mientras termina el proceso', class = 'finalizar_pgj'),
                      tags$div(id = 'div_load_pgj', class = 'finalizar_pgj', style = 'display: block; margin: auto; width: 50%;',
                               tags$img(src = 'loading.gif' , style = 'max-width:100%; max-height:100%; vertical-align: middle;'))))
    # =
    url = 'https://datos.cdmx.gob.mx/api/records/1.0/download/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&q=(delito+%3D+%27DAÑO+EN+PROPIEDAD+AJENA+CULPOSA+POR+TRÁNSITO+VEHICULAR+A+AUTOMOVIL%27)+OR+(delito+%3D+%27DAÑO+EN+PROPIEDAD+AJENA+CULPOSA+POR+TRÁNSITO+VEHICULAR+A+BIENES+INMUEBLES%27)+OR+(delito+%3D+%27HOMICIDIO+CULPOSO+POR+TRÁNSITO+VEHICULAR%27)+OR+(delito+%3D+%27HOMICIDIO+CULPOSO+POR+TRÁNSITO+VEHICULAR+(ATROPELLADO)%27)+OR+(delito+%3D+%27HOMICIDIO+CULPOSO+POR+TRÁNSITO+VEHICULAR+(COLISION)%27)+OR+(delito+%3D+%27LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR%27)+OR+(delito+%3D+%27LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR+EN+COLISION%27)&format=json'
    special_bd$active_pgj <- fromJSON(url)$fields
    # =
    special_bd$active_pgj['hora_de_hechos'] <- times(paste0(substr(special_bd$active_pgj$fecha_hechos , 12 , 16), ':00'))
    special_bd$active_pgj['fecha_de_hechos'] <- dates(substr(special_bd$active_pgj$fecha_hechos , 1 , 10) , format = 'y-m-d')
    special_bd$active_pgj['timestamp'] <- chron(special_bd$active_pgj$fecha_de_hechos , special_bd$active_pgj$hora_de_hechos)
    special_bd$active_pgj <- filter(special_bd$active_pgj , timestamp >= dates('2016-01-01' , format = 'y-m-d'))
    # =
    special_bd$active_pgj['geopoint'] <- NULL
    special_bd$active_pgj <- special_bd$active_pgj[order(special_bd$active_pgj$timestamp),]
    special_bd$active_pgj['id'] <- seq.int(nrow(special_bd$active_pgj))
    special_bd$active_pgj <- filter(special_bd$active_pgj , latitud != 'NA' & longitud != 'NA')
    special_bd$active_pgj <- filter(special_bd$active_pgj , !is.na(timestamp))
    # =
    special_bd$active_pgj <- st_transform(st_as_sf(special_bd$active_pgj , coords = c('longitud','latitud') , crs = 4326), 32614)
    # =
    insertUI(selector = '#div_load_pgj' , where = 'afterEnd' ,
             tags$div(tags$p(style = 'font-size: 20px;',
                             strong('Proceso Finalizado')),
                      tags$p('Presione el botón para regresar a la aplicación'),
                      tags$div(style = 'display: block; margin: auto; width: 50%;',
                               fluidRow(column(2 , offset = 4 ,
                                               actionButton(inputId = 'fin_importar_pgj' , label = 'Finalizar'))))))
    removeUI(selector = '.finalizar_pgj' , multiple = TRUE)
    # =
    key_remote$k_pgj <- TRUE
  })
  
  observeEvent(input$fin_importar_pgj , {
    pgj_importada$bd <- special_bd$active_pgj
    removeModal()
  })
  
  
  # ===== BASES DE DATOS REMOTAS - C5 =====
  observeEvent(input$bd_c5 , {
    if (input$bd_c5 == 'B' & key_remote$k_c5 == FALSE) {
      showModal(modalDialog(title = NULL , footer = NULL,
                            tags$div(id = 'div_modalc5_a'),
                            tags$div(id = 'div_modalc5_b',
                                     tags$p(style = 'font-size: 20px;',
                                            strong('Importante')),
                                     tags$p(style = 'text-align: justify',
                                            'Al elegir la Base de Datos Remota, usted permite a la aplicación trabajar con la versión más actualizada disponible en',
                                            tags$a(href = 'https://datos.cdmx.gob.mx/explore/dataset/incidentes-viales-c5/table/' , 'Datos Abiertos de la CDMX'),
                                            ', permitiéndole analizar los sucesos más recientes disponibles en dicha tabla.') ,
                                     tags$p(style = 'text-align: justify',
                                            'Sin embargo, la importación de esta base depende directamente del peso de ésta y de su conexión a internet, razón por la cual puede tardar algunos minutos en finalizarse este proceso.'),
                                     tags$p(style = 'font-size: 25px; text-align: center;',
                                            strong('¿Desea continuar con la importación?')),
                                     fluidRow(column(2, offset = 4,
                                                     actionButton(inputId = 'importar_c5' , label = strong('Continuar'),
                                                                  style = 'background-color: #00A65B; color: white; border-color: ;')),
                                              column(2,
                                                     actionButton(inputId = 'no_importar_c5' , label = strong(' Detener '),
                                                                  style = 'background-color: #DD4C39; color: white; border-color: ;')))
                            )))
    }
    else if (input$bd_c5 == 'B' & key_remote$k_c5 == TRUE) special_bd$active_c5 <- c5_importada$bd
    else if (input$bd_c5 == 'A' & key_remote$k_c5 == TRUE) special_bd$active_c5 <- c5
  })
  
  observeEvent(input$no_importar_c5 , {
    removeModal()
    updateRadioButtons(session , inputId = 'bd_c5' , label = NULL, inline = TRUE, selected = 'A' ,
                       choiceNames = c('Local' , 'Remota'),
                       choiceValues = c('A' , 'B'))
  })
  
  observeEvent(input$importar_c5 , {
    removeUI(selector = '#div_modalc5_b' , immediate = TRUE)
    insertUI(selector = '#div_modalc5_a' , where = 'afterEnd' ,  immediate = TRUE ,
             tags$div(id = '#div_modalc5_b',
                      tags$p(style = 'font-size: 20px;', class = 'finalizar_c5',
                             strong('Importando Base de Datos de C5')),
                      tags$p('Espere mientras termina el proceso', class = 'finalizar_c5'),
                      tags$div(id = 'div_load_c5', class = 'finalizar_c5', style = 'display: block; margin: auto; width: 50%;',
                               tags$img(src = 'loading.gif' , style = 'max-width:100%; max-height:100%; vertical-align: middle;'))))
    # =
    url = 'https://datos.cdmx.gob.mx/api/records/1.0/download/?dataset=incidentes-viales-c5&q=(codigo_cierre+%3D+"(A)+La+unidad+de+atención+a+emergencias+fue+despachada,+llegó+al+lugar+de+los+hechos+y+confirmó+la+emergencia+reportada")+OR+(codigo_cierre+%3D+"(I)+El+incidente+reportado+es+afirmativo+y+se+añade+información+adicional+al+evento")&format=json'
    special_bd$active_c5 <- fromJSON(url)$fields
    # =
    special_bd$active_c5['hora_creacion'] <- times(special_bd$active_c5$hora_creacion)
    special_bd$active_c5['fecha_creacion'] <- dates(as.character(special_bd$active_c5$fecha_creacion), format = 'd/m/y')
    special_bd$active_c5['timestamp'] <- chron(special_bd$active_c5$fecha_creacion , special_bd$active_c5$hora_creacion)
    special_bd$active_c5 <- filter(special_bd$active_c5 , timestamp >= dates('2014-01-01' , format = 'y-m-d'))
    # =
    special_bd$active_c5['geopoint'] <- NULL
    special_bd$active_c5 <- filter(special_bd$active_c5 , !is.na(latitud) & !is.na(longitud))
    special_bd$active_c5 <- filter(special_bd$active_c5 , !is.na(timestamp))
    # =
    special_bd$active_c5 <- st_transform(st_as_sf(special_bd$active_c5 , coords = c('longitud','latitud') , crs = 4326), 32614)
    # =
    insertUI(selector = '#div_load_c5' , where = 'afterEnd' ,
             tags$div(tags$p(style = 'font-size: 20px;',
                             strong('Proceso Finalizado')),
                      tags$p('Presione el botón para regresar a la aplicación'),
                      tags$div(style = 'display: block; margin: auto; width: 50%;',
                               fluidRow(column(2 , offset = 4 ,
                                               actionButton(inputId = 'fin_importar_c5' , label = 'Finalizar'))))))
    removeUI(selector = '.finalizar_c5' , multiple = TRUE)
    # =
    key_remote$k_c5 <- TRUE
  })
  
  observeEvent(input$fin_importar_c5 , {
    c5_importada$bd <- special_bd$active_c5
    removeModal()
  })
  
  
  # ===== DESCRIPTORES DE VARIABLES =====
  # = Campos PGJ
  output$texto_campos_pgj <- renderText({
    if (input$campos_pgj == 'Delito') {
      'Para incidentes viales, las posibilidades son: "Daño en Propiedad Ajena Culposa por Tránsito Vehicular", "Lesiones Culposas por Tránsito Vehicular" u "Homicidio Culposo por Tránsito Vehicular"'
    } else if (input$campos_pgj == 'Fiscalía') {
      'Establece si la Carpeta de Investigación fue abierta dentro de una Alcaldía (e.g. Benito Juárez) o por alguna entidad especializada (e.g. Agencia Central de Investigación).'
    } else if (input$campos_pgj == 'Agencia') {
      'Código de la Agencia específica en la cual fue abierta la Carpeta de Investigación (e.g. "BJ-3" para la Coordinación Territorial 3 de la Alcaldía Benito Juárez)'
    } else if (input$campos_pgj == 'U. Investigación') {
      'Ayuda a determinar si existieron o no detenidos dentro de la Carpeta de Investigación.'
    } else if (input$campos_pgj == 'Fecha de Hechos') {
      'Deterimina el día, mes, año y hora en la cual se llevó a cabo el delito reportado, de acuerdo con la información recabada por la Procuraduría'
    } else if (input$campos_pgj == 'Fecha de Inicio') {
      'Establece el día, mes, año y hora en la cual se inició la Carpeta de Investigación, esto en, en que se recibió la denuncia. No necesariamente corresponde con la fecha en la que ocurrió el delito.'
    }
  })
  
  # = Campos SSC
  output$texto_campos_ssc <- renderText({
    if (input$campos_ssc == 'Tipo de Evento') {
      'Establece puntualmente el tipo de Incidente Vial ocurrido. Las variantes son: "Atropellado", "Caída de Ciclista", "Caída de Pasajero", "Choque", "Derrapado" y "Volcadura"'
    } else if (input$campos_ssc == 'Intersección') {
      'Describe el tipo de cruce vial en el cual se llevó a cabo el incidente, pudiendo ser: "Cruz", "Curva", "Desnivel", "Gaza", "Glorieta", "Ramas Múltiples", "Recta", "T", y "Y"'
    } else if (input$campos_ssc == 'Cuadrante') {
      'Contiene el código del cuadrante dentro del cual ocurrió el incidente, de acuerdo a la "Estrategia de Proximidad de Cuadrantes" de la SSC'
    } else if (input$campos_ssc == 'Vehículo') {
      'Detalla el o los tipos de vehículos involucrados en el incidente vial. En caso de ser más de uno, cada afectado es asignado a una de cuatro columnas posibles dentro de la base.'
    } else if (input$campos_ssc == 'Ruta T. Público') {
      'En caso de que uno de los involucrados fuese un vehículo de Transporte Público, ya sea concesionado o no, la ruta a la que pertenece se detalla en esta columna.'
    } else if (input$campos_ssc == 'Condición') {
      'Establece si el afectado principal del incidente falleció durante el mismo ("Occiso") se encuentra lesionado ("Lesionado"). Únicamente a uno de los afectados, en caso de que existan más de uno.'
    } else if (input$campos_ssc == 'Total Occisos') {
      'Determina el número de individuos fallecidos durante el incidente vial, por lo que permite determinar si existió más de un afectado. También existe la columna "Total Lesionados" que cumple la misma función.'
    } else if (input$campos_ssc == 'Identidad') {
      'Aporta más detalles sobre el individuo detallado en la columna de "Condición", permitiendo conocer si pertenece a una de estas categorías: "Ciclista", "Conductor" , "Motociclista", "Pasajero" y "Peatón"'
    } else if (input$campos_ssc == 'U. M. de Apoyo') {
      'Si una Unidad Médica de apoyo acudió al lugar del incidente para atención de los involucrados, aquí se menciona la entidad pública o privada a la cual pertenece.'
    } else if (input$campos_ssc == 'Observaciones') {
      'Cualquier tipo de anotación adicional detallada por el personal policiaco presente en el incidente, y que no puede ser mencionado a detalle en alguna de las otras columnas.'
    }
  })
  
  # = Campos C5
  output$texto_campos_c5 <- renderText({
    if (input$campos_c5 == 'Folio') {
      'Folio de Identificación único asignado a cada uno de los registros de la base, sin importar su código de cierre ni el canal a través del cual fue reportado al C5.'
    } else if (input$campos_c5 == 'Fecha Creación') {
      'Momento del tiempo en el cual el incidente fue reportado al C5 por cualquiera de sus canales y, por ende, se le fue asignado un número de folio.'
    } else if (input$campos_c5 == 'Fecha de Cierre') {
      'Instante en el cual el personal del C5 dio cierre a un número de folio, considerándose el evento como atendido. No necesariamente corresponde con la Fecha de Creación.'
    } else if (input$campos_c5 == 'Incidente C4') {
      'Clasifiación interna del C5 para el incidente atendido. Puede pertenecer a las clases "Accidente", "Cadáver", "Detención Ciudadana" o "Lesionado", complementándose ésta con detalles del suceso.'
    } else if (input$campos_c5 == 'Código de Cierre') {
      'Describen las condiciones bajo las cuales un Número de Folio fue atendido. Para que un incidente se considere "Real", éste debe de ser "(A) Afirmativo" o "(I) Informativo", de acuerdo al propio centro.'
    } else if (input$campos_c5 == 'C. con F. Alarma') {
      'Asigna una clase que permite discernir si se trata de una Falsa Alarma. Puede tener los valores: "Delito", "Emergencia", "Falsa Alarma" o "Urgencias Médicas"'
    } else if (input$campos_c5 == 'Tipo de Entrada') {
      'Establece el canal a través del cual el C5 fue enterado del incidente. Las opciones son "Botón de Auxilio en Cámara", "Llamada en App del 911", "Llamada del 066", "Llamada del 911", "Radio", "Redes" y "Zello"'
    }
  })
  
  # = Campos AXA
  output$texto_campos_axa <- renderText({
    if (input$campos_axa == 'Siniestro') {
      'Identificador único asignado por AXA Seguros para cada uno de los percances viales registrados.'
    } else if (input$campos_axa == 'Causa Siniestro') {
      'Detalla lo sucedido durante el percance vial. Sus opciones son: "Atropello" , "Colisión y/o Vuelco", "Daños por la Carga", "Fenómenos de la Naturaleza", "Huelgas y Alborotos", "Incendio, Rayo o Explosión" y "Transportación".'
    } else if (input$campos_axa == 'Vehículo') {
      'Especifica el tipo de vehículo involucrado en el siniestro. Puede ser "Auto", "Camión", "Camión Ligero" o "Motocicleta".'
    } else if (input$campos_axa == 'Nivel de Daño') {
      'Indica cuál fue la gravedad de los daños sufridos por el vehículo especificado en la columna "Tipo de Vehículo". Los niveles son "Alto", "Medio", "Bajo" y "Sin Daño".'
    } else if (input$campos_axa == 'Punto Impacto') {
      'Describe la zona del vehículo en la cual se recibió el impacto principal durante el percance vial.'
    } else if (input$campos_axa == 'Año de Reporte') {
      'Año en el cual se registró el percance vial descrito. No necesariamente corresponde al momento en cual ocurrió éste; sin embargo, es el único campo de la base que especifica un momento del tiempo.'
    } else if (input$campos_axa == 'T. Lesionados') {
      'Especifica el número de individuos que resultaron lesionados por el percance vial, ya sea dentro o fuera del vehículo.'
    } else if (input$campos_axa == 'Rol Lesionado') {
      'Especifica la naturaleza del lesionado durante el percance vial ("Conductor", "Pasajero", "Peatón" o "Viajero"), así como si se encontraba asegurado por parte de AXA.'
    } else if (input$campos_axa == 'Nivel de Lesión') {
      'De forma similar a la columna "Nivel de Daño", coloca en una escala de "Alto", "Medio" y "Bajo" el nivel de la lesión sufrida por el individuo descrito en "Rol Lesionado."'
    } else if (input$campos_axa == 'Fallecido') {
      'Indica si el lesionado descrito en la columna "Rol Lesionado" falleció durante el percance vial.'
    } else if (input$campos_axa == 'Hospitalizado') {
      'Indica si el lesionado descrito en la columna "Rol Lesionado" fue trasladado a alguna Unidad Médica.'
    } else if (input$campos_axa == 'Var. Binarias') {
      'Describen si un factor participó en el percance. Éstas son "Ambulancia", "Árbol", "Piedra", "Dormido", "Grúa", "Daño Obra Civil", "Pavimento Mojado", "Explosión Llanta", "Volcadura", "Pérdida Total", entre otras.'
    }
  })
  
  # = Campos Repubikla
  output$texto_campos_repubikla <- renderText({
    if (input$campos_repubikla == 'Comentario') {
      'Contiene de forma completa el Titular de la Noticia, Tweet o cualquier otro tipo de descriptor encontrado en la fuente de donde fue obtenido el registro.'
    } else if (input$campos_repubikla == 'Modo') {
      'Describe la naturaleza del individuo y/o vehículo involucrado en el incidente vial. Puede ser "Ciclista", "Motociclista", "Mototaxi", "Peatón" o "Triciclo"'
    } else if (input$campos_repubikla == 'Fuente') {
      'Se trata de una liga directa a la fuente a través de la cual el incidente vial pudo ser captado por Repubikla, ya sea un Tweet, una noticia de periódico, una publicación en un blog, etc.'
    } else if (input$campos_repubikla == 'Responsable') {
      'Determina qué vehículo fue el probable responsable del incidente vial registrado. Comúnmente contiene vehículos no motorizados, tales como "Autobús", "Camión", "Metrobús", "Taxi", "Tráiler", "Trolebús", entre otros.'
    } else if (input$campos_repubikla == 'Gravedad') {
      'Aporta más detalles sobre las lesiones presentadas por el individio involucrado, así como si fue requerida una Unidad Médica o, inclusive, si derivó en una muerte, caso en el que la columna contiene "Mortal"'
    } else if (input$campos_repubikla == 'Seguimiento') {
      'Aporta más detalles sobre lo sucedido después del incidente. Por ejemplo, menciona si el responsable fue detenido por las autoridades, huyó de la escena, existió algún traslado a hospital, acudieron bomberos, entre otros.'
    }
  })
  
  
  # ===== CUADROS DE INFORMACIÓN EXTRA =====
  output$extra1 <- renderValueBox({
    if (length(input$menu_bd) == 0) valor = '-'
    else if (input$menu_bd == 'PGJ') valor = format(nrow(special_bd$active_pgj) , big.mark = ',')
    else if (input$menu_bd == 'SSC') valor = format(nrow(ssc) , big.mark = ',')
    else if (input$menu_bd == 'C5') valor = format(nrow(special_bd$active_c5) , big.mark = ',')
    else if (input$menu_bd == 'AXA') valor = format(nrow(axa) , big.mark = ',')
    else if (input$menu_bd == 'Repubikla') valor = format(nrow(repubikla) , big.mark = ',')
    # =
    valueBox(value = valor , subtitle = 'Incidentes Viales Registrados' , icon = icon('layer-group'), color = 'green')
  })
  
  output$extra2 <- renderValueBox({
    if (length(input$menu_bd) == 0) valor = '-'
    else if (input$menu_bd == 'PGJ') {
      tmp <- filter(special_bd$active_pgj , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'SSC') {
      tmp <- filter(ssc , total_occisos > 0)
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'C5') {
      tmp <- filter(special_bd$active_c5 , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'AXA') {
      tmp <- filter(axa , fallecido == 'SI')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'Repubikla') {
      tmp <- filter(repubikla , gravedad == 'mortal')
      valor = format(nrow(tmp) , big.mark = ',')
    }
    # =
    valueBox(value = valor , subtitle = 'Decesos' , icon = icon('skull'), color = 'red')
  })
  
  output$extra3 <- renderValueBox({
    if (length(input$menu_bd) == 0) valor = '-'
    else if (input$menu_bd == 'PGJ') {
      tmp <- filter(special_bd$active_pgj , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'SSC') {
      tmp <- filter(ssc , total_lesionados > 0 & total_occisos == 0)
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'C5') {
      tmp <- filter(special_bd$active_c5 , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'AXA') {
      tmp <- filter(axa , lesionados > 0 & fallecido != 'SI')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'Repubikla') {
      tmp <- filter(repubikla , gravedad == 'ambulancia requerida' | gravedad == 'costillas fracturadas' | gravedad == 'grave' | gravedad == 'leve' | gravedad == 'moderada')
      valor = format(nrow(tmp) , big.mark = ',')
    }
    # =
    valueBox(value = valor , subtitle = 'Lesionados' , icon = icon('medkit'), color = 'orange')
  })
  
  output$extra4 <- renderValueBox({
    if (length(input$menu_bd) == 0) valor = '-'
    else if (input$menu_bd == 'PGJ') {
      tmp <- filter(special_bd$active_pgj , delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL' | delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'SSC') {
      tmp <- filter(ssc , total_lesionados == 0 & total_occisos == 0)
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'C5') {
      tmp <- filter(special_bd$active_c5 , incidente_c4 == 'accidente-choque sin lesionados' | incidente_c4 == 'detención ciudadana-accidente automovilístico' | incidente_c4 == 'accidente-ciclista' | incidente_c4 == 'accidente-ferroviario' | incidente_c4 == 'accidente-monopatín' | incidente_c4 == 'accidente-motociclista' | incidente_c4 == 'accidente-otros')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'AXA') {
      tmp <- filter(axa , lesionados == 0 & fallecido != 'SI')
      valor = format(nrow(tmp) , big.mark = ',')
    } else if (input$menu_bd == 'Repubikla') {
      tmp <- filter(repubikla , gravedad != 'ambulancia requerida' & gravedad != 'costillas fracturadas' & gravedad != 'grave' & gravedad != 'leve' & gravedad != 'moderada' & gravedad != 'mortal')
      valor = format(nrow(tmp) , big.mark = ',')
    }
    # =
    valueBox(value = valor , subtitle = 'Accidentes' , icon = icon('car-crash'), color = 'yellow')
  })
  
  output$texto_rndm <- renderText({
    if (length(input$menu_bd) == 0) '-'
    else if (input$menu_bd == 'PGJ') '(PGJ)'
    else if (input$menu_bd == 'SSC') '(SSC)'
    else if (input$menu_bd == 'C5') '(C5)'
    else if (input$menu_bd == 'AXA') '(AXA)'
    else if (input$menu_bd == 'Repubikla') '(Repubikla)'
  })
  
  # ===== INSTRUCCIONES VISUALIZADOR =====
  observeEvent(input$boton_ver_bd2 , updateTabItems(session , inputId = 'menu_1' , selected = 'bd'))
  
  observeEvent(input$boton_ver_instrucciones , {
    if (input$boton_ver_instrucciones %% 2 == 0) {
      hideElement(id = 'berenjena')
      updateActionButton(session , inputId = 'boton_ver_instrucciones' , label = 'Ver Instrucciones' , icon = icon('question-circle'))
    }
    else {
      showElement(id = 'berenjena')
      updateActionButton(session , inputId = 'boton_ver_instrucciones' , label = 'Ocultar Instrucciones' , icon = icon('times-circle'))
    }
  })
  
  observeEvent(input$l_instrucciones , {
    if (input$no_instrucciones == 1) updateNumericInput(session , inputId = 'no_instrucciones' , value = 8)
    else updateNumericInput(session , inputId = 'no_instrucciones' , value = input$no_instrucciones - 1)
  })
  
  observeEvent(input$r_instrucciones , {
    if (input$no_instrucciones == 8) updateNumericInput(session , inputId = 'no_instrucciones' , value = 1)
    else updateNumericInput(session , inputId = 'no_instrucciones' , value = input$no_instrucciones + 1)
  })
  
  output$pagina_instrucciones <- renderText(input$no_instrucciones)
  
  output$titulo_instrucciones <- renderText({
    if (input$no_instrucciones == 1) 'Descripción del Visualizador'
    else if (input$no_instrucciones == 2) 'Selección de Bases de Datos'
    else if (input$no_instrucciones == 3) 'Filtros de Incidentes Viales'
    else if (input$no_instrucciones == 4) 'Mapa Interactivo'
    else if (input$no_instrucciones == 5) 'Gráficas de Incidentes Viales'
    else if (input$no_instrucciones == 6) 'Gráficas por Totales'
    else if (input$no_instrucciones == 7) 'Gráficas por Día y Hora'
    else if (input$no_instrucciones == 8) 'Video Explicativo del Visualizador'
  })
  
  # ===== ACOMODO DE FECHAS LÍMITE =====
  observeEvent(filtro_bd_sp() , ignoreNULL = FALSE, {
    rangos_min <- NULL
    rangos_max <- NULL
    # =
    if ('PGJ' %in% filtro_bd_sp()) {
      rangos_min <- append(rangos_min , range(special_bd$active_pgj$timestamp)[1])
      rangos_max <- append(rangos_max , range(special_bd$active_pgj$timestamp)[2])}
    if ('SSC' %in% filtro_bd_sp()) {
      rangos_min <- append(rangos_min , range(ssc$timestamp)[1])
      rangos_max <- append(rangos_max , range(ssc$timestamp)[2])}
    if ('C5' %in% filtro_bd_sp()) {
      rangos_min <- append(rangos_min , range(special_bd$active_c5$timestamp)[1])
      rangos_max <- append(rangos_max , range(special_bd$active_c5$timestamp)[2])}
    if ('AXA' %in% filtro_bd_sp()) {
      rangos_min <- append(rangos_min , range(axa$timestamp)[1])
      rangos_max <- append(rangos_max , range(axa$timestamp)[2])}
    if ('Repubikla' %in% filtro_bd_sp()) {
      rangos_min <- append(rangos_min , range(repubikla$timestamp)[1])
      rangos_max <- append(rangos_max , range(repubikla$timestamp)[2])}
    # =
    if (!is.null(rangos_min) & !is.null(rangos_max)) {
      updateSliderInput(session , inputId = 'filtro_fecha' , label = 'Periodo de Tiempo', timeFormat = '%d/%m/%Y',
                        value = c(as.Date('2018-01-01',"%Y-%m-%d") , as.Date('2018-12-31',"%Y-%m-%d")),
                        min = as.Date(format(max(rangos_min), '%Y-%m-%d') , '%Y-%m-%d'),
                        max = as.Date(format(min(rangos_max), '%Y-%m-%d') , '%Y-%m-%d'))
    }
  })
  
  # ===== FILTRO BASE DE DATOS =====
  observeEvent(c(filtro_bd_sp() , input$filtro_incidente , filtro_fecha_sp() , input$filtro_lugar), ignoreNULL = FALSE, {
    if ('PGJ' %in% filtro_bd_sp()) {
      # mapa_proxy %>%
      #   clearGroup(group = 'pgj')
      # =
      bd$tmp_pgj <- special_bd$active_pgj
      # =
      if (input$filtro_incidente == 'Decesos') {
        bd$tmp_pgj <- filter(bd$tmp_pgj , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
      } else if (input$filtro_incidente == 'Lesionados') {
        bd$tmp_pgj <- filter(bd$tmp_pgj , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
      } else if (input$filtro_incidente == 'Accidentes') {
        bd$tmp_pgj <- filter(bd$tmp_pgj , delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL' | delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES')
      }
      # =
      bd$tmp_pgj <- filter(bd$tmp_pgj , timestamp >= dates(as.character(filtro_fecha_sp()[1]) , format = 'y-m-d') & timestamp <= (dates(as.character(filtro_fecha_sp()[2]) , format = 'y-m-d') + 1))
      # =
      if (input$filtro_lugar != 'Total Ciudad de México') {
        tmp_contains <- st_contains(st_transform(filter(cdmx , nom_mun == input$filtro_lugar), 32614) , bd$tmp_pgj)
        bd$tmp_pgj <- bd$tmp_pgj[tmp_contains[[1]],]
      }
      # =
      
    }
    else {
      bd$tmp_pgj <- NULL
      # mapa_proxy %>%
      #   clearGroup(group = 'pgj')
    } 
    # =
    if ('SSC' %in% filtro_bd_sp()) {
      # mapa_proxy %>%
      #   clearGroup(group = 'ssc')
      # =
      bd$tmp_ssc <- ssc
      # =
      if (input$filtro_incidente == 'Decesos') {
        bd$tmp_ssc <- filter(bd$tmp_ssc , total_occisos > 0)
      } else if (input$filtro_incidente == 'Lesionados') {
        bd$tmp_ssc <- filter(bd$tmp_ssc , total_lesionados > 0 & total_occisos == 0)
      } else if (input$filtro_incidente == 'Accidentes') {
        bd$tmp_ssc <- filter(bd$tmp_ssc , total_lesionados == 0 & total_occisos == 0)
      }
      # =
      bd$tmp_ssc <- filter(bd$tmp_ssc , timestamp >= dates(as.character(filtro_fecha_sp()[1]) , format = 'y-m-d') & timestamp <= (dates(as.character(filtro_fecha_sp()[2]) , format = 'y-m-d') + 1))
      # =
      if (input$filtro_lugar != 'Total Ciudad de México') {
        tmp_contains <- st_contains(st_transform(filter(cdmx , nom_mun == input$filtro_lugar), 32614) , bd$tmp_ssc)
        bd$tmp_ssc <- bd$tmp_ssc[tmp_contains[[1]],]
      }
      # =
      # mapa_proxy %>%
      #   addCircleMarkers(data = st_transform(bd$tmp_ssc , 4326) , group = 'ssc',
      #                    fillColor = '#0A5E9A' , color = '#043A5F',
      #                    clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
    }
    else {
      bd$tmp_ssc <- NULL
      # mapa_proxy %>%
      #   clearGroup(group = 'ssc')
    }
    # =
    if ('C5' %in% filtro_bd_sp()) {
      # mapa_proxy %>%
      #   clearGroup(group = 'c5')
      # =
      bd$tmp_c5 <- special_bd$active_c5
      # =
      if (input$filtro_incidente == 'Decesos') {
        bd$tmp_c5 <- filter(bd$tmp_c5 , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
      } else if (input$filtro_incidente == 'Lesionados') {
        bd$tmp_c5 <- filter(bd$tmp_c5 , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
      } else if (input$filtro_incidente == 'Accidentes') {
        bd$tmp_c5 <- filter(bd$tmp_c5 , incidente_c4 == 'accidente-choque sin lesionados' | incidente_c4 == 'detención ciudadana-accidente automovilístico' | incidente_c4 == 'accidente-ciclista' | incidente_c4 == 'accidente-ferroviario' | incidente_c4 == 'accidente-monopatín' | incidente_c4 == 'accidente-motociclista' | incidente_c4 == 'accidente-otros')
      }
      # =
      bd$tmp_c5 <- filter(bd$tmp_c5 , timestamp >= dates(as.character(filtro_fecha_sp()[1]) , format = 'y-m-d') & timestamp <= (dates(as.character(filtro_fecha_sp()[2]) , format = 'y-m-d') + 1))
      # =
      if (input$filtro_lugar != 'Total Ciudad de México') {
        tmp_contains <- st_contains(st_transform(filter(cdmx , nom_mun == input$filtro_lugar), 32614) , bd$tmp_c5)
        bd$tmp_c5 <- bd$tmp_c5[tmp_contains[[1]],]
      }
      # =
      # mapa_proxy %>%
      #   addCircleMarkers(data = st_transform(bd$tmp_c5 , 4326) , group = 'c5',
      #                    fillColor = '#F0B300' , color = '#956F00')
    }
    else {
      bd$tmp_c5 <- NULL
      # mapa_proxy %>%
      #   clearGroup(group = 'c5')
    }
    # =
    if ('AXA' %in% filtro_bd_sp()) {
      # mapa_proxy %>%
      #   clearGroup(group = 'axa')
      # =
      bd$tmp_axa <- axa
      # =
      if (input$filtro_incidente == 'Decesos') {
        bd$tmp_axa <- filter(bd$tmp_axa , fallecido == 'SI')
      } else if (input$filtro_incidente == 'Lesionados') {
        bd$tmp_axa <- filter(bd$tmp_axa , lesionados > 0 & fallecido != 'SI')
      } else if (input$filtro_incidente == 'Accidentes') {
        bd$tmp_axa <- filter(bd$tmp_axa , lesionados == 0 & fallecido != 'SI')
      }
      # =
      bd$tmp_axa <- filter(bd$tmp_axa , timestamp >= dates(as.character(filtro_fecha_sp()[1]) , format = 'y-m-d') & timestamp <= (dates(as.character(filtro_fecha_sp()[2]) , format = 'y-m-d') + 1))
      # =
      if (input$filtro_lugar != 'Total Ciudad de México') {
        tmp_contains <- st_contains(st_transform(filter(cdmx , nom_mun == input$filtro_lugar), 32614) , bd$tmp_axa)
        bd$tmp_axa <- bd$tmp_axa[tmp_contains[[1]],]
      }
      # =
      bd$tmp_axa$relacion_lesionados[bd$tmp_axa$relacion_lesionados == '\\N' | bd$tmp_axa$relacion_lesionados == ''] <- 'Sin Datos'
      bd$tmp_axa$tipo_vehiculo[bd$tmp_axa$tipo_vehiculo == '\\N' | bd$tmp_axa$tipo_vehiculo == ''] <- 'Sin Datos'
      bd$tmp_axa$causa_siniestro[bd$tmp_axa$causa_siniestro == '\\N' | bd$tmp_axa$causa_siniestro == ''] <- 'Sin Datos'
      # =
      # mapa_proxy %>%
      #   addCircleMarkers(data = st_transform(bd$tmp_axa , 4326) , group = 'axa',
      #                    fillColor = '#97019C' , color = '#5E0061')
    }
    else {
      bd$tmp_axa <- NULL
      # mapa_proxy %>%
      #   clearGroup(group = 'axa')
    }
    # =
    if ('Repubikla' %in% filtro_bd_sp()) {
      # mapa_proxy %>%
      #   clearGroup(group = 'repubikla')
      # =
      bd$tmp_repubikla <- repubikla
      # =
      if (input$filtro_incidente == 'Decesos') {
        bd$tmp_repubikla <- filter(bd$tmp_repubikla , gravedad == 'mortal')
      } else if (input$filtro_incidente == 'Lesionados') {
        bd$tmp_repubikla <- filter(bd$tmp_repubikla , gravedad == 'ambulancia requerida' | gravedad == 'costillas fracturadas' | gravedad == 'grave' | gravedad == 'leve' | gravedad == 'moderada')
      } else if (input$filtro_incidente == 'Accidentes') {
        bd$tmp_repubikla <- filter(bd$tmp_repubikla , gravedad != 'ambulancia requerida' & gravedad != 'costillas fracturadas' & gravedad != 'grave' & gravedad != 'leve' & gravedad != 'moderada' & gravedad != 'mortal')
      }
      # =
      bd$tmp_repubikla <- filter(bd$tmp_repubikla , timestamp >= dates(as.character(filtro_fecha_sp()[1]) , format = 'y-m-d') & timestamp <= (dates(as.character(filtro_fecha_sp()[2]) , format = 'y-m-d') + 1))
      # =
      if (input$filtro_lugar != 'Total Ciudad de México') {
        tmp_contains <- st_contains(st_transform(filter(cdmx , nom_mun == input$filtro_lugar), 32614) , bd$tmp_repubikla)
        bd$tmp_repubikla <- bd$tmp_repubikla[tmp_contains[[1]],]
      }
      # =
      # mapa_proxy %>%
      #   addCircleMarkers(data = st_transform(bd$tmp_repubikla , 4326) , group = 'repubikla',
      #                    fillColor = '#65D700' , color = '#3F8500')
    }
    else {
      bd$tmp_repubikla <- NULL
      # mapa_proxy %>%
      #   clearGroup(group = 'repubikla')
    }
  })
  
  # ===== EVENTOS DEL MAPA =====
  output$mapa <- renderLeaflet({
    if (input$filtro_lugar == 'Total Ciudad de México') {
      lugar <- cdmx_sa
      lon <- -99.152613
      lat <- 19.320497
      zoom <- 11
    }
    else {
      lugar <- filter(cdmx , nom_mun == input$filtro_lugar)
      bb <- st_bbox(lugar)
      lon <- unname(bb$xmin + bb$xmax) / 2
      lat <- unname(bb$ymin + bb$ymax) / 2
      bb <- st_bbox(st_transform(lugar, 32614))
      diagonal <- unname(sqrt((((bb$xmax)-(bb$xmin))^2)+(((bb$ymax)-(bb$ymin))^2)))
      zoom <- log2((40075000)*((cos(lat))/(diagonal))) + 2
    }
    # =
    paleta_nombres <- NULL
    paleta_colores <- NULL
    l <- leaflet(data = lugar) %>%
      addTiles(urlTemplate = '//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      setView(lng = lon , lat = lat, zoom = zoom) %>%
      addPolygons(fillColor = '#00A65A' , fillOpacity = 0.10 , color = '#006738' , opacity = 0.75)
    if (!is.null(bd$tmp_pgj)) {
      l <- l %>%
        addMarkers(data = st_transform(bd$tmp_pgj, 4326) , group = 'pgj' ,
                   icon = list(iconUrl = ifelse(bd$tmp_pgj$delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | bd$tmp_pgj$delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | bd$tmp_pgj$delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | bd$tmp_pgj$delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR',
                                                'pgj_a.png',
                                                ifelse(bd$tmp_pgj$delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | bd$tmp_pgj$delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION',
                                                       'pgj_b.png' , 'pgj_c.png')),
                               iconSize = c(35,35)),
                   options = markerOptions(opacity = 1),
                   popup = paste0('<b>Incidente PGJ</b><br/>',
                                  '<b>Delito</b>: ' , str_to_title(bd$tmp_pgj$delito, locale = 'es') , '<br/>',
                                  '<b>Fecha y Hora de Hechos</b>: ' , format(bd$tmp_pgj$timestamp , format = '%d/%m/%Y, %T'), '<br/>',
                                  '<b>Fiscalía</b>: ' , str_to_title(bd$tmp_pgj$fiscalia, locale = 'es'), '<br/>' ,
                                  '<b>Agencia</b>: ' , bd$tmp_pgj$agencia , '<br/>',
                                  '<b>Unidad de Investigación</b>: ' , bd$tmp_pgj$unidad_investigacion),
                   popupOptions = popupOptions(maxWidth = 450),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE , iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(149,40,0,0.75); color: #461300;\"><span><strong>' + cluster.getChildCount() + '</strong></div><span>',
                                               className: 'marker-cluster'
                                             });
                                          }")))
      paleta_nombres <- append(paleta_nombres , 'PGJ')
      paleta_colores <- append(paleta_colores , '#952800')}
    if (!is.null(bd$tmp_ssc)) {
      l <- l %>%
        addMarkers(data = st_transform(bd$tmp_ssc, 4326) , group = 'ssc' ,
                   icon = list(iconUrl = ifelse(bd$tmp_ssc$total_occisos > 0,
                                                'ssc_a.png',
                                                ifelse(bd$tmp_ssc$total_lesionados > 0 & bd$tmp_ssc$total_occisos == 0,
                                                       'ssc_b.png' , 'ssc_c.png')),
                               iconSize = c(35,35)),
                   options = markerOptions(opacity = 0.9),
                   popup = paste0('<b>Incidente SSC</b><br/>',
                                  '<b>Tipo de Evento</b>: ' , str_to_title(bd$tmp_ssc$tipo_evento, locale = 'es') , '<br/>',
                                  '<b>Fecha y Hora de Hechos</b>: ' , format(bd$tmp_ssc$timestamp , format = '%d/%m/%Y, %T'), '<br/>',
                                  '<b>Tipo de Intersección</b>: ' , str_to_title(bd$tmp_ssc$tipo_interseccion, locale = 'es'), '<br/>' ,
                                  '<b>Cuadrante SSC</b>: ' , ifelse(bd$tmp_ssc$cuadrante == 'SD' , 'Sin Datos' , bd$tmp_ssc$cuadrante) , '<br/>',
                                  '<b>Vehículos Involucrados</b>: ' , str_to_title(str_replace(ifelse(!is.na(bd$tmp_ssc$tipo_vehiculo_4) , paste(sep = ', ' , bd$tmp_ssc$tipo_vehiculo_1 , bd$tmp_ssc$tipo_vehiculo_2 , bd$tmp_ssc$tipo_vehiculo_3 , bd$tmp_ssc$tipo_vehiculo_4),
                                                                             ifelse(!is.na(bd$tmp_ssc$tipo_vehiculo_3) , paste(sep = ', ' , bd$tmp_ssc$tipo_vehiculo_1 , bd$tmp_ssc$tipo_vehiculo_2 , bd$tmp_ssc$tipo_vehiculo_3),
                                                                                    ifelse(!is.na(bd$tmp_ssc$tipo_vehiculo_2) , paste(sep = ', ' , bd$tmp_ssc$tipo_vehiculo_1 , bd$tmp_ssc$tipo_vehiculo_2) ,
                                                                                           bd$tmp_ssc$tipo_vehiculo_1))) , 'SD' , 'Sin Datos') , locale = 'es') , '<br/>' ,
                                  '<b>Ruta de Transporte Público</b>: ' , ifelse(bd$tmp_ssc$ruta_transporte_publico == 'SD' , 'Sin Datos', bd$tmp_ssc$ruta_transporte_publico) , '<br/>' ,
                                  '<b>Condición de Víctima Principal</b>: ' , str_to_title(bd$tmp_ssc$condicion , locale = 'es') , '<br/>' ,
                                  '<b>Identidad de Víctima Principal</b>: ' , str_to_title(bd$tmp_ssc$identidad, locale = 'es') , '<br/>',
                                  '<b>Total de Occisos</b>: ' , bd$tmp_ssc$total_occisos , '<br/>' ,
                                  '<b>Total de Lesionados</b>: ' , bd$tmp_ssc$total_lesionados , '<br/>',
                                  '<b>Unidad Médica de Apoyo</b>: ' , bd$tmp_ssc$unidad_medica_de_apoyo , '<br/>',
                                  '<b>Observaciones</b>: ' , ifelse(bd$tmp_ssc$observaciones != 'NA' , str_to_title(bd$tmp_ssc$observaciones , locale = 'es') , bd$tmp_ssc$observaciones)
                                  ),
                   popupOptions = popupOptions(maxWidth = 450),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE , iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(4,58,95,0.75); color: #00192A;\"><span><strong>' + cluster.getChildCount() + '</strong></div><span>',
                                               className: 'marker-cluster'
                                             });
                                          }")))
      paleta_nombres <- append(paleta_nombres , 'SSC')
      paleta_colores <- append(paleta_colores , '#043A5F')}
    if (!is.null(bd$tmp_c5)) {
      l <- l %>%
        addMarkers(data = st_transform(bd$tmp_c5, 4326) , group = 'c5' ,
                   icon = list(iconUrl = ifelse(bd$tmp_c5$incidente_c4 == 'cadáver-accidente automovilístico' | bd$tmp_c5$incidente_c4 == 'cadáver-atropellado',
                                                'c5_a.png',
                                                ifelse(bd$tmp_c5$incidente_c4 == 'accidente-choque con lesionados' | bd$tmp_c5$incidente_c4 == 'accidente-choque con prensados' | bd$tmp_c5$incidente_c4 == 'accidente-persona atrapada / desbarrancada' | bd$tmp_c5$incidente_c4 == 'accidente-vehiculo atrapado' | bd$tmp_c5$incidente_c4 == 'accidente-vehículo atrapado-varado' | bd$tmp_c5$incidente_c4 == 'accidente-vehiculo desbarrancado' | bd$tmp_c5$incidente_c4 == 'accidente-volcadura' | bd$tmp_c5$incidente_c4 == 'detención ciudadana-atropellado' | bd$tmp_c5$incidente_c4 == 'lesionado-accidente automovilístico' | bd$tmp_c5$incidente_c4 == 'lesionado-atropellado',
                                                       'c5_b.png' , 'c5_c.png')),
                               iconSize = c(35,35)),
                   options = markerOptions(opacity = 0.9),
                   popup = paste0('<b>Incidente C5</b><br/>',
                                  '<b>Incidente C4</b>: ' , str_replace(str_to_title(bd$tmp_c5$incidente_c4, locale = 'es') , '-' , ' / ') , '<br/>',
                                  '<b>Fecha y Hora de Hechos</b>: ' , format(bd$tmp_c5$timestamp , format = '%d/%m/%Y, %T'), '<br/>',
                                  '<b>No. de Folio</b>: ' , bd$tmp_c5$folio, '<br/>',
                                  '<b>Clasificación del Incidente</b>: ' , str_to_title(bd$tmp_c5$clas_con_f_alarma, locale = 'es') , '<br/>',
                                  '<b>Tipo de Entrada</b>: ' , str_to_title(bd$tmp_c5$tipo_entrada, locale = 'es')
                                  ),
                   popupOptions = popupOptions(maxWidth = 450),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE , iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(148,110,0,0.75); color: #423100;\"><span><strong>' + cluster.getChildCount() + '</strong></div><span>',
                                               className: 'marker-cluster'
                                             });
                                          }")))
      paleta_nombres <- append(paleta_nombres , 'C5')
      paleta_colores <- append(paleta_colores , '#956F00')}
    if (!is.null(bd$tmp_axa)) {
      l <- l %>%
        addMarkers(data = st_transform(bd$tmp_axa, 4326) , group = 'axa' ,
                   icon = list(iconUrl = ifelse(bd$tmp_axa$fallecido == 'SI',
                                                'axa_a.png',
                                                ifelse(bd$tmp_axa$lesionados > 0 & bd$tmp_axa$fallecido != 'SI',
                                                       'axa_b.png' , 'axa_c.png')),
                               iconSize = c(35,35)),
                   options = markerOptions(opacity = 0.9),
                   popup = paste0('<b>Incidente AXA</b><br/>',
                                  '<b>Causa del Siniestro</b>: ' , str_to_title(bd$tmp_axa$causa_siniestro, locale = 'es') , '<br/>',
                                  '<b>Fecha y Hora de Hechos</b>: ' , format(bd$tmp_axa$timestamp , format = '%d/%m/%Y, %T') , '<br/>',
                                  '<b>Vehículo</b>: ' , ifelse(bd$tmp_axa$tipo_vehiculo != 'Auto' & bd$tmp_axa$tipo_vehiculo != 'Camión' & bd$tmp_axa$tipo_vehiculo != 'Camión Ligero' & bd$tmp_axa$tipo_vehiculo != 'Motocicleta',
                                                               'Sin Datos', bd$tmp_axa$tipo_vehiculo) , '<br/>',
                                  '<b>Nivel de Daño del Vehículo</b>: ' , ifelse(bd$tmp_axa$nivel_dano_vehiculo != 'Alto' & bd$tmp_axa$nivel_dano_vehiculo != 'Bajo' & bd$tmp_axa$nivel_dano_vehiculo != 'Medio' & bd$tmp_axa$nivel_dano_vehiculo != 'Sin daño',
                                                                    'Sin Datos' , bd$tmp_axa$nivel_dano_vehiculo) , '<br/>',
                                  '<b>Punto de Impacto al Vehículo</b>: ' , ifelse(bd$tmp_axa$punto_impacto == '\\N' | bd$tmp_axa$punto_impacto == '',
                                                                                   'Sin Datos' , bd$tmp_axa$punto_impacto) , '<br/>',
                                  '<b>Total de Lesionados</b>: ' , bd$tmp_axa$lesionados , '<br/>',
                                  '<b>Rol del Lesionado Principal</b>: ' , ifelse(bd$tmp_axa$relacion_lesionados == '\\N' | bd$tmp_axa$relacion_lesionados == '',
                                                                                  'Sin Datos' , bd$tmp_axa$relacion_lesionados) , '<br/>',
                                  '<b>Nivel de Lesión</b>: ' , ifelse(bd$tmp_axa$nivel_lesionado == '\\N' | bd$tmp_axa$nivel_lesionado == '',
                                                                      'Sin Datos' , str_to_title(bd$tmp_axa$nivel_lesionado , locale = 'es')) , '<br/>',
                                  '<b>¿Hospitalizado?</b>: ' , str_to_title(bd$tmp_axa$hospitalizado , locale = 'es')
                                  ),
                   popupOptions = popupOptions(maxWidth = 450),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE , iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(93,0,97,0.75); color: #29002B;\"><span><strong>' + cluster.getChildCount() + '</strong></div><span>',
                                               className: 'marker-cluster'
                                             });
                                          }")))
      paleta_nombres <- append(paleta_nombres , 'AXA')
      paleta_colores <- append(paleta_colores , '#5E0061')}
    if (!is.null(bd$tmp_repubikla)) {
      l <- l %>%
        addMarkers(data = st_transform(bd$tmp_repubikla, 4326) , group = 'repubikla' ,
                   icon = list(iconUrl = ifelse(bd$tmp_repubikla$gravedad == 'mortal',
                                                'repubikla_a.png',
                                                ifelse(bd$tmp_repubikla$gravedad == 'ambulancia requerida' | bd$tmp_repubikla$gravedad == 'costillas fracturadas' | bd$tmp_repubikla$gravedad == 'grave' | bd$tmp_repubikla$gravedad == 'leve' | bd$tmp_repubikla$gravedad == 'moderada',
                                                       'repubikla_b.png' , 'repubikla_c.png')),
                               iconSize = c(35,35)),
                   options = markerOptions(opacity = 0.9),
                   popup = paste0('<b>Incidente Repubikla</b><br/>',
                                  '<b>Fecha y Hora de Hechos</b>: ' , format(bd$tmp_repubikla$timestamp , format = '%d/%m/%Y, %T'), '<br/>',
                                  '<b>Comentario</b>: ' , bd$tmp_repubikla$comentario , '<br/>',
                                  '<b>Modo</b>: ' , str_to_title(bd$tmp_repubikla$modo , locale = 'es') , '<br/>' ,
                                  '<b>Fuente</b>: <a href="' , bd$tmp_repubikla$fuente_1 , '">' , bd$tmp_repubikla$fuente_1 , '</a><br/>',
                                  '<b>Responsable</b>: ' , ifelse(bd$tmp_repubikla$responsable == '' , 'Sin Datos',
                                                                  str_to_title(bd$tmp_repubikla$responsable , locale = 'es')), '<br/>' ,
                                  '<b>Gravedad</b>: ' , ifelse(bd$tmp_repubikla$gravedad == '' , 'Sin Datos',
                                                               str_to_title(bd$tmp_repubikla$gravedad , locale = 'es')) , '<br/>',
                                  '<b>Seguimiento</b>: ' , ifelse(bd$tmp_repubikla$seguimiento == '' , 'Sin Datos',
                                                                  str_to_title(bd$tmp_repubikla$seguimiento , locale = 'es'))
                                  ),
                   popupOptions = popupOptions(maxWidth = 450),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE , iconCreateFunction = JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(62,132,0,0.75); color: #1C3B00;\"><span><strong>' + cluster.getChildCount() + '</strong></div><span>',
                                               className: 'marker-cluster'
                                             });
                                          }")))
      paleta_nombres <- append(paleta_nombres , 'Repubikla')
      paleta_colores <- append(paleta_colores , '#3F8500')}
    # =
    leyenda_s <- NULL
    if (input$filtro_incidente == 'Decesos' | input$filtro_incidente == 'Todos') leyenda_s <- paste0(leyenda_s , "<img src='a.svg' style = 'width: 20px'> Decesos<br/>")
    if (input$filtro_incidente == 'Lesionados' | input$filtro_incidente == 'Todos') leyenda_s <- paste0(leyenda_s , "<img src='b.svg' style = 'width: 20px'> Lesionados<br/>")
    if (input$filtro_incidente == 'Accidentes' | input$filtro_incidente == 'Todos') leyenda_s <- paste0(leyenda_s , "<img src='c.svg' style = 'width: 20px'> Accidentes<br/>")
    if (!is.null(leyenda_s) & !is.null(filtro_bd_sp())) {
      leyenda_s <- paste0('<b>Simbología</b><br/>' , leyenda_s)
      l <- l %>%
        addControl(position = 'bottomright' , html = leyenda_s)}
    # =
    if (!is.null(paleta_colores)) {
      l <- l %>%
        addLegend(position = 'bottomright' , opacity = 1 , title = 'Fuente de Datos',
                  colors = paleta_colores , labels = paleta_nombres)
    }
    # =
    l
  })
  
  # ===== MENÚ TIPO DE GRÁFICA - POR TOTALES =====
  observeEvent(filtro_bd_sp() , ignoreNULL = FALSE, {
    if (is.null(filtro_bd_sp())) {
      updateSelectInput(session , inputId = 'tipo_grafica' , label = 'Datos a Graficar' , choices = '')
    } else {
      opciones = as.character(filtro_bd_sp())
      if (length(opciones) > 1) opciones = append(opciones, 'Gráficas Combinadas' , after = 0)
      updateSelectInput(session , inputId = 'tipo_grafica' , label = 'Datos a Graficar' , choices = opciones)
    }
  })
  
  # ===== FOOTER GRÁFICA - POR TOTALES =====
  observeEvent(c(input$tipo_grafica , filtro_bd_sp()) , ignoreNULL = FALSE, {
    if (is.null(filtro_bd_sp())) {
      hideElement(id = 'boton_zoom_grafica')
      removeUI(selector = '#div_grafica_b')
      insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
               tags$div(id = 'div_grafica_b' , style = 'width: 100%; height: 50px; background-color: white;'))
    }
    else {
      showElement(id = 'boton_zoom_grafica')
      removeUI(selector = '#div_grafica_b')
      # =
      if (input$tipo_grafica == 'Gráficas Combinadas') {
        opciones = as.character(filtro_bd_sp())
        insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
                 # tags$div(id = 'div_grafica_b' ,
                 #          checkboxGroupInput(inputId = 'grafica_bd' , label = 'Fuentes a Visualizar', inline = TRUE,
                 #                             choices = opciones , selected = opciones))
                 tags$div(id = 'div_grafica_b' , style = 'width: 100%; height: 50px; background-color: white;')
                 )
      }
      else if (input$tipo_grafica == 'PGJ') {
        insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b' ,
                          selectInput(inputId = 'subgrafica_pgj' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Delito'))))
      }
      else if (input$tipo_grafica == 'SSC') {
        insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b' ,
                          selectInput(inputId = 'subgrafica_ssc' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Tipo de Evento' , 'Identidad'))))
      }
      else if (input$tipo_grafica == 'C5') {
        insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b' ,
                          selectInput(inputId = 'subgrafica_c5' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Incidente C4' , 'Clase del Incidente' , 'Tipo de Entrada'))))
      }
      else if (input$tipo_grafica == 'AXA') {
        insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b' ,
                          selectInput(inputId = 'subgrafica_axa' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Causa del Siniestro' , 'Tipo de Vehículo' , 'Rol del Lesionado'))))
      }
      else if (input$tipo_grafica == 'Repubikla') {
        insertUI(selector = '#div_grafica_a' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b' ,
                          selectInput(inputId = 'subgrafica_repubikla' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Modo'))))
      }
    }
  })
  
  # ===== GRÁFICAS - POR TOTALES =====
  observeEvent(filtro_fecha_sp() , {
    i <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
    ref_dia <- 1
    ref_mes <- 1
    mes <- 0
    df_referencia <- data.frame(fecha = as.character() , ref_dia = as.integer(), ref_mes = as.integer(), etiqueta = as.character() , stringsAsFactors = FALSE)
    while (i <= dates(as.character(filtro_fecha_sp()[[2]]) , format = 'y-m-d')) {
      df_referencia[nrow(df_referencia) + 1,] <- c(format(chron(i , times('00:00:00')) , format = '%d/%m/%Y') , ref_dia , NA , NA)
      # =
      if (month(i) != mes) {
        mes <- month(i)
        t <- month(i)
        # =
        if (t == 1) {
          t <- 'Ene'
        } else if (t == 2) {
          t <- 'Feb'
        } else if (t == 3) {
          t <- 'Mar'
        } else if (t == 4) {
          t <- 'Abr'
        } else if (t == 5) {
          t <- 'May'
        } else if (t == 6) {
          t <- 'Jun'
        } else if (t == 7) {
          t <- 'Jul'
        } else if (t == 8) {
          t <- 'Ago'
        } else if (t == 9) {
          t <- 'Sep'
        } else if (t == 10) {
          t <- 'Oct'
        } else if (t == 11) {
          t <- 'Nov'
        } else t <- 'Dic'
        # =
        t <- paste0(t ,'/' , substring(as.character(year(i)) , 3))
        # =
        df_referencia[df_referencia$ref_dia == ref_dia , 'etiqueta'] <- t
        df_referencia[df_referencia$ref_dia == ref_dia , 'ref_mes'] <- ref_mes
        ref_mes <- ref_mes + 1
      }
      # =
      ref_dia <- ref_dia + 1
      i <- i + 1
    }
    # =
    bd$df_referencia <- df_referencia
  })
  
  output$grafica_sp <- renderPlot({
    count_final <- data.frame(n = as.integer() , ref = as.integer() , etiqueta = as.character() , categoria = as.character())
    grafica <- ggplot()
    is_graph <- FALSE
    if (input$tipo_grafica != '') {
      if (input$tipo_grafica == 'Gráficas Combinadas' & !is.null(filtro_bd_sp())) {
        is_graph <- TRUE
        paleta = c()
        if ('PGJ' %in% filtro_bd_sp()) paleta <- append(paleta , '#952800')
        if ('SSC' %in% filtro_bd_sp()) paleta <- append(paleta , '#043A5F')
        if ('C5' %in% filtro_bd_sp()) paleta <- append(paleta , '#956F00')
        if ('AXA' %in% filtro_bd_sp()) paleta <- append(paleta , '#5E0061')
        if ('Repubikla' %in% filtro_bd_sp()) paleta <- append(paleta , '#3F8500')
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          for (bd_sp in filtro_bd_sp()) {
            if (bd_sp == 'PGJ') tmp = bd$tmp_pgj
            else if (bd_sp == 'SSC') tmp = bd$tmp_ssc
            else if (bd_sp == 'C5') tmp = bd$tmp_c5
            else if (bd_sp == 'AXA') tmp = bd$tmp_axa
            else if (bd_sp == 'Repubikla') tmp = bd$tmp_repubikla
            tmp$geometry <- NULL
            # =
            count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
            count$etiqueta <- count$mes
            # =
            count$etiqueta[count$etiqueta == 1] <- 'Ene'
            count$etiqueta[count$etiqueta == 2] <- 'Feb'
            count$etiqueta[count$etiqueta == 3] <- 'Mar'
            count$etiqueta[count$etiqueta == 4] <- 'Abr'
            count$etiqueta[count$etiqueta == 5] <- 'May'
            count$etiqueta[count$etiqueta == 6] <- 'Jun'
            count$etiqueta[count$etiqueta == 7] <- 'Jul'
            count$etiqueta[count$etiqueta == 8] <- 'Ago'
            count$etiqueta[count$etiqueta == 9] <- 'Sep'
            count$etiqueta[count$etiqueta == 10] <- 'Oct'
            count$etiqueta[count$etiqueta == 11] <- 'Nov'
            count$etiqueta[count$etiqueta == 12] <- 'Dic'
            # =
            if (nrow(count) != 0) count$etiqueta <- paste0(count$etiqueta , '/' , substring(as.character(count$ao), 3 , 4))
            # =
            count <- merge(x = count , y = bd$df_referencia[!is.na(bd$df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
            count$categoria <- bd_sp
            count$ref_mes <- as.integer(count$ref_mes)
            count <- count[order(count$ref_mes),]
            count$n[is.na(count$n)] <- 0
            # =
            count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes')) 
          }
        }
        else if (input$tiempo_grafica == 'Por Día') {
          for (bd_sp in filtro_bd_sp()) {
            if (bd_sp == 'PGJ') tmp = bd$tmp_pgj
            else if (bd_sp == 'SSC') tmp = bd$tmp_ssc
            else if (bd_sp == 'C5') tmp = bd$tmp_c5
            else if (bd_sp == 'AXA') tmp = bd$tmp_axa
            else if (bd_sp == 'Repubikla') tmp = bd$tmp_repubikla
            tmp$geometry <- NULL
            # =
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , n = 0)
            }
            else count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
            count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
            # =
            count <- merge(x = count , y = bd$df_referencia , by = 'fecha' , all.y = TRUE)
            count$categoria <- bd_sp
            count$ref_dia <- as.integer(count$ref_dia)
            count <- count[order(count$ref_dia),]
            count$n[is.na(count$n)] <- 0
            # =
            count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
          }
        }
      }
      else if (input$tipo_grafica == 'PGJ' & !is.null(input$subgrafica_pgj) & !is.null(bd$tmp_pgj)) {
        is_graph <- TRUE
        tmp <- bd$tmp_pgj
        tmp$geometry <- NULL
        if (input$subgrafica_pgj == 'Sin Categoría') paleta <- c('#952800')
        else paleta <- c('#97173A' , '#7F2B5F' , '#593E70' , '#36476B' , '#2F4858' , '#C03C7A' , '#B65FB2' , '#9682DD' , '#65A3F8')
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          if (input$subgrafica_pgj == 'Sin Categoría') count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , n = 0 ,  categoria = 'Sin Datos')
            }
            else if (input$subgrafica_pgj == 'Delito') count <- count(tmp , year(timestamp) , month(timestamp) , delito) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='delito')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$etiqueta <- count$mes
          count$etiqueta[count$etiqueta == 1] <- 'Ene'
          count$etiqueta[count$etiqueta == 2] <- 'Feb'
          count$etiqueta[count$etiqueta == 3] <- 'Mar'
          count$etiqueta[count$etiqueta == 4] <- 'Abr'
          count$etiqueta[count$etiqueta == 5] <- 'May'
          count$etiqueta[count$etiqueta == 6] <- 'Jun'
          count$etiqueta[count$etiqueta == 7] <- 'Jul'
          count$etiqueta[count$etiqueta == 8] <- 'Ago'
          count$etiqueta[count$etiqueta == 9] <- 'Sep'
          count$etiqueta[count$etiqueta == 10] <- 'Oct'
          count$etiqueta[count$etiqueta == 11] <- 'Nov'
          count$etiqueta[count$etiqueta == 12] <- 'Dic'
          # =
          if (nrow(count) != 0) count$etiqueta <- paste0(count$etiqueta , '/' , substring(as.character(count$ao), 3 , 4))
          # =
          count <- merge(x = count , y = bd$df_referencia[!is.na(bd$df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
          count$ref_mes <- as.integer(count$ref_mes)
          # =
          if (input$subgrafica_pgj == 'Sin Categoría') {
            count <- count[order(count$ref_mes),]
            count$categoria <- 'PGJ'
            count$n[is.na(count$n)] <- 0
          }
          else {
            for (f in unique(count$fecha)) {
              tmp1 <- unique(count[count$fecha == f,]$ref_dia)
              tmp2 <- unique(count[count$fecha == f,]$ref_mes)
              tmp3 <- unique(count[count$fecha == f,]$etiqueta)
              for (cat in unique(count[!is.na(count$categoria),]$categoria)) {
                if (nrow(filter(count , fecha == f & categoria == cat)) == 0) count[nrow(count) + 1,] <- c(tmp3 , NA , NA , cat , 0 , f , tmp1 , tmp2)
              }}
            count <- count[!is.na(count$n),]
            count$n <- as.integer(count$n)
            count$ref_mes <- as.integer(count$ref_mes)
            count <- count[order(-count$n),]
          }
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes'))
        }
        # =
        else if (input$tiempo_grafica == 'Por Día') {
          if (input$subgrafica_pgj == 'Sin Categoría') {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , n = 0)
            }
            count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
          }
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_pgj == 'Delito') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , delito) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' , 'categoria'='delito')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
          # =
          if (input$subgrafica_pgj != 'Sin Categoría') {
            for (f in unique(count$fecha)) {
              if (nrow(count[count$fecha == f,]) < length(unique(count$categoria))) {
                for (cat in unique(count$categoria)) {
                  if (nrow(count[count$fecha == f & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(NA , NA , NA , cat , 0 , f)}}}
          }
          # =
          count <- merge(x = count , y = bd$df_referencia , by = 'fecha' , all.y = TRUE)
          # =
          if (nrow(count[is.na(count$n),]) != 0) {
            if (input$subgrafica_pgj == 'Sin Categoría') count[is.na(count$n),]$n <- 0
            else {
              for (f in unique(count[is.na(count$n),]$fecha)) {
                tmp1 <- count[count$fecha == f,]$ref_dia
                tmp2 <- count[count$fecha == f,]$ref_mes
                tmp3 <- count[count$fecha == f,]$etiqueta
                for (cat in unique(count[!is.na(count$n) ,]$categoria)) {
                  count[nrow(count) + 1,] <- c(f, NA , NA , NA , cat , 0 , tmp1 , tmp2 , tmp3)}}
              count <- count[!is.na(count$n),]
            }
          }
          # =
          count$ref_dia <- as.integer(count$ref_dia)
          count$n <- as.integer(count$n)
          # =
          if (input$subgrafica_pgj == 'Sin Categoría') {
            count$categoria <- 'PGJ'
            count <- count[order(count$ref_dia),]
          }
          else count <- count[order(-count$n),]
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
        }
      }
      else if (input$tipo_grafica == 'SSC' & !is.null(input$subgrafica_ssc) & !is.null(bd$tmp_ssc)) {
        is_graph <- TRUE
        tmp <- bd$tmp_ssc
        tmp$geometry <- NULL
        if (input$subgrafica_ssc == 'Sin Categoría') paleta <- c('#043A5F')
        else paleta <- c('#006386' , '#008E97' , '#00B891' , '#8CDC7D' , '#F9F871' , '#6B6399' , '#9B77B0' , '#CC8BC3' , '#FDA1D1')
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          if (input$subgrafica_ssc == 'Sin Categoría') count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp), categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_ssc == 'Tipo de Evento') count <- count(tmp , year(timestamp) , month(timestamp) , tipo_evento) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='tipo_evento')
            else if (input$subgrafica_ssc == 'Identidad') count <- count(tmp , year(timestamp) , month(timestamp) , identidad) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='identidad')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$etiqueta <- count$mes
          count$etiqueta[count$etiqueta == 1] <- 'Ene'
          count$etiqueta[count$etiqueta == 2] <- 'Feb'
          count$etiqueta[count$etiqueta == 3] <- 'Mar'
          count$etiqueta[count$etiqueta == 4] <- 'Abr'
          count$etiqueta[count$etiqueta == 5] <- 'May'
          count$etiqueta[count$etiqueta == 6] <- 'Jun'
          count$etiqueta[count$etiqueta == 7] <- 'Jul'
          count$etiqueta[count$etiqueta == 8] <- 'Ago'
          count$etiqueta[count$etiqueta == 9] <- 'Sep'
          count$etiqueta[count$etiqueta == 10] <- 'Oct'
          count$etiqueta[count$etiqueta == 11] <- 'Nov'
          count$etiqueta[count$etiqueta == 12] <- 'Dic'
          # =
          if (nrow(count) != 0) count$etiqueta <- paste0(count$etiqueta , '/' , substring(as.character(count$ao), 3 , 4))
          # =
          count <- merge(x = count , y = bd$df_referencia[!is.na(bd$df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
          count$ref_mes <- as.integer(count$ref_mes)
          # =
          if (input$subgrafica_ssc == 'Sin Categoría') {
            count <- count[order(count$ref_mes),]
            count$categoria <- 'SSC'
            count$n[is.na(count$n)] <- 0
          }
          else {
            for (f in unique(count$fecha)) {
              tmp1 <- unique(count[count$fecha == f,]$ref_dia)
              tmp2 <- unique(count[count$fecha == f,]$ref_mes)
              tmp3 <- unique(count[count$fecha == f,]$etiqueta)
              for (cat in unique(count[!is.na(count$categoria),]$categoria)) {
                if (nrow(filter(count , fecha == f & categoria == cat)) == 0) count[nrow(count) + 1,] <- c(tmp3 , NA , NA , cat , 0 , f , tmp1 , tmp2)
              }}
            count <- count[!is.na(count$n),]
            count$n <- as.integer(count$n)
            count$ref_mes <- as.integer(count$ref_mes)
            count <- count[order(-count$n),]
          }
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes'))
        }
        # =
        else if (input$tiempo_grafica == 'Por Día') {
          if (input$subgrafica_ssc == 'Sin Categoría') {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , n = 0)
            }
            else count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
          }
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_ssc == 'Tipo de Evento') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , tipo_evento) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' , 'categoria'='tipo_evento')
            else if (input$subgrafica_ssc == 'Identidad') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , identidad) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' ,  'categoria'='identidad')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
          # =
          if (input$subgrafica_ssc != 'Sin Categoría') {
            for (f in unique(count$fecha)) {
              if (nrow(count[count$fecha == f,]) < length(unique(count$categoria))) {
                for (cat in unique(count$categoria)) {
                  if (nrow(count[count$fecha == f & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(NA , NA , NA , cat , 0 , f)}}}
          }
          # =
          count <- merge(x = count , y = bd$df_referencia , by = 'fecha' , all.y = TRUE)
          # =
          if (nrow(count[is.na(count$n),]) != 0) {
            if (input$subgrafica_ssc == 'Sin Categoría') count[is.na(count$n),]$n <- 0
            else {
              for (f in unique(count[is.na(count$n),]$fecha)) {
                tmp1 <- count[count$fecha == f,]$ref_dia
                tmp2 <- count[count$fecha == f,]$ref_mes
                tmp3 <- count[count$fecha == f,]$etiqueta
                for (cat in unique(count[!is.na(count$n) ,]$categoria)) {
                  count[nrow(count) + 1,] <- c(f, NA , NA , NA , cat , 0 , tmp1 , tmp2 , tmp3)}}
              count <- count[!is.na(count$n),]
            }
          }
          # =
          count$ref_dia <- as.integer(count$ref_dia)
          count$n <- as.integer(count$n)
          # =
          if (input$subgrafica_ssc == 'Sin Categoría') {
            count$categoria <- 'SSC'
            count <- count[order(count$ref_dia),]
          }
          else count <- count[order(-count$n),]
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
        }
      }
      else if (input$tipo_grafica == 'C5' & !is.null(input$subgrafica_c5) & !is.null(bd$tmp_c5)) {
        is_graph <- TRUE
        tmp <- bd$tmp_c5
        tmp$geometry <- NULL
        if (input$subgrafica_c5 == 'Sin Categoría') paleta <- c('#956F00')
        else paleta <- c('#956F00', '#5D731D' , '#276E40' , '#006459' , '#015762' , '#2F4858' , '#C2A573' , '#FFEECB' , '#00C9B1' , '#4E4637' , '#B5AA99' , '#987061' , '#FFC1B2' , '#FE8A7D' , '#C1554C' ,
                         '#a78e72' , '#e48d24' , '#725238' , '#e1b53f' , '#7f6342' , '#e6c392' , '#715c09' , '#dda25a' , '#895c1d' , '#ad8220' , '#a2814d' , '#b2712f')
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          if (input$subgrafica_c5 == 'Sin Categoría') count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp), categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_c5 == 'Incidente C4') count <- count(tmp , year(timestamp) , month(timestamp) , incidente_c4) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='incidente_c4')
            else if (input$subgrafica_c5 == 'Clase del Incidente') count <- count(tmp , year(timestamp) , month(timestamp) , clas_con_f_alarma) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='clas_con_f_alarma')
            else if (input$subgrafica_c5 == 'Tipo de Entrada') count <- count(tmp , year(timestamp) , month(timestamp) , tipo_entrada) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='tipo_entrada')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$etiqueta <- count$mes
          count$etiqueta[count$etiqueta == 1] <- 'Ene'
          count$etiqueta[count$etiqueta == 2] <- 'Feb'
          count$etiqueta[count$etiqueta == 3] <- 'Mar'
          count$etiqueta[count$etiqueta == 4] <- 'Abr'
          count$etiqueta[count$etiqueta == 5] <- 'May'
          count$etiqueta[count$etiqueta == 6] <- 'Jun'
          count$etiqueta[count$etiqueta == 7] <- 'Jul'
          count$etiqueta[count$etiqueta == 8] <- 'Ago'
          count$etiqueta[count$etiqueta == 9] <- 'Sep'
          count$etiqueta[count$etiqueta == 10] <- 'Oct'
          count$etiqueta[count$etiqueta == 11] <- 'Nov'
          count$etiqueta[count$etiqueta == 12] <- 'Dic'
          # =
          if (nrow(count) != 0) count$etiqueta <- paste0(count$etiqueta , '/' , substring(as.character(count$ao), 3 , 4))
          # =
          count <- merge(x = count , y = bd$df_referencia[!is.na(bd$df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
          count$ref_mes <- as.integer(count$ref_mes)
          # =
          if (input$subgrafica_c5 == 'Sin Categoría') {
            count <- count[order(count$ref_mes),]
            count$categoria <- 'C5'
            count$n[is.na(count$n)] <- 0
          }
          else {
            for (f in unique(count$fecha)) {
              tmp1 <- unique(count[count$fecha == f,]$ref_dia)
              tmp2 <- unique(count[count$fecha == f,]$ref_mes)
              tmp3 <- unique(count[count$fecha == f,]$etiqueta)
              for (cat in unique(count[!is.na(count$categoria),]$categoria)) {
                if (nrow(filter(count , fecha == f & categoria == cat)) == 0) count[nrow(count) + 1,] <- c(tmp3 , NA , NA , cat , 0 , f , tmp1 , tmp2)
              }}
            count <- count[!is.na(count$n),]
            count$n <- as.integer(count$n)
            count$ref_mes <- as.integer(count$ref_mes)
            count <- count[order(-count$n),]
          }
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes'))
        }
        # =
        else if (input$tiempo_grafica == 'Por Día') {
          if (input$subgrafica_c5 == 'Sin Categoría') {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , n = 0)
            }
            else count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
          }
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_c5 == 'Incidente C4') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , incidente_c4) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' , 'categoria'='incidente_c4')
            else if (input$subgrafica_c5 == 'Clase del Incidente') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , clas_con_f_alarma) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' ,  'categoria'='clas_con_f_alarma')
            else if (input$subgrafica_c5 == 'Tipo de Entrada') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , tipo_entrada) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' ,  'categoria'='tipo_entrada')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
          # =
          if (input$subgrafica_c5 != 'Sin Categoría') {
            for (f in unique(count$fecha)) {
              if (nrow(count[count$fecha == f,]) < length(unique(count$categoria))) {
                for (cat in unique(count$categoria)) {
                  if (nrow(count[count$fecha == f & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(NA , NA , NA , cat , 0 , f)}}}
          }
          # =
          count <- merge(x = count , y = bd$df_referencia , by = 'fecha' , all.y = TRUE)
          # =
          if (nrow(count[is.na(count$n),]) != 0) {
            if (input$subgrafica_c5 == 'Sin Categoría') count[is.na(count$n),]$n <- 0
            else {
              for (f in unique(count[is.na(count$n),]$fecha)) {
                tmp1 <- count[count$fecha == f,]$ref_dia
                tmp2 <- count[count$fecha == f,]$ref_mes
                tmp3 <- count[count$fecha == f,]$etiqueta
                for (cat in unique(count[!is.na(count$n) ,]$categoria)) {
                  count[nrow(count) + 1,] <- c(f, NA , NA , NA , cat , 0 , tmp1 , tmp2 , tmp3)}}
              count <- count[!is.na(count$n),]
            }
          }
          # =
          count$ref_dia <- as.integer(count$ref_dia)
          count$n <- as.integer(count$n)
          # =
          if (input$subgrafica_c5 == 'Sin Categoría') {
            count$categoria <- 'C5'
            count <- count[order(count$ref_dia),]
          }
          else count <- count[order(-count$n),]
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
        }
      }
      else if (input$tipo_grafica == 'AXA' & !is.null(input$subgrafica_axa) & !is.null(bd$tmp_axa)) {
        is_graph <- TRUE
        tmp <- bd$tmp_axa
        tmp$geometry <- NULL
        if (input$subgrafica_axa == 'Sin Categoría') paleta <- c('#5E0061')
        else paleta <- c('#5E0061' , '#A1145F' , '#D44755' , '#F37F4B' , '#FFBB4F' , '#F9F871' , '#474197' , '#006CBD' , '#36D9D3' , '#7D527C' , '#FFE7FF')
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          if (input$subgrafica_axa == 'Sin Categoría') count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp), categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_axa == 'Causa del Siniestro') count <- count(tmp , year(timestamp) , month(timestamp) , causa_siniestro) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='causa_siniestro')
            else if (input$subgrafica_axa == 'Tipo de Vehículo') count <- count(tmp , year(timestamp) , month(timestamp) , tipo_vehiculo) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='tipo_vehiculo')
            else if (input$subgrafica_axa == 'Rol del Lesionado') count <- count(tmp , year(timestamp) , month(timestamp) , relacion_lesionados) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='relacion_lesionados')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$etiqueta <- count$mes
          count$etiqueta[count$etiqueta == 1] <- 'Ene'
          count$etiqueta[count$etiqueta == 2] <- 'Feb'
          count$etiqueta[count$etiqueta == 3] <- 'Mar'
          count$etiqueta[count$etiqueta == 4] <- 'Abr'
          count$etiqueta[count$etiqueta == 5] <- 'May'
          count$etiqueta[count$etiqueta == 6] <- 'Jun'
          count$etiqueta[count$etiqueta == 7] <- 'Jul'
          count$etiqueta[count$etiqueta == 8] <- 'Ago'
          count$etiqueta[count$etiqueta == 9] <- 'Sep'
          count$etiqueta[count$etiqueta == 10] <- 'Oct'
          count$etiqueta[count$etiqueta == 11] <- 'Nov'
          count$etiqueta[count$etiqueta == 12] <- 'Dic'
          # =
          if (nrow(count) != 0) count$etiqueta <- paste0(count$etiqueta , '/' , substring(as.character(count$ao), 3 , 4))
          # =
          count <- merge(x = count , y = bd$df_referencia[!is.na(bd$df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
          count$ref_mes <- as.integer(count$ref_mes)
          # =
          if (input$subgrafica_axa == 'Sin Categoría') {
            count <- count[order(count$ref_mes),]
            count$categoria <- 'AXA'
            count$n[is.na(count$n)] <- 0
          }
          else {
            for (f in unique(count$fecha)) {
              tmp1 <- unique(count[count$fecha == f,]$ref_dia)
              tmp2 <- unique(count[count$fecha == f,]$ref_mes)
              tmp3 <- unique(count[count$fecha == f,]$etiqueta)
              for (cat in unique(count[!is.na(count$categoria),]$categoria)) {
                if (nrow(filter(count , fecha == f & categoria == cat)) == 0) count[nrow(count) + 1,] <- c(tmp3 , NA , NA , cat , 0 , f , tmp1 , tmp2)
              }}
            count <- count[!is.na(count$n),]
            count$n <- as.integer(count$n)
            count$ref_mes <- as.integer(count$ref_mes)
            count <- count[order(-count$n),]
          }
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes'))
        }
        # =
        else if (input$tiempo_grafica == 'Por Día') {
          if (input$subgrafica_axa == 'Sin Categoría') {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , n = 0)
            }
            else count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
          }
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_axa == 'Causa del Siniestro') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , causa_siniestro) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' , 'categoria'='causa_siniestro')
            else if (input$subgrafica_axa == 'Tipo de Vehículo') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , tipo_vehiculo) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' ,  'categoria'='tipo_vehiculo')
            else if (input$subgrafica_axa == 'Rol del Lesionado') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , relacion_lesionados) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' ,  'categoria'='relacion_lesionados')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
          # =
          if (input$subgrafica_axa != 'Sin Categoría') {
            for (f in unique(count$fecha)) {
              if (nrow(count[count$fecha == f,]) < length(unique(count$categoria))) {
                for (cat in unique(count$categoria)) {
                  if (nrow(count[count$fecha == f & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(NA , NA , NA , cat , 0 , f)}}}
          }
          # =
          count <- merge(x = count , y = bd$df_referencia , by = 'fecha' , all.y = TRUE)
          # =
          if (nrow(count[is.na(count$n),]) != 0) {
            if (input$subgrafica_axa == 'Sin Categoría') count[is.na(count$n),]$n <- 0
            else {
              for (f in unique(count[is.na(count$n),]$fecha)) {
                tmp1 <- count[count$fecha == f,]$ref_dia
                tmp2 <- count[count$fecha == f,]$ref_mes
                tmp3 <- count[count$fecha == f,]$etiqueta
                for (cat in unique(count[!is.na(count$n) ,]$categoria)) {
                  count[nrow(count) + 1,] <- c(f, NA , NA , NA , cat , 0 , tmp1 , tmp2 , tmp3)}}
              count <- count[!is.na(count$n),]
            }
          }
          # =
          count$ref_dia <- as.integer(count$ref_dia)
          count$n <- as.integer(count$n)
          # =
          if (input$subgrafica_axa == 'Sin Categoría') {
            count$categoria <- 'AXA'
            count <- count[order(count$ref_dia),]
          }
          else count <- count[order(-count$n),]
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
        }
      }
      else if (input$tipo_grafica == 'Repubikla' & !is.null(input$subgrafica_repubikla) & !is.null(bd$tmp_repubikla)) {
        is_graph <- TRUE
        tmp <- bd$tmp_repubikla
        tmp$geometry <- NULL
        if (input$subgrafica_repubikla == 'Sin Categoría') paleta <- c('#3F8500')
        else paleta <- c('#3F8500' , '#00735C' , '#00666B' , '#005769' , '#2F4858' , '#008DA8' , '#008ABF' , '#97B27E' , '#E4F7D2' , '#5CB7D5')
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          if (input$subgrafica_repubikla == 'Sin Categoría') count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp), categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_repubikla == 'Modo') count <- count(tmp , year(timestamp) , month(timestamp) , modo) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='modo')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$etiqueta <- count$mes
          count$etiqueta[count$etiqueta == 1] <- 'Ene'
          count$etiqueta[count$etiqueta == 2] <- 'Feb'
          count$etiqueta[count$etiqueta == 3] <- 'Mar'
          count$etiqueta[count$etiqueta == 4] <- 'Abr'
          count$etiqueta[count$etiqueta == 5] <- 'May'
          count$etiqueta[count$etiqueta == 6] <- 'Jun'
          count$etiqueta[count$etiqueta == 7] <- 'Jul'
          count$etiqueta[count$etiqueta == 8] <- 'Ago'
          count$etiqueta[count$etiqueta == 9] <- 'Sep'
          count$etiqueta[count$etiqueta == 10] <- 'Oct'
          count$etiqueta[count$etiqueta == 11] <- 'Nov'
          count$etiqueta[count$etiqueta == 12] <- 'Dic'
          # =
          if (nrow(count) != 0) count$etiqueta <- paste0(count$etiqueta , '/' , substring(as.character(count$ao), 3 , 4))
          # =
          count <- merge(x = count , y = bd$df_referencia[!is.na(bd$df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
          count$ref_mes <- as.integer(count$ref_mes)
          # =
          if (input$subgrafica_repubikla == 'Sin Categoría') {
            count <- count[order(count$ref_mes),]
            count$categoria <- 'Repubikla'
            count$n[is.na(count$n)] <- 0
          }
          else {
            for (f in unique(count$fecha)) {
              tmp1 <- unique(count[count$fecha == f,]$ref_dia)
              tmp2 <- unique(count[count$fecha == f,]$ref_mes)
              tmp3 <- unique(count[count$fecha == f,]$etiqueta)
              for (cat in unique(count[!is.na(count$categoria),]$categoria)) {
                if (nrow(filter(count , fecha == f & categoria == cat)) == 0) count[nrow(count) + 1,] <- c(tmp3 , NA , NA , cat , 0 , f , tmp1 , tmp2)
              }}
            count <- count[!is.na(count$n),]
            count$n <- as.integer(count$n)
            count$ref_mes <- as.integer(count$ref_mes)
            count <- count[order(-count$n),]
          }
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes'))
        }
        # =
        else if (input$tiempo_grafica == 'Por Día') {
          if (input$subgrafica_repubikla == 'Sin Categoría') {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , n = 0)
            }
            else count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
          }
          else {
            if (nrow(tmp) == 0) {
              tmp <- dates(as.character(filtro_fecha_sp()[[1]]) , format = 'y-m-d')
              count <- data.frame(ao = year(tmp) , mes = month(tmp) , dia = day(tmp) , categoria = 'Sin Datos' , n = 0)
            }
            else if (input$subgrafica_repubikla == 'Modo') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , modo) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' , 'categoria'='modo')
            count$categoria <- str_to_title(count$categoria , locale = 'es')}
          # =
          count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
          # =
          if (input$subgrafica_repubikla != 'Sin Categoría') {
            for (f in unique(count$fecha)) {
              if (nrow(count[count$fecha == f,]) < length(unique(count$categoria))) {
                for (cat in unique(count$categoria)) {
                  if (nrow(count[count$fecha == f & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(NA , NA , NA , cat , 0 , f)}}}
          }
          # =
          count <- merge(x = count , y = bd$df_referencia , by = 'fecha' , all.y = TRUE)
          # =
          if (nrow(count[is.na(count$n),]) != 0) {
            if (input$subgrafica_repubikla == 'Sin Categoría') count[is.na(count$n),]$n <- 0
            else {
              for (f in unique(count[is.na(count$n),]$fecha)) {
                tmp1 <- count[count$fecha == f,]$ref_dia
                tmp2 <- count[count$fecha == f,]$ref_mes
                tmp3 <- count[count$fecha == f,]$etiqueta
                for (cat in unique(count[!is.na(count$n) ,]$categoria)) {
                  count[nrow(count) + 1,] <- c(f, NA , NA , NA , cat , 0 , tmp1 , tmp2 , tmp3)}}
              count <- count[!is.na(count$n),]
            }
          }
          # =
          count$ref_dia <- as.integer(count$ref_dia)
          count$n <- as.integer(count$n)
          # =
          if (input$subgrafica_repubikla == 'Sin Categoría') {
            count$categoria <- 'Repubikla'
            count <- count[order(count$ref_dia),]
          }
          else count <- count[order(-count$n),]
          # =
          count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
        }
      }
      # ===
      if (is_graph == TRUE) {
        if (input$tiempo_grafica == 'Por Mes') {
          grafica = grafica +
            geom_line(data = count_final , aes(x = ref , y = n , color = categoria) , size = 2) +
            scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                               minor_breaks = NULL,
                               labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
            scale_color_manual(values = paleta,
                               limits = unique(count_final$categoria),
                               name = 'Fuente de Datos' ,
                               labels = unique(count_final$categoria)) +
            ylim(0 , NA) +
            labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Mes')
        }
        else if (input$tiempo_grafica == 'Por Día') {
          grafica = grafica +
            geom_line(data = count_final , aes(x = ref , y = n , color = categoria), alpha = 0.2) +
            geom_smooth(data = count_final , aes(x = ref , y = n , color = categoria), method = 'loess' , formula = 'y ~ x' , se = FALSE , size = 2 , na.rm = TRUE) +
            scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                               minor_breaks = NULL,
                               labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
            scale_color_manual(values = paleta,
                               limits = unique(count_final$categoria),
                               name = 'Fuente de Datos' ,
                               labels = unique(count_final$categoria)) +
            ylim(0 , NA) +
            labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Día')
        }
      }
    }
    # ===
    graf_modal$g <- grafica
    hover_h$h <- count_final
    grafica
  })
  
  # ===== ZOOM GRÁFICA - POR TOTALES =====
  observeEvent(input$boton_zoom_grafica , {
    showModal(tags$div(id = 'div_modal_zoom', style = 'font-size; 120%;',
                       modalDialog(size = 'l' , title = NULL , footer = NULL , easyClose = TRUE,
                                   plotOutput(outputId = 'grafica_modal_a' , height = '85vh',
                                              click = clickOpts(id = 'plot_click_modal_a')),
                                   uiOutput(outputId = 'click_info_modal_a'))))
  })
  output$grafica_modal_a <- renderPlot({
    graf_modal$g +
      theme(text = element_text(size = 18))
  })
  
  # ===== MENÚ TIPO DE GRÁFICA - POR DÍA Y HORA =====
  observeEvent(filtro_bd_sp() , ignoreNULL = FALSE, {
    if (is.null(filtro_bd_sp())) {
      updateSelectInput(session , inputId = 'tipo_grafica2' , label = 'Datos a Graficar' , choices = '')
    } else {
      opciones = as.character(filtro_bd_sp())
      if (length(opciones) > 1) opciones = append(opciones, 'Gráficas Combinadas' , after = 0)
      updateSelectInput(session , inputId = 'tipo_grafica2' , label = 'Datos a Graficar' , choices = opciones)
    }
  })
  
  # ===== FOOTER GRÁFICA - POR DÍA Y HORA =====
  observeEvent(c(input$tipo_grafica2 , filtro_bd_sp()) , ignoreNULL = FALSE, {
    if (is.null(filtro_bd_sp())) hideElement(id = 'boton_zoom_grafica2')
    else {
      showElement(id = 'boton_zoom_grafica2')
      # =
      if (input$tipo_grafica2 == 'PGJ') {
        insertUI(selector = '#div_grafica_a2' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b2' ,
                          selectInput(inputId = 'subgrafica_pgj2' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Delito'))))
      }
    }
  })
  
  # ===== GRÁFICAS - POR DÍA Y HORA =====
  output$grafica_null <- renderPlot(NULL)
  
  output$grafica_sp2 <- renderPlot({
    count_final <- data.frame(n = as.integer() , ref = as.integer() , etiqueta = as.character() , categoria = as.character())
    grafica <- ggplot()
    is_graph <- FALSE
    if (input$tipo_grafica2 != '') {
      if (input$tipo_grafica2 == 'Gráficas Combinadas' & !is.null(filtro_bd_sp()) & !is.null(input$tiempo_grafica2)) {
        is_graph <- TRUE
        paleta = c()
        if ('PGJ' %in% filtro_bd_sp()) paleta <- append(paleta , '#952800')
        if ('SSC' %in% filtro_bd_sp()) paleta <- append(paleta , '#043A5F')
        if ('C5' %in% filtro_bd_sp()) paleta <- append(paleta , '#956F00')
        if ('AXA' %in% filtro_bd_sp()) paleta <- append(paleta , '#5E0061')
        if ('Repubikla' %in% filtro_bd_sp()) paleta <- append(paleta , '#3F8500')
        # =
        max <- 0
        for (bd_sp in filtro_bd_sp()) {
          if (bd_sp == 'PGJ') tmp = bd$tmp_pgj
          else if (bd_sp == 'SSC') tmp = bd$tmp_ssc
          else if (bd_sp == 'C5') tmp = bd$tmp_c5
          else if (bd_sp == 'AXA') tmp = bd$tmp_axa
          else if (bd_sp == 'Repubikla') tmp = bd$tmp_repubikla
          tmp$geometry <- NULL
          # =
          if (!is.null(tmp)) {
            if (nrow(tmp) != 0) {
              max_tmp <- max(count(tmp , wday(timestamp))$n)
              if (max_tmp > max) max <- ceiling(max_tmp/10)*10
            }}
          # =
          if (!is.null(tmp)) {
            if (input$tiempo_grafica2 == 'Mañana (6AM - 12PM)') tmp <- filter(tmp , hour(timestamp) %in% c(6, 7, 8, 9, 10, 11, 12))
            else if (input$tiempo_grafica2 == 'Tarde (1PM - 9PM)') tmp <- filter(tmp , hour(timestamp) %in% c(13, 14, 15, 16, 17, 18, 19, 20, 21))
            else if (input$tiempo_grafica2 == 'Noche (10PM - 5AM)') tmp <- filter(tmp , hour(timestamp) %in% c(22, 23, 0, 1, 2, 3, 4, 5))
          }
          # =
          if (nrow(tmp) == 0) count <- data.frame(ref = 1 , n = 0) 
          else count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)
          }
          # =
          count$etiqueta <- count$ref
          count$etiqueta[count$etiqueta == 1] <- 'Domingo'
          count$etiqueta[count$etiqueta == 2] <- 'Lunes'
          count$etiqueta[count$etiqueta == 3] <- 'Martes'
          count$etiqueta[count$etiqueta == 4] <- 'Miércoles'
          count$etiqueta[count$etiqueta == 5] <- 'Jueves'
          count$etiqueta[count$etiqueta == 6] <- 'Viernes'
          count$etiqueta[count$etiqueta == 7] <- 'Sábado'
          # =
          count$categoria <- bd_sp
          count$ref <- as.integer(count$ref)
          count <- count[order(count$ref),]
          # =
          count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
        }
      }
      else if (input$tipo_grafica2 == 'PGJ' & !is.null(input$subgrafica_pgj2) & !is.null(bd$tmp_pgj)) {
        is_graph <- TRUE
        if (input$subgrafica_pgj2 == 'Sin Categoría') paleta <- c('#952800')
        else paleta <- c('#97173A' , '#7F2B5F' , '#593E70' , '#36476B' , '#2F4858' , '#C03C7A' , '#B65FB2' , '#9682DD' , '#65A3F8')
        # =
        max <- 0
        tmp <- bd$tmp_pgj
        tmp$geometry <- NULL
        if (!is.null(tmp)) {
          if (nrow(tmp) != 0) {
            if (input$subgrafica_pgj2 == 'Sin Categoría') max <- ceiling(max(count(tmp , wday(timestamp))$n)/10)*10
            else if (input$subgrafica_pgj2 == 'Delito') max <- ceiling(max(count(tmp , wday(timestamp) , delito)$n)/10)*10
          }}
        # =
        if (!is.null(tmp)) {
          if (input$tiempo_grafica2 == 'Mañana (6AM - 12PM)') tmp <- filter(tmp , hour(timestamp) %in% c(6, 7, 8, 9, 10, 11, 12))
          else if (input$tiempo_grafica2 == 'Tarde (1PM - 9PM)') tmp <- filter(tmp , hour(timestamp) %in% c(13, 14, 15, 16, 17, 18, 19, 20, 21))
          else if (input$tiempo_grafica2 == 'Noche (10PM - 5AM)') tmp <- filter(tmp , hour(timestamp) %in% c(22, 23, 0, 1, 2, 3, 4, 5))
        }
        # =
        if (nrow(tmp) == 0) {
          count <- data.frame(ref = 1 , categoria = 'Sin Datos' , n = 0)
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
        } 
        else if (input$subgrafica_pgj2 == 'Sin Categoría') {
          count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
          count$categoria <- 'PGJ'
        }
        else if (input$subgrafica_pgj2 == 'Delito') {
          count <- count(tmp , wday(timestamp) , delito) %>% rename('ref'='wday(timestamp)' , 'categoria'='delito')
        }
        
      }
      # =====
      if (is_graph == TRUE) {
        if (max == 0) max <- NA
        grafica = grafica +
          geom_col(data = count_final , aes(x = ref , y = n , fill = categoria) , position = 'dodge') +
          scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                             minor_breaks = NULL,
                             labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
          scale_fill_manual(values = paleta,
                             limits = unique(count_final$categoria),
                             name = 'Fuente de Datos' ,
                             labels = unique(count_final$categoria)) +
          ylim(0 , max) +
          labs(x = 'Día de la Semana' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Día de la Semana')
      }
    }
    # ===
    graf_modal$g2 <- grafica
    hover_h$h2 <- count_final
    grafica
  })
  
  # ===== GRÁFICA DE PASTEL =====
  observeEvent(hover_h$h2 , ignoreNULL = FALSE , {
    if (!is.null(hover_h$h2)) {
      if (nrow(hover_h$h2) != 0) {
        df <- data.frame(categoria = as.character() , z = as.integer() , stringsAsFactors = FALSE)
        i <- 1
        for (cat in unique(hover_h$h2$categoria)) {
          df[nrow(df) + 1,] <- c(cat , i)
          i <- i + 1
        }
        df$z <- as.integer(df$z)
        # =
        text_pastel$df <- df
        text_pastel$z <- 1
        text_pastel$max <- i - 1
      }
    }
  })
  
  observeEvent(input$pastel_left , {
    if (!is.null(hover_h$h2)) {
      if (nrow(hover_h$h2) != 0) {
        if (text_pastel$z == 1) text_pastel$z <- text_pastel$max
        else text_pastel$z <- text_pastel$z - 1
      }
    }
  })
  
  observeEvent(input$pastel_right , {
    if (!is.null(hover_h$h2)) {
      if (nrow(hover_h$h2) != 0) {
        if (text_pastel$z == text_pastel$max) text_pastel$z <- 1
        else text_pastel$z <- text_pastel$z + 1
      }
    }
  })
  
  observeEvent(c(hover_h$h2 , text_pastel$z) , ignoreNULL = FALSE , {
    disable(id = 'pastel_texto')
    if (is.null(hover_h$h2)) {
      updateTextInput(session , inputId = 'pastel_texto' , value = '')
      text_pastel$n <- 0
      text_pastel$max <- 0}
    else if (nrow(hover_h$h2) == 0) {
      updateTextInput(session , inputId = 'pastel_texto' , value = '')
      text_pastel$n <- 0
      text_pastel$max <- 0}
    else {
      txt <- text_pastel$df[text_pastel$df$z == text_pastel$z,]$categoria
      updateTextInput(session , inputId = 'pastel_texto' , value = txt)
    }
  })
  
  output$grafica_pastel <- renderPlot({
    grafica <- ggplot()
    # =
    if (!is.null(hover_h$h2)) {
      if (nrow(hover_h$h2) != 0) {
        if (input$pastel_texto == 'PGJ' | input$tipo_grafica2 == 'PGJ') paleta <- c('#97173A' , '#7F2B5F')
        else if (input$pastel_texto == 'SSC' | input$tipo_grafica2 == 'SSC') paleta <- c('#006386' , '#008E97')
        else if (input$pastel_texto == 'C5' | input$tipo_grafica2 == 'C5') paleta <- c('#956F00', '#5D731D')
        else if (input$pastel_texto == 'AXA' | input$tipo_grafica2 == 'AXA') paleta <- c('#5E0061' , '#A1145F')
        else if (input$pastel_texto == 'Repubikla' | input$tipo_grafica2 == 'Repubikla') paleta <- c('#3F8500' , '#00735C')
        else paleta <- c('#D3D3D3' , '#808080')
        # =
        cat <- input$pastel_texto
        df <- hover_h$h2[hover_h$h2$categoria == cat,]
        eje_y <- c(sum(df[df$ref %in% c(2,3,4,5),]$n) , sum(df[df$ref %in% c(1,6,7),]$n))
        # =
        if (eje_y[1] == 0 & eje_y[2] == 0) {
          grafica <- grafica +
            geom_col(aes(x = 1 , y = 1 , fill = 'a')) +
            annotate('text' , x = 1 , y = 0.5 , label = 'Sin Datos') +
            scale_fill_manual(values = '#A9A9A9', limits = 'a')
        }
        else {
          grafica <- grafica +
            geom_col(aes(x = c(1,1) , y = eje_y , fill = c('Entre Semana','Fines de Semana'))) +
            annotate('text' , x = 1 , y = eje_y[1]/2 , label = paste0('Fines de Semana (' , round((eje_y[2]/sum(eje_y))*100 , 2) , '%)')) +
            annotate('text' , x = 1 , y = eje_y[1] + eje_y[2]/2 , label = paste0('Entre Semana (' , round((eje_y[1]/sum(eje_y))*100 , 2) , '%)')) +
            scale_fill_manual(values = paleta, limits = c('Entre Semana','Fines de Semana'))
        }
        # =
        grafica <- grafica +
          coord_flip() +
          guides(fill = FALSE) +
          theme_void()
      }
    }
    # =
    grafica
  })
  
  # ===== ZOOM GRÁFICA - POR DÍA Y HORA =====
  observeEvent(input$boton_zoom_grafica2 , {
    showModal(tags$div(id = 'div_modal_zoom2', style = 'font-size; 120%;',
                       modalDialog(size = 'l' , title = NULL , footer = NULL , easyClose = TRUE,
                                   plotOutput(outputId = 'grafica_modal_b' , height = '85vh',
                                              click = clickOpts(id = 'plot_click_modal_b')),
                                   uiOutput(outputId = 'click_info_modal_b'))))
  })
  output$grafica_modal_b <- renderPlot({
    graf_modal$g2 +
      theme(text = element_text(size = 18))
  })
  
  # ===== HOVER INFO =====
  output$click_info <- renderUI({
    hover <- input$plot_click
    k <- hover_h$h
    if (!is.null(k) & !is.null(hover)) {
      if (nrow(k) != 0) {
        point <- nearPoints(df = k, coordinfo = hover, threshold = 5, maxpoints = 1)
        if (nrow(point) == 0) return(NULL)
        # =
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        # =
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        # =
        style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                        "left:", left_px + 10, "px; top:", top_px + 140, "px;
                      padding: 5px 10px 0px; border-radius: 10px;")
        # =
        if (input$filtro_incidente == 'Decesos') palabra <- ' Decesos'
        else if (input$filtro_incidente == 'Lesionados') palabra <- ' Lesionados'
        else if (input$filtro_incidente == 'Accidentes') palabra <- ' Accidentes'
        else if (input$filtro_incidente == 'Todos') palabra <- ' Incidentes'
        # =
        if (point$n == 1) palabra <- substring(palabra , 0 , nchar(palabra) - 1)
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          tags$div(
            style = style,
            p(HTML(paste0('<b>', point$categoria ,'</b><br/>',
                          point$n , palabra)))
          )
        }
        else if (input$tiempo_grafica == 'Por Día') {
          fecha <- bd$df_referencia[bd$df_referencia$ref_dia == point$ref,]$fecha
          # =
          tags$div(
            style = style,
            p(HTML(paste0('<b>', point$categoria ,'</b><br/>',
                          fecha , '</b><br/>',
                          point$n , palabra)))
          )
        }
      }
      else NULL
    }
    else NULL
  })
  
  output$click_info_modal_a <- renderUI({
    hover <- input$plot_click_modal_a
    k <- hover_h$h
    if (!is.null(k)) {
      point <- nearPoints(df = k, coordinfo = hover, threshold = 5, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      # =
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      # =
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      # =
      style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                      "left:", left_px + 10, "px; top:", top_px - 40, "px;
                      padding: 5px 10px 0px; border-radius: 10px;
                      font-size: 125%;")
      # =
      if (input$filtro_incidente == 'Decesos') palabra <- ' Decesos'
      else if (input$filtro_incidente == 'Lesionados') palabra <- ' Lesionados'
      else if (input$filtro_incidente == 'Accidentes') palabra <- ' Accidentes'
      else if (input$filtro_incidente == 'Todos') palabra <- ' Incidentes'
      # =
      if (point$n == 1) palabra <- substring(palabra , 0 , nchar(palabra) - 1)
      # =
      if (input$tiempo_grafica == 'Por Mes') {
        tags$div(
          style = style,
          p(HTML(paste0('<b>', point$categoria ,'</b><br/>',
                        point$n , palabra)))
        )
      }
      else if (input$tiempo_grafica == 'Por Día') {
        fecha <- bd$df_referencia[bd$df_referencia$ref_dia == point$ref,]$fecha
        # =
        tags$div(
          style = style,
          p(HTML(paste0('<b>', point$categoria ,'</b><br/>',
                        fecha , '</b><br/>',
                        point$n , palabra)))
        )
      }
    } else NULL
  })
  
  output$click_info2 <- renderUI({
    hover <- input$plot_click2
    k <- hover_h$h2
    if (!is.null(k) & !is.null(hover)) {
      if (nrow(k) != 0) {
        # =====
        range <- c(NA , NA)
        if (hover$x > 0.5 & hover$x <= 1.5) range <- c(0.5 , 1.5)
        else if (hover$x > 1.5 & hover$x <= 2.5) range <- c(1.5 , 2.5)
        else if (hover$x > 2.5 & hover$x <= 3.5) range <- c(2.5 , 3.5)
        else if (hover$x > 3.5 & hover$x <= 4.5) range <- c(3.5 , 4.5)
        else if (hover$x > 4.5 & hover$x <= 5.5) range <- c(4.5 , 5.5)
        else if (hover$x > 5.5 & hover$x <= 6.5) range <- c(5.5 , 6.5)
        else if (hover$x > 6.5 & hover$x <= 7.5) range <- c(6.5 , 7.5)
        if (is.na(range[1])) return(NULL)
        else {
          intv <- 1/length(unique(k$categoria))
          df <- data.frame(cat = as.character() , pos = as.integer() , min = as.numeric() , max = as.numeric(), stringsAsFactors = FALSE)
          i <- 1
          for (cat in sort(unique(k$categoria))) {
            df[nrow(df) + 1,] <- c(cat , i , range[1] + (intv*(i-1)) , range[1] + (intv*i))
            i <- i + 1}
          for (i in 1:length(df$cat)) {
            if (hover$x > df$min[i] & hover$x <= df$max[i]) {
              ref <- (range[1] + range[2])/2
              cat <- df[df$pos == i,]$cat
              point <- k[k$categoria == cat & k$ref == ref,]
              z <- FALSE}}
        }
        if (nrow(point) == 0) return(NULL)
        # =====
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        # =
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        # =
        style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                        "left:", left_px + 10, "px; top:", top_px + 140, "px;
                      padding: 5px 10px 0px; border-radius: 10px;")
        # =
        if (input$filtro_incidente == 'Decesos') palabra <- ' Decesos'
        else if (input$filtro_incidente == 'Lesionados') palabra <- ' Lesionados'
        else if (input$filtro_incidente == 'Accidentes') palabra <- ' Accidentes'
        else if (input$filtro_incidente == 'Todos') palabra <- ' Incidentes'
        # =
        if (point$n == 1) palabra <- substring(palabra , 0 , nchar(palabra) - 1)
        # =
        if (point$n <= hover$y) NULL
        else {
          tags$div(
            style = style,
            p(HTML(paste0('<b>', point$categoria ,'</b><br/>',
                          point$n , palabra)))
          )
        }
      }
      else NULL
    }
    else NULL
  })
  
  output$click_info_modal_b <- renderUI({
    hover <- input$plot_click_modal_b
    k <- hover_h$h2
    if (!is.null(k) & !is.null(hover)) {
      # =====
      range <- c(NA , NA)
      if (hover$x > 0.5 & hover$x <= 1.5) range <- c(0.5 , 1.5)
      else if (hover$x > 1.5 & hover$x <= 2.5) range <- c(1.5 , 2.5)
      else if (hover$x > 2.5 & hover$x <= 3.5) range <- c(2.5 , 3.5)
      else if (hover$x > 3.5 & hover$x <= 4.5) range <- c(3.5 , 4.5)
      else if (hover$x > 4.5 & hover$x <= 5.5) range <- c(4.5 , 5.5)
      else if (hover$x > 5.5 & hover$x <= 6.5) range <- c(5.5 , 6.5)
      else if (hover$x > 6.5 & hover$x <= 7.5) range <- c(6.5 , 7.5)
      if (is.na(range[1])) return(NULL)
      else {
        intv <- 1/length(unique(k$categoria))
        df <- data.frame(cat = as.character() , pos = as.integer() , min = as.numeric() , max = as.numeric(), stringsAsFactors = FALSE)
        i <- 1
        for (cat in sort(unique(k$categoria))) {
          df[nrow(df) + 1,] <- c(cat , i , range[1] + (intv*(i-1)) , range[1] + (intv*i))
          i <- i + 1}
        for (i in 1:length(df$cat)) {
          if (hover$x > df$min[i] & hover$x <= df$max[i]) {
            ref <- (range[1] + range[2])/2
            cat <- df[df$pos == i,]$cat
            point <- k[k$categoria == cat & k$ref == ref,]
            z <- FALSE}}
      }
      if (nrow(point) == 0) return(NULL)
      # =====
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      # =
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      # =
      style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                      "left:", left_px + 10, "px; top:", top_px - 40, "px;
                      padding: 5px 10px 0px; border-radius: 10px;")
      # =
      if (input$filtro_incidente == 'Decesos') palabra <- ' Decesos'
      else if (input$filtro_incidente == 'Lesionados') palabra <- ' Lesionados'
      else if (input$filtro_incidente == 'Accidentes') palabra <- ' Accidentes'
      else if (input$filtro_incidente == 'Todos') palabra <- ' Incidentes'
      # =
      if (point$n == 1) palabra <- substring(palabra , 0 , nchar(palabra) - 1)
      # =
      if (point$n <= hover$y) NULL
      else {
        tags$div(
          style = style,
          p(HTML(paste0('<b>', point$categoria ,'</b><br/>',
                        point$n , palabra)))
        )
      }
    } else NULL
  })
  
  # ===== TABLA CON TOTALES =====
  # output$tabla_totales <- renderTable(striped = TRUE , digits = 0, spacing = 'xs' , {
  #   k <- NULL
  #   if (!is.null(hover_h$hk1)) k <- hover_h$hk1
  #   else if (!is.null(hover_h$hk2)) k <- hover_h$hk2
  #   # =
  #   if (is.null(k)) NULL
  #   else if (input$tipo_grafica == 'Gráficas Combinadas') {
  #     a <- data.frame(Fuente = unique(k$fuente))
  #     for (i in unique(k$etiqueta)) {
  #       a[i] <- filter(k , etiqueta == i)$n}
  #     # =
  #     tmp <- aggregate(k$n , by = list(fuente = k$fuente) , FUN = sum)
  #     orden <- c('PGJ' , 'SSC' , 'C5' , 'AXA' , 'Repubikla')
  #     tmp$fuente <- factor(as.character(tmp$fuente) , levels = orden)
  #     tmp <- tmp[order(tmp$fuente),]
  #     a['Total'] <- tmp$x
  #     # =
  #     a
  #   }
  # })
}

shinyApp(ui = ui, server = server)