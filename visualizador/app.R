# Visualizador de SEMOVI

# Cargar dependencias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, sf, tidyverse, leaflet, chron, lubridate, readxl, jsonlite,
shinycssloaders, shinydashboard, shinyjs, leaflet.extras, htmltools, janitor)

# Cargar funciones
funcs <- c("get_final_counts")
for (f in funcs) {
  if (!exists(f, mode = "function")) source("functions.R")
}


# library(shiny)
# library(sf)
# library(tidyverse)
# library(leaflet)

# library(chron)
# library(lubridate)
# library(readxl)
# library(jsonlite)
# library(shinycssloaders)

# library(shinydashboard)
# library(shinyjs)
# library(leaflet.extras)
# library(htmltools)

# ===== OPERACIONES INICIALES =====
cdmx <- read_sf(dsn = "data/cdmx.shp", layer = "cdmx")
cdmx_sa <- read_sf(dsn = "data/cdmx_sa.shp", layer = "cdmx_sa")
# ===== PGJ =====
pgj <- read.csv('data/PGJ.csv', encoding = 'UTF-8' , stringsAsFactors = FALSE)
# =
pgj['hora_de_hechos'] <- times(paste0(substr(pgj$fecha_hechos , 12 , 16), ':00'))
pgj['fecha_de_hechos'] <- dates(substr(pgj$fecha_hechos , 1 , 10) , format = 'y-m-d')
pgj['timestamp'] <- chron(pgj$fecha_de_hechos , pgj$hora_de_hechos)
pgj <- filter(pgj , timestamp >= dates('2016-01-01' , format = 'y-m-d'))
# =
pgj['geopoint'] <- NULL
pgj <- pgj[order(pgj$timestamp),]
pgj['id'] <- seq.int(nrow(pgj))
pgj <- filter(pgj , !is.na(latitud) & !is.na(longitud))
pgj <- filter(pgj , !is.na(timestamp))
# =
pgj <- st_transform(st_as_sf(pgj , coords = c('longitud','latitud') , crs = 4326), 32614)

# ===== SSC =====
ssc <- read.csv('data/SSC.csv' , sep = ',', encoding = 'UTF-8', stringsAsFactors = FALSE)
ssc <- clean_names(ssc , 'snake')
# =
ssc$fecha_evento <- dates(ssc$fecha_evento , format = 'y-m-d')
ssc$hora_evento <- times(paste0(ifelse(nchar(ssc$hora2) == 1 , paste0('0',ssc$hora2) , ssc$hora2) , ':' , ifelse(str_sub(ssc$hora_evento , -1) == '.' , substr(ssc$hora_evento , 4 , 5) , str_sub(ssc$hora_evento , -2)) , ':00'))
ssc$timestamp <- chron(ssc$fecha_evento , ssc$hora_evento)
# =
ssc['no_folio'] <- as.character(ssc$no_folio)
ssc$no_folio[ssc$no_folio == 'SD'] <- paste0('SinID_', seq(1:nrow(filter(ssc , no_folio == 'SD'))))
tmp <- filter(as.data.frame(table(ssc$no_folio) , stringsAsFactors = FALSE) , Freq > 1)
for (i in tmp$Var1) {
  ssc$no_folio[ssc$no_folio == i] <- paste0(i, '_' , seq(1:tmp[tmp$Var1 == i,]$Freq))
}
rm(tmp , i)
# =
ssc <- filter(ssc , !is.na(coordenada_y) & !is.na(coordenada_x))
ssc <- filter(ssc , !is.na(timestamp))
# =
ssc <- st_transform(st_as_sf(ssc , coords = c('coordenada_x','coordenada_y') , crs = 4326), 32614)

# ===== C5 =====
c5 <- read.csv('data/C5.csv' , sep = ',', encoding = 'UTF-8', stringsAsFactors = FALSE)
# =
c5['hora_creacion'] <- times(c5$hora_creacion)
c5['fecha_creacion'] <- dates(as.character(c5$fecha_creacion), format = 'd/m/y')
c5['timestamp'] <- chron(c5$fecha_creacion , c5$hora_creacion)
c5 <- filter(c5 , timestamp >= dates('2014-01-01' , format = 'y-m-d'))
# =
c5['geopoint'] <- NULL
c5 <- filter(c5 , !is.na(latitud) & !is.na(longitud))
c5 <- filter(c5 , !is.na(timestamp))
# =
c5 <- st_transform(st_as_sf(c5 , coords = c('longitud','latitud') , crs = 4326), 32614)

# ===== AXA =====
axa <- read.csv('data/AXA.csv' , sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE)
# =
axa['hora'] <- times(paste0(axa$hora, ':01:00'))
axa['mes'] <- as.character(axa$mes)
axa$mes[axa$mes == 'ENERO'] <- 1
axa$mes[axa$mes == 'FEBRERO'] <- 2
axa$mes[axa$mes == 'MARZO'] <- 3
axa$mes[axa$mes == 'ABRIL'] <- 4
axa$mes[axa$mes == 'MAYO'] <- 5
axa$mes[axa$mes == 'JUNIO'] <- 6
axa$mes[axa$mes == 'JULIO'] <- 7
axa$mes[axa$mes == 'AGOSTO'] <- 8
axa$mes[axa$mes == 'SEPTIEMBRE'] <- 9
axa$mes[axa$mes == 'OCTUBRE'] <- 10
axa$mes[axa$mes == 'NOVIEMBRE'] <- 11
axa$mes[axa$mes == 'DICIEMBRE'] <- 12
axa['fecha'] <- dates(paste0(axa$dia_numero , '/' , axa$mes , '/' , axa$ao) , format = 'd/m/y')
axa['timestamp'] <- chron(axa$fecha , axa$hora)
# =
axa['causa_siniestro'] <- as.character(axa$causa_siniestro)
axa$causa_siniestro[axa$causa_siniestro == '\\N'] <- paste0('SinID_', seq(1:nrow(filter(axa , causa_siniestro == '\\N'))))
axa <- filter(axa , !is.na(latitud) & !is.na(longitud))
axa <- filter(axa , !is.na(timestamp))
# =
axa <- st_transform(st_as_sf(axa , coords = c('longitud','latitud') , crs = 4326), 32614)

# ===== Repubikla =====
repubikla <- read.csv('data/repubikla.csv' , sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE)
# =
repubikla['hora'] <- times(paste0(as.character(repubikla$hora), ':00'))
repubikla['fecha'] <- dates(as.character(repubikla$fecha) , format = 'y-m-d')
repubikla['timestamp'] <- chron(repubikla$fecha , repubikla$hora)
repubikla <- filter(repubikla , !is.na(lat) & !is.na(lon))
repubikla <- filter(repubikla , !is.na(timestamp))
# =
repubikla <- st_transform(st_as_sf(repubikla , coords = c('lon','lat') , crs = 4326), 32614)
# ===== FINAL DE OPERACIONES INICIALES =====


# ===== FRONT END =====
ui <- dashboardPage(title = 'Visualizador de Datos de Incidentes Viales - SEMOVI', skin = 'green',
  dashboardHeader(title = 'Incidentes Viales'),
  # ===== SIDEBAR =====
  dashboardSidebar(sidebarMenu(id = 'menu_1',
                               menuItem(text = 'Introducción' , selected = TRUE , icon = icon('door-open') , tabName = 'introduccion'),
                               menuItem(text = 'Bases de Datos' , icon = icon('layer-group') , tabName = 'bd'),
                               menuItem(text = 'Visualizador', icon = icon('globe-americas') , tabName = 'visualizador'),
                               menuItem(text = 'Integración de Información' , icon = icon('question-circle') , tabName = 'instrucciones'))
                   ),
  dashboardBody(useShinyjs() , tags$head(tags$link(rel = 'stylesheet' , type = 'text/css' , href = 'custom.css?version=21')), tabItems(
    # ===== TAB INTRODUCCIÓN =====
    tabItem(tabName = 'introduccion' ,
            tags$div(style = 'width: 100%; height: 90vh; text-align: center; padding-top: 1vh;',
                     tags$div(style = 'background-color: white; margin: auto; width: 80%; padding: 30px; border-radius: 10px;',
                              tags$img(src = 'logo_semovi.png' , style = 'height: 100px;'),
                              tags$p(strong('Visualizador de Incidentes Viales') , style = 'font-size: 32pt; color: #848888; padding-bottom: 15px;'),
                              tags$div(style = 'text-align: justify; margin: auto; width: 90%; font-size: 12pt; color: #697070;',
                                       fluidRow(column(9,
                                                       tags$p(strong('Panorama General'), style = 'font-size: 18pt; color: #848888; text-align: left;'),
                                                       tags$p('Uno de los compromisos de la Secretaría de Movilidad de la Ciudad de México (SEMOVI) es entender las características de los hechos de tránsito que se suscitan en la ciudad, con el objetivo de planear estrategias de seguridad vial con base en evidencia. Por lo que a partir de la liberación de distintas fuentes de datos oficiales, la SEMOVI en colaboración con el Laboratorio de Datos Geoespaciales (DataLab) del Centro de Ciencias de Información Geoespacial (CentroGeo) y la Aseguradora AXA, se dieron a la tarea de desarrollar herramientas de visualización y manejo de información, para entender la dinámica espacial que siguen los hechos de tránsito.')),
                                                column(3,
                                                       tags$img(src = 'img/intro_a.jpg' , style = 'width: 100%;'))),
                                       tags$div(style = 'height: 20px; background-color: white;'),
                                       fluidRow(column(3,
                                                       tags$img(src = 'img/intro_b.jpg' , style = 'width: 100%;')),
                                                column(9,
                                                       tags$p(strong('Herramienta de Geovisualización'), style = 'font-size: 18pt; color: #848888; text-align: left;'),
                                                       tags$p('Esta herramienta permite a los usuarios explorar y analizar de forma interactiva, los datos disponibles de la Secretaría de Seguridad Ciudadana (SSC), la Procuraduría General de Justicia (PGJ), el Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México (C5), la aseguradora AXA y el proyecto colaborativo Repubikla.'),
                                                       tags$div(style = 'font-size: 24pt; text-align: right;',
                                                                actionButton(inputId = 'boton_ver_visualizador' , label = 'Ir a Visualizador' , icon = icon('globe-americas') , style = 'background-color: #00AA5A; color: white; border-color: ; font-size: 14pt;')
                                                                )
                                                       )),
                                       tags$div(style = 'height: 20px; background-color: white;'),
                                       fluidRow(column(9,
                                                       tags$p(strong('Herramienta de Integración de Información'), style = 'font-size: 18pt; color: #848888; text-align: left;'),
                                                       tags$p('En la Ciudad de México los datos de incidentes viales  son recopilados por diferentes instituciones gubernamentales basándose en los objetivos particulares de cada una de ellas, por lo que no existe una única fuente de datos. Para la SEMOVI es importante poder contar con un panorama general, que integre los datos de las diferentes instituciones. Por lo que se desarrolló una aplicación que permite explorar los datos para complementar e integrar registros en una sola fuente.'),
                                                       tags$div(style = 'font-size: 24pt; text-align: left;',
                                                                actionButton(inputId = 'boton_ver_integracion' , label = 'Información sobre Herramienta de Integración' , icon = icon('info-circle') , style = 'background-color: #00AA5A; color: white; border-color: ; font-size: 14pt;'))),
                                                column(3,
                                                       tags$img(src = 'img/intro_c.jpg' , style = 'width: 100%;')))
                                       ),
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
    tabItem(tabName = 'bd' ,
            fluidRow(column(6 , actionButton(inputId = 'boton_ver_visualizador3' , label = 'Regresar a Visualizador' , icon = icon('globe-americas') , style = 'background-color: #00AA5A; color: white; border-color: ; font-size: 12pt;'))),
            tags$div(style = 'height: 20px;'),
            box(width = 12,
                tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                         tags$p(strong('Bases de Datos'), style = 'font-size: 18pt; color: #848888; text-align: left;'),
                         tags$p('A continuación, encontrará información detallada sobre las Bases de Datos utilizadas en esta aplicación, a modo de conocer más a fondo su función original en el organismo generador y su utilidad para los objetivos de la SEMOVI.')),
                tabsetPanel(tabPanel(title = 'PGJ',
                                     fluidRow(column(6,
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$div(style = 'font-size: 18pt; color: #848888; text-align: left;',
                                                              tags$p(tags$img(src = 'fgj.png' , style = 'height: 85px; float: right;'),
                                                                     strong('Fiscalía General de Justicia (FGJ)'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Objetivo de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li('A través del Ministerio Público, tiene las atribuciones de investigar los delitos de orden común, y perseguir a los imputados. Promoviendo la pronta, expedita y debida procuración de Justicia. '),
                                                                      tags$li('Esta fuente recaba una gran parte de información que se complementa en el lugar de investigación como cada uno de los campos pertenecientes a su base de datos.'),
                                                                      tags$li('Esta fuente se considera fundamental debido a que un accidente vial puede resultar en la comisión de delitos como daños, lesiones o incluso homicidio (no intencional). Por lo cual le corresponderá a la FGJ tomar la investigación y con ello la recopilación de información.'),
                                                                      tags$li('Para lo anterior, el Ministerio Público quien coordina la investigación, recaba información de las entrevistas a víctimas o imputados, las policías o bien de los peritos.')),
                                                              tags$p(style = 'font-size: 10pt;',
                                                                     strong('Referencia') , ' – ' , tags$a('Ley Orgánica de la Procuraduría General de Justicia del Distrito Federal' , href = 'http://data.consejeria.cdmx.gob.mx/images/leyes/leyes/LEY_ORGANICA_DE_LA_PROCURADURIA_GRAL_DE_JUSTICIA_DEL_DF_1.pdf'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Información de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li(strong('Fuente') , ' – ' , tags$a('Datos Abiertos de la CDMX' , href = 'https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico/')),
                                                                      tags$li(strong('Número de Registros') , ' – ' , textOutput(outputId = 'pgj_cuantos' , inline = TRUE)),
                                                                      tags$li(strong('Periodo Temporal') , ' – ' , textOutput(outputId = 'pgj_cuando1' , inline = TRUE), ' a ' , textOutput(outputId = 'pgj_cuando2' , inline = TRUE)))),
                                                     tags$div(style = 'height: 15px;')
                                                     # tags$p(strong('Base de Datos Remota'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     # tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                     #          tags$p('"Local" trabaja con una versión almacenada en la aplicación; "Remota" utiliza la Base de Datos más reciente disponible en línea.'),
                                                     #          radioButtons(inputId = 'bd_pgj' , label = NULL, inline = TRUE,
                                                     #                       choiceNames = c('Local' , 'Remota'),
                                                     #                       choiceValues = c('A' , 'B')))
                                                     ),
                                              column(6,
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Diccionario de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     # ===== Tabla PGJ =====
                                                     tags$table(style = 'width: 100%; font-size: 10pt;' , class = 'diccionario',
                                                                tags$col(width = '17%'), tags$col(width = '58%'), tags$col(width = '25%'),
                                                                tags$tr(class = 'diccionario dicc_header' ,
                                                                        tags$th('Nombre de la Variable', class = 'diccionario dicc_center') , tags$th('Descripción', class = 'diccionario dicc_center') , tags$th('Tipo o Categorías', class = 'diccionario dicc_center')),
                                                                tags$tr(tags$td('ao_hechos', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Año en que ocurrió el hecho', class = 'diccionario'),
                                                                        tags$td('Entero', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('mes_hechos', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Mes en el que ocurrió el hecho', class = 'diccionario'),
                                                                        tags$td('Texto', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('fecha_hechos', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Día, mes, año y hora en el que ocurrió el hecho', class = 'diccionario'),
                                                                        tags$td('Texto, en formato “aaaa-mm-dd hh:mm” (24 hrs)', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('ao_inicio', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Año en el cual se abrió la carpeta de investigación', class = 'diccionario'),
                                                                        tags$td('Entero', class = 'diccionario')),
                                                                tags$tr(tags$td('mes_inicio', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Mes en el cual se abrió la carpeta de investigación', class = 'diccionario'),
                                                                        tags$td('Texto', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('fecha_inicio' , class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Día, mes, año y hora en el cual se abrió la carpeta de investigación', class = 'diccionario'),
                                                                        tags$td('Texto, en formato “aaaa-mm-dd hh:mm” (24 hrs)', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('delito', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Tipo penal con base en Código Penal de la CDMX' , class = 'diccionario'),
                                                                        tags$td('8 Tipos Penales (', tags$span(id = 'bd_delitos-pgj' , tags$u('Ver') , style = 'color: #00AA5A;'), ')' , class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('fiscalia', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Entidad pública encargada de la investigación' , class = 'diccionario'),
                                                                        tags$td('24 fiscalías' , class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('agencia' , class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Clave de la agencia encargada de la investigación' , class = 'diccionario'),
                                                                        tags$td('89 agencias' , class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('unidad_investigacion' , class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Clave con unidad de investigación detallando si existieron detenidos' , class = 'diccionario'),
                                                                        tags$td('20 claves' , class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('categoria_delito' , class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Categoría del delito con base en Código Penal de la CDMX' , class = 'diccionario'),
                                                                        tags$td('1 categoría (Delito de Bajo Impacto)' , class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('calle_hechos', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Nombre de la calle del hecho', class = 'diccionario'),
                                                                        tags$td('12,336 calles', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('calle_hechos2', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Segunda referencia al lugar donde ocurrieron los hechos', class = 'diccionario'),
                                                                        tags$td('8,581 calles', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('colonia_hechos', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Nombre de la colonia del hecho', class = 'diccionario'),
                                                                        tags$td('1,370 colonias', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('alcaldia_hechos', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Nombre de la alcaldia del hecho', class = 'diccionario'),
                                                                        tags$td('16 alcaldías', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('longitud', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Coordenada X', class = 'diccionario'),
                                                                        tags$td('Numérico', class = 'diccionario'),
                                                                        class = 'diccionario'),
                                                                tags$tr(tags$td('latitud', class = 'diccionario dicc_center dicc_rndm'),
                                                                        tags$td('Coordenada Y', class = 'diccionario'),
                                                                        tags$td('Numérico', class = 'diccionario'),
                                                                        class = 'diccionario')
                                                                )
                                                     # =====
                                                     ))),
                            tabPanel(title = 'SSC',
                                     fluidRow(column(6,
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$div(style = 'font-size: 18pt; color: #848888; text-align: left;',
                                                              tags$p(tags$img(src = 'ssc.png' , style = 'height: 85px; float: right;'),
                                                                     strong('Secretaría de Seguridad Ciudadana (SSC)'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Objetivo de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li('Sus atribuciones en la Ciudad de México, encaminan las acciones dirigidas a salvaguardar la integridad y patrimonio de las personas, prevenir la comisión de delitos e infracciones a las disposiciones gubernativas y de policía, así como a preservar las libertades, el orden y la paz públicos.'),
                                                                      tags$li('La información que tiene la SSC está en función a la atención que se brinda a través de la policía, por lo cual pueden allegarse de información que sólamente es posible recuperar cuando el incidente vial es atendido en campo.'),
                                                                      tags$li('En la cadena de operaciones, son el primer contacto físico con la ciudadanía que se encuentre involucrada en un accidente vial, por lo cual tienen la facilidad de recabar información casi al momento del evento.'),
                                                                      tags$li('A través de esta atención que brindan, los policías, obtienen información del accidente, la cual se pasa a través de formatos homologados a las áreas correspondientes de captura.')),
                                                              tags$p(style = 'font-size: 10pt;',
                                                                     strong('Referencia') , ' – ' , tags$a('Ley Orgánica de la Secretaría de Seguridad Pública del Distrito Federal' , href = 'http://data.consejeria.cdmx.gob.mx/images/leyes/leyes/LEY_ORGANICA_DE_LA_SECRETARIA_DE_SEGURIDAD_PUBLICA_DEL_DF_1.pdf'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Información de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li(strong('Fuente') , ' – ' , tags$a('Datos Abiertos de la CDMX' , href = 'https://datos.cdmx.gob.mx/explore/dataset/hechos-de-transito-registrados-por-la-ssc-serie-para-comparaciones-interanuales-')),
                                                                      tags$li(strong('Número de Registros') , ' – ' , textOutput(outputId = 'ssc_cuantos' , inline = TRUE)),
                                                                      tags$li(strong('Periodo Temporal') , ' – ' , textOutput(outputId = 'ssc_cuando1' , inline = TRUE), ' a ' , textOutput(outputId = 'ssc_cuando2' , inline = TRUE)))),
                                                     tags$div(style = 'height: 15px;')
                                     ),
                                     column(6,
                                            tags$div(style = 'height: 15px;'),
                                            tags$p(strong('Diccionario de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                            # ===== Tabla SSC =====
                                            tags$table(style = 'width: 100%; font-size: 10pt;' , class = 'diccionario',
                                                       tags$col(width = '17%'), tags$col(width = '58%'), tags$col(width = '25%'),
                                                       tags$tr(class = 'diccionario dicc_header' ,
                                                               tags$th('Nombre de la Variable', class = 'diccionario dicc_center') , tags$th('Descripción', class = 'diccionario dicc_center') , tags$th('Tipo o Categorías', class = 'diccionario dicc_center')),
                                                       tags$tr(tags$td('no_folio', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Número de folio único asignado a cada registro', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('fecha_evento', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Fecha en la cual ocurrió el incidente vial', class = 'diccionario'),
                                                               tags$td('Texto en formato "aa-mm-dd"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('año_evento', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Año en el cual ocurrió el incidente vial', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('mes_evento', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Mes en el cual ocurrió el incidente vial', class = 'diccionario'),
                                                               tags$td('12 meses', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('hora_evento', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Hora en la cual ocurrió el incidente vial', class = 'diccionario'),
                                                               tags$td('Texto en formato "hh:mm"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('condicion', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Detalla si la víctima principal resultó lesionada o falleció en el incidente', class = 'diccionario'),
                                                               tags$td('Texto (Lesionado y Occiso)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('tipo_evento', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción del tipo de incidente vial ocurrido', class = 'diccionario'),
                                                               tags$td('6 Tipos de Eventos (', tags$span(id = 'bd_evento-ssc' , tags$u('Ver') , style = 'color: #00AA5A;'), ')' , class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('coordenada_x', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Longitud del incidente', class = 'diccionario'),
                                                               tags$td('Numérico', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('coordenada_y', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Latitud del incidente', class = 'diccionario'),
                                                               tags$td('Numérico', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('punto_1', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Calle de referencia en la cual ocurrió el incidente', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('punto_2', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Calle secundaria de referencia en la cual ocurrió el incidente', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('colonia', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Colonia dentro de la cual ocurrió el incidente', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('alcaldia', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Alcaldía dentro de la cual ocurrió el incidente', class = 'diccionario'),
                                                               tags$td('16 Alcaldías', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('tipo_interseccion', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Intersección sobre la cual ocurrió el incidente', class = 'diccionario'),
                                                               tags$td('Texto (Cruz, Curva, Desnivel, Gaza, Glorieta, Ramas Múltiples, Recta, T o Y)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('tipo_vehiculo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción de los vehículos involucrados en el incidente. Cada uno se encuentra descrito en columnas diferentes', class = 'diccionario'),
                                                               tags$td('15 Tipos de Vehículos (', tags$span(id = 'bd_vehiculo-ssc' , tags$u('Ver') , style = 'color: #00AA5A;'), ')' , class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('marca_vehiculo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Marca de los vehículos involucrados en el incidente. Cada marca se encuentra descrita en columnas diferentes', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('lesiones', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción detallada de las lesiones sufridas por la víctima principal del incidente', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('Edades de Occisos / Lesionados', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Conjunto de columnas que describen las edades', class = 'diccionario'),
                                                               tags$td('15 Tipos de Vehículos (', tags$span(id = 'bd_vehiculo-ssc' , tags$u('Ver') , style = 'color: #00AA5A;'), ')' , class = 'diccionario'),
                                                               class = 'diccionario')
                                            )
                                            # =====
                                     ))),
                            tabPanel(title = 'C5',
                                     fluidRow(column(6,
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$div(style = 'font-size: 18pt; color: #848888; text-align: left;',
                                                              tags$p(tags$img(src = 'c5.png' , style = 'height: 85px; float: right;'),
                                                                     strong('Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México (C5)'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Objetivo de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li('Entre las atribuciones que tiene son las de proveer información a la Jefa de Gobierno para la oportuna e inmediata toma de decisiones, a través de video monitoreo de la ciudad, la administración del Servicio de Atención de llamadas de emergencia 9-1-1 CDMX, así como Denuncia Anónima 089 y LOCATEL.'),
                                                                      tags$li('La información que se tiene corresponde a los reportes hechos por la ciudadanía directamente, por lo cual la información que puede ser pública es la fecha y hora, ubicación, la clasificación interna que se realiza. Existe una alta probabilidad de que esta institución tenga más reportes que las otras, debido a que algunos de estos, no tienen continuidad ante la policía o la PGJ, claro está, cuando no hay personas fallecidas o lesionadas.'),
                                                                      tags$li('La integración de C5, ayuda a reducir la brecha de reportes, ya que suele ser el primer contacto no físico con alguna autoridad ante un accidente vial.'),
                                                                      tags$li('Para ello, las y los operadores de C5 toman la información directa de la ciudadanía y realizan una clasificación de los eventos.')),
                                                              tags$p(style = 'font-size: 10pt;',
                                                                     strong('Referencia') , ' – ' , tags$a('Manual Administrativo del C5' , href = 'https://www.c5.cdmx.gob.mx/storage/app/uploads/public/5be/b2e/318/5beb2e31874de742733714.pdf'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Información de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li(strong('Fuente') , ' – ' , tags$a('Datos Abiertos de la CDMX' , href = 'https://datos.cdmx.gob.mx/explore/dataset/incidentes-viales-c5')),
                                                                      tags$li(strong('Número de Registros') , ' – ' , textOutput(outputId = 'c5_cuantos' , inline = TRUE)),
                                                                      tags$li(strong('Periodo Temporal') , ' – ' , textOutput(outputId = 'c5_cuando1' , inline = TRUE), ' a ' , textOutput(outputId = 'c5_cuando2' , inline = TRUE)))),
                                                     tags$div(style = 'height: 15px;')
                                     ),
                                     column(6,
                                            tags$div(style = 'height: 15px;'),
                                            tags$p(strong('Diccionario de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                            # ===== Tabla C5 =====
                                            tags$table(style = 'width: 100%; font-size: 10pt;' , class = 'diccionario',
                                                       tags$col(width = '17%'), tags$col(width = '58%'), tags$col(width = '25%'),
                                                       tags$tr(class = 'diccionario dicc_header' ,
                                                               tags$th('Nombre de la Variable', class = 'diccionario dicc_center') , tags$th('Descripción', class = 'diccionario dicc_center') , tags$th('Tipo o Categorías', class = 'diccionario dicc_center')),
                                                       tags$tr(tags$td('folio', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Código único alfa numérico que se la asigna a cada uno de los incidentes, compuesto por dos iniciales del Centro que recibió la emergencia, fecha en formato AA/MM/DD y número consecutivo de ingreso', class = 'diccionario'),
                                                               tags$td('Texto Alfanumérico Variable', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('fecha_creacion', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Fecha de apertura del folio del evento', class = 'diccionario'),
                                                               tags$td('Fecha en formato "aaaa-mm-dd"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('hora_creacion', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Hora de apertura del folio del evento', class = 'diccionario'),
                                                               tags$td('Hora en formato "hh:mm:ss"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('dia_semana', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Día de apertura del folio', class = 'diccionario'),
                                                               tags$td('7 días', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('fecha_cierre', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Fecha de cierre del folio del evento', class = 'diccionario'),
                                                               tags$td('Fecha en formato "aaaa-mm-dd"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('año_cierre', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Año de cierre del folio del evento', class = 'diccionario'),
                                                               tags$td('Número Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('mes_cierre', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Mes de cierre del folio del evento', class = 'diccionario'),
                                                               tags$td('12 meses', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('hora_cierre', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Hora de cierre del folio del evento', class = 'diccionario'),
                                                               tags$td('Hora en formato "hh:mm:ss"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('delegacion_inicio', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Alcaldía donde inicialmente se reportó el incidente', class = 'diccionario'),
                                                               tags$td('16 alcaldías', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('incidente_c4', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Tipo de incidente', class = 'diccionario'),
                                                               tags$td('21 Incidentes Posibles (', tags$span(id = 'bd_incidentes-c5' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('latitud', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Coordenada Y', class = 'diccionario'),
                                                               tags$td('Numérico', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('longitud', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Coordenada X', class = 'diccionario'),
                                                               tags$td('Numérico', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('codigo_cierre', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Código que fue asignado al incidente en el cierre', class = 'diccionario'),
                                                               tags$td('Texto, 2 Categorías (', tags$span(id = 'bd_cierre-c5' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('clas_con_f_alarma', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Clasificación del Incidente', class = 'diccionario'),
                                                               tags$td('Texto, 4 Categorías (', tags$span(id = 'bd_clas-c5' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('tipo_entrada', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Medio por el cual se dio aviso del incidente', class = 'diccionario'),
                                                               tags$td('Texto, 6 Categorías (', tags$span(id = 'bd_entrada-c5' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('delegacion_cierre', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Alcaldía donde cierra el folio del incidente', class = 'diccionario'),
                                                               tags$td('16 alcaldías', class = 'diccionario'),
                                                               class = 'diccionario')
                                            )
                                            # =====
                                     ))),
                            tabPanel(title = 'AXA',
                                     fluidRow(column(6,
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$div(style = 'font-size: 18pt; color: #848888; text-align: left;',
                                                              tags$p(tags$img(src = 'axa.png' , style = 'height: 85px; float: right;'),
                                                                     strong('AXA México'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Objetivo de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li('Es una aseguradora multirramo de origen francés con presencia en 64 países'),
                                                                      tags$li('Esta fuente, contiene información referente a sus clientes asegurados, sobre los datos generales, así como un desglose sobre causas y entorno que intervino en el accidente'),
                                                                      tags$li('A través de “Fundación Axa” se suman generando un convenio de colaboración para la integración de su información, cuidando como siempre, la protección de datos personales de sus clientes'),
                                                                      tags$li('El personal que atiende a sus asegurados, se recopila la información la cual es capturada en un sistema de registro'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Información de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li(strong('Fuente') , ' – ' , tags$a('Instituto Internacional de Ciencia de Datos' , href = 'http://i2ds.org/datos-abiertos-percances-viales/')),
                                                                      tags$li(strong('Número de Registros') , ' – ' , textOutput(outputId = 'axa_cuantos' , inline = TRUE)),
                                                                      tags$li(strong('Periodo Temporal') , ' – ' , textOutput(outputId = 'axa_cuando1' , inline = TRUE), ' a ' , textOutput(outputId = 'axa_cuando2' , inline = TRUE)))),
                                                     tags$div(style = 'height: 15px;')
                                     ),
                                     column(6,
                                            tags$div(style = 'height: 15px;'),
                                            tags$p(strong('Diccionario de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                            # ===== Tabla AXA =====
                                            tags$table(style = 'width: 100%; font-size: 10pt;' , class = 'diccionario',
                                                       tags$col(width = '17%'), tags$col(width = '58%'), tags$col(width = '25%'),
                                                       tags$tr(class = 'diccionario dicc_header' ,
                                                               tags$th('Nombre de la Variable', class = 'diccionario dicc_center') , tags$th('Descripción', class = 'diccionario dicc_center') , tags$th('Tipo o Categorías', class = 'diccionario dicc_center')),
                                                       tags$tr(tags$td('siniestro', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Identificador único del percance vial', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('calle', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Nombre de la calle donde ocurrió el siniestro', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('colonia', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Nombre de la colonia donde ocurrió el siniestro', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('codigo_postal', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Ubicación postal del percance vial', class = 'diccionario'),
                                                               tags$td('Texto (Número Entero que puede iniciar con cero)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('alcaldia', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Nombre de la alcaldia donde ocurrió el siniestro', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('causa_siniestro', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción del tipo de percance ocurrido', class = 'diccionario'),
                                                               tags$td('7 Causas Posibles (', tags$span(id = 'bd_siniestro-axa' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('tipo_vehiculo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Vehículo involucrado en el percance vial', class = 'diccionario'),
                                                               tags$td('4 Vehículos Posibles (', tags$span(id = 'bd_vehiculo-axa' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('color', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Color del vehículo asegurado', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('modelo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Año del vehículo asegurado', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('nivel_dano_vehiculo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Grado de daño al vehículo en el percance vial', class = 'diccionario'),
                                                               tags$td('Texto (Alto, Medio, Bajo y Sin Daños)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('punto_impacto', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Parte del vehículo donde ocurrió el daño', class = 'diccionario'),
                                                               tags$td('Texto (Frontal, Trasero, Lateral Derecho o Lateral Izquierdo)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('ao', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Año en que se registró el percance vial. No necesariamente corresponde al año de ocurrencia', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('mes', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Mes en que registró el percance vial. No necesariamente corresponde al mes de ocurrencia', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('dia_numero', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Día en que registró el percance. No necesariamente correpsonde al día de ocurrencia', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('dia', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Día de la semana en la que se registró el percance', class = 'diccionario'),
                                                               tags$td('7 días de la semana', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('hora', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Hora de ocurrencia del percance vial. No necesariamente correpsonde a la hora de ocurrencia', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('lesionados', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Cantidad de personas lesionadas en el siniestro', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('edad_lesionado', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Corresponde a la edad de cada lesionado', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('relacion_lesionados', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Rol que posee el lesionado del percance, especificando si es asegurado o no', class = 'diccionario'),
                                                               tags$td('6 Roles Posibles (', tags$span(id = 'bd_rol-axa' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('genero_lesionado', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Corresponde al género de cada lesionado', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('nivel_lesionado', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Grado de la lesión sufrida', class = 'diccionario'),
                                                               tags$td('Texto (Alto, Medio, Bajo y Sin Lesión)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('hospitalizado', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Confirma si cada lesionado fue o no hospitalizado', class = 'diccionario'),
                                                               tags$td('Booleano', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('fallecido', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Confirma si cada lesionado falleció o no en el siniestro', class = 'diccionario'),
                                                               tags$td('Booleano', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('Variables Booleanas', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Conjunto de datos que describren a detalle las variables involucradas en el incidente', class = 'diccionario'),
                                                               tags$td('18 Variables (', tags$span(id = 'bd_boolean-axa' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario')
                                            )
                                            # =====
                                     ))),
                            tabPanel(title = 'Repubikla',
                                     fluidRow(column(6,
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$div(style = 'font-size: 18pt; color: #848888; text-align: left;',
                                                              tags$p(tags$img(src = 'repubikla2.png' , style = 'height: 85px; float: right;'),
                                                                     strong('Repubikla'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Objetivo de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li('Repubikla es una plataforma de mapeo alimentada por ciudadanos que con una estrategia de crowdsourcing busca generar y centralizar datos sobre la movilidad no motorizada en las ciudades.'),
                                                                      tags$li('Esta fuente cuenta con información general sobre los eventos, con un enfoque ciudadano.'),
                                                                      tags$li('De esta forma, se fomenta el empoderamiento de la ciudadanía los procesos de gobernanza, y la plena participación en la generación de recursos para la toma de decisiones.'))),
                                                     tags$div(style = 'height: 15px;'),
                                                     tags$p(strong('Información de la Base de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                                     tags$div(style = 'text-align: justify; font-size: 12pt; color: #697070;',
                                                              tags$ul(tags$li(strong('Fuente') , ' – ' , tags$a('Página Oficial Repubikla' , href = 'https://repubikla.herokuapp.com/')),
                                                                      tags$li(strong('Número de Registros') , ' – ' , textOutput(outputId = 'repubikla_cuantos' , inline = TRUE)),
                                                                      tags$li(strong('Periodo Temporal') , ' – ' , textOutput(outputId = 'repubikla_cuando1' , inline = TRUE), ' a ' , textOutput(outputId = 'repubikla_cuando2' , inline = TRUE)))),
                                                     tags$div(style = 'height: 15px;')
                                     ),
                                     column(6,
                                            tags$div(style = 'height: 15px;'),
                                            tags$p(strong('Diccionario de Datos'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                                            # ===== Tabla Repubikla =====
                                            tags$table(style = 'width: 100%; font-size: 10pt;' , class = 'diccionario',
                                                       tags$col(width = '17%'), tags$col(width = '58%'), tags$col(width = '25%'),
                                                       tags$tr(class = 'diccionario dicc_header' ,
                                                               tags$th('Nombre de la Variable', class = 'diccionario dicc_center') , tags$th('Descripción', class = 'diccionario dicc_center') , tags$th('Tipo o Categorías', class = 'diccionario dicc_center')),
                                                       tags$tr(tags$td('id', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Identificador único del reporte', class = 'diccionario'),
                                                               tags$td('Entero', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('comentario', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Comentario a través del cual el incidente fue reportado. Puede provenir del título de una noticia, contenido de un tweet, entre otros', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('tipo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Tipo de incidente registrado', class = 'diccionario'),
                                                               tags$td('Texto (Incidente Vial)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('fecha', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Fecha en la cual se realizó el reporte del incidente', class = 'diccionario'),
                                                               tags$td('Texto en formato "dd/mm/aa"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('hora', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Hora en la cual se realizó el reporte del incidente', class = 'diccionario'),
                                                               tags$td('Texto en formato "hh:mm"', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('calle_1', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Referencia del lugar en el cual ocurrió el incidente vial', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('calle_2', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Referencia complementaria del lugar en el cual ocurrió el incidente vial', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('modo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción de la víctima principal del incidente', class = 'diccionario'),
                                                               tags$td('5 Modos posibles (', tags$span(id = 'bd_modo-repubikla' , tags$u('Ver') , style = 'color: #00AA5A;') , ')', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('fuente', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Hipervínculo directo a la fuente de donde se obtuvo el registro del incidente (Twitter, Blog de Noticias, etc.)', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('responsable', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción detallada del vehículo o instrumento responsable del incidente vial', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('gravedad', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción detallada del nivel de las lesiones sufridas por la víctima principal', class = 'diccionario'),
                                                               tags$td('Texto (Leve, Moderada, Grave, Ambulancia Requerida y Mortal)', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('victima_sexo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Sexo de la víctima principal del incidente', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('victima_edad', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Edad de la víctima principal del incidente', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('seguimiento', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción detallada de los suscesos ocurridos posterior al incidente.', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('placa', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Placa del vehículo respondable del incidente vial', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('vehiculo', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Descripción más detallada del vehículo responsable del incidente vial', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario'),
                                                       tags$tr(tags$td('condicion', class = 'diccionario dicc_center dicc_rndm'),
                                                               tags$td('Detalla si el responsable se encontraba en estado de ebriedad o no', class = 'diccionario'),
                                                               tags$td('Texto', class = 'diccionario'),
                                                               class = 'diccionario')
                                            )
                                            # =====
                                     ))))
                )
            ),
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
            tags$div(style = 'text-align: left;',
                     fluidRow(column(6 , actionButton(inputId = 'boton_ver_instrucciones' , label = 'Ver Instrucciones' , icon = icon('question-circle') , style = 'background-color: #00AA5A; color: white; border-color: ; font-size: 12pt;'),
                                     actionButton(inputId = 'boton_ver_bd2' , label = 'Descripción de Bases de Datos' , icon = icon('layer-group') , style = 'background-color: #00AA5A; color: white; border-color: ; font-size: 12pt;'),
                                     actionButton(inputId = 'boton_ver_integracion2' , label = 'Información sobre Herramienta de Integración' , icon = icon('info-circle') , style = 'background-color: #00AA5A; color: white; border-color: ; font-size: 12pt;'))
                              )),
            tags$div(id = 'berenjena', style = 'display: none; text-align: center;' ,
                     tags$div(style = 'height: 20px;'),
                     box(width = 12 ,
                         fluidRow(column(5 ,
                                         fluidRow(column(2 , actionButton(inputId = 'l_instrucciones' , label = '' , icon = icon('angle-left'))),
                                                  column(8 , tags$p(textOutput(outputId = 'pagina_instrucciones' , inline = TRUE) , '/ 8')),
                                                  column(2 , actionButton(inputId = 'r_instrucciones' , label = '' , icon = icon('angle-right')))),
                                         tags$div(style = 'height: 20px;'),
                                         tags$p(strong(textOutput(outputId = 'titulo_instrucciones' , inline = TRUE)), style = 'font-size: 18pt; color: #848888; text-align: left;'),
                                         tags$p(htmlOutput(outputId = 'desc_instrucciones' , inline = TRUE) , style = 'text-align: justify; font-size: 12pt; color: #697070;'),
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
  
  observeEvent(input$boton_ver_integracion , updateTabItems(session , inputId = 'menu_1' , selected = 'instrucciones'))
  
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
  observeEvent(input$boton_ver_visualizador3 , updateTabItems(session , inputId = 'menu_1' , selected = 'visualizador'))
  
  output$pgj_cuantos <- renderText(format(nrow(special_bd$active_pgj) , big.mark = ','))
  output$pgj_cuando1 <- renderText(str_to_title(format(range(special_bd$active_pgj$timestamp)[1] , format = '%d/%B/%Y') , locale = 'es'))
  output$pgj_cuando2 <- renderText(str_to_title(format(range(special_bd$active_pgj$timestamp)[2] , format = '%d/%B/%Y') , locale = 'es'))
  
  onclick(id = 'bd_delitos-pgj' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Tipos Penales registrados por PGJ'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Daño en Propiedad Ajena Culposa por Tránsito Vehicular a Automóvil'),
                                  tags$li('Daño en Propiedad Ajena Culposa por Tránsito Vehicular a Bienes Inmuebles'),
                                  tags$li('Lesiones Culposas por Tránsito Vehicular'),
                                  tags$li('Homicidio Culposo por Tránsito Vehicular'),
                                  tags$li('Homicidio Culposo por Tránsito Vehicular (Colisión)'),
                                  tags$li('Homicidio Culposo por Tránsito Vehicular (Atropellado)'),
                                  tags$li('Homicidio Culposo por Tránsito Vehicular (Caída)'))
                          ))
  })
  
  output$ssc_cuantos <- renderText(format(nrow(ssc) , big.mark = ','))
  output$ssc_cuando1 <- renderText(str_to_title(format(range(ssc$timestamp)[1] , format = '%d/%B/%Y') , locale = 'es'))
  output$ssc_cuando2 <- renderText(str_to_title(format(range(ssc$timestamp)[2] , format = '%d/%B/%Y') , locale = 'es'))
  
  onclick(id = 'bd_evento-ssc' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Tipos Penales registrados por PGJ'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Atropellado'),
                                  tags$li('Caída de Ciclista'),
                                  tags$li('Caída de Pasajero'),
                                  tags$li('Choque'),
                                  tags$li('Derrapado'),
                                  tags$li('Volcadura'))
    ))
  })
  onclick(id = 'bd_vehiculo-ssc' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Tipos Penales registrados por PGJ'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Autobús de Pasajeros'),
                                  tags$li('Automóvil'),
                                  tags$li('Bicicleta'),
                                  tags$li('Camión de Carga'),
                                  tags$li('Camioneta'),
                                  tags$li('Ferrocarril'),
                                  tags$li('Metrobús'),
                                  tags$li('Microbús'),
                                  tags$li('Motocicleta'),
                                  tags$li('Objeto Fijo'),
                                  tags$li('Taxi'),
                                  tags$li('Tren'),
                                  tags$li('Tren Ligero'),
                                  tags$li('Tren Suburbano'),
                                  tags$li('Trolebús'))
    ))
  })
  
  output$c5_cuantos <- renderText(format(nrow(special_bd$active_c5) , big.mark = ','))
  output$c5_cuando1 <- renderText(str_to_title(format(range(special_bd$active_c5$timestamp)[1] , format = '%d/%B/%Y') , locale = 'es'))
  output$c5_cuando2 <- renderText(str_to_title(format(range(special_bd$active_c5$timestamp)[2] , format = '%d/%B/%Y') , locale = 'es'))
  
  onclick(id = 'bd_incidentes-c5' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Tipos de Incidentes Viales registrados por C5'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('accidente-choque con lesionados'),
                                  tags$li('accidente-choque con prensados'),
                                  tags$li('accidente-choque sin lesionados'),
                                  tags$li('accidente-ciclista'),
                                  tags$li('accidente-ferroviario'),
                                  tags$li('accidente-monopatín'),
                                  tags$li('accidente-motociclista'),
                                  tags$li('accidente-otros'),
                                  tags$li('accidente-persona atrapada/ desbarrancada'),
                                  tags$li('accidente-vehiculo atrapado'),
                                  tags$li('accidente-vehiculo atrapado-varado'),
                                  tags$li('accidente-vehiculo desbarrancado'),
                                  tags$li('accidente-volcadura'),
                                  tags$li('cadáver-accidente automovilístico'),
                                  tags$li('cadáver-atropellado'),
                                  tags$li('detención ciudadana-accidente automovilístico'),
                                  tags$li('detención ciudadana-atropellado'),
                                  tags$li('lesionado-atropellado'),
                                  tags$li('sismo-choque con lesionados'),
                                  tags$li('sismo-choque con prensados'),
                                  tags$li('sismo-choque sin lesionados'),
                                  tags$li('sismo-persona atropellada'))
    ))
  })
  onclick(id = 'bd_cierre-c5' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Códigos de Cierre para Incidentes Viales registrados por C5'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li(strong('Afirmativo (A)') , ' - La unidad de atención a emergencias fue despachada, llegó al lugar de los hechos y confirmó la emergencia reportada'),
                                  tags$li(strong('Informativo (I)') , ' - El incidente reportado es afirmativo y se añade información adicional al evento'))
    ))
  })
  onclick(id = 'bd_clas-c5' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Códigos de Cierre para Incidentes Viales registrados por C5'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Emergencia'),
                                  tags$li('Urgencias Médicas'),
                                  tags$li('Falsa Alarma'),
                                  tags$li('Delito'))
    ))
  })
  onclick(id = 'bd_entrada-c5' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Códigos de Cierre para Incidentes Viales registrados por C5'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Botón de Auxilio'),
                                  tags$li('Cámara'),
                                  tags$li('Llamada App 911'),
                                  tags$li('Llamada del 911'),
                                  tags$li('Radio'),
                                  tags$li('Redes'))
    ))
  })
  
  output$axa_cuantos <- renderText(format(nrow(axa) , big.mark = ','))
  output$axa_cuando1 <- renderText(str_to_title(format(range(axa$timestamp)[1] , format = '%d/%B/%Y') , locale = 'es'))
  output$axa_cuando2 <- renderText(str_to_title(format(range(axa$timestamp)[2] , format = '%d/%B/%Y') , locale = 'es'))
  
  onclick(id = 'bd_siniestro-axa' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Causas de Siniestros Viales registrados por AXA'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Atropello'),
                                  tags$li('Colisión y/o Vuelco'),
                                  tags$li('Daños por la Carga'),
                                  tags$li('Fenómenos de la Naturaleza'),
                                  tags$li('Huelgas y Alborotos'),
                                  tags$li('Incendio, Rayo o Explosión'),
                                  tags$li('Transportación'))
    ))
  })
  onclick(id = 'bd_vehiculo-axa' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Tipo de Vehículos en Siniestros registrados por AXA'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Auto'),
                                  tags$li('Camión'),
                                  tags$li('Camión Ligero'),
                                  tags$li('Motocicleta'))
    ))
  })
  onclick(id = 'bd_rol-axa' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Rol de Lesionados en Siniestros registrados por AXA'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Conductor Asegurado'),
                                  tags$li('Pasajero Asegurado'),
                                  tags$li('Tercero Conductor'),
                                  tags$li('Tercero Pasajero'),
                                  tags$li('Tercero Peatón'),
                                  tags$li('Tercero Viajero'))
    ))
  })
  onclick(id = 'bd_boolean-axa' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE, size = 'l',
                          tags$p(strong('Variables Booleanas manejadas por AXA'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$table(style = 'width: 100%; font-size: 10pt;' , class = 'diccionario',
                                     tags$col(width = '17%'), tags$col(width = '58%'), tags$col(width = '25%'),
                                     tags$tr(class = 'diccionario dicc_header' ,
                                             tags$th('Nombre de la Variable', class = 'diccionario dicc_center') , tags$th('Descripción', class = 'diccionario dicc_center') , tags$th('Tipo o Categorías', class = 'diccionario dicc_center')),
                                     tags$tr(tags$td('ambulancia', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si una ambulancia estuvo presente al momento del percance', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('arbol', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si un árbol, arbusto, rama o tronco estuvieron involucrados en el accidente', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('piedra', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si una piedra, roca o peña estuvieron incolucradas en el incidente vial', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('dormido', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el asegurado manisfestó haberse dormido segundos antes del accidente', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('grua', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si una grúa fue necesario después del percance', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('obra_civil', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el incidente vial causó daños en la infraestructura civil (banqueta, postes, sardinel, etc)', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('pavimento_mojado', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si había evidencia de pavimento mojado en el lugar del percance', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('explosion_llanta', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el asegurado manifiesta explosión de llantas como consecuencia del incidente', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('volcadura', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el ajustador maniesfiesta volcadura del vehículo durante el percance', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('perdida_total', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el auto fue enviado para valuación de pérdida total', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('conductor_distraido', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el asegurado manifestó haberse distraido segundos antes del accidente', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('fuga', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el el asegurado o ajustador manifiestan fuga del tercero responsable', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('alcohol', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el ajustador percibe aliento alcohólico en el conductor o estado de ebriedad', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('motocicleta', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el ajustador manifiesta involucramiento de motocicleta en el accidente', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('bicicleta', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si el ajustador manifiesta involucramiento de bicicleta en el incidente', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('seguro', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si alguno de los terceros vinculados en el percance vial contaba con un seguro de autos o responsabilidad civil', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('taxi', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si alguno de los vehículos involucrados correspondia a un servicio público de taxi o especial', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario'),
                                     tags$tr(tags$td('animal', class = 'diccionario dicc_center dicc_rndm'),
                                             tags$td('Si en el accidente estuvo involucrado algún semoviente (perro, burro, vaca, gato, otros)', class = 'diccionario'),
                                             tags$td('Booleano', class = 'diccionario'),
                                             class = 'diccionario')
                          )
    ))
  })
  
  output$repubikla_cuantos <- renderText(format(nrow(repubikla) , big.mark = ','))
  output$repubikla_cuando1 <- renderText(str_to_title(format(range(repubikla$timestamp)[1] , format = '%d/%B/%Y') , locale = 'es'))
  output$repubikla_cuando2 <- renderText(str_to_title(format(range(repubikla$timestamp)[2] , format = '%d/%B/%Y') , locale = 'es'))
  
  onclick(id = 'bd_modo-repubikla' , {
    showModal(modalDialog(title = NULL , footer = NULL, easyClose = TRUE,
                          tags$p(strong('Modos de Víctimas registrados por Repubikla'), style = 'font-size: 14pt; color: #848888; text-align: left;'),
                          tags$ul(style = 'color: #697070;',
                                  tags$li('Ciclista'),
                                  tags$li('Motocilista'),
                                  tags$li('Mototaxi'),
                                  tags$li('Peatón'),
                                  tags$li('Triciclo'))
    ))
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
  
  observeEvent(input$boton_ver_integracion2 , updateTabItems(session , inputId = 'menu_1' , selected = 'instrucciones'))
  
  observeEvent(input$boton_ver_instrucciones , {
    if (input$boton_ver_instrucciones %% 2 == 0) {
      hideElement(id = 'berenjena' , anim = TRUE)
      updateActionButton(session , inputId = 'boton_ver_instrucciones' , label = 'Ver Instrucciones' , icon = icon('question-circle'))
    }
    else {
      showElement(id = 'berenjena' , anim = TRUE)
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
  
  output$desc_instrucciones <- renderUI({
    if (input$no_instrucciones == 1) HTML(paste0('Esta aplicación es un <b>Visualizador de Incidentes Viales</b> registrados por distintas instituciones públicas y privadas a través de diferentes Bases de Datos.',
                                                'La herramienta se conforma de tres secciones principales que permiten facilitar la exploración de estas bases:</br>',
                                                '<ul><li><b>Mapa Interactivo</b> – Permite identificar la posición geográfica de cada uno de los incidentes, así como acceder a información detallada de cada uno.</li>',
                                                '<li><b>Filtro de Incidentes</b> – Ayuda a filtrar las Bases de Datos en función de un lugar, tiempo o tipo de incidente en específico.</li>',
                                                '<li><b>Gráficas de Incidentes Viales</b> – Permiten observar tendencias y estadísticos generales sobre los incidentes observados en el mapa.</li></ul>',
                                                'Como tal, todas las bases pueden ser analizadas a detalle en función de las necesidades del usuario.'))
    else if (input$no_instrucciones == 2) HTML(paste0('Para iniciar, es necesario seleccionar alguna de las Bases de Datos disponibles, lo cual permitirá visualizar los incidentes que contenga en el Mapa Interactivo. Cada Base de Datos se encuentra asociada a un color para facilitar su identificación:</br></br>',
                                                      '<table style = "margin: auto; width: 60%;">
                                                        <tr>
                                                          <td style = "background-color: #952800; width: 20px;"></td>
                                                          <td style = "padding: 0px 2px;"><b>PGJ</b></td>
                                                          <td style = "background-color: #043A5F; width: 20px;"></td>
                                                          <td style = "padding: 0px 2px;"><b>SSC</b></td>
                                                          <td style = "background-color: #956F00; width: 20px;"></td>
                                                          <td style = "padding: 0px 2px;"><b>C5</b></td>
                                                          <td style = "background-color: #5E0061; width: 20px;"></td>
                                                          <td style = "padding: 0px 2px;"><b>AXA</b></td>
                                                          <td style = "background-color: #3F8500; width: 20px;"></td>
                                                          <td style = "padding: 0px 2px;"><b>Repubikla</b></td>
                                                        </tr>
                                                      </table>',
                                                      '</br>Es posible seleccionar más de una Base de Datos a la vez, permitiéndo observarlas todas al mismo tiempo si el usuario así lo desea.'))
    else if (input$no_instrucciones == 3) HTML(paste0('Es posible filtrar los incidentes observados en el mapa de tres maneras distintas:',
                                                      '<ul>
                                                        <li><b>Fecha</b> – Utilizando el deslizador ubicado debajo del mapa, el usuario puede seleccionar un periodo de tiempo específico.</li>
                                                        <li><b>Lugar</b> – El menú bajo el nombre de "Área de Análisis" permite observar los incidentes ocurridos en toda la extensión de la CDMX, o únicamente en alguna de sus 16 alcaldías.</li>
                                                        <li><b>Tipo de Incidente</b> – Los incidentes viales se han dividido en tres tipos:
                                                          <ul>
                                                            <li><i>Decesos</i> – Todos aquellos en los que se registró por lo menos algún fallecido.</li>
                                                            <li><i>Lesionados</i> – Aquellos en los que la respectiva institución registró a la víctima del incidente lesionada de alguna forma.</li>
                                                            <li><i>Accidentes</i> – Todos los incidentes donde no se registró ni un fallecido ni un lesionado.</li>
                                                          </ul></li>
                                                      </ul>',
                                                      'El usuario puede visualizar sólo un tipo de Incidente Vial a la vez, o todos ellos al mismo tiempo. Asimismo, cada tipo de incidente se encuentra asociado a un ícono en particular:</br></br>',
                                                      '<table style = "margin: auto; width: 60%;">
                                                        <tr>
                                                          <td><div class="fa fa-skull" style = "color:black; font-size:20pt;"></div></td>
                                                          <td style = "padding: 0px 2px;"><b>Decesos</b></td>
                                                          <td><div class="fa fa-medkit" style = "color:black; font-size:20pt;"></div></td>
                                                          <td style = "padding: 0px 2px;"><b>Lesionados</b></td>
                                                          <td><div class="fa fa-car-crash" style = "color:black; font-size:20pt;"></div></td>
                                                          <td style = "padding: 0px 2px;"><b>Accidentes</b></td>
                                                        </tr>
                                                      </table>'))
    else if (input$no_instrucciones == 4) HTML(paste0('En el mapa aparecerán todos los Incidentes Viales registrados por las instituciones seleccionadas que cumplan con los filtros establecidos. Los marcadores del mapa señalarán el color y el símbolo asociado a cada incidente, de acuerdo a la simbología presente en la esquina inferior derecha del mapa. Sus caracerísticas más importantes son:</br>',
                                                      '<ul>
                                                        <li>Los círculos con números, llamados <b>Clústers</b> indicarán el número de incidentes ocurridos en una zona en particular y, al dar clic en ellos, el mapa se acercará a ésta y mostrará los incidentes correspondientes.</li>
                                                        <li>Al hacer clic en alguno de los incidentes, aparecerá información detallada sobre éste, como la fecha y hora del mismo, lo cual permitirá conocer más información sobre el mismo.</li>
                                                        <li>El usuario puede moverse por el mapa a lo largo de toda la Ciudad, así como alejarse o acercarse tanto como se necesite.</li>
                                                      </ul>',
                                                      'De esta forma, tanto la Secretaría como los usuarios son capaces de entender en un primer nivel la dinámica espacial de los hechos de tránsito.'))
    else if (input$no_instrucciones == 5) HTML(paste0('Los hechos de tránsito visualizados en el mapa también generarán gráficas que permiten entender el panorama general de los mismos a través de estadísticos básicos. De éstas, cabe destacar:</br>',
                                                      '<ul>
                                                        <li>En general, el usuario verá en el Eje Horizontal los meses correspondientes al periodo de tiempo aplicado en los filtros anteriores, y en el Eje Vertical un conteo del número de incidentes correspondientes.</li>
                                                        <li>Es posible dar clic a la gráfica para conocer el número específico de incidentes viales en algún punto en particular.</li>
                                                        <li>El usuario es capaz de <b>Categorizar</b> los Incidentes Viales a través del menú "Datos a Graficar", en función de las características de cada Base de Datos:</li>
                                                          <ul>
                                                            <li>Seleccionar "Gráficas Combinadas" genera una gráfica donde se realiza un conteo de todas las Bases de Datos seleccionadas</li>
                                                            <li>Cualquier otra opción permitirá analizar a detalle alguna base en particular y categorizar según sus variables particulares. Por ejemplo, "PGJ" permite categorizar según el delito registrado en la Carpeta de Investigación.</li>
                                                          </ul>
                                                      </ul>',
                                                      'Además, el botón en la esquina superior derecha de la sección de gráficas permite realizar un acercamiento a las mismas.'))
    else if (input$no_instrucciones == 6) HTML(paste0('<p>La primera pestaña de la sección de gráficas, "Gráficas por Totales", permite obtener el total de Incidentes Viales ocurridos según los filtros de las secciones anteriores. A través de "Temporalidad a Graficar" es posible obtener los totales por mes o por día:</p>',
                                                      '<ul>
                                                        <li><b>Por Mes</b> únicamente arrojará Gráficas de Línea con los totales de cada uno de los meses seleccionados. Al dar clic en ellas, se puede obtener el número exacto de incidentes en dicho mes.</li>
                                                        <li><b>Por Día</b> permitirá saber el número de incidentes ocurridos durante cada uno de los días del periodo seleccionado. La línea delgada representa la frecuencia exacta de los incidentes, mientras que la más gruesa muestra un suavizado con la tendencia de estos incidentes. Al dar clic sobre la línea delgada, se obtendrá no sólo el total de incidentes, sino también la fecha exacta en la que ocurrieron.</li>
                                                      </ul>',
                                                      '<p>Las funciones de categorización y acercamiento a las gráficas son utilizables.</p>'))
    else if (input$no_instrucciones == 7) HTML(paste0('<p>La segunda pestaña, "Gráficas por Día y Hora", permite conocer el número de incidentes viales ocurridos en función del día de la semana y el momento del día en el que ocurrieron.</p>',
                                                      '<p>Debajo de "Temporalidad a Graficar" puede seleccionarse los Incidentes Viales ocurridos durante la <i>Mañana</i> (6:00 AM a 12:59 PM), <i>Tarde</i> (1:00 PM a 9:59PM) o durante la <i>Noche</i> (10:00 PM a 5:59 AM)</p>',
                                                      '<p>Asimismo, en la parte inferior de la gráfica se puede encontrar una Gráfica de Barras que representa la proporción de incidentes ocurridos durante los Fines de Semana (Viernes, Sábado y Domingo) o Entre Semana (Lunes, Martes, Miércoles y Jueves)</p>',
                                                      '<p>Las funcionalidades de click para obtener totales, categorización de datos y acercamiento a gráficas también se encuentran disponibles.</p>'))
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
      lugar <- cdmx
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
                                  '<b>Tipo de Evento</b>: ' , str_to_title(bd$tmp_ssc$tipo_de_evento, locale = 'es') , '<br/>',
                                  '<b>Fecha y Hora de Hechos</b>: ' , format(bd$tmp_ssc$timestamp , format = '%d/%m/%Y, %T'), '<br/>',
                                  '<b>Tipo de Intersección</b>: ' , str_to_title(bd$tmp_ssc$tipo_de_interseccion, locale = 'es'), '<br/>' ,
                                  '<b>Vehículos Involucrados</b>: ' , str_to_title(str_replace(ifelse(!is.na(bd$tmp_ssc$tipo_de_vehiculo_4) , paste(sep = ', ' , bd$tmp_ssc$tipo_de_vehiculo_1 , bd$tmp_ssc$tipo_de_vehiculo_2 , bd$tmp_ssc$tipo_de_vehiculo_3 , bd$tmp_ssc$tipo_de_vehiculo_4),
                                                                             ifelse(!is.na(bd$tmp_ssc$tipo_de_vehiculo_3) , paste(sep = ', ' , bd$tmp_ssc$tipo_de_vehiculo_1 , bd$tmp_ssc$tipo_de_vehiculo_2 , bd$tmp_ssc$tipo_de_vehiculo_3),
                                                                                    ifelse(!is.na(bd$tmp_ssc$tipo_de_vehiculo_2) , paste(sep = ', ' , bd$tmp_ssc$tipo_de_vehiculo_1 , bd$tmp_ssc$tipo_de_vehiculo_2) ,
                                                                                           bd$tmp_ssc$tipo_de_vehiculo_1))) , 'SD' , 'Sin Datos') , locale = 'es') , '<br/>' ,
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
            else if (input$subgrafica_ssc == 'Tipo de Evento') count <- count(tmp , year(timestamp) , month(timestamp) , tipo_de_evento) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'categoria'='tipo_de_evento')
            else if (input$subgrafica_ssc == 'Identidad') {
              count <- count(tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
              years <- unique(count$ao)
              months <- unique(count$mes)
              count <- data.frame(ao = as.integer() , mes = as.integer() , categoria = as.character() , n = as.integer() , stringsAsFactors = FALSE)
              for (ao in years) {
                for (mes in months) {
                  tmp2 <- filter(tmp , year(timestamp) == ao & month(timestamp) == mes)
                  if (nrow(tmp2) != 0) {
                    tmp2 <- as.data.frame(table(unlist(strsplit(tmp2$identidad , ' '))) , stringsAsFactors = FALSE) %>% rename('categoria'='Var1' , 'n'='Freq')
                    tmp2$ao <- ao
                    tmp2$mes <- mes
                    tmp2 <- tmp2 %>% select(ao , mes , categoria , n)
                    count <- rbind(count , tmp2)
                  }
                }
              }
            }
            count$categoria <- str_to_title(count$categoria , locale = 'es')
          }
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
            else if (input$subgrafica_ssc == 'Tipo de Evento') count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp) , tipo_de_evento) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)' , 'categoria'='tipo_de_evento')
            else if (input$subgrafica_ssc == 'Identidad') {
              count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia'='day(timestamp)')
              years <- unique(count$ao)
              months <- unique(count$mes)
              days <- unique(count$dia)
              count <- data.frame(ao = as.integer() , mes = as.integer() , dia = as.integer() , categoria = as.character() , n = as.integer() , stringsAsFactors = FALSE)
              for (ao in years) {
                for (mes in months) {
                  for (dia in days) {
                    tmp2 <- filter(tmp , year(timestamp) == ao & month(timestamp) == mes & day(timestamp) == dia)
                    if (nrow(tmp2) != 0) {
                      tmp2 <- as.data.frame(table(unlist(strsplit(tmp2$identidad , ' '))) , stringsAsFactors = FALSE) %>% rename('categoria'='Var1' , 'n'='Freq')
                      tmp2$ao <- ao
                      tmp2$mes <- mes
                      tmp2$dia <- dia
                      tmp2 <- tmp2 %>% select(ao , mes , dia , categoria , n)
                      count <- rbind(count , tmp2)
                    }
                  }
                }
              }
            }
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
          if (length(unique(count_final$etiqueta)) > 3) {
            grafica = grafica +
              geom_line(data = count_final , aes(x = ref , y = n , color = categoria) , size = 1.5) +
              scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                                 minor_breaks = NULL,
                                 labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
              scale_color_manual(values = paleta,
                                 limits = unique(count_final$categoria),
                                 name = 'Fuente de Datos' ,
                                 labels = unique(count_final$categoria)) +
              ylim(0 , NA) +
              labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Mes') +
              theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
          }
          else {
            grafica = grafica +
              geom_col(data = count_final , aes(x = ref , y = n , fill = categoria) , position = 'dodge') +
              scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                                 minor_breaks = NULL,
                                 labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
              scale_fill_manual(values = paleta,
                                 limits = unique(count_final$categoria),
                                 name = 'Fuente de Datos' ,
                                 labels = unique(count_final$categoria)) +
              labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Mes')
          }
        }
        else if (input$tiempo_grafica == 'Por Día') {
          grafica = grafica +
            geom_line(data = count_final , aes(x = ref , y = n , color = categoria), alpha = 0.2, size = 1.5) +
            geom_smooth(data = count_final , aes(x = ref , y = n , color = categoria), method = 'loess' , formula = 'y ~ x' , se = FALSE , size = 2 , na.rm = TRUE) +
            scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                               minor_breaks = NULL,
                               labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
            scale_color_manual(values = paleta,
                               limits = unique(count_final$categoria),
                               name = 'Fuente de Datos' ,
                               labels = unique(count_final$categoria)) +
            ylim(0 , NA) +
            labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Día') +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
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
    removeUI(selector = '#div_grafica_b2')
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
      else if (input$tipo_grafica2 == 'SSC') {
        insertUI(selector = '#div_grafica_a2' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b2' ,
                          selectInput(inputId = 'subgrafica_ssc2' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Tipo de Evento' , 'Identidad'))))
      }
      else if (input$tipo_grafica2 == 'C5') {
        insertUI(selector = '#div_grafica_a2' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b2' ,
                          selectInput(inputId = 'subgrafica_c52' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Incidente C4' , 'Clase del Incidente' , 'Tipo de Entrada'))))
      }
      else if (input$tipo_grafica2 == 'AXA') {
        insertUI(selector = '#div_grafica_a2' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b2' ,
                          selectInput(inputId = 'subgrafica_axa2' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Causa del Siniestro' , 'Tipo de Vehículo' , 'Rol del Lesionado'))))
      }
      else if (input$tipo_grafica2 == 'Repubikla') {
        insertUI(selector = '#div_grafica_a2' , where = 'afterEnd',
                 tags$div(id = 'div_grafica_b2' ,
                          selectInput(inputId = 'subgrafica_repubikla2' , label = 'Categorización de Datos' , selected = 'Sin Categoría' , width = '100%',
                                      choices = c('Sin Categoría' , 'Modo'))))
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
        # Aquí es cuando hay más de una base seleccionada
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
        count_final <- get_final_counts(bd$tmp_pgj, input)
        if (input$subgrafica_pgj2 == 'Sin Categoría') paleta <- c('#952800')
        else paleta <- c('#97173A' , '#7F2B5F' , '#593E70' , '#36476B' , '#2F4858' , '#C03C7A' , '#B65FB2' , '#9682DD' , '#65A3F8')
        # # =
        # max <- 0
        # tmp <- bd$tmp_pgj
        # #print(head(tmp))
        # tmp$geometry <- NULL
        # if (!is.null(tmp)) {
        #   if (nrow(tmp) != 0) {
        #     if (input$subgrafica_pgj2 == 'Sin Categoría') max <- ceiling(max(count(tmp , wday(timestamp))$n)/10)*10
        #     else if (input$subgrafica_pgj2 == 'Delito') max <- ceiling(max(count(tmp , wday(timestamp) , delito)$n)/10)*10
        #   }}
        # # =
        # if (!is.null(tmp)) {
        #   if (input$tiempo_grafica2 == 'Mañana (6AM - 12PM)') tmp <- filter(tmp , hour(timestamp) %in% c(6, 7, 8, 9, 10, 11, 12))
        #   else if (input$tiempo_grafica2 == 'Tarde (1PM - 9PM)') tmp <- filter(tmp , hour(timestamp) %in% c(13, 14, 15, 16, 17, 18, 19, 20, 21))
        #   else if (input$tiempo_grafica2 == 'Noche (10PM - 5AM)') tmp <- filter(tmp , hour(timestamp) %in% c(22, 23, 0, 1, 2, 3, 4, 5))
        # }
        # # =
        # if (nrow(tmp) == 0) {
        #   count <- data.frame(ref = 1 , categoria = 'Sin Datos' , n = 0)
        #   for (i in seq(7)) {
        #     if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 'Sin Datos' , 0)}
        # } 
        # else if (input$subgrafica_pgj2 == 'Sin Categoría') {
        #   count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
        #   for (i in seq(7)) {
        #     if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
        #   count$categoria <- 'PGJ'
        # }
        # else if (input$subgrafica_pgj2 == 'Delito') {
        #   count <- count(tmp , wday(timestamp) , delito) %>% rename('ref'='wday(timestamp)' , 'categoria'='delito')
        #   for (i in seq(7)) {
        #     for (cat in unique(count$categoria)) {
        #       if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
        #     }}
        #   count$categoria <- str_to_title(count$categoria , locale = 'es')
        # }
        # # =
        # count$etiqueta <- count$ref
        # count$etiqueta[count$etiqueta == 1] <- 'Domingo'
        # count$etiqueta[count$etiqueta == 2] <- 'Lunes'
        # count$etiqueta[count$etiqueta == 3] <- 'Martes'
        # count$etiqueta[count$etiqueta == 4] <- 'Miércoles'
        # count$etiqueta[count$etiqueta == 5] <- 'Jueves'
        # count$etiqueta[count$etiqueta == 6] <- 'Viernes'
        # count$etiqueta[count$etiqueta == 7] <- 'Sábado'
        # # =
        # count$ref <- as.integer(count$ref)
        # count$n <- as.integer(count$n)
        # count <- count[order(count$ref),]
        # #print(head(count))
        # # =
        # count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
      }
      else if (input$tipo_grafica2 == 'SSC' & !is.null(input$subgrafica_ssc2) & !is.null(bd$tmp_ssc)) {
        is_graph <- TRUE
        if (input$subgrafica_ssc2 == 'Sin Categoría') paleta <- c('#043A5F')
        else paleta <- c('#006386' , '#008E97' , '#00B891' , '#8CDC7D' , '#F9F871' , '#6B6399' , '#9B77B0' , '#CC8BC3' , '#FDA1D1')
        # =
        max <- 0
        tmp <- bd$tmp_ssc
        tmp$geometry <- NULL
        if (!is.null(tmp)) {
          if (nrow(tmp) != 0) {
            if (input$subgrafica_ssc2 == 'Sin Categoría') max <- ceiling(max(count(tmp , wday(timestamp))$n)/10)*10
            else if (input$subgrafica_ssc2 == 'Tipo de Evento') max <- ceiling(max(count(tmp , wday(timestamp) , tipo_de_evento)$n)/10)*10
            else if (input$subgrafica_ssc2 == 'Identidad') max <- (ceiling(max(count(tmp , wday(timestamp) , identidad)$n)/10)*10) + 5
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
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 'Sin Datos' , 0)}
        } 
        else if (input$subgrafica_ssc2 == 'Sin Categoría') {
          count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
          count$categoria <- 'SSC'
        }
        else if (input$subgrafica_ssc2 == 'Tipo de Evento') {
          count <- count(tmp , wday(timestamp) , tipo_de_evento) %>% rename('ref'='wday(timestamp)' , 'categoria'='tipo_de_evento')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
        }
        else if (input$subgrafica_ssc2 == 'Identidad') {
          count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)' )
          weekdays <- unique(count$ref)
          count <- data.frame(ref = as.integer() , categoria = as.character() , n = as.integer() , stringsAsFactors = FALSE)
          for (wd in weekdays) {
            tmp2 <- filter(tmp , wday(timestamp) == wd)
            if (nrow(tmp2) != 0) {
              tmp2 <- as.data.frame(table(unlist(strsplit(tmp2$identidad , ' '))) , stringsAsFactors = FALSE) %>% rename('categoria'='Var1' , 'n'='Freq')
              tmp2$ref <- wd
              tmp2 <- tmp2 %>% select(ref , categoria , n)
              count <- rbind(count , tmp2)
            }
          }
          # =====
          # count <- count(tmp , wday(timestamp) , identidad) %>% rename('ref'='wday(timestamp)' , 'categoria'='identidad')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
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
        count$ref <- as.integer(count$ref)
        count$n <- as.integer(count$n)
        count <- count[order(count$ref),]
        # =
        count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
      }
      else if (input$tipo_grafica2 == 'C5' & !is.null(input$subgrafica_c52) & !is.null(bd$tmp_c5)) {
        is_graph <- TRUE
        if (input$subgrafica_c52 == 'Sin Categoría') paleta <- c('#956F00')
        else paleta <- c('#956F00', '#5D731D' , '#276E40' , '#006459' , '#015762' , '#2F4858' , '#C2A573' , '#FFEECB' , '#00C9B1' , '#4E4637' , '#B5AA99' , '#987061' , '#FFC1B2' , '#FE8A7D' , '#C1554C' ,
                         '#a78e72' , '#e48d24' , '#725238' , '#e1b53f' , '#7f6342' , '#e6c392' , '#715c09' , '#dda25a' , '#895c1d' , '#ad8220' , '#a2814d' , '#b2712f')
        # =
        max <- 0
        tmp <- bd$tmp_c5
        tmp$geometry <- NULL
        if (!is.null(tmp)) {
          if (nrow(tmp) != 0) {
            if (input$subgrafica_c52 == 'Sin Categoría') max <- ceiling(max(count(tmp , wday(timestamp))$n)/10)*10
            else if (input$subgrafica_c52 == 'Incidente C4') max <- ceiling(max(count(tmp , wday(timestamp) , incidente_c4)$n)/10)*10
            else if (input$subgrafica_c52 == 'Clase del Incidente') max <- ceiling(max(count(tmp , wday(timestamp) , clas_con_f_alarma)$n)/10)*10
            else if (input$subgrafica_c52 == 'Tipo de Entrada') max <- ceiling(max(count(tmp , wday(timestamp) , tipo_entrada)$n)/10)*10
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
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 'Sin Datos' , 0)}
        } 
        else if (input$subgrafica_c52 == 'Sin Categoría') {
          count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
          count$categoria <- 'C5'
        }
        else if (input$subgrafica_c52 == 'Incidente C4') {
          count <- count(tmp , wday(timestamp) , incidente_c4) %>% rename('ref'='wday(timestamp)' , 'categoria'='incidente_c4')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
        }
        else if (input$subgrafica_c52 == 'Clase del Incidente') {
          count <- count(tmp , wday(timestamp) , clas_con_f_alarma) %>% rename('ref'='wday(timestamp)' , 'categoria'='clas_con_f_alarma')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
        }
        else if (input$subgrafica_c52 == 'Tipo de Entrada') {
          count <- count(tmp , wday(timestamp) , tipo_entrada) %>% rename('ref'='wday(timestamp)' , 'categoria'='tipo_entrada')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
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
        count$ref <- as.integer(count$ref)
        count$n <- as.integer(count$n)
        count <- count[order(count$ref),]
        # =
        count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
      }
      else if (input$tipo_grafica2 == 'AXA' & !is.null(input$subgrafica_axa2) & !is.null(bd$tmp_axa)) {
        is_graph <- TRUE
        if (input$subgrafica_axa2 == 'Sin Categoría') paleta <- c('#5E0061')
        else paleta <- c('#5E0061' , '#A1145F' , '#D44755' , '#F37F4B' , '#FFBB4F' , '#F9F871' , '#474197' , '#006CBD' , '#36D9D3' , '#7D527C' , '#FFE7FF')
        # =
        max <- 0
        tmp <- bd$tmp_axa
        tmp$geometry <- NULL
        if (!is.null(tmp)) {
          if (nrow(tmp) != 0) {
            if (input$subgrafica_axa2 == 'Sin Categoría') max <- ceiling(max(count(tmp , wday(timestamp))$n)/10)*10
            else if (input$subgrafica_axa2 == 'Causa del Siniestro') max <- ceiling(max(count(tmp , wday(timestamp) , causa_siniestro)$n)/10)*10
            else if (input$subgrafica_axa2 == 'Tipo de Vehículo') max <- ceiling(max(count(tmp , wday(timestamp) , tipo_vehiculo)$n)/10)*10
            else if (input$subgrafica_axa2 == 'Rol del Lesionado') max <- ceiling(max(count(tmp , wday(timestamp) , relacion_lesionados)$n)/10)*10
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
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 'Sin Datos' , 0)}
        } 
        else if (input$subgrafica_axa2 == 'Sin Categoría') {
          count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
          count$categoria <- 'AXA'
        }
        else if (input$subgrafica_axa2 == 'Causa del Siniestro') {
          count <- count(tmp , wday(timestamp) , causa_siniestro) %>% rename('ref'='wday(timestamp)' , 'categoria'='causa_siniestro')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
        }
        else if (input$subgrafica_axa2 == 'Tipo de Vehículo') {
          count <- count(tmp , wday(timestamp) , tipo_vehiculo) %>% rename('ref'='wday(timestamp)' , 'categoria'='tipo_vehiculo')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
        }
        else if (input$subgrafica_axa2 == 'Rol del Lesionado') {
          count <- count(tmp , wday(timestamp) , relacion_lesionados) %>% rename('ref'='wday(timestamp)' , 'categoria'='relacion_lesionados')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
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
        count$ref <- as.integer(count$ref)
        count$n <- as.integer(count$n)
        count <- count[order(count$ref),]
        # =
        count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
      }
      else if (input$tipo_grafica2 == 'Repubikla' & !is.null(input$subgrafica_repubikla2) & !is.null(bd$tmp_repubikla)) {
        is_graph <- TRUE
        if (input$subgrafica_repubikla2 == 'Sin Categoría') paleta <- c('#3F8500')
        else paleta <- c('#3F8500' , '#00735C' , '#00666B' , '#005769' , '#2F4858' , '#008DA8' , '#008ABF' , '#97B27E' , '#E4F7D2' , '#5CB7D5')
        # =
        max <- 0
        tmp <- bd$tmp_repubikla
        tmp$geometry <- NULL
        if (!is.null(tmp)) {
          if (nrow(tmp) != 0) {
            if (input$subgrafica_repubikla2 == 'Sin Categoría') max <- ceiling(max(count(tmp , wday(timestamp))$n)/10)*10
            else if (input$subgrafica_repubikla2 == 'Modo') max <- ceiling(max(count(tmp , wday(timestamp) , modo)$n)/10)*10
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
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 'Sin Datos' , 0)}
        } 
        else if (input$subgrafica_repubikla2 == 'Sin Categoría') {
          count <- count(tmp , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
          for (i in seq(7)) {
            if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
          count$categoria <- 'Repubikla'
        }
        else if (input$subgrafica_repubikla2 == 'Modo') {
          count <- count(tmp , wday(timestamp) , modo) %>% rename('ref'='wday(timestamp)' , 'categoria'='modo')
          for (i in seq(7)) {
            for (cat in unique(count$categoria)) {
              if (nrow(count[count$ref == i & count$categoria == cat,]) == 0) count[nrow(count) + 1,] <- c(i , cat , 0)
            }}
          count$categoria <- str_to_title(count$categoria , locale = 'es')
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
        count$ref <- as.integer(count$ref)
        count$n <- as.integer(count$n)
        count <- count[order(count$ref),]
        # =
        count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
      }
      # =====
      print("graficaaaaaaaaaaaaa")
      if (is_graph == TRUE) {
        if (max == 0) max <- NA
        grafica = grafica +
          geom_col(data = count_final , aes(x = ref , y = n , fill = categoria) , position = 'dodge') +
          scale_x_continuous(breaks = unique(count_final$ref[!is.na(count_final$etiqueta)]),
                             minor_breaks = NULL,
                             labels = unique(count_final$etiqueta[!is.na(count_final$etiqueta)])) +
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
        if (input$tiempo_grafica == 'Por Mes' & length(unique(k$etiqueta)) <= 3) {
          range <- c(NA , NA)
          if (hover$x > 0.5 & hover$x <= 1.5) range <- c(0.5 , 1.5)
          else if (hover$x > 1.5 & hover$x <= 2.5) range <- c(1.5 , 2.5)
          else if (hover$x > 2.5 & hover$x <= 3.5) range <- c(2.5 , 3.5)
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
        }
        else point <- nearPoints(df = k, coordinfo = hover, threshold = 5, maxpoints = 1)
        if (nrow(point) == 0) return(NULL)
        # =
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        # =
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        # =
        if (input$tiempo_grafica == 'Por Mes' & length(unique(k$etiqueta)) <= 3) {
          style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                          "left:", left_px + 10, "px; top:", top_px + 140, "px;
                      padding: 5px 10px 0px; border-radius: 10px;")
        }
        else {
          style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                          "left:", left_px + 10, "px; top:", top_px + 140, "px;
                      padding: 5px 10px 0px; border-radius: 10px;")
        }
        # =
        if (input$filtro_incidente == 'Decesos') palabra <- ' Decesos'
        else if (input$filtro_incidente == 'Lesionados') palabra <- ' Lesionados'
        else if (input$filtro_incidente == 'Accidentes') palabra <- ' Accidentes'
        else if (input$filtro_incidente == 'Todos') palabra <- ' Incidentes'
        # =
        if (point$n == 1) palabra <- substring(palabra , 0 , nchar(palabra) - 1)
        # =
        if (input$tiempo_grafica == 'Por Mes') {
          if (length(unique(k$etiqueta)) <= 3 & point$n <= hover$y) return(NULL)
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
    if (is.null(hover)) return(NULL)
    k <- hover_h$h
    if (!is.null(k)) {
      if (input$tiempo_grafica == 'Por Mes' & length(unique(k$etiqueta)) <= 3) {
        range <- c(NA , NA)
        if (hover$x > 0.5 & hover$x <= 1.5) range <- c(0.5 , 1.5)
        else if (hover$x > 1.5 & hover$x <= 2.5) range <- c(1.5 , 2.5)
        else if (hover$x > 2.5 & hover$x <= 3.5) range <- c(2.5 , 3.5)
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
      }
      else point <- nearPoints(df = k, coordinfo = hover, threshold = 5, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      # =
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      # =
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      # =
      if (input$tiempo_grafica == 'Por Mes' & length(unique(k$etiqueta)) <= 3) {
        style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                        "left:", left_px + 10, "px; top:", top_px - 40, "px;
                      padding: 5px 10px 0px; border-radius: 10px;")
      }
      else {
        style <- paste0("position:absolute; z-index:100; background-color: rgba(205, 205, 205, 0.80); ",
                        "left:", left_px + 10, "px; top:", top_px - 40, "px;
                      padding: 5px 10px 0px; border-radius: 10px;
                      font-size: 125%;") 
      }
      # =
      if (input$filtro_incidente == 'Decesos') palabra <- ' Decesos'
      else if (input$filtro_incidente == 'Lesionados') palabra <- ' Lesionados'
      else if (input$filtro_incidente == 'Accidentes') palabra <- ' Accidentes'
      else if (input$filtro_incidente == 'Todos') palabra <- ' Incidentes'
      # =
      if (point$n == 1) palabra <- substring(palabra , 0 , nchar(palabra) - 1)
      # =
      if (input$tiempo_grafica == 'Por Mes') {
        if (length(unique(k$etiqueta)) <= 3 & point$n <= hover$y) return(NULL)
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
  
  
}

shinyApp(ui = ui, server = server)