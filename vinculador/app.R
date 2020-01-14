# Versión Final Vinculación

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

# ===== FRONT-END =====
ui <- dashboardPage(title = 'Vinculación de Incidentes Viales - SEMOVI',
                    dashboardHeader(title = 'Vinculación de Incidentes'),
                    # ===== SIDEBAR =====
                    dashboardSidebar(sidebarMenu(id = 'menu_completo',
                                                 menuItem(text = 'Bases de Datos' , selected = TRUE , icon = icon('layer-group') , tabName = 'bases_de_datos'),
                                                 menuItemOutput(outputId = 'menu_2'),
                                                 menuItemOutput(outputId = 'menu_3')),
                                     tags$div(tags$p(strong('Realizado en colaboración por:')),
                                              tags$table(style = 'width: 100%;',
                                                         tags$tr(tags$th('') , tags$th('')),
                                                         tags$tr(tags$td(colspan = 2,
                                                                         tags$img(src = 'gobcdmx.png' , style = 'display: block; margin: auto; width: 100%; padding-bottom: 15px'))),
                                                         tags$tr(tags$td(tags$div(style = 'text-align: center; padding-bottom: 15px',
                                                                                  tags$img(src = 'axa.png' , style = 'height: 45px;'))),
                                                                 tags$td(tags$div(style = 'text-align: center; padding-bottom: 15px',
                                                                                  tags$img(src = 'conacyt.png' , style = 'height: 50px;')))),
                                                         tags$tr(tags$td(tags$div(style = 'text-align: center;',
                                                                                  tags$img(src = 'centrogeo.png' , style = 'height: 50px;'))),
                                                                 tags$td(tags$div(style = 'text-align: center;',
                                                                                  tags$img(src = 'datalab.png' , style = 'width: 80px;'))))),
                                              style = 'position: absolute; bottom: 0; left: 0; padding: 10px 10px; background-color: white; width: 100%; color: #697070;')),
                    # ===== BODY =====
                    dashboardBody(useShinyjs() , tags$head(tags$link(rel = 'stylesheet' , type = 'text/css' , href = 'custom.css')),
                                  tabItems(
                                    # ===== TAB BASE DE DATOS =====
                                    tabItem(tabName = 'bases_de_datos',
                                            # ===== INSTRUCCIONES =====
                                            box(width = 12,
                                                tags$div(tags$img(src = 'gobcdmx.png',
                                                                  style = 'padding-bottom: 3px; height: 80px;'),
                                                         tags$br(),
                                                         strong('Vinculación de Incidentes Viales'),
                                                         style = 'text-align: center; font-size: 24pt; color: #848888; padding-bottom: 5px;'),
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
                                                         
                                                         column(5 , # tags$video(src = 'tmp.mp4' , width = '100%' , height = '350px' , type = 'video/mp4' , controls = 'controls')
                                                                tags$div('Placeholder' , style = 'background-color: #848888; height: 350px;')
                                                         ))),
                                            box(width = 12,
                                                # ===== INFORMACIÓN DE BASE =====
                                                fluidRow(column(6, strong('Bases de Datos Disponibles', style = 'color: #848888; font-size: 16pt;'),
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
                                                                                                        
                                                                                               )))
                                                                )),
                                                         # ===== FILTROS APLICABLES =====
                                                         column(6, strong('Filtros Aplicables', style = 'color: #848888; font-size: 16pt;'),
                                                                dateRangeInput(inputId = 'filtro_fecha' , label = 'Periodo de Tiempo', format = 'dd/MM/yyyy', language = 'es', separator = 'a',
                                                                               start = '2018-01-01' , end = '2018-12-31',
                                                                               min = '2018-01-01' , max = '2019-04-30'),
                                                                radioButtons(inputId = 'filtro_incidente' , label = 'Tipo de Inciente', inline = TRUE ,
                                                                             choiceNames = c('Decesos' , 'Lesionados' , 'Accidentes' , 'Todos'),
                                                                             choiceValues = c('Decesos' , 'Lesionados', 'Accidentes' , 'Todos')),
                                                                checkboxGroupInput(inputId = 'filtro_bd' , label = 'Bases de Datos a Utilizar', inline = TRUE,
                                                                                   choices = c('PGJ' , 'SSC' , 'C5'),
                                                                                   selected = c('PGJ' , 'SSC' , 'C5')),
                                                                tags$div(id = 'div_alerta_a'),
                                                                tags$div(style = 'width: 100%; height: 5px; background-color: white;'),
                                                                actionButton(inputId = 'generar_bd' , label = strong('Generar Base de Datos') , icon = icon('layer-group')),
                                                                tags$div(id = 'div_alerta_c'))
                                                         ))
                                            ),
                                    # ===== TAB VINCULACION =====
                                    tabItem(tabName = 'vinculacion',
                                            fluidRow(column(6 , leafletOutput(outputId = 'mapa', height = '900px')),
                                                     column(6 , tags$div(id = 'div_tab2_a',
                                                                         box(width = 12,
                                                                             tags$p(strong('Selección de Muestra') , style = 'color: #848888; font-size: 16pt;'),
                                                                             fluidRow(column(6,
                                                                                             tags$p(strong('Número de Eventos por Base de Datos')),
                                                                                             tableOutput(outputId = 'tabla_bd_eventos'),
                                                                                             tags$p('La Base de Datos de mayor importancia es ' , strong(textOutput(outputId = 'texto_bd_imp' , inline = TRUE)))),
                                                                                      column(6,
                                                                                             sliderInput(inputId = 'no_muestras' , label = 'Número de Incidentes a Vincular Manualmente',
                                                                                                         min = 5 , max = 100 , value = 50 , post = ' %'),
                                                                                             tags$p('Se han seleccionado ' , strong(textOutput(outputId = 'texto_muestras_a' , inline = TRUE),
                                                                                                                                    textOutput(outputId = 'texto_muestras_b' , inline = TRUE)),
                                                                                                    'de ' , textOutput(outputId = 'texto_muestras_c' , inline = TRUE) , ' posibles.'))),
                                                                             fluidRow(column(3, actionButton(inputId = 'boton_muestra' , label = strong('Iniciar Vinculación') , icon = icon('exchange-alt'))),
                                                                                      column(3, actionButton(inputId = 'boton_defecto' , label = strong('Utilizar Parámetros por Defecto') , icon = icon('cube'))))
                                                                             )),
                                                            tags$div(id = 'div_tab2_b',
                                                                     box(width = 12,
                                                                         tags$p(strong('Vinculación de Incidentes Viales') , style = 'color: #848888; font-size: 16pt;'),
                                                                         tags$p(strong(textOutput(outputId = 'texto_bd_principal' , inline = TRUE))),
                                                                         tableOutput(outputId = 'tabla_principal'),
                                                                         fluidRow(column(6,
                                                                                         sliderInput(inputId = 'filtro_distancia' , label = 'Seleccione un Radio de Búsqueda',
                                                                                                     min = 100 , max = 1000 , value = 250 , step = 50 , post = ' m')),
                                                                                  column(6,
                                                                                         sliderInput(inputId = 'filtro_tiempo' , label = 'Seleccione un Intervalo de Búsqueda',
                                                                                                     min = 30 , max = 180 , value = 60, step = 30 , post = ' min'))),
                                                                         fluidRow(column(3,
                                                                                         actionButton(inputId = 'boton_vincular' , label = strong('Vincular Incidentes') , icon = icon('link'))),
                                                                                  column(3,
                                                                                         actionButton(inputId = 'boton_novincular' , label = strong('Eventos no Encontrados') , icon = icon('unlink')))),
                                                                         fluidRow(column(6,
                                                                                         tags$p(strong(textOutput(outputId = 'texto_bd_auxiliar_a' , inline = TRUE))),
                                                                                         tableOutput(outputId = 'tabla_bd_auxiliar_a')),
                                                                                  column(6,
                                                                                         tags$p(strong(textOutput(outputId = 'texto_bd_auxiliar_b' , inline = TRUE))),
                                                                                         tableOutput(outputId = 'tabla_bd_auxiliar_b')))
                                                                         )),
                                                            tags$div(id = 'div_tab2_c',
                                                                     box(width = 12,
                                                                         tags$p(strong('Vínculos Logrados') , style = 'color: #848888; font-size: 16pt;'),
                                                                         dataTableOutput(outputId = 'vinculos_logrados_a'),
                                                                         dataTableOutput(outputId = 'vinculos_logrados_b')))))),
                                    # ===== TAB RESULTADOS =====
                                    tabItem(tabName = 'resultados',
                                            fluidRow(column(6 , withSpinner(leafletOutput(outputId = 'mapa_2', height = '756px'),
                                                                            type = 3 , color = '#002A24' , size = 2, color.background = '#ecf0f5'),
                                                            tags$div(style = 'width: 100%; height: 20px; background-color: white; opacity: 0;'),
                                                            fluidRow(box(width = 12,
                                                                         sliderInput(inputId = 'resultados_fecha' , label = 'Periodo de Tiempo' , width = '100%', timeFormat = '%d/%m/%Y',
                                                                                     min = as.Date('2018-01-01',"%Y-%m-%d"),
                                                                                     max = as.Date('2018-12-31',"%Y-%m-%d"),
                                                                                     value = c(as.Date('2018-01-01',"%Y-%m-%d") , as.Date('2018-12-31',"%Y-%m-%d")))))),
                                                     column(6 , box(width = 12 ,
                                                                    tags$p('Resultados Finales',
                                                                           style = 'font-size: 16pt;'),
                                                                    selectInput(inputId = 'resultados_lugar' , label = 'Área de Análisis',
                                                                                choices = c('Total Ciudad de México' , 'Álvaro Obregón' , 'Azcapotzalco' , 'Benito Juárez' , 'Coyoacán',
                                                                                            'Cuajimalpa de Morelos' , 'Cuauhtémoc' , 'Gustavo A. Madero' ,
                                                                                            'Iztacalco' , 'Iztapalapa' , 'La Magdalena Contreras' , 'Miguel Hidalgo',
                                                                                            'Milpa Alta' , 'Tlalpan' , 'Tláhuac' , 'Venustiano Carranza', 'Xochimilco')),
                                                                    fluidRow(column(6,
                                                                                    checkboxGroupInput(inputId = 'resultados_bd' , label = 'Vínculos a Visualizar' , inline = TRUE,
                                                                                                       choices = c('Totales' , 'Parciales' , 'Nulos'))),
                                                                             column(6)),
                                                                    tags$p(strong('Descarga de Datos')),
                                                                    fluidRow(column(4,
                                                                                    actionButton(inputId = 'descarga_a' , label = strong('Vínculos Totales') , icon = icon('download'))),
                                                                             column(4,
                                                                                    actionButton(inputId = 'descarga_b' , label = strong('Vínculos Parciales') , icon = icon('download'))),
                                                                             column(4,
                                                                                    actionButton(inputId = 'descarga_c' , label = strong('Vínculos Nulos') , icon = icon('download')))),
                                                                    actionButton(inputId = 'descarga_d' , label = strong('Todos los Incidentes Viales') , icon = icon('download'))),
                                                            box(width = 12,
                                                                selectInput(inputId = 'tipo_grafica', label = 'Datos a Graficar', choices = c('Total de Incidentes' , 'Categorización por Tipo de Vehículo')),
                                                                withSpinner(plotOutput(outputId = 'grafica_sp', height = '380px',
                                                                                       click = clickOpts(id = 'plot_click')),
                                                                            type = 3 , color = '#002A24' , size = 1 , color.background = '#FFFFFF'),
                                                                uiOutput(outputId = 'click_info'),
                                                                tags$div(id = 'div_grafica_a'),
                                                                tags$div(tableOutput(outputId = 'tabla_totales'),
                                                                         style = 'font-size: 80%; width: 100%; margin: auto;')
                                                            ))))
                                  )))

server <- function(input, output, session) {
  # ===== REACTIVE VARIABLES =====
  pgj_importada <- reactiveValues(bd = NULL)
  c5_importada <- reactiveValues(bd = NULL)
  special_bd <- reactiveValues(active_pgj = pgj , active_c5 = c5)
  key_remote <- reactiveValues(k_pgj = FALSE , k_c5 = FALSE , k_rndm = FALSE)
  
  bd <- reactiveValues(unificada = NULL , reference = NULL , muestra = NULL , posibles = NULL)
  count_muestra <- reactiveValues(i = 1 , max = NULL)
  eventos_mapa <- reactiveValues(principal = NULL , aux1 = NULL , aux1_t = NULL , aux2 = NULL , aux2_t = NULL)
  seleccionados <- reactiveValues(a = NULL , b = NULL)
  resultado_final <- reactiveValues(parcial = NULL)
  
  tmp <- reactiveValues(a = NULL)
  
  # ===== MAPA INICIAL =====
  output$mapa <- renderLeaflet({
    leaflet(data = cdmx_sa) %>%
      addTiles(urlTemplate = '//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      setView(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
      addPolygons(fillColor = '#57948B' , fillOpacity = 0.25 , color = '#002A24' , opacity = 0.75)
  })
  
  mapa_proxy <- leafletProxy('mapa')
  
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
  
  # ===== FILTROS DE BASES DE DATOS =====
  # = Alerta BD
  observeEvent(input$filtro_bd , {
    tmp = NULL
    if (!('PGJ' %in% input$filtro_bd)) tmp = 'PGJ'
    else if (!('SSC' %in% input$filtro_bd)) tmp = 'SSC'
    if (!is.null(tmp)) {
      removeUI(selector = '#div_alerta_b')
      insertUI(selector = '#div_alerta_a' , where = 'afterEnd',
               tags$div(id = 'div_alerta_b', style = 'color: white; background-color: #8F232E; padding: 5px;',
                        tags$span('X' , style = 'margin-left: 15px; color: white; font-weight: bold; float: right; cursor: pointer; transition: 1s;',
                                  onclick = 'this.parentElement.style.display="none";'),
                        strong('¡Importante!'), 'La Base de Datos de ', tmp ,' es vital para realizar la Vinculación. No puede removerse.'))
      updateCheckboxGroupInput(session , inputId = 'filtro_bd' , selected = append(input$filtro_bd , tmp))
    }
  })
  
  output$menu_2 <- renderMenu({
    if (is.null(bd$unificada)) NULL
    else menuItem(text = 'Vinculación', icon = icon('exchange-alt') , tabName = 'vinculacion')
  })
  
  observeEvent(input$generar_bd , {
    if (input$generar_bd == 1) {
      insertUI(selector = '#div_alerta_c' , where = 'afterEnd' , immediate = TRUE,
               tags$div(id = 'div_alerta_d' ,
                        strong('Por favor espere en lo que se cargan los datos') , tags$img(src = 'loading.gif' , style = 'height: 25px;')))
      # =
      bd$unificada <- data.frame(id_original = as.character() , base_original = as.character(), timestamp = chron() , geometry = st_sfc(crs = 32614))
      # =
      if ('PGJ' %in% input$filtro_bd) {
        pgj_tmp = pgj
        # =
        if (input$filtro_incidente == 'Decesos') {
          pgj_tmp <- filter(pgj_tmp , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
        } else if (input$filtro_incidente == 'Lesionados') {
          pgj_tmp <- filter(pgj_tmp , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
        } else if (input$filtro_incidente == 'Accidentes') {
          pgj_tmp <- filter(pgj_tmp , delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL' | delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES')
        }
        # =
        pgj_tmp <- filter(pgj_tmp , timestamp >= dates(as.character(input$filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(input$filtro_fecha[2]) , format = 'y-m-d'))
        # =
        tmp = data.frame(id_original = as.character(pgj_tmp$id),
                         base_original = replicate(nrow(pgj_tmp) , 'PGJ'),
                         timestamp = pgj_tmp$timestamp,
                         geometry = pgj_tmp$geometry)
        # tmp = tmp[complete.cases(tmp),]
        if (nrow(tmp) != 0) bd$unificada <- rbind(bd$unificada , tmp)
      }
      # =
      if ('SSC' %in% input$filtro_bd) {
        ssc_tmp = ssc
        # =
        if (input$filtro_incidente == 'Decesos') {
          ssc_tmp <- filter(ssc_tmp , total_occisos > 0)
        } else if (input$filtro_incidente == 'Lesionados') {
          ssc_tmp <- filter(ssc_tmp , total_lesionados > 0 & total_occisos == 0)
        } else if (input$filtro_incidente == 'Accidentes') {
          ssc_tmp <- filter(ssc_tmp , total_lesionados == 0 & total_occisos == 0)
        }
        # =
        ssc_tmp <- filter(ssc_tmp , timestamp >= dates(as.character(input$filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(input$filtro_fecha[2]) , format = 'y-m-d'))
        # =
        tmp = data.frame(id_original = as.character(ssc_tmp$id),
                         base_original = replicate(nrow(ssc_tmp) , 'SSC'),
                         timestamp = ssc_tmp$timestamp,
                         geometry = ssc_tmp$geometry)
        # tmp = tmp[complete.cases(tmp),]
        if (nrow(tmp) != 0) bd$unificada <- rbind(bd$unificada , tmp)
      }
      # =
      if ('C5' %in% input$filtro_bd) {
        c5_tmp = c5
        # =
        if (input$filtro_incidente == 'Decesos') {
          c5_tmp <- filter(c5_tmp , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
        } else if (input$filtro_incidente == 'Lesionados') {
          c5_tmp <- filter(c5_tmp , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
        } else if (input$filtro_incidente == 'Accidentes') {
          c5_tmp <- filter(c5_tmp , incidente_c4 == 'accidente-choque sin lesionados' | incidente_c4 == 'detención ciudadana-accidente automovilístico' | incidente_c4 == 'accidente-ciclista' | incidente_c4 == 'accidente-ferroviario' | incidente_c4 == 'accidente-monopatín' | incidente_c4 == 'accidente-motociclista' | incidente_c4 == 'accidente-otros')
        }
        # =
        c5_tmp <- filter(c5_tmp , timestamp >= dates(as.character(input$filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(input$filtro_fecha[2]) , format = 'y-m-d'))
        # =
        tmp = data.frame(id_original = as.character(c5_tmp$folio),
                         base_original = replicate(nrow(c5_tmp) , 'C5'),
                         timestamp = c5_tmp$timestamp,
                         geometry = c5_tmp$geometry)
        # tmp = tmp[complete.cases(tmp),]
        if (nrow(tmp) != 0) bd$unificada <- rbind(bd$unificada , tmp)
      }
      # =
      bd$unificada['id_original'] <- as.character(bd$unificada$id_original)
      # =
      bd$unificada['id_global'] <- seq.int(nrow(bd$unificada))
      bd$unificada['id_PGJ'] <- as.character()
      bd$unificada['id_SSC'] <- as.character()
      bd$unificada['id_C5'] <- as.character()
      # =
      bd$unificada$id_PGJ <- as.character(bd$unificada$id_PGJ)
      bd$unificada$id_SSC <- as.character(bd$unificada$id_SSC)
      bd$unificada$id_C5 <- as.character(bd$unificada$id_C5)
      # =
      bd$unificada[bd$unificada$base_original == 'PGJ' , 'id_PGJ'] <- as.character(bd$unificada[bd$unificada$base_original == 'PGJ' , 'id_original'])
      bd$unificada[bd$unificada$base_original == 'SSC' , 'id_SSC'] <- as.character(bd$unificada[bd$unificada$base_original == 'SSC' , 'id_original'])
      bd$unificada[bd$unificada$base_original == 'C5' , 'id_C5'] <- as.character(bd$unificada[bd$unificada$base_original == 'C5' , 'id_original'])
      # =
      removeUI(selector = '#div_alerta_d')
      showElement(id = 'div_tab2_a' , anim = TRUE , animType = 'fade')
      hideElement(id = 'div_tab2_b' , anim = TRUE , animType = 'fade')
      hideElement(id = 'div_tab2_c' , anim = TRUE , animType = 'fade')
      # =
      updateTabItems(session , inputId = 'menu_completo' , selected = 'vinculacion')
    }
    else if (input$generar_bd > 1) {
      showModal(modalDialog(title = NULL , footer = NULL,
                            tags$p(style = 'font-size: 20px;',
                                   strong('Importante')),
                            tags$p(style = 'text-align: justify',
                                   'Reiniciar la vinculación eliminará por completo todo el trabajo realizado hasta ahora, y tendrá que iniciar el proceso nuevamente desde el inicio.'),
                            tags$p(style = 'font-size: 25px; text-align: center;',
                                   strong('¿Desea reiniciar la Vinculación?')),
                            fluidRow(column(2, offset = 4,
                                            actionButton(inputId = 'reiniciar' , label = strong('Reiniciar'),
                                                         style = 'background-color: #00A65B; color: white; border-color: ;')),
                                     column(2,
                                            actionButton(inputId = 'no_reiniciar' , label = strong('Cancelar'),
                                                         style = 'background-color: #DD4C39; color: white; border-color: ;')))))
    }
  })
  
  observeEvent(input$reiniciar , {
    removeModal()
    key_remote$k_tab2 <- 1
    insertUI(selector = '#div_alerta_c' , where = 'afterEnd' , immediate = TRUE,
             tags$div(id = 'div_alerta_d' ,
                      strong('Por favor espere en lo que se cargan los datos') , tags$img(src = 'loading.gif' , style = 'height: 25px;')))
    # =
    bd$unificada <- data.frame(id_original = as.character() , base_original = as.character(), timestamp = chron() , geometry = st_sfc(crs = 32614))
    # =
    if ('PGJ' %in% input$filtro_bd) {
      pgj_tmp = pgj
      # =
      if (input$filtro_incidente == 'Decesos') {
        pgj_tmp <- filter(pgj_tmp , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
      } else if (input$filtro_incidente == 'Lesionados') {
        pgj_tmp <- filter(pgj_tmp , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
      }
      # =
      pgj_tmp <- filter(pgj_tmp , timestamp >= dates(as.character(input$filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(input$filtro_fecha[2]) , format = 'y-m-d'))
      # =
      tmp = data.frame(id_original = as.character(pgj_tmp$id),
                       base_original = replicate(nrow(pgj_tmp) , 'PGJ'),
                       timestamp = pgj_tmp$timestamp,
                       geometry = pgj_tmp$geometry)
      # tmp = tmp[complete.cases(tmp),]
      if (nrow(tmp) != 0) bd$unificada <- rbind(bd$unificada , tmp)
    }
    # =
    if ('SSC' %in% input$filtro_bd) {
      ssc_tmp = ssc
      # =
      if (input$filtro_incidente == 'Decesos') {
        ssc_tmp <- filter(ssc_tmp , total_occisos > 0)
      } else if (input$filtro_incidente == 'Lesionados') {
        ssc_tmp <- filter(ssc_tmp , total_lesionados > 0 & total_occisos == 0)
      }
      # =
      ssc_tmp <- filter(ssc_tmp , timestamp >= dates(as.character(input$filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(input$filtro_fecha[2]) , format = 'y-m-d'))
      # =
      tmp = data.frame(id_original = as.character(ssc_tmp$id),
                       base_original = replicate(nrow(ssc_tmp) , 'SSC'),
                       timestamp = ssc_tmp$timestamp,
                       geometry = ssc_tmp$geometry)
      # tmp = tmp[complete.cases(tmp),]
      if (nrow(tmp) != 0) bd$unificada <- rbind(bd$unificada , tmp)
    }
    # =
    if ('C5' %in% input$filtro_bd) {
      c5_tmp = c5
      # =
      if (input$filtro_incidente == 'Decesos') {
        c5_tmp <- filter(c5_tmp , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
      } else if (input$filtro_incidente == 'Lesionados') {
        c5_tmp <- filter(c5_tmp , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
      }
      # =
      c5_tmp <- filter(c5_tmp , timestamp >= dates(as.character(input$filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(input$filtro_fecha[2]) , format = 'y-m-d'))
      # =
      tmp = data.frame(id_original = as.character(c5_tmp$folio),
                       base_original = replicate(nrow(c5_tmp) , 'C5'),
                       timestamp = c5_tmp$timestamp,
                       geometry = c5_tmp$geometry)
      # tmp = tmp[complete.cases(tmp),]
      if (nrow(tmp) != 0) bd$unificada <- rbind(bd$unificada , tmp)
    }
    # =
    bd$unificada['id_global'] <- seq.int(nrow(bd$unificada))
    bd$unificada['id_PGJ'] <- as.character()
    bd$unificada['id_SSC'] <- as.character()
    bd$unificada['id_C5'] <- as.character()
    # =
    bd$unificada$id_PGJ <- as.character(bd$unificada$id_PGJ)
    bd$unificada$id_SSC <- as.character(bd$unificada$id_SSC)
    bd$unificada$id_C5 <- as.character(bd$unificada$id_C5)
    # =
    bd$unificada[bd$unificada$base_original == 'PGJ' , 'id_PGJ'] <- as.character(bd$unificada[bd$unificada$base_original == 'PGJ' , 'id_original'])
    bd$unificada[bd$unificada$base_original == 'SSC' , 'id_SSC'] <- as.character(bd$unificada[bd$unificada$base_original == 'SSC' , 'id_original'])
    bd$unificada[bd$unificada$base_original == 'C5' , 'id_C5'] <- as.character(bd$unificada[bd$unificada$base_original == 'C5' , 'id_original'])
    # =
    removeUI(selector = '#div_alerta_d')
    showElement(id = 'div_tab2_a' , anim = TRUE , animType = 'fade')
    hideElement(id = 'div_tab2_b' , anim = TRUE , animType = 'fade')
    hideElement(id = 'div_tab2_c' , anim = TRUE , animType = 'fade')
    updateTabItems(session , inputId = 'menu_completo' , selected = 'vinculacion')
    eventos_mapa$aux1 <- NULL
    eventos_mapa$aux1_t <- NULL
    eventos_mapa$aux2 <- NULL
    eventos_mapa$aux2_t <- NULL
    # =
    mapa_proxy %>%
      clearShapes() %>%
      clearMarkers() %>%
      setView(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
      addPolygons(data = cdmx_sa , fillColor = '#57948B' , fillOpacity = 0.25 , color = '#002A24' , opacity = 0.75)
  })
  
  observeEvent(input$no_reiniciar , removeModal())
  
  # ===== VINCULAR EVENTOS =====
  # = Definir Referencia
  observeEvent(bd$unificada , {
    tmp <- count(bd$unificada , base_original)
    tmp <- tmp[order(-tmp$n),]
    bd$reference <- as.character(tmp[1,]$base_original)
  })
  
  output$tabla_bd_eventos <- renderTable(striped = TRUE , bordered = TRUE , spacing = 'xs', {
    tmp <- data.frame(tmp = 1)
    for (i in input$filtro_bd) {
      tmp[i] <- nrow(filter(bd$unificada , base_original == i))
    }
    tmp$tmp <- NULL
    tmp
  })
  output$texto_bd_imp <- renderText(bd$reference)
  
  # Update Slider
  observeEvent(bd$unificada , {
    if (!is.null(bd$unificada)) {
      total = nrow(filter(bd$unificada , base_original == bd$reference))
      # =
      min = (25 / total) * 100
      if (min < 1) min = round(min , 2)
      else min = floor(min)
      # =
      max = (150 / total) * 100
      if (max < 1) max = round(max , 2)
      else max = ceiling(max)
      # =
      if (min < 1 | max < 1) step = 0.01
      else NULL
      # =
      updateSliderInput(session , inputId = 'no_muestras' , min = min , max = max , step = step , value = (min + max) / 2)
    }
  })
  
  output$texto_muestras_a <- renderText(floor(nrow(filter(bd$unificada , base_original == bd$reference)) * (input$no_muestras / 100)))
  output$texto_muestras_b <- renderText({
    if (input$filtro_incidente == 'Todos') 'Incidentes'
    else input$filtro_incidente
  })
  output$texto_muestras_c <- renderText(nrow(filter(bd$unificada , base_original == bd$reference)))
  
  observeEvent(input$boton_muestra , {
    hideElement(id = 'div_tab2_a' , anim = TRUE , animType = 'fade')
    showElement(id = 'div_tab2_b' , anim = TRUE , animType = 'fade')
    # =
    tmp <- filter(bd$unificada , base_original == bd$reference)
    tmp2 <- floor(nrow(tmp) * (input$no_muestras / 100))
    bd$muestra <- tmp[sample(nrow(tmp) , tmp2) ,]
    # print(bd$muestra)
    # =
    count_muestra$i <- 1
    count_muestra$max <- nrow(bd$unificada)
    eventos_mapa$principal <- bd$muestra[count_muestra$i,]
    lat = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[2]
    lon = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[1]
    # =
    mapa_proxy %>%
      flyTo(lng = lon , lat = lat , zoom = 16) %>%
      addCircleMarkers(lng = lon , lat = lat , group = 'principal',
                       stroke = FALSE , fillOpacity = 0.85 , radius = 20,
                       fillColor = ifelse(bd$reference == 'PGJ' , '#952800' ,
                                          ifelse(bd$reference == 'SSC' , '#043A5F',
                                                 ifelse(bd$reference == 'C5' , '#956F00' , "#03F")))) %>%
      clearShapes()
  })
  # ===
  # = Tabla Principal
  output$texto_bd_principal <- renderText({
    if (bd$reference == 'PGJ') 'Procuraduría General de Justicia (PGJ)'
    else if (bd$reference == 'SSC') 'Secretaría de Seguridad Ciudadana (SSC)'
    else if (bd$reference == 'C5') 'Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México (C5)'
  })
  output$tabla_principal <- renderTable(colnames = FALSE , {
    if (!is.null(eventos_mapa$principal)) {
      a = data.frame(variable = as.character() , dato = as.character())
      a$variable <- as.character(a$variable)
      a$dato <- as.character(a$dato)
      if (eventos_mapa$principal$base_original == 'PGJ') {
        tmp <- filter(special_bd$active_pgj , id == eventos_mapa$principal$id_original)
        tmp['geometry'] <- NULL
        a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
        a[nrow(a) + 1,] = c('Delito', str_to_title(tmp$delito , locale = 'es'))
        a[nrow(a) + 1,] = c('Calle', str_to_title(tmp$calle_hechos , locale = 'es'))
        a[nrow(a) + 1,] = c('Colonia', str_to_title(tmp$colonia_hechos , locale = 'es'))
        a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$alcaldia_hechos , locale = 'es'))
      }
      if (eventos_mapa$principal$base_original == 'SSC') {
        tmp <- filter(ssc , id == eventos_mapa$principal$id_original)
        tmp['geometry'] <- NULL
        a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
        a[nrow(a) + 1,] = c('Tipo de Evento', str_to_title(tmp$tipo_evento , locale = 'es'))
        a[nrow(a) + 1,] = c('Calle', str_to_title(tmp$punto_1 , locale = 'es'))
        a[nrow(a) + 1,] = c('Colonia', str_to_title(tmp$colonia , locale = 'es'))
        a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$alcaldia , locale = 'es'))
        a[nrow(a) + 1,] = c('Identidad de Víctima', str_to_title(tmp$identidad , locale = 'es'))
      }
      if (eventos_mapa$principal$base_original == 'C5') {
        tmp <- filter(special_bd$active_c5 , folio == eventos_mapa$principal$id_original)
        tmp['geometry'] <- NULL
        a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
        a[nrow(a) + 1,] = c('Incidente C4', str_to_title(tmp$incidente_c4 , locale = 'es'))
        a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$delegacion_inicio , locale = 'es'))
        a[nrow(a) + 1,] = c('Tipo de Entrada', str_to_title(tmp$tipo_entrada , locale = 'es'))
      }
      a
    }
  })
  
  # = Círculo en Mapa
  observeEvent(c(input$filtro_distancia , eventos_mapa$principal) , {
    if (!is.null(eventos_mapa$principal)) {
      mapa_proxy %>%
        clearGroup(group = 'circulo') %>%
        addCircles(lng = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[1] ,
                   lat = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[2] ,
                   radius = input$filtro_distancia , group = 'circulo',
                   color = '#06433B' , opacity = 1 ,
                   fillColor = '#175D53' , fillOpacity = 0.2)
    }
  })
  # = Auxiliares en Mapa
  observeEvent(c(input$filtro_distancia , input$filtro_tiempo , eventos_mapa$principal) , {
    if (!is.null(eventos_mapa$principal)) {
      tmp <- filter(bd$unificada , base_original != bd$reference)
      # =
      t_min <- eventos_mapa$principal$timestamp - (input$filtro_tiempo / 1440)
      t_max <- eventos_mapa$principal$timestamp + (input$filtro_tiempo / 1440)
      tmp <- filter(tmp , timestamp > t_min & timestamp < t_max)
      # =
      tmp_within <- st_is_within_distance(eventos_mapa$principal$geometry , tmp$geometry , input$filtro_distancia)
      tmp <- tmp[tmp_within[[1]],]
      # =
      print(tmp)
      bd$posibles <- tmp
      mapa_proxy %>%
        clearGroup(group = 'auxiliar_1') %>%
        clearGroup(group = 'auxiliar_2') %>%
        clearGroup(group = 'auxiliar_3') %>%
        addCircleMarkers(data = st_transform(tmp$geometry , 4326) ,
                         group = ifelse(tmp$base_original == 'PGJ' , 'auxiliar_1',
                                        ifelse(tmp$base_original == 'SSC' , 'auxiliar_2',
                                               ifelse(tmp$base_original == 'C5' , 'auxiliar_3' , '???'))),
                         stroke = FALSE , fillOpacity = 0.85 , radius = 20,
                         fillColor = ifelse(tmp$base_original == 'PGJ' , '#952800' ,
                                            ifelse(tmp$base_original == 'SSC' , '#043A5F',
                                                   ifelse(tmp$base_original == 'C5' , '#956F00' , "#03F"))))
    }
  })
  
  # = Tabla Auxiliar A
  output$texto_bd_auxiliar_a <- renderText({
    a <- input$filtro_bd[input$filtro_bd != bd$reference][1]
    eventos_mapa$aux1_t <- a
    if (a == 'PGJ') 'Procuraduría General de Justicia (PGJ)'
    else if (a == 'SSC') 'Secretaría de Seguridad Ciudadana (SSC)'
    else if (a == 'C5') 'Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México (C5)'
  })
  output$tabla_bd_auxiliar_a <- renderTable(colnames = FALSE , eventos_mapa$aux1)
  
  # = Tabla Auxiliar B
  output$texto_bd_auxiliar_b <- renderText({
    if (!is.na(input$filtro_bd[input$filtro_bd != bd$reference][2])) {
      a <- input$filtro_bd[input$filtro_bd != bd$reference][2]
      eventos_mapa$aux2_t <- a 
      if (a == 'PGJ') 'Procuraduría General de Justicia (PGJ)'
      else if (a == 'SSC') 'Secretaría de Seguridad Ciudadana (SSC)'
      else if (a == 'C5') 'Centro de Comando, Control, Cómputo, Comunicaciones y Contacto Ciudadano de la Ciudad de México (C5)'
    }
  })
  output$tabla_bd_auxiliar_b <- renderTable(colnames = FALSE , {
    if (!is.na(input$filtro_bd[input$filtro_bd != bd$reference][2])) {
      eventos_mapa$aux2
    }
  })
  
  # = Definición Correcta de Tablas y Cosas
  observeEvent(input$mapa_marker_click , {
    a = data.frame(variable = as.character() , dato = as.character())
    a$variable <- as.character(a$variable)
    a$dato <- as.character(a$dato)
    # =
    print(input$mapa_marker_click)
    if (eventos_mapa$aux1_t == 'PGJ' & input$mapa_marker_click$group == 'auxiliar_1') {
      punto <- st_transform((st_sfc(st_point(x = c(input$mapa_marker_click$lng , input$mapa_marker_click$lat) , dim = 'XY') , crs = 4326)), 32614)
      tmp_within <- st_is_within_distance(punto , bd$unificada$geometry , 1)
      tmp <- bd$unificada[tmp_within[[1]],]
      tmp <- filter(tmp , base_original == 'PGJ')
      tmp <- filter(tmp , id_global %in% bd$posibles$id_global)
      seleccionados$a <- tmp
      tmp <- filter(special_bd$active_pgj , id == tmp$id_original)
      # =
      tmp['geometry'] <- NULL
      a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
      a[nrow(a) + 1,] = c('Delito', str_to_title(tmp$delito , locale = 'es'))
      a[nrow(a) + 1,] = c('Calle', str_to_title(tmp$calle_hechos , locale = 'es'))
      a[nrow(a) + 1,] = c('Colonia', str_to_title(tmp$colonia_hechos , locale = 'es'))
      a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$alcaldia_hechos , locale = 'es'))
      eventos_mapa$aux1 <- a
    }
    else if (eventos_mapa$aux1_t == 'SSC' & input$mapa_marker_click$group == 'auxiliar_2') {
      punto <- st_transform((st_sfc(st_point(x = c(input$mapa_marker_click$lng , input$mapa_marker_click$lat) , dim = 'XY') , crs = 4326)), 32614)
      tmp_within <- st_is_within_distance(punto , bd$unificada$geometry , 1)
      tmp <- bd$unificada[tmp_within[[1]],]
      tmp <- filter(tmp , base_original == 'SSC')
      tmp <- filter(tmp , id_global %in% bd$posibles$id_global)
      seleccionados$a <- tmp
      tmp <- filter(ssc , id == tmp$id_original)
      # =
      tmp['geometry'] <- NULL
      a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
      a[nrow(a) + 1,] = c('Tipo de Evento', str_to_title(tmp$tipo_evento , locale = 'es'))
      a[nrow(a) + 1,] = c('Calle', str_to_title(tmp$punto_1 , locale = 'es'))
      a[nrow(a) + 1,] = c('Colonia', str_to_title(tmp$colonia , locale = 'es'))
      a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$alcaldia , locale = 'es'))
      a[nrow(a) + 1,] = c('Identidad de Víctima', str_to_title(tmp$identidad , locale = 'es'))
      eventos_mapa$aux1 <- a
    }
    else if (eventos_mapa$aux1_t == 'C5' & input$mapa_marker_click$group == 'auxiliar_3') {
      punto <- st_transform((st_sfc(st_point(x = c(input$mapa_marker_click$lng , input$mapa_marker_click$lat) , dim = 'XY') , crs = 4326)), 32614)
      tmp_within <- st_is_within_distance(punto , bd$unificada$geometry , 1)
      tmp <- bd$unificada[tmp_within[[1]],]
      tmp <- filter(tmp , base_original == 'C5')
      tmp <- filter(tmp , id_global %in% bd$posibles$id_global)
      seleccionados$a <- tmp
      tmp <- filter(special_bd$active_c5 , folio == tmp$id_original)
      # =
      tmp['geometry'] <- NULL
      a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
      a[nrow(a) + 1,] = c('Incidente C4', str_to_title(tmp$incidente_c4 , locale = 'es'))
      a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$delegacion_inicio , locale = 'es'))
      a[nrow(a) + 1,] = c('Tipo de Entrada', str_to_title(tmp$tipo_entrada , locale = 'es'))
      eventos_mapa$aux1 <- a
    }
  })
  
  observeEvent(input$mapa_marker_click , {
    if (!is.na(input$filtro_bd[input$filtro_bd != bd$reference][2])) {
      a = data.frame(variable = as.character() , dato = as.character())
      a$variable <- as.character(a$variable)
      a$dato <- as.character(a$dato)
      # =
      if (eventos_mapa$aux2_t == 'PGJ' & input$mapa_marker_click$group == 'auxiliar_1') {
        punto <- st_transform((st_sfc(st_point(x = c(input$mapa_marker_click$lng , input$mapa_marker_click$lat) , dim = 'XY') , crs = 4326)), 32614)
        tmp_within <- st_is_within_distance(punto , bd$unificada$geometry , 1)
        tmp <- bd$unificada[tmp_within[[1]],]
        tmp <- filter(tmp , base_original == 'PGJ')
        tmp <- filter(tmp , id_global %in% bd$posibles$id_global)
        seleccionados$b <- tmp
        tmp <- filter(special_bd$active_pgj , id == tmp$id_original)
        # =
        tmp['geometry'] <- NULL
        a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
        a[nrow(a) + 1,] = c('Delito', str_to_title(tmp$delito , locale = 'es'))
        a[nrow(a) + 1,] = c('Calle', str_to_title(tmp$calle_hechos , locale = 'es'))
        a[nrow(a) + 1,] = c('Colonia', str_to_title(tmp$colonia_hechos , locale = 'es'))
        a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$alcaldia_hechos , locale = 'es'))
        eventos_mapa$aux2 <- a
      }
      else if (eventos_mapa$aux2_t == 'SSC' & input$mapa_marker_click$group == 'auxiliar_2') {
        punto <- st_transform((st_sfc(st_point(x = c(input$mapa_marker_click$lng , input$mapa_marker_click$lat) , dim = 'XY') , crs = 4326)), 32614)
        tmp_within <- st_is_within_distance(punto , bd$unificada$geometry , 1)
        tmp <- bd$unificada[tmp_within[[1]],]
        tmp <- filter(tmp , base_original == 'SSC')
        tmp <- filter(tmp , id_global %in% bd$posibles$id_global)
        seleccionados$b <- tmp
        tmp <- filter(ssc , id == tmp$id_original)
        # =
        tmp['geometry'] <- NULL
        a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
        a[nrow(a) + 1,] = c('Tipo de Evento', str_to_title(tmp$tipo_evento , locale = 'es'))
        a[nrow(a) + 1,] = c('Calle', str_to_title(tmp$punto_1 , locale = 'es'))
        a[nrow(a) + 1,] = c('Colonia', str_to_title(tmp$colonia , locale = 'es'))
        a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$alcaldia , locale = 'es'))
        a[nrow(a) + 1,] = c('Identidad de Víctima', str_to_title(tmp$identidad , locale = 'es'))
        eventos_mapa$aux2 <- a
      }
      else if (eventos_mapa$aux2_t == 'C5' & input$mapa_marker_click$group == 'auxiliar_3') {
        punto <- st_transform((st_sfc(st_point(x = c(input$mapa_marker_click$lng , input$mapa_marker_click$lat) , dim = 'XY') , crs = 4326)), 32614)
        tmp_within <- st_is_within_distance(punto , bd$unificada$geometry , 1)
        tmp <- bd$unificada[tmp_within[[1]],]
        tmp <- filter(tmp , base_original == 'C5')
        tmp <- filter(tmp , id_global %in% bd$posibles$id_global)
        seleccionados$b <- tmp
        tmp <- filter(special_bd$active_c5 , folio == tmp$id_original)
        # =
        tmp['geometry'] <- NULL
        a[nrow(a) + 1,] = c('Fecha y Hora de Hechos', format(tmp$timestamp , format = '%d/%m/%Y, %T'))
        a[nrow(a) + 1,] = c('Incidente C4', str_to_title(tmp$incidente_c4 , locale = 'es'))
        a[nrow(a) + 1,] = c('Alcaldía', str_to_title(tmp$delegacion_inicio , locale = 'es'))
        a[nrow(a) + 1,] = c('Tipo de Entrada', str_to_title(tmp$tipo_entrada , locale = 'es'))
        eventos_mapa$aux2 <- a
      }
    }
    
  })
  
  # = Evento Vinculado
  observeEvent(input$boton_vincular , {
    if (count_muestra$i >= (nrow(bd$muestra))) {
      hideElement(id = 'div_tab2_a' , anim = TRUE , animType = 'fade')
      hideElement(id = 'div_tab2_b' , anim = TRUE , animType = 'fade')
      showElement(id = 'div_tab2_c' , anim = TRUE , animType = 'fade')
      # =
      mapa_proxy %>%
        clearShapes() %>%
        clearMarkers() %>%
        addTiles(urlTemplate = '//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        flyTo(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
        addPolygons(fillColor = '#57948B' , fillOpacity = 0.25 , color = '#002A24' , opacity = 0.75 , data = cdmx_sa)
    }
    else {
      if (!is.null(eventos_mapa$aux1)) {
        if (eventos_mapa$aux1_t == 'PGJ') bd$unificada[bd$unificada$id_global == eventos_mapa$principal$id_global | bd$unificada$id_global == seleccionados$b$id_global, 'id_PGJ'] <- as.character(seleccionados$a$id_original)
        else if (eventos_mapa$aux1_t == 'SSC') bd$unificada[bd$unificada$id_global == eventos_mapa$principal$id_global | bd$unificada$id_global == seleccionados$b$id_global, 'id_SSC'] <- as.character(seleccionados$a$id_original)
        else if (eventos_mapa$aux1_t == 'C5') bd$unificada[bd$unificada$id_global == eventos_mapa$principal$id_global | bd$unificada$id_global == seleccionados$b$id_global, 'id_C5'] <- as.character(seleccionados$a$id_original)
      }
      if (!is.null(eventos_mapa$aux2)) {
        if (eventos_mapa$aux2_t == 'PGJ') bd$unificada[bd$unificada$id_global == eventos_mapa$principal$id_global | bd$unificada$id_global == seleccionados$a$id_global, 'id_PGJ'] <- as.character(seleccionados$b$id_original)
        else if (eventos_mapa$aux2_t == 'SSC') bd$unificada[bd$unificada$id_global == eventos_mapa$principal$id_global | bd$unificada$id_global == seleccionados$a$id_global, 'id_SSC'] <- as.character(seleccionados$b$id_original)
        else if (eventos_mapa$aux2_t == 'C5') bd$unificada[bd$unificada$id_global == eventos_mapa$principal$id_global | bd$unificada$id_global == seleccionados$a$id_global, 'id_C5'] <- as.character(seleccionados$b$id_original)
      }
      # ===
      count_muestra$i <- count_muestra$i + 1
      eventos_mapa$principal <- bd$muestra[count_muestra$i,]
      lat = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[2]
      lon = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[1]
      # =
      mapa_proxy %>%
        clearShapes() %>%
        clearMarkers() %>%
        flyTo(lng = lon , lat = lat , zoom = 16) %>%
        addCircleMarkers(lng = lon , lat = lat , group = 'principal',
                         stroke = FALSE , fillOpacity = 0.85 , radius = 20,
                         fillColor = ifelse(bd$reference == 'PGJ' , '#952800' ,
                                            ifelse(bd$reference == 'SSC' , '#043A5F',
                                                   ifelse(bd$reference == 'C5' , '#956F00' , "#03F"))))
      # =
      eventos_mapa$aux1 <- NULL
      eventos_mapa$aux2 <- NULL
    }
  })
  
  # = Evento no Encontrado
  observeEvent(input$boton_novincular , {
    if (count_muestra$i >= (nrow(bd$muestra))) {
      hideElement(id = 'div_tab2_a' , anim = TRUE , animType = 'fade')
      hideElement(id = 'div_tab2_b' , anim = TRUE , animType = 'fade')
      showElement(id = 'div_tab2_c' , anim = TRUE , animType = 'fade')
      # =
      mapa_proxy %>%
        clearShapes() %>%
        clearMarkers() %>%
        addTiles(urlTemplate = '//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        flyTo(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
        addPolygons(fillColor = '#57948B' , fillOpacity = 0.25 , color = '#002A24' , opacity = 0.75 , data = cdmx_sa)
    }
    else {
      eventos_mapa$aux1 <- NULL
      eventos_mapa$aux2 <- NULL
      # =
      count_muestra$i <- count_muestra$i + 1
      eventos_mapa$principal <- bd$muestra[count_muestra$i,]
      lat = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[2]
      lon = st_coordinates(st_transform(eventos_mapa$principal$geometry , 4326))[1]
      # =
      mapa_proxy %>%
        clearShapes() %>%
        clearMarkers() %>%
        flyTo(lng = lon , lat = lat , zoom = 16) %>%
        addCircleMarkers(lng = lon , lat = lat , group = 'principal',
                         stroke = FALSE , fillOpacity = 0.85 , radius = 20,
                         fillColor = ifelse(bd$reference == 'PGJ' , '#952800' ,
                                            ifelse(bd$reference == 'SSC' , '#043A5F',
                                                   ifelse(bd$reference == 'C5' , '#956F00' , "#03F"))))
    }
      
  })
  
  # ===
  output$vinculos_logrados_a <- renderDataTable({
    if ('C5' %in% input$filtro_bd) {
       tmp <- filter(bd$unificada , !is.na(id_PGJ) & !is.na(id_SSC) & !is.na(id_C5))
       tmp$timestamp <- format(tmp$timestamp , format = '%d/%m/%Y, %T')
       tmp <- tmp %>% select('timestamp' , 'id_PGJ' , 'id_SSC' , 'id_C5') %>% rename('Fecha y Hora de Hechos'='timestamp')
    }
  })
  
  # ===== PARÁMETROS POR DEFECTO =====
  observeEvent(input$boton_defecto , {
    showModal(modalDialog(title = NULL , footer = NULL ,
                          tags$p(style = 'font-size: 20px;', class = 'finalizar_defecto',
                                 strong('Ejecutando Algoritmo de Vinculación')),
                          tags$p('Espere mientras se vinculan todos los Incidentes Viales', class = 'finalizar_defecto'),
                          tags$div(id = 'div_load_pgj', class = 'finalizar_defecto', style = 'display: block; margin: auto; width: 50%;',
                                   tags$img(src = 'loading.gif' , style = 'max-width:100%; max-height:100%; vertical-align: middle;'))))
    # =
    distancia_final <- 600 # metros
    tiempo_final <- 2.5 / 24 # días
    # =
    algoritmo_sp <- function(fila , bd) {
      print('Yoshi')
      bd_id <- paste0('id_' , bd)
      # =
      if(!is.na(fila[bd_id][[1]])) {
        return(as.character(fila[bd_id]))}
      # =
      shp <- filter(bd$unificada , id_global == as.integer(fila['id_global']))
      tmp_within <- st_is_within_distance(shp$geometry , bd$unificada$geometry , distancia_final)
      posibles <- bd$unificada[tmp_within[[1]],]
      # =
      posibles <- filter(posibles , base_original == bd)
      # =
      min_tiempo <- shp$timestamp - tiempo_final
      max_tiempo <- shp$timestamp + tiempo_final
      posibles <- filter(posibles , timestamp >= min_tiempo & timestamp <= max_tiempo)
      # =
      if (nrow(posibles) > 1) {
        posibles <- posibles[which.min(posibles$timestamp - shp$timestamp) , ]
        return(as.character(posibles$id_original))}
      else if (is.null(nrow(posibles))) {
        return(NA)}
      else {
        return(as.character(posibles$id_original))}
    }
    # =
    # for (bd in input$filtro_bd) {
    #   id <- paste0('id_',bd)
    #   bd$unificada$tmp <- as.character(apply(bd$unificada , MARGIN = 1 , FUN = algoritmo_sp , bd = bd))
    #   bd$unificada[id] <- bd$unificada$tmp
    # }
    # # =
    # a <- filter(bd$unificada , id_PGJ == 'character(0)')$id_global
    # bd$unificada[bd$unificada$id_global %in% a , 'id_PGJ'] <- NA
    # # =
    # a <- filter(bd$unificada , id_SSC == 'character(0)')$id_global
    # bd$unificada[bd$unificada$id_global %in% a , 'id_SSC'] <- NA
    # # =
    # a <- filter(bd$unificada , id_C5 == 'character(0)')$id_global
    # bd$unificada[bd$unificada$id_global %in% a , 'id_C5'] <- NA
    # =
    Sys.sleep(5)
    tmp$a <- 1
    removeModal()
    updateTabItems(session , inputId = 'menu_completo' , selected = 'resultados')
  })
  
  output$menu_3 <- renderMenu({
    if (is.null(tmp$a)) NULL
    else menuItem(text = 'Resultados', icon = icon('chart-bar') , tabName = 'resultados')
  })
  
  output$mapa_2 <- renderLeaflet({
    leaflet(data = cdmx_sa) %>%
      addTiles(urlTemplate = '//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      setView(lng = -99.152613 , lat = 19.320497, zoom = 11) %>%
      addPolygons(fillColor = '#57948B' , fillOpacity = 0.25 , color = '#002A24' , opacity = 0.75)
  })
  
  mapa_proxy2 <- leafletProxy('mapa_2')
  
  
  
  
  # ===== MAPA RESULTADOS =====
  # observeEvent(input$resultados_bd , ignoreNULL = FALSE ,{
  #   tmp <- iv[1,]
  #   tmp <- tmp[-1,]
  #   if ('Totales' %in% input$resultados_bd) {
  #     a <- iv[1:159,]
  #     tmp <- rbind(tmp , a)}
  #   if ('Parciales' %in% input$resultados_bd) {
  #     a <- iv[160:324,]
  #     tmp <- rbind(tmp , a)}
  #   if ('Nulos' %in% input$resultados_bd) {
  #     a <- iv[160:643,]
  #     tmp <- rbind(tmp , a)}
  #   # =
  #   mapa_proxy2 %>%
  #     clearMarkers() %>%
  #     addCircleMarkers(data = st_transform(tmp, 4326))
  # })
  # 
  # output$grafica_sp <- renderPlot({
  #   if (input$tipo_grafica == 'Total de Incidentes') {
  #     count_pgj <- iv
  #     count_pgj$geometry <- NULL
  #     count_pgj <- count(count_pgj , month(timestamp)) %>% rename('mes'='month(timestamp)')
  #     count_pgj$etiqueta <- count_pgj$mes
  #     # =
  #     count_pgj$etiqueta[count_pgj$etiqueta == 1] <- 'Ene'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 2] <- 'Feb'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 3] <- 'Mar'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 4] <- 'Abr'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 5] <- 'May'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 6] <- 'Jun'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 7] <- 'Jul'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 8] <- 'Ago'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 9] <- 'Sep'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 10] <- 'Oct'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 11] <- 'Nov'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 12] <- 'Dic'
  #     # =
  #     grafica = ggplot() +
  #       geom_line(data = count_pgj , aes(x = mes , y = n) , size = 1) +
  #       scale_x_continuous(breaks = count_pgj$mes,
  #                          minor_breaks = NULL,
  #                          labels = count_pgj$etiqueta) +
  #       labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Mes')
  #     grafica
  #   }
  #   else {
  #     count_pgj <- iv
  #     count_pgj$geometry <- NULL
  #     count_pgj[is.na(count_pgj$tipo_vehiculo_1), 'tipo_vehiculo_1'] <- 'SD'
  #     count_pgj <- count(count_pgj , month(timestamp) , tipo_vehiculo_1 , .drop = FALSE) %>% rename('mes'='month(timestamp)')
  #     count_pgj$etiqueta <- count_pgj$mes
  #     # =
  #     count_pgj$etiqueta[count_pgj$etiqueta == 1] <- 'Ene'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 2] <- 'Feb'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 3] <- 'Mar'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 4] <- 'Abr'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 5] <- 'May'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 6] <- 'Jun'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 7] <- 'Jul'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 8] <- 'Ago'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 9] <- 'Sep'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 10] <- 'Oct'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 11] <- 'Nov'
  #     count_pgj$etiqueta[count_pgj$etiqueta == 12] <- 'Dic'
  #     # =
  #     tmp <- filter(count_pgj , tipo_vehiculo_1 != 'SD')
  #     grafica = ggplot() +
  #       geom_line(data = tmp , aes(x = mes , y = n , color = tipo_vehiculo_1) , size = 1) +
  #       scale_x_continuous(breaks = tmp$mes,
  #                          minor_breaks = NULL,
  #                          labels = tmp$etiqueta) +
  #       labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Mes' , color = 'Tipo de Vehículo')
  #     grafica
  #   }
  # })
}

shinyApp(ui = ui, server = server)