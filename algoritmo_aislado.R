library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

library(chron)
library(readxl)

# ¡Importante! Cambiar la ubicación del script
setwd('F:/accidentes_viales/04_unified')
# =====================================================
# Primero se crea, vacía, el DataFrame que contendrá todos los eventos unificados, con todo y el tipo
base_unificada <- data.frame(id_original = as.integer(),     # Columna con el ID original de cada evento, en su Base de Datos
                             base_original = as.character(), # String que especifica de dónde viene el evento
                             lat = as.numeric(),             # Latitud
                             lon = as.numeric(),             # Longitud
                             timestamp = chron())            # Timestamp derivado de fecha y hora

# Preprocesamiento de la base de la SSC
ssc <- read.csv("data/SSC.csv")
# Se conservan sólo los eventos donde existieron lesionados, más no difuntos
ssc <- filter(ssc , total_lesionados != 0 & total_occisos == 0)
# La hora se coloca en el formato correcto
ssc['hora'] <- times(as.numeric(as.character(ssc$hora)))
# Lo mismo para la fecha
ssc['fecha_evento'] <- dates(as.character(ssc$fecha_evento) , format = c(dates='y-m-d'))
# Se juntan la hora y la fecha para crear un Timestamp
ssc['timestamp'] <- chron(ssc$fecha_evento , ssc$hora)

# Preprocesamiento de la base de la PGJ
pgj <- read.csv("data/PGJ.csv")
# Se mantienen sólo los registros donde derivaron lesionados, según el tipo del delito cometido
pgj <- filter(pgj , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
# Misma lógica para crear el timestamp
pgj['hora_de_hechos'] <- times(paste0(pgj$hora_de_hechos,':00'))
pgj['fecha_de_hechos'] <- dates(as.character(pgj$fecha_de_hechos) , format = c(dates = 'y-m-d'))
pgj['timestamp'] <- chron(pgj$fecha_de_hechos , pgj$hora_de_hechos)

# Se reestructura en un DataFrame temporal la base de la PGJ, según el formato que necesita la Base Unificada
tmp <- data.frame(id_original = pgj$X,
                  base_original = replicate(nrow(pgj) , 'PGJ'),
                  lat = pgj$latitud ,
                  lon = pgj$longitud ,
                  timestamp = pgj$timestamp)

# Se eliminan todas las filas donde, en cualquiera de las columnas, exista un valor nulo
tmp <- tmp[complete.cases(tmp),]

# Se adicionan las filas de la PGJ a la Base Unificada
base_unificada <- rbind(base_unificada , tmp)

# Se realiza la misma reestructuración para la base de la SSC
tmp <- data.frame(id_original = ssc$X,
                  base_original = replicate(nrow(ssc) , 'SSC'),
                  lat = ssc$latitud ,
                  lon = ssc$longitud ,
                  timestamp = ssc$timestamp)

# Nuevamente, se eliminan todas las filas con valores nulos
tmp <- tmp[complete.cases(tmp),]

# Se añaden las filas de la SSC a la Base Unificada
base_unificada <- rbind(base_unificada , tmp)

# Una vez colocados todos los eventos, se crea un ID Global de cada evento para la Base Unificada
base_unificada$id_global <- seq.int(nrow(base_unificada))

# Se crea un nuevo DataFrame con una columna de geometría, para cualquier operacíon espacial. CRS = 32614
base_unificada_sf <- st_transform(st_as_sf(base_unificada , coords = c('lon','lat') , crs = 4326), 32614)

# Se definen valores para la distancia y tiempo promedio utilizados por el algoritmo de vinculación
# Éstos los definí a ojo de buen cubero, según todas las pruebas que he hecho, para este experimento rápido
distancia_final <- 600     # 600m
tiempo_final <- 0.1041667  # 2.5h

# Definir formalmente el algoritmo
# Lo que realiza en este caso es determinar qué ID del SSC corresponde a cada uno de los eventos de la Base Unificada
# Para los eventos de la PGJ, esto es la vinculación que hemos hecho hasta ahora con la aplicación
# Para los eventos de la SSC, es simplemente replicar el ID de su base original.
algoritmo_idssc <- function(fila) {
  if (fila['base_original'][[1]] == 'SSC') {
    return(as.integer(fila['id_original']))                                          # Si el evento viene de la base de SSC, entonces ése será lo que devuelva la función.
  } else {
    shp <- filter(base_unificada_sf , id_global == as.integer(fila['id_global']))    # Se aisla la geometría del evento/fila con la que se esté trabajando
    # =
    tmp_within <- st_is_within_distance(shp , base_unificada_sf , distancia_final)   # Todos los eventos de la Base Unificada dentro de la distancia establecida anteriormente
    posibles <- base_unificada[tmp_within[[1]],]                                     # Seleccionar todas las filas según los índices arrojados por la operacíon espacial
    # =
    posibles <- filter(posibles , id_global != as.integer(fila['id_global']))        # Eliminar la fila de la cual se está buscando enlazar  
    posibles <- filter(posibles , base_original == 'SSC')                            # Seleccionar sólo los eventos pertenecientes a la SSC
    # =
    min_tiempo <- shp$timestamp - tiempo_final                                       # Mínimo del intervalo del tiempo en el que puede encontrarse el vínculo
    max_tiempo <- shp$timestamp + tiempo_final                                       # Máximo del intervalo del tiempo en el que puede encontrarse el vínculo
    posibles <- filter(posibles , timestamp >= min_tiempo & timestamp <= max_tiempo) # Filtrar según los que se encuentren en el intervalo de tiempo dado
    # =
    if (length(posibles$id_original) > 1) {                                          # En teoría, debería quedar un solo registro de la SSC dado lo anterior; si aún hay más de uno
      posibles <- posibles[which.min(posibles$timestamp - shp$timestamp) , ]         # Se selecciona el que se encuentre más próximo en tiempos.
      return(posibles$id_original)
    } else {
      return(posibles$id_original)
    }
  }
}

# Se adiciona la columna del identificador del evento del SSC a la Base Unificada, que permite realizar el merge.
# La función tarda poco menos de 30s en ejecutar, por lo que hay que darle tiempo
base_unificada$id_ssc <- as.numeric(apply(base_unificada , MARGIN = 1 , FUN = algoritmo_idssc))

# Con sólo esto es posible obtener todos los números básicos necesarios para el análisis
# Número de Lesionados, según la SSC
sum(ssc$total_lesionados)

# Número de Lesionados registrados en la PGJ
nrow(pgj)

# Número de vínculos logrados entre la SSC y la PGJ
nrow(filter(base_unificada , !is.na(id_ssc) & base_original == 'PGJ'))

# En teoría, el Número de Vínculos no logrados sería la diferencia de los dos valores anteriores
nrow(pgj) - nrow(filter(base_unificada , !is.na(id_ssc) & base_original == 'PGJ'))

# Pero, esto, bajo el supuesto de que la base de la SSC contenga todos los eventos de Lesionados posibles
# Con difuntos, esto era posible ya que PGJ tenía todos los difuntos posibles, por lo que era imposible que
# SSC tuviese registrado algún difunto que PGJ no.
# Pero, para lesionados, PGJ SÍ tiene lesionados no registrados por la SSC
# Entonces, de la diferencia anterior... ¿cómo distinguir entre aquellos vínculos que no logró realizar el
# algoritmo (es decir, aquellos lesionados que existen tanto en PGJ como en SSC) y los eventos que sólo existen
# en una de las tablas, pero no en la otra? (es decir, aquellos sólo en PGJ pero no en SSC, nuestra teórica "Tabla Madre")

# El código anterior da una idea bastante clara de qué es lo que hace la aplicación que presentamos en general
# Así que, si hay alguna modificación del código o algo que les gustaría realizar, por favor no duden en decirme.

# ========================================================

# Código en Construcción. Favor de no continuar.
# ... De verás, puede usted dejar de leer en este momento.
# ... En serio, de aquí en adelante ya no hay nada más
# ... ¿Por qué sigue leyendo?
# ¿Cree que le estoy mintiendo a caso?
# ¿Yo? Un simple comentario en un código de R
# ¿Cree que tendría la capacidad de mentirle sobre lo que procede en este código?
# Digo, si hay más cosas
# Pero nada que de verdad tenga sentido
# En un futuro tal vez lo tenga, pero ahora mismo aún no.
# De todas formas, ¿quién soy yo para detener su lectura?
# Puede proseguir leyendo el código si así lo desea
# Pero no diga que no le advertí.

# algoritmo_idpgj <- function(fila) {
#   if (fila['base_original'][[1]] == 'PGJ') {
#     return(as.integer(fila['id_original']))
#   } else {
#     
#   }
# }
