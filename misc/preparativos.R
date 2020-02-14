setwd('/home/datalab/git/plataforma_semovi/visualizador')

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

# =========================================================

pastel_muerto <- read.csv('data/pastel_muerto.csv' , sep = ',', encoding = 'UTF-8', stringsAsFactors = FALSE)

# =========================================================

# # ===== Incidentes Viales =====
# iv <- read.csv('data/incidentes_viales.csv', encoding = 'UTF8' , stringsAsFactors = FALSE)
# iv['hora_incidente'] <- times(iv$hora_incidente)
# iv['fecha_incidente'] <- dates(iv$fecha_incidente , format = 'd/m/y')
# iv['timestamp'] <- chron(iv$fecha_incidente , iv$hora_incidente)
# iv <- st_transform(st_as_sf(iv , coords = c('lon','lat') , crs = 4326), 32614)
# 
# # ===== Vínculados Entrenamiento
# u <- read.csv('data/vinculados.csv', encoding = 'UTF8' , stringsAsFactors = FALSE)
# # u$timestamp <- chron(dates(str_sub(u$timestamp , 1 , 10) , format = 'd/m/y') , times(str_sub(u$timestamp , -8)))
# # u <- st_transform(st_as_sf(u , coords = c('lon','lat') , crs = 4326), 32614)
# 
# u_1 <- u[sample(nrow(u) , 105),]
# u_2 <- unificada[unificada$id_global %in% u$id_global & !unificada$id_global %in% u_1$id_global,]
# 
# m_dist <- mean(append(append(u_1$d_PGJ , u_1$d_SSC) , u_1$d_C5) , na.rm = TRUE)
# sd_dist <- sd(append(append(u_1$d_PGJ , u_1$d_SSC) , u_1$d_C5) , na.rm = TRUE)
# 
# m_tiempo <- mean(append(append(u_1$t_PGJ , u_1$t_SSC) , u_1$t_C5) , na.rm = TRUE) / 60 / 24
# sd_tiempo <- sd(append(append(u_1$t_PGJ , u_1$t_SSC) , u_1$t_C5) , na.rm = TRUE) / 60 / 24
# 
# distancia_final <- m_dist + 2*sd_dist
# tiempo_final <- m_tiempo + 2*sd_tiempo
# 
# for (bd in filtro_bd) {
#   id <- paste0('id_',bd)
#   u_2[id] <- as.character(apply(u_2 , MARGIN = 1 , FUN = algoritmo_sp , bd = bd))
# }
# 
# a <- filter(u_2 , id_PGJ == 'character(0)')$id_global
# u_2[u_2$id_global %in% a , 'id_PGJ'] <- NA
# # =
# a <- filter(u_2 , id_SSC == 'character(0)')$id_global
# u_2[u_2$id_global %in% a , 'id_SSC'] <- NA
# # =
# a <- filter(u_2 , id_C5 == 'character(0)')$id_global
# u_2[u_2$id_global %in% a , 'id_C5'] <- NA
# 
# u_2$geometry <- NULL
# 
# u_2c <- filter(u , !id_global %in% u_1$id_global)
# u_2c$id_C5[u_2c$id_C5 == ''] <- NA
# 
# u_2 <- u_2[order(u_2$id_global),]
# u_2c <- u_2c[order(u_2c$id_global),]
# 
# tmp <- u_2 %>% select(id_SSC) %>% rename('false_SSC'='id_SSC')
# tmp$false_PGJ <- u_2$id_PGJ
# tmp$true_SSC <- u_2c$id_SSC
# tmp$true_PGJ <- u_2c$id_PGJ
# row.names(tmp) <- 1:nrow(tmp)
# 
# nrow(filter(tmp , true_PGJ == false_PGJ & true_SSC == false_SSC))
# tmp <- tmp[rowSums(is.na(tmp)) > 0,]
# tmp <- filter(tmp , !(true_PGJ == false_PGJ & true_SSC == false_SSC))



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
axa['siniestro'] <- as.character(axa$siniestro)
axa$siniestro[axa$siniestro == '\\N'] <- paste0('SinID_', seq(1:nrow(filter(axa , siniestro == '\\N'))))
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