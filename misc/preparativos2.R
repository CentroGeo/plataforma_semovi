algoritmo_sp <- function(fila , bd) {
  bd_id <- paste0('id_' , bd)
  # =====
  if(!is.na(fila[bd_id][[1]])) {
    return(as.character(fila[bd_id]))}
  # =====
  # if (fila['base_original'][[1]] != bd) {
  #   if (fila['base_original'][[1]] == 'PGJ') {
  #     if (fila['id_original'][[1]] %in% filter(unificada , base_original == bd)$id_PGJ) {
  #       z <- filter(unificada , base_original == bd & id_PGJ == fila['id_original'][[1]])[1,]
  #       return(as.character(z[bd_id]))
  #     }
  #   }
  #   else if (fila['base_original'][[1]] == 'SSC') {
  #     if (fila['id_original'][[1]] %in% filter(unificada , base_original == bd)$id_SSC) {
  #       z <- filter(unificada , base_original == bd & id_SSC == fila['id_original'][[1]])[1,]
  #       return(as.character(z[bd_id]))
  #     }
  #   }
  #   else if (fila['base_original'][[1]] == 'C5') {
  #     if (fila['id_original'][[1]] %in% filter(unificada , base_original == bd)$id_C5) {
  #       z <- filter(unificada , base_original == bd & id_C5 == fila['id_original'][[1]])[1,]
  #       return(as.character(z[bd_id]))
  #     }
  #   }
  # }
  # =====
  shp <- filter(unificada , id_global == as.integer(fila['id_global']))
  tmp_within <- st_is_within_distance(shp$geometry , unificada$geometry , distancia_final)
  posibles <- unificada[tmp_within[[1]],]
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

distancia_final <- 2000 # metros
tiempo_final <- 1.5 / 24 # días

filtro_fecha <- list('2018-01-01' , '2019-12-31')
filtro_bd <- c('PGJ' , 'SSC' , 'C5')

unificada <- data.frame(id_original = as.character() , base_original = as.character(), timestamp = chron() , geometry = st_sfc(crs = 32614) , stringsAsFactors = FALSE)

pgj_tmp = pgj
# pgj_tmp <- filter(pgj_tmp , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
pgj_tmp <- filter(pgj_tmp , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
# pgj_tmp <- filter(pgj_tmp , delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL' | delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES')
pgj_tmp <- filter(pgj_tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp = data.frame(id_original = as.character(pgj_tmp$id),
                 base_original = replicate(nrow(pgj_tmp) , 'PGJ'),
                 timestamp = pgj_tmp$timestamp,
                 geometry = pgj_tmp$geometry)
if (nrow(tmp) != 0) unificada <- rbind(unificada , tmp)

ssc_tmp = ssc
# ssc_tmp <- filter(ssc_tmp , total_occisos > 0)
ssc_tmp <- filter(ssc_tmp , total_lesionados > 0 & total_occisos == 0)
ssc_tmp <- filter(ssc_tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp = data.frame(id_original = as.character(ssc_tmp$no_folio),
                 base_original = replicate(nrow(ssc_tmp) , 'SSC'),
                 timestamp = ssc_tmp$timestamp,
                 geometry = ssc_tmp$geometry)
if (nrow(tmp) != 0) unificada <- rbind(unificada , tmp)

c5_tmp = c5
# c5_tmp <- filter(c5_tmp , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
c5_tmp <- filter(c5_tmp , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
c5_tmp <- filter(c5_tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp = data.frame(id_original = as.character(c5_tmp$folio),
                 base_original = replicate(nrow(c5_tmp) , 'C5'),
                 timestamp = c5_tmp$timestamp,
                 geometry = c5_tmp$geometry)
if (nrow(tmp) != 0) unificada <- rbind(unificada , tmp)

unificada['id_original'] <- as.character(unificada$id_original)
unificada['id_global'] <- seq.int(nrow(unificada))

unificada$id_PGJ <- NULL
unificada$id_SSC <- NULL
unificada$id_C5 <- NULL 

unificada['id_global'] <- seq.int(nrow(unificada))
unificada['id_PGJ'] <- as.character()
unificada['id_SSC'] <- as.character()
unificada['id_C5'] <- as.character()
# =
unificada$id_PGJ <- as.character(unificada$id_PGJ)
unificada$id_SSC <- as.character(unificada$id_SSC)
unificada$id_C5 <- as.character(unificada$id_C5)
# =
unificada[unificada$base_original == 'PGJ' , 'id_PGJ'] <- as.character(unificada[unificada$base_original == 'PGJ' , 'id_original'])
unificada[unificada$base_original == 'SSC' , 'id_SSC'] <- as.character(unificada[unificada$base_original == 'SSC' , 'id_original'])
unificada[unificada$base_original == 'C5' , 'id_C5'] <- as.character(unificada[unificada$base_original == 'C5' , 'id_original'])

for (bd in filtro_bd) {
  id <- paste0('id_',bd)
  unificada[id] <- as.character(apply(unificada , MARGIN = 1 , FUN = algoritmo_sp , bd = bd))
  a <- unificada[unificada[id] == 'character(0)' ,]$id_global
  unificada[unificada$id_global %in% a , id] <- NA
}

rm(tmp , pgj_tmp , ssc_tmp , c5_tmp , a , bd , id)

# ========================================

# === Totales
tmp <- filter(unificada , base_original == 'PGJ') %>% select(id_original , id_SSC , id_C5)
tmp <- merge(x = pgj , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = ssc , y = tmp , by.x = 'no_folio' , by.y = 'id_SSC')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_PGJ'='id')
tmp <- tmp %>% rename('id_SSC'='no_folio')
tmp <- merge(x = c5 , y = tmp , by.x = 'folio' , by.y = 'id_C5')
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp))
tmp <- tmp %>% rename('id_C5'='folio')
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
incidentes_viales <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                                       delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                                       tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                                       codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)

# === PGJ / SSC
tmp <- filter(unificada , base_original == 'PGJ') %>% select(id_original , id_SSC , id_C5)
tmp <- merge(x = pgj , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = ssc , y = tmp , by.x = 'no_folio' , by.y = 'id_SSC')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_PGJ'='id')
tmp <- tmp %>% rename('id_SSC'='no_folio')
tmp <- filter(tmp , !id_PGJ %in% incidentes_viales$id_PGJ)
tmp$id_global <- seq.int(nrow(tmp)) + tail(incidentes_viales$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp.x , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp.x , format = '%T')
tmp$id_C5 <- NA
tmp[setdiff(names(incidentes_viales) , names(tmp))] <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)
incidentes_viales <- incidentes_viales[-nrow(incidentes_viales),]

# === PGJ/C5
tmp <- filter(unificada , base_original == 'PGJ') %>% select(id_original , id_SSC , id_C5)
tmp <- merge(x = pgj , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = c5 , y = tmp , by.x = 'folio' , by.y = 'id_C5')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_C5'='folio' , 'id_PGJ' = 'id')
tmp <- filter(tmp , !id_PGJ %in% incidentes_viales$id_PGJ)
tmp$id_global <- seq.int(nrow(tmp)) + tail(incidentes_viales$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp.x , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp.x , format = '%T')
tmp$id_SSC <- NA
tmp[setdiff(names(incidentes_viales) , names(tmp))] <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)
incidentes_viales <- incidentes_viales[-nrow(incidentes_viales),]

# === SSC/C5
tmp <- filter(unificada , base_original == 'SSC') %>% select(id_original , id_C5)
tmp <- merge(x = ssc , y = tmp , by.x = 'no_folio' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = c5 , y = tmp , by.x = 'folio' , by.y = 'id_C5')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_C5'='folio' , 'id_SSC' = 'no_folio')
tmp <- filter(tmp , !id_SSC %in% incidentes_viales$id_SSC)
tmp$id_global <- seq.int(nrow(tmp)) + tail(incidentes_viales$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp.x , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp.x , format = '%T')
tmp$id_PGJ <- NA
tmp <- tmp %>% rename('calle_hechos'='punto_1' , 'colonia_hechos' = 'colonia' , 'alcaldia_hechos' = 'alcaldia')
tmp[setdiff(names(incidentes_viales) , names(tmp))] <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)
incidentes_viales <- incidentes_viales[-nrow(incidentes_viales),]

# === PGJ
tmp <- filter(pgj , id %in% filter(unificada , !is.na(id_PGJ) , is.na(id_SSC) , is.na(id_C5))$id_original)
tmp <- tmp %>% rename('id_PGJ'='id')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp)) + tail(incidentes_viales$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
tmp$id_SSC <- NA
tmp$id_C5 <- NA
tmp[setdiff(names(incidentes_viales) , names(tmp))] <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)
incidentes_viales <- incidentes_viales[-nrow(incidentes_viales),]

# === SSC
tmp <- filter(ssc , no_folio %in% filter(unificada , is.na(id_PGJ) , !is.na(id_SSC) , is.na(id_C5))$id_original)
tmp <- tmp %>% rename('id_SSC'='no_folio')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp)) + tail(incidentes_viales$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
tmp$id_PGJ <- NA
tmp$id_C5 <- NA
tmp <- tmp %>% rename('calle_hechos'='punto_1' , 'colonia_hechos'='colonia' , 'alcaldia_hechos'='alcaldia')
tmp[setdiff(names(incidentes_viales) , names(tmp))] <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)
incidentes_viales <- incidentes_viales[-nrow(incidentes_viales),]

# === C5
tmp <- filter(c5 , folio %in% filter(unificada , is.na(id_PGJ) , is.na(id_SSC) , !is.na(id_C5))$id_original)
tmp <- tmp %>% rename('id_C5'='folio')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp)) + tail(incidentes_viales$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
tmp$id_PGJ <- NA
tmp$id_SSC <- NA
tmp <- tmp %>% rename('alcaldia_hechos'='delegacion_inicio')
tmp[setdiff(names(incidentes_viales) , names(tmp))] <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      tipo_de_evento , tipo_de_interseccion , tipo_de_vehiculo_1 , tipo_de_vehiculo_2 , tipo_de_vehiculo_3 , tipo_de_vehiculo_4, marca_de_vehiculo_1 , marca_de_vehiculo_2 , marca_de_vehiculo_3 , marca_de_vehiculo_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , lugar_del_deceso , trasladado_s_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada)
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)
incidentes_viales <- incidentes_viales[-nrow(incidentes_viales),]


# =====
write.csv(incidentes_viales , file = 'data/finales/incidentes_viales.csv' , row.names = FALSE)
write.csv(vinculos_total , file = 'data/finales/vinculos_total.csv', row.names = FALSE)
write.csv(vinculos_pgj_ssc , file = 'data/finales/vinculos_pgj_ssc.csv', row.names = FALSE)
write.csv(vinculos_pgj_C5 , file = 'data/finales/vinculos_pgj_c5.csv', row.names = FALSE)
write.csv(vinculos_ssc_C5 , file = 'data/finales/vinculos_ssc_c5.csv', row.names = FALSE)
write.csv(pgj_sin_vinculo , file = 'data/finales/pgj_sin_vinculo.csv', row.names = FALSE)
write.csv(ssc_sin_vinculo , file = 'data/finales/ssc_sin_vinculo.csv', row.names = FALSE)
write.csv(c5_sin_vinculo , file = 'data/finales/c5_sin_vinculo.csv', row.names = FALSE)