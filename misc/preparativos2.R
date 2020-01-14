algoritmo_sp <- function(fila , bd) {
  bd_id <- paste0('id_' , bd)
  # =
  if(!is.na(fila[bd_id][[1]])) {
    return(as.character(fila[bd_id]))}
  # =
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

filtro_fecha <- list('2018-01-01' , '2018-12-31')
filtro_bd <- c('PGJ' , 'SSC' , 'C5')

unificada <- data.frame(id_original = as.character() , base_original = as.character(), timestamp = chron() , geometry = st_sfc(crs = 32614))

pgj_tmp = pgj
pgj_tmp <- filter(pgj_tmp , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
# pgj_tmp <- filter(pgj_tmp , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
# pgj_tmp <- filter(pgj_tmp , delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL' | delito == 'DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES')
pgj_tmp <- filter(pgj_tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp = data.frame(id_original = as.character(pgj_tmp$id),
                 base_original = replicate(nrow(pgj_tmp) , 'PGJ'),
                 timestamp = pgj_tmp$timestamp,
                 geometry = pgj_tmp$geometry)
if (nrow(tmp) != 0) unificada <- rbind(unificada , tmp)

ssc_tmp = ssc
ssc_tmp <- filter(ssc_tmp , total_occisos > 0)
# ssc_tmp <- filter(ssc_tmp , total_lesionados > 0 & total_occisos == 0)
ssc_tmp <- filter(ssc_tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp = data.frame(id_original = as.character(ssc_tmp$id),
                 base_original = replicate(nrow(ssc_tmp) , 'SSC'),
                 timestamp = ssc_tmp$timestamp,
                 geometry = ssc_tmp$geometry)
if (nrow(tmp) != 0) unificada <- rbind(unificada , tmp)

c5_tmp = c5
c5_tmp <- filter(c5_tmp , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
# c5_tmp <- filter(c5_tmp , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
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
}

a <- filter(unificada , id_PGJ == 'character(0)')$id_global
unificada[unificada$id_global %in% a , 'id_PGJ'] <- NA
# =
a <- filter(unificada , id_SSC == 'character(0)')$id_global
unificada[unificada$id_global %in% a , 'id_SSC'] <- NA
# =
a <- filter(unificada , id_C5 == 'character(0)')$id_global
unificada[unificada$id_global %in% a , 'id_C5'] <- NA

rm(tmp , pgj_tmp , ssc_tmp , c5_tmp , a , bd , id)

# ========================================

# ===== Vínculos Totales =====
tmp <- filter(unificada , base_original == 'PGJ') %>% select(id_original , id_SSC , id_C5)
tmp <- merge(x = pgj , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = ssc , y = tmp , by.x = 'id' , by.y = 'id_SSC')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_PGJ'='id.y')
tmp <- tmp %>% rename('id_SSC'='id')
tmp <- merge(x = c5 , y = tmp , by.x = 'folio' , by.y = 'id_C5')
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp))
tmp <- tmp %>% rename('id_C5'='folio')
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
vinculos_total <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')

# ===== Vínculos PGJ/SSC =====
tmp <- filter(unificada , base_original == 'PGJ') %>% select(id_original , id_SSC , id_C5)
tmp <- merge(x = pgj , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = ssc , y = tmp , by.x = 'id' , by.y = 'id_SSC')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_PGJ'='id.y')
tmp <- tmp %>% rename('id_SSC'='id')
tmp <- filter(tmp , !id_PGJ %in% vinculos_total$id_PGJ)
tmp$id_global <- seq.int(nrow(tmp)) + tail(vinculos_total$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp.x , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp.x , format = '%T')
tmp$id_C5 <- NA
vinculos_pgj_ssc <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                                   delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                                   no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones')
# =
tmp$codigo_cierre <- NA
tmp$incidente_c4 <- NA
tmp$clas_con_f_alarma <- NA
tmp$tipo_entrada <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                                 delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                                 no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                                 codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
incidentes_viales <- rbind(vinculos_total , tmp , stringAsFactors = FALSE)

# ===== Vínculos PGJ / C5 =====
tmp <- filter(unificada , base_original == 'PGJ') %>% select(id_original , id_SSC , id_C5)
tmp <- merge(x = pgj , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = c5 , y = tmp , by.x = 'folio' , by.y = 'id_C5')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_C5'='folio' , 'id_PGJ' = 'id')
tmp <- filter(tmp , !id_PGJ %in% vinculos_total$id_PGJ)
tmp$id_global <- seq.int(nrow(tmp)) + tail(vinculos_pgj_ssc$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp.x , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp.x , format = '%T')
tmp$id_SSC <- NA
vinculos_pgj_C5 <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                                 delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                                 codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
# =
tmp$no_folio <- NA
tmp$tipo_evento <- NA
tmp$tipo_interseccion <- NA
tmp$zona <- NA
tmp$cuadrante <- NA
tmp$sector <- NA
tmp$reporte <- NA
tmp$tipo_vehiculo_1 <- NA
tmp$tipo_vehiculo_2 <- NA
tmp$tipo_vehiculo_3 <- NA
tmp$tipo_vehiculo_4 <- NA
tmp$marca_vehiculo_1 <- NA
tmp$marca_vehiculo_2 <- NA
tmp$marca_vehiculo_3 <- NA
tmp$marca_vehiculo_4 <- NA
tmp$ruta_transporte_publico <- NA
tmp$matricula_1 <- NA
tmp$matricula_2 <- NA
tmp$matricula_3 <- NA
tmp$matricula_4 <- NA
tmp$condicion <- NA
tmp$lesiones <- NA
tmp$edad_occiso <- NA
tmp$edad_lesionado <- NA
tmp$total_occisos <- NA
tmp$total_lesionados <- NA
tmp$identidad <- NA
tmp$unidad_medica_de_apoyo <- NA
tmp$matricula_unidad_medica <- NA
tmp$lugar_del_deceso. <- NA
tmp$trasladados_lesionado <- NA
tmp$hospital <- NA
tmp$observaciones <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)


# ===== Vínculos SSC/C5 =====
tmp <- filter(unificada , base_original == 'SSC') %>% select(id_original , id_C5)
tmp <- merge(x = ssc , y = tmp , by.x = 'id' , by.y = 'id_original')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp <- merge(x = c5 , y = tmp , by.x = 'folio' , by.y = 'id_C5')
tmp$geometry <- NULL
tmp <- tmp %>% rename('id_C5'='folio' , 'id_SSC' = 'id')
tmp <- filter(tmp , !id_SSC %in% vinculos_total$id_SSC)
tmp$id_global <- seq.int(nrow(tmp)) + tail(vinculos_pgj_C5$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp.x , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp.x , format = '%T')
tmp$id_PGJ <- NA
vinculos_ssc_C5 <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , punto_1 , colonia , alcaldia , lat , lon,
                                  no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                                  codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='punto_1' , 'colonia'='colonia' , 'alcaldia'='alcaldia' ,
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
# =
tmp$delito <- NA
tmp$categoria_delito <- NA
tmp$fiscalia <- NA
tmp$agencia <- NA
tmp$unidad_investigacion <- NA
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , punto_1 , colonia , alcaldia , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='punto_1' , 'colonia'='colonia' , 'alcaldia'='alcaldia' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)

# ===== LOS QUE FALTAN =====
# ===== PGJ =====
a <- vinculos_total$id_PGJ
a <- append(a , vinculos_pgj_ssc$id_PGJ)
a <- append(a , vinculos_pgj_C5$id_PGJ)
tmp = pgj
# tmp <- filter(tmp , delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)' | delito == 'HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR')
tmp <- filter(tmp , delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR' | delito == 'LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION')
tmp <- filter(tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp <- filter(tmp , !id %in% a)
tmp <- tmp %>% rename('id_PGJ'='id')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp)) + tail(vinculos_ssc_C5$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
tmp$id_SSC <- NA
tmp$id_C5 <- NA
pgj_sin_vinculo <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito')
# =
tmp$codigo_cierre <- NA
tmp$incidente_c4 <- NA
tmp$clas_con_f_alarma <- NA
tmp$tipo_entrada <- NA
tmp$no_folio <- NA
tmp$tipo_evento <- NA
tmp$tipo_interseccion <- NA
tmp$zona <- NA
tmp$cuadrante <- NA
tmp$sector <- NA
tmp$reporte <- NA
tmp$tipo_vehiculo_1 <- NA
tmp$tipo_vehiculo_2 <- NA
tmp$tipo_vehiculo_3 <- NA
tmp$tipo_vehiculo_4 <- NA
tmp$marca_vehiculo_1 <- NA
tmp$marca_vehiculo_2 <- NA
tmp$marca_vehiculo_3 <- NA
tmp$marca_vehiculo_4 <- NA
tmp$ruta_transporte_publico <- NA
tmp$matricula_1 <- NA
tmp$matricula_2 <- NA
tmp$matricula_3 <- NA
tmp$matricula_4 <- NA
tmp$condicion <- NA
tmp$lesiones <- NA
tmp$edad_occiso <- NA
tmp$edad_lesionado <- NA
tmp$total_occisos <- NA
tmp$total_lesionados <- NA
tmp$identidad <- NA
tmp$unidad_medica_de_apoyo <- NA
tmp$matricula_unidad_medica <- NA
tmp$lugar_del_deceso. <- NA
tmp$trasladados_lesionado <- NA
tmp$hospital <- NA
tmp$observaciones <- NA
# =
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , calle_hechos , colonia_hechos , alcaldia_hechos , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='calle_hechos' , 'colonia'='colonia_hechos' , 'alcaldia'='alcaldia_hechos' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)

# ===== SSC =====
a <- vinculos_total$id_SSC
a <- append(a , vinculos_pgj_ssc$id_SSC)
a <- append(a , vinculos_ssc_C5$id_SSC)
tmp <- ssc
# tmp <- filter(tmp , total_occisos > 0)
tmp <- filter(tmp , total_lesionados > 0 & total_occisos == 0)
tmp <- filter(tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp <- filter(tmp , !id %in% a)
tmp <- tmp %>% rename('id_SSC'='id')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp)) + tail(pgj_sin_vinculo$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
tmp$id_PGJ <- NA
tmp$id_C5 <- NA
ssc_sin_vinculo <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , punto_1 , colonia , alcaldia , lat , lon,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones) %>%
  rename('calle'='punto_1' , 'colonia'='colonia' , 'alcaldia'='alcaldia' ,
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones')
# =
tmp$delito <- NA
tmp$categoria_delito <- NA
tmp$fiscalia <- NA
tmp$agencia <- NA
tmp$unidad_investigacion <- NA
tmp$codigo_cierre <- NA
tmp$incidente_c4 <- NA
tmp$clas_con_f_alarma <- NA
tmp$tipo_entrada <- NA
# =
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , punto_1 , colonia , alcaldia , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='punto_1' , 'colonia'='colonia' , 'alcaldia'='alcaldia' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)

# ===== C5 =====
a <- vinculos_total$id_C5
a <- append(a , vinculos_pgj_C5$id_C5)
a <- append(a , vinculos_ssc_C5$id_C5)
tmp = c5
# tmp <- filter(tmp , incidente_c4 == 'cadáver-accidente automovilístico' | incidente_c4 == 'cadáver-atropellado')
tmp <- filter(tmp , incidente_c4 == 'accidente-choque con lesionados' | incidente_c4 == 'accidente-choque con prensados' | incidente_c4 == 'accidente-persona atrapada / desbarrancada' | incidente_c4 == 'accidente-vehiculo atrapado' | incidente_c4 == 'accidente-vehículo atrapado-varado' | incidente_c4 == 'accidente-vehiculo desbarrancado' | incidente_c4 == 'accidente-volcadura' | incidente_c4 == 'detención ciudadana-atropellado' | incidente_c4 == 'lesionado-accidente automovilístico' | incidente_c4 == 'lesionado-atropellado')
tmp <- filter(tmp , timestamp >= dates(as.character(filtro_fecha[1]) , format = 'y-m-d') & timestamp <= dates(as.character(filtro_fecha[2]) , format = 'y-m-d'))
tmp <- filter(tmp , !folio %in% a)
tmp <- tmp %>% rename('id_C5'='folio')
tmp$lat <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,2])
tmp$lon <- as.numeric(st_coordinates(st_transform(tmp$geometry , 4326))[,1])
tmp$geometry <- NULL
tmp$id_global <- seq.int(nrow(tmp)) + tail(ssc_sin_vinculo$id_global , n = 1)
tmp$fecha_incidente <- format(tmp$timestamp , format = '%d/%m/%Y')
tmp$hora_incidente <- format(tmp$timestamp , format = '%T')
tmp$id_PGJ <- NA
tmp$id_SSC <- NA
# =
tmp$punto_1 <- NA
tmp$colonia <- NA
tmp$alcaldia <- NA
c5_sin_vinculo <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , punto_1 , colonia , alcaldia , lat , lon,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='punto_1' , 'colonia'='colonia' , 'alcaldia'='alcaldia' ,
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
# =
tmp$delito <- NA
tmp$categoria_delito <- NA
tmp$fiscalia <- NA
tmp$agencia <- NA
tmp$unidad_investigacion <- NA
tmp$no_folio <- NA
tmp$tipo_evento <- NA
tmp$tipo_interseccion <- NA
tmp$zona <- NA
tmp$cuadrante <- NA
tmp$sector <- NA
tmp$reporte <- NA
tmp$tipo_vehiculo_1 <- NA
tmp$tipo_vehiculo_2 <- NA
tmp$tipo_vehiculo_3 <- NA
tmp$tipo_vehiculo_4 <- NA
tmp$marca_vehiculo_1 <- NA
tmp$marca_vehiculo_2 <- NA
tmp$marca_vehiculo_3 <- NA
tmp$marca_vehiculo_4 <- NA
tmp$ruta_transporte_publico <- NA
tmp$matricula_1 <- NA
tmp$matricula_2 <- NA
tmp$matricula_3 <- NA
tmp$matricula_4 <- NA
tmp$condicion <- NA
tmp$lesiones <- NA
tmp$edad_occiso <- NA
tmp$edad_lesionado <- NA
tmp$total_occisos <- NA
tmp$total_lesionados <- NA
tmp$identidad <- NA
tmp$unidad_medica_de_apoyo <- NA
tmp$matricula_unidad_medica <- NA
tmp$lugar_del_deceso. <- NA
tmp$trasladados_lesionado <- NA
tmp$hospital <- NA
tmp$observaciones <- NA
# =
tmp <- tmp %>% select(id_global , id_PGJ , id_SSC , id_C5 , fecha_incidente , hora_incidente , punto_1 , colonia , alcaldia , lat , lon,
                      delito, categoria_delito , fiscalia , agencia , unidad_investigacion,
                      no_folio , tipo_evento , tipo_interseccion , zona , cuadrante , sector, reporte , tipo_vehiculo_1 , tipo_vehiculo_2 , tipo_vehiculo_3 , tipo_vehiculo_4, marca_vehiculo_1 , marca_vehiculo_2 , marca_vehiculo_3 , marca_vehiculo_4 , ruta_transporte_publico, matricula_1, matricula_2 , matricula_3 , matricula_4, condicion , lesiones , edad_occiso , edad_lesionado , total_occisos , total_lesionados , identidad , unidad_medica_de_apoyo , matricula_unidad_medica , lugar_del_deceso. , trasladados_lesionado , hospital , observaciones,
                      codigo_cierre , incidente_c4 , clas_con_f_alarma , tipo_entrada) %>%
  rename('calle'='punto_1' , 'colonia'='colonia' , 'alcaldia'='alcaldia' ,
         'delito_pgj'='delito' , 'categoria_delito_pgj'='categoria_delito',
         'no_folio_ssc'='no_folio' , 'tipo_evento_ssc'='tipo_evento' , 'zona_ssc'='zona' , 'cuadrante_ssc'='cuadrante' , 'sector_ssc'='sector', 'reporte_ssc'='reporte' , 'lugar_del_deceso'='lugar_del_deceso.' , 'observaciones_ssc'='observaciones',
         'codigo_cierre_c5'='codigo_cierre' , 'clas_con_f_alarma_c5'='clas_con_f_alarma' , 'tipo_entrada_c5'='tipo_entrada')
incidentes_viales <- rbind(incidentes_viales , tmp , stringAsFactors = FALSE)


# =====
write.csv(incidentes_viales , file = 'data/finales/incidentes_viales.csv' , row.names = FALSE)
write.csv(vinculos_total , file = 'data/finales/vinculos_total.csv', row.names = FALSE)
write.csv(vinculos_pgj_ssc , file = 'data/finales/vinculos_pgj_ssc.csv', row.names = FALSE)
write.csv(vinculos_pgj_C5 , file = 'data/finales/vinculos_pgj_c5.csv', row.names = FALSE)
write.csv(vinculos_ssc_C5 , file = 'data/finales/vinculos_ssc_c5.csv', row.names = FALSE)
write.csv(pgj_sin_vinculo , file = 'data/finales/pgj_sin_vinculo.csv', row.names = FALSE)
write.csv(ssc_sin_vinculo , file = 'data/finales/ssc_sin_vinculo.csv', row.names = FALSE)
write.csv(c5_sin_vinculo , file = 'data/finales/c5_sin_vinculo.csv', row.names = FALSE)