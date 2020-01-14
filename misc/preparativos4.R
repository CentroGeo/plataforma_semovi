filtro_fecha <- list('2018-01-01' , '2018-12-31')
tiempo_grafica <- 'Por Mes'
tipo_grafica <- 'Gráficas Combinadas'
filtro_bd <- c('PGJ' , 'SSC' , 'C5')

# ===== DF CON REFERENCIAS =====
i <- dates(filtro_fecha[[1]] , format = 'y-m-d')
ref_dia <- 1
ref_mes <- 1
mes <- 0

df_referencia <- data.frame(fecha = as.character() , ref_dia = as.integer(), ref_mes = as.integer(), etiqueta = as.character() , stringsAsFactors = FALSE)

while (i <= dates(filtro_fecha[[2]] , format = 'y-m-d')) {
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

# ===
tmp_pgj <- pgj_tmp
tmp_ssc <- ssc_tmp
tmp_c5 <- c5_tmp

rm(pgj_tmp , ssc_tmp , c5_tmp , i , mes , ref_dia , ref_mes , t)

# ===== GRÁFICAS =====
count_final <- data.frame(n = as.integer() , ref = as.integer() , etiqueta = as.character() , categoria = as.character())
grafica <- ggplot()

if (tipo_grafica == 'Gráficas Combinadas') {
  paleta = c()
  if ('PGJ' %in% filtro_bd) paleta <- append(paleta , '#952800')
  if ('SSC' %in% filtro_bd) paleta <- append(paleta , '#043A5F')
  if ('C5' %in% filtro_bd) paleta <- append(paleta , '#956F00')
  if ('AXA' %in% filtro_bd) paleta <- append(paleta , '#5E0061')
  if ('Repubikla' %in% filtro_bd) paleta <- append(paleta , '#3F8500')
  # =
  if (tiempo_grafica == 'Por Mes') {
    for (bd in filtro_bd) {
      if (bd == 'PGJ') tmp = tmp_pgj
      else if (bd == 'SSC') tmp = tmp_ssc
      else if (bd == 'C5') tmp = tmp_c5
      else if (bd == 'AXA') tmp = tmp_axa
      else if (bd == 'Repubikla') tmp = tmp_repubikla
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
      count <- merge(x = count , y = df_referencia[!is.na(df_referencia$etiqueta),] , by = 'etiqueta' , all.y = TRUE)
      count$categoria <- bd
      count$ref_mes <- as.integer(count$ref_mes)
      count <- count[order(count$ref_mes),]
      count$n[is.na(count$n)] <- 0
      # =
      count_final <- rbind(count_final , count %>% select(n , ref_mes , etiqueta , categoria) %>% rename('ref'='ref_mes')) 
    }
  }
  else if (tiempo_grafica == 'Por Día') {
    for (bd in filtro_bd) {
      if (bd == 'PGJ') tmp = tmp_pgj
      else if (bd == 'SSC') tmp = tmp_ssc
      else if (bd == 'C5') tmp = tmp_c5
      else if (bd == 'AXA') tmp = tmp_axa
      else if (bd == 'Repubikla') tmp = tmp_repubikla
      tmp$geometry <- NULL
      # =
      count <- count(tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
      count$fecha <- format(chron(dates(paste0(count$dia , '/' , count$mes , '/' ,count$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
      # =
      count <- merge(x = count , y = df_referencia , by = 'fecha' , all.y = TRUE)
      count$categoria <- bd
      count$ref_dia <- as.integer(count$ref_dia)
      count <- count[order(count$ref_dia),]
      count$n[is.na(count$n)] <- 0
      # =
      count_final <- rbind(count_final , count %>% select(n , ref_dia , etiqueta , categoria) %>% rename('ref'='ref_dia')) 
    }
  }
  # =
  grafica = grafica +
    geom_line(data = count_final , aes(x = ref , y = n , color = categoria), alpha = 0.2) +
    geom_smooth(data = count_final , aes(x = ref , y = n , color = categoria), method = 'loess' , formula = 'y ~ x' , se = FALSE , size = 0.7) +
    # geom_point(data = count_final , aes(x = ref , y = n , color = categoria , alpha = n) , show.legend = FALSE , size = 0.7) +
    # geom_area(data = count_final , aes(x = ref , y = n , fill = categoria) , position = 'identity' , alpha = 0.6) +
    # geom_ribbon(data = count_final , aes(x = ref , ymin = n - (n * 0.02) , ymax = n + (n * 0.02), fill = categoria)) +
    # geom_step(data = count_final , aes(x = ref , y = n , color = categoria)) +
    # geom_quantile(data = count_final , aes(x = ref , y = n , color = categoria)) +
    # geom_bin2d(data = count_final , aes(x = ref , y = n , fill = categoria)) +
    scale_x_continuous(breaks = unique(count_final$ref[!is.na(count$etiqueta)]),
                       minor_breaks = NULL,
                       labels = unique(count_final$etiqueta[!is.na(count$etiqueta)])) +
    scale_color_manual(values = paleta,
                       limits = unique(count_final$categoria),
                       name = 'Fuente de Datos' ,
                       labels = unique(count_final$categoria)) +
    # scale_fill_manual(values = paleta,
    #                    limits = unique(count_final$categoria),
    #                    name = 'Fuente de Datos' ,
    #                    labels = unique(count_final$categoria)) +
    ylim(0 , NA) +
    labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Incidentes por Mes')
}