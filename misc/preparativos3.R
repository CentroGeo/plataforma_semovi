# ===== PREPARACIÓN POR DÍA =====
filtro_fecha <- list('2018-01-01' , '2018-12-31')

i <- dates(filtro_fecha[[1]] , format = 'y-m-d')
ref_dia <- 1
ref_mes <- 1
mes <- 0

df <- data.frame(fecha = as.character() , ref_dia = as.integer(), ref_mes = as.integer(), etiqueta = as.character() , stringsAsFactors = FALSE)

while (i <= dates(filtro_fecha[[2]] , format = 'y-m-d')) {
  df[nrow(df) + 1,] <- c(format(chron(i , times('00:00:00')) , format = '%d/%m/%Y') , ref_dia , NA , NA)
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
    df[df$ref_dia == ref_dia , 'etiqueta'] <- t
    df[df$ref_dia == ref_dia , 'ref_mes'] <- ref_mes
    ref_mes <- ref_mes + 1
  }
  # =
  ref_dia <- ref_dia + 1
  i <- i + 1
}

# ==========

count_pgj2 <- count(pgj_tmp , year(timestamp) , month(timestamp) , day(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)' , 'dia' = 'day(timestamp)')
count_pgj2$geometry <- NULL
count_pgj2$fecha <- format(chron(dates(paste0(count_pgj2$dia , '/' , count_pgj2$mes , '/' ,count_pgj2$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
count_pgj2 <- merge(x = count_pgj2 , y = df , by = 'fecha' , all.y = TRUE)
count_pgj2$ref <- as.integer(count_pgj2$ref)
count_pgj2 <- count_pgj2[order(count_pgj2$ref),]
count_pgj2[is.na(count_pgj2)] <- 0

# ===

ggplot() +
  geom_point(data = count_pgj2 , aes(x = ref , y = n) , alpha = 0) +
  geom_smooth(data = count_pgj2 , aes(x = ref , y = n)) +
  scale_x_continuous(breaks = count_pgj2$ref[count_pgj2$etiqueta != ''],
                     minor_breaks = NULL,
                     labels = count_pgj2$etiqueta[count_pgj2$etiqueta != '']) +
  labs(x = 'Día del Año' , y = 'Número de Incidentes' , title = 'Número de Decesos al Día')

# ==========

# ===== PREPARACIÓN POR MES =====
mi_a <- as.integer(substring(as.character(filtro_fecha[1]) , 6 , 7))
if (mi_a == 1) {
  mi_b <- 'Ene'
} else if (mi_a == 2) {
  mi_b <- 'Feb'
} else if (mi_a == 3) {
  mi_b <- 'Mar'
} else if (mi_a == 4) {
  mi_b <- 'Abr'
} else if (mi_a == 5) {
  mi_b <- 'May'
} else if (mi_a == 6) {
  mi_b <- 'Jun'
} else if (mi_a == 7) {
  mi_b <- 'Jul'
} else if (mi_a == 8) {
  mi_b <- 'Ago'
} else if (mi_a == 9) {
  mi_b <- 'Sep'
} else if (mi_a == 10) {
  mi_b <- 'Oct'
} else if (mi_a == 11) {
  mi_b <- 'Nov'
} else mi_b <- 'Dic'
yi <- as.integer(substring(as.character(filtro_fecha[1]) , 1 , 4))
etiqueta_inicial <- paste0(mi_b , '/' , substring(as.character(yi) , 3 , 4))
# =
mf_a <- as.integer(substring(as.character(filtro_fecha[2]) , 6 , 7))
if (mf_a == 1) {
  mf_b <- 'Ene'
} else if (mf_a == 2) {
  mf_b <- 'Feb'
} else if (mf_a == 3) {
  mf_b <- 'Mar'
} else if (mf_a == 4) {
  mf_b <- 'Abr'
} else if (mf_a == 5) {
  mf_b <- 'May'
} else if (mf_a == 6) {
  mf_b <- 'Jun'
} else if (mf_a == 7) {
  mf_b <- 'Jul'
} else if (mf_a == 8) {
  mf_b <- 'Ago'
} else if (mf_a == 9) {
  mf_b <- 'Sep'
} else if (mf_a == 10) {
  mf_b <- 'Oct'
} else if (mf_a == 11) {
  mf_b <- 'Nov'
} else mf_b <- 'Dic'
etiqueta_final <- paste0(mf_b , '/' , substring(as.character(filtro_fecha[2]) , 3 , 4))
# =
lista_etiquetas <- c(etiqueta_inicial)
i <- etiqueta_inicial
while (i != etiqueta_final) {
  mi_a = mi_a + 1
  if (mi_a == 13) {
    mi_a = 1
    yi = yi + 1
  }
  # =
  if (mi_a == 1) {
    mi_b <- 'Ene'
  } else if (mi_a == 2) {
    mi_b <- 'Feb'
  } else if (mi_a == 3) {
    mi_b <- 'Mar'
  } else if (mi_a == 4) {
    mi_b <- 'Abr'
  } else if (mi_a == 5) {
    mi_b <- 'May'
  } else if (mi_a == 6) {
    mi_b <- 'Jun'
  } else if (mi_a == 7) {
    mi_b <- 'Jul'
  } else if (mi_a == 8) {
    mi_b <- 'Ago'
  } else if (mi_a == 9) {
    mi_b <- 'Sep'
  } else if (mi_a == 10) {
    mi_b <- 'Oct'
  } else if (mi_a == 11) {
    mi_b <- 'Nov'
  } else mi_b <- 'Dic'
  # =
  i <- paste0(mi_b , '/' , substring(as.character(yi) , 3 , 4))
  lista_etiquetas <- append(lista_etiquetas , i)
}
df_etiquetas <- data.frame(etiqueta = lista_etiquetas,
                           ref = seq.int(length(lista_etiquetas)))

# ==========
count_pgj <- count(pgj_tmp , year(timestamp) , month(timestamp)) %>% rename('ao'='year(timestamp)' , 'mes'='month(timestamp)')
count_pgj$geometry <- NULL
count_pgj$etiqueta <- count_pgj$mes
# =
count_pgj$etiqueta[count_pgj$etiqueta == 1] <- 'Ene'
count_pgj$etiqueta[count_pgj$etiqueta == 2] <- 'Feb'
count_pgj$etiqueta[count_pgj$etiqueta == 3] <- 'Mar'
count_pgj$etiqueta[count_pgj$etiqueta == 4] <- 'Abr'
count_pgj$etiqueta[count_pgj$etiqueta == 5] <- 'May'
count_pgj$etiqueta[count_pgj$etiqueta == 6] <- 'Jun'
count_pgj$etiqueta[count_pgj$etiqueta == 7] <- 'Jul'
count_pgj$etiqueta[count_pgj$etiqueta == 8] <- 'Ago'
count_pgj$etiqueta[count_pgj$etiqueta == 9] <- 'Sep'
count_pgj$etiqueta[count_pgj$etiqueta == 10] <- 'Oct'
count_pgj$etiqueta[count_pgj$etiqueta == 11] <- 'Nov'
count_pgj$etiqueta[count_pgj$etiqueta == 12] <- 'Dic'
# =
if (nrow(count_pgj) != 0) {
  count_pgj$etiqueta <- paste0(count_pgj$etiqueta , '/' , substring(as.character(count_pgj$ao), 3 , 4))
}
# =
count_pgj <- merge(x = count_pgj , y = df_etiquetas , by = 'etiqueta' , all.y = TRUE)
count_pgj <- count_pgj[order(count_pgj$ref),]
count_pgj[is.na(count_pgj)] <- 0
# =
count_pgj$fecha <- format(chron(dates(paste0('01' , '/' , count_pgj$mes , '/' ,count_pgj$ao) , format = 'd/m/y') , '00:00:00') , format = '%d/%m/%Y')
count_pgj$ref <- NULL
count_pgj$etiqueta <- NULL
count_pgj <- merge(x = count_pgj , y = df , by = 'fecha' , all.y = TRUE)
count_pgj$ref <- as.integer(count_pgj$ref)
count_pgj <- count_pgj[order(count_pgj$ref),]

# =====

ggplot() +
  geom_point(data = count_pgj , aes(x = ref , y = n)) +
  geom_smooth(data = count_pgj2 , aes(x = ref , y = n)) +
  scale_x_continuous(breaks = count_pgj2$ref[count_pgj2$etiqueta != ''],
                     minor_breaks = NULL,
                     labels = count_pgj2$etiqueta[count_pgj2$etiqueta != '']) +
  labs(x = 'Día del Año' , y = 'Número de Incidentes' , title = 'Número de Decesos')

# =====

ggplot() +
  geom_point(data = count_pgj , aes(x = ref , y = n)) +
  geom_smooth(data = count_pgj , aes(x = ref , y = n)) +
  scale_x_continuous(breaks = count_pgj$ref[count_pgj$etiqueta != ''],
                     minor_breaks = NULL,
                     labels = count_pgj$etiqueta[count_pgj$etiqueta != '']) +
  labs(x = 'Mes' , y = 'Número de Incidentes' , title = 'Número de Decesos por Mes')


# ==========

tmp <- count_pgj %>% select(n , ref) %>% rename('n_mensual'='n')
tmp <- tmp[complete.cases(tmp),]
count_pgj2 <- merge(x = count_pgj2 , y = tmp , by = 'ref' , all.x = TRUE)

ggplot() +
  geom_point(data = count_pgj2 , aes(x = ref , y = n_mensual)) +
  geom_smooth(data = count_pgj2 , aes(x = ref , y = n*49/6)) +
  scale_y_continuous(sec.axis = sec_axis(~.*6/49, name = 'Número de Incidentes al Día')) +
  scale_x_continuous(breaks = count_pgj2$ref[count_pgj2$etiqueta != ''],
                     minor_breaks = NULL,
                     labels = count_pgj2$etiqueta[count_pgj2$etiqueta != '']) +
  labs(x = 'Día del Año' , y = 'Número de Incidentes por Mes' , title = 'Número de Decesos')

