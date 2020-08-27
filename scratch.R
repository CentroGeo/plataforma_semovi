count_final <- data.frame(n = as.integer() , ref = as.integer() , etiqueta = as.character() , categoria = as.character())

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
