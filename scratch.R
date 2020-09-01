load("visualizador/coun_final_pgj.Rdata")

count_final <- data.frame(n = as.integer() , ref = as.integer() , etiqueta = as.character() , categoria = as.character())
max <- 0
tmp$geometry <- NULL
datos <- tmp
max <- ceiling(max(count(datos , wday(timestamp))$n)/10)*10

# if (!is.null(datos)) {
#     if (nrow(datos) != 0) {
#         if (user_input$subgrafica_pgj2 == 'Sin Categoría') max <- ceiling(max(count(datos , wday(timestamp))$n)/10)*10
#         else if (user_input$subgrafica_pgj2 == 'Delito') max <- ceiling(max(count(datos , wday(timestamp) , delito)$n)/10)*10
#     }}


grafica = ggplot() +
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

grafica = ggplot() +
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


if (input$subgrafica_ssc2 == 'Identidad') {
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