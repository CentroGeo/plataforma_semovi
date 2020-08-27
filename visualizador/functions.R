# load("pgj.Rdata")
# user_input = list(subgrafica_pgj2 = 'Delito', tiempo_grafica2 = 'Mañana (6AM - 12PM)')
# 
# 
# f <- get_final_counts(pgj, user_input)
# head(f)

get_final_counts <- function(datos, user_input){
  count_final <- data.frame(n = as.integer() , ref = as.integer() , etiqueta = as.character() , categoria = as.character())
  max <- 0
  #print(head(datos))
  datos$geometry <- NULL
  if (!is.null(datos)) {
    if (nrow(datos) != 0) {
      if (user_input$subgrafica_pgj2 == 'Sin Categoría') max <- ceiling(max(count(datos , wday(timestamp))$n)/10)*10
      else if (user_input$subgrafica_pgj2 == 'Delito') max <- ceiling(max(count(datos , wday(timestamp) , delito)$n)/10)*10
    }}
  # =
  if (!is.null(datos)) {
    if (user_input$tiempo_grafica2 == 'Mañana (6AM - 12PM)') datos <- filter(datos , hour(timestamp) %in% c(6, 7, 8, 9, 10, 11, 12))
    else if (user_input$tiempo_grafica2 == 'Tarde (1PM - 9PM)') datos <- filter(datos , hour(timestamp) %in% c(13, 14, 15, 16, 17, 18, 19, 20, 21))
    else if (user_input$tiempo_grafica2 == 'Noche (10PM - 5AM)') datos <- filter(datos , hour(timestamp) %in% c(22, 23, 0, 1, 2, 3, 4, 5))
  }
  # =
  if (nrow(datos) == 0) {
    count <- data.frame(ref = 1 , categoria = 'Sin Datos' , n = 0)
    for (i in seq(7)) {
      if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 'Sin Datos' , 0)}
  } 
  else if (user_input$subgrafica_pgj2 == 'Sin Categoría') {
    count <- count(datos , wday(timestamp)) %>% rename('ref'='wday(timestamp)')
    for (i in seq(7)) {
      if (nrow(count[count$ref == i,]) == 0) count[nrow(count) + 1,] <- c(i , 0)}
    count$categoria <- 'PGJ'
  }
  else if (user_input$subgrafica_pgj2 == 'Delito') {
    count <- count(datos , wday(timestamp) , delito) %>% rename('ref'='wday(timestamp)' , 'categoria'='delito')
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
  #print(head(count))
  # =
  count_final <- rbind(count_final , count %>% select(n , ref , etiqueta , categoria))
  return(count_final)
}