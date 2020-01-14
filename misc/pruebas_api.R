library(shiny)

library(httr)
library(jsonlite)

ui <- fluidPage(
  tableOutput(outputId = 'texto')
)

server <- function(input, output) {
  output$texto <- renderTable({
    fromJSON(GET('https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=denuncias-victimas-pgj&q=delito+%3D+%27FRAUDE%27&facet=delito&facet=categoria&facet=ao&facet=mes&facet=sexo&facet=tipopersona&facet=clasificaciondelito',
        accept_json()))
  })
}

shinyApp(ui = ui, server = server)

'https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=denuncias-victimas-pgj&q=(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(COLISION))+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+AUTOMOVIL)+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+BIENES+INMUEBLES)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(ATROPELLADO))+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(CAIDA))+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR)+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR+EN+COLISION)&facet=delito&facet=categoria&facet=ao&facet=mes&facet=sexo&facet=tipopersona&facet=clasificaciondelito'

'https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&facet=ao_hechos&facet=mes_hechos&facet=delito&facet=categoria_delito&facet=fiscalia&facet=alcaldia_hechos&facet=colonia_hechos&facet=ao_inicio'

tmp <- as.character(GET('https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&facet=ao_hechos&facet=mes_hechos&facet=delito&facet=categoria_delito&facet=fiscalia&facet=alcaldia_hechos&facet=colonia_hechos&facet=ao_inicio',
                        accept_json()))

df <- fromJSON(tmp)$records

df2 <- df$fields

achu <- read_sf(as.character(GET('https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&format=geojson', accept_json())))

#(delito = HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (COLISION)) OR (delito = DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A AUTOMOVIL) OR (delito = DAÑO EN PROPIEDAD AJENA CULPOSA POR TRÁNSITO VEHICULAR A BIENES INMUEBLES) OR (delito = HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR) OR (delito = HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (ATROPELLADO)) OR (delito = HOMICIDIO CULPOSO POR TRÁNSITO VEHICULAR (CAIDA)) OR (delito = LESIONES CULPOSAS POR TRANSITO VEHICULAR) OR (delito = LESIONES CULPOSAS POR TRANSITO VEHICULAR EN COLISION)
#q=(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(COLISION))+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+AUTOMOVIL)+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+BIENES+INMUEBLES)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(ATROPELLADO))+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(CAIDA))+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR)+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR+EN+COLISION)&facet=delito&facet=categoria&facet=ao&facet=mes&facet=sexo&facet=tipopersona&facet=clasificaciondelito
# =========================
i <- 0
url <- 'https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=tiraderos-clandestinos-al-cierre-de-2017&fields=alcaldia,calle,colonia&rows=100'

repeat {
  if (i == 0) {
    tmp <- as.character(GET(url,
                            accept_json()))
    df_final <- fromJSON(tmp)$records$fields
    i <- i + 100
  } else {
    tmp <- as.character(GET(paste0(url,'&start=',i),
                            accept_json()))
    df <- fromJSON(tmp)$records$fields
    if (nrow(df) < 100) {
      df_final <- rbind(df_final , df)
      break
    } else {
      df_final <- rbind(df_final , df)
      i <- i + 100
    }
  }
}

# ==========
i <- 0
url <- 'https://datos.cdmx.gob.mx/api/records/1.0/search/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&rows=100&q=(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(COLISION))+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+AUTOMOVIL)+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+BIENES+INMUEBLES)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(ATROPELLADO))+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(CAIDA))+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR)+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR+EN+COLISION)'

repeat {
  if (i == 0) {
    tmp <- as.character(GET(url,
                            accept_json()))
    df_final <- fromJSON(tmp)$records$fields
    i <- i + 100
  } else {
    tmp <- as.character(GET(paste0(url,'&start=',i),
                            accept_json()))
    df <- fromJSON(tmp)$records$fields
    if (nrow(df) < 100) {
      df_final <- rbind(df_final , df)
      break
    } else {
      df_final <- rbind(df_final , df)
      i <- i + 100
    }
  }
}

# ===================== Download API

url <- 'https://datos.cdmx.gob.mx/api/records/1.0/download/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&q=(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(COLISION))+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+AUTOMOVIL)+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+BIENES+INMUEBLES)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(ATROPELLADO))+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(CAIDA))+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR)+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR+EN+COLISION)&format=json'

tmp <- as.character(GET(url , accept_json()))
df <- fromJSON(tmp)$fields

# ======

url <- 'https://datos.cdmx.gob.mx/api/records/1.0/download/?dataset=carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico&q=(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(COLISION))+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+AUTOMOVIL)+OR+(delito+%3D+DA%C3%91O+EN+PROPIEDAD+AJENA+CULPOSA+POR+TR%C3%81NSITO+VEHICULAR+A+BIENES+INMUEBLES)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR)+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(ATROPELLADO))+OR+(delito+%3D+HOMICIDIO+CULPOSO+POR+TR%C3%81NSITO+VEHICULAR+(CAIDA))+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR)+OR+(delito+%3D+LESIONES+CULPOSAS+POR+TRANSITO+VEHICULAR+EN+COLISION)&format=json'

#format=json (t = 22.39s)
df <- fromJSON(paste0(url , '&format=json'))$fields

#format=csv (t = 24.14s)
df2 <- read.csv(paste0(url , '&format=csv') , sep = ";")$fields

# https://help.opendatasoft.com/apis/ods-search-v1/#record-search-api