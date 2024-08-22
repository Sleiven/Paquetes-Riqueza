# Script para transformar el conjunto de datos en formato occurrence DwC
# a una tabla de sitios x especie + tabla de coordenadas

# DM.Murillo-Cardona
# version 2 | 2024-07-31
# Esta version toma el recurso desde la cuenta de github, y da mayor explicacion a los procesos

# 1. Fuente de los datos ----
# Recurso creado por Ramírez et al (2023), es un consolidado de registros de 
# artrópodos, entre 2021 y 2022, de un monitoreo realizado en los Parques 
# Distritales Ecológicos de Montaña: Entre Nubes, La Serrania de Zuqué, Soratama 
# y Mirador de los Nevados
# https://doi.org/10.15472/cwrg3e


# 2. Carga de librerías ----
library(tidyverse)
library(data.table)
library(httr)


# 3. Importar el archivo con las ocurrencias desde github ----
urlfile <- "https://raw.githubusercontent.com/Sleiven/Paquetes-Riqueza/main/Ramirez_etal_2023_Artropodos_v1.4/occurrence.txt"
token <- "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"

accessURL <- GET(urlfile, add_headers(Authorization = paste("token", token)))
crudo <- content(accessURL, as = "text")

registros <- fread(text = crudo)
#>> Se utilizó fread {data.table} porque es mas eficiente con grandes conjuntos
#   de datos que read.table. 
#>> Se utilizó GET {httr} para especificar el token de acceso ya que el 
#   repositorio está privado


# 4. Revisión de la tabla ----

#> Cantidad de filas y columnas
dim(registros) # 1453 registros y 80 elementos DwC

#> Verificar que se haya copiado bien los nombres de columna
original_colname <- "id	type	language	accessRights	institutionID	institutionCode	basisOfRecord	occurrenceID	recordNumber	recordedBy	recordedByID	individualCount	sex	lifeStage	reproductiveCondition	establishmentMeans	occurrenceStatus	preparations	otherCatalogNumbers	occurrenceRemarks	organismRemarks	eventID	parentEventID	eventDate	eventTime	year	month	day	habitat	samplingProtocol	sampleSizeValue	sampleSizeUnit	samplingEffort	eventRemarks	continent	country	countryCode	stateProvince	county	municipality	locality	verbatimLocality	minimumElevationInMeters	maximumElevationInMeters	verbatimElevation	locationAccordingTo	locationRemarks	decimalLatitude	decimalLongitude	geodeticDatum	coordinateUncertaintyInMeters	coordinatePrecision	verbatimCoordinates	verbatimLatitude	verbatimLongitude	verbatimCoordinateSystem	verbatimSRS	georeferencedBy	verbatimIdentification	identificationQualifier	identifiedBy	identifiedByID	dateIdentified	identificationRemarks	scientificName	higherClassification	kingdom	phylum	class	order	family	subfamily	genus	specificEpithet	infraspecificEpithet	taxonRank	verbatimTaxonRank	scientificNameAuthorship	vernacularName	taxonomicStatus"
names <- original_colname %>% strsplit("\t") %>% unlist()
#>> Se tomaron los nombres de columna y se separaron

sum(colnames(registros) == names) # Debe dar igual a 80


# 5. Tomar las columnas de importancia

registros_sitio <- registros %>% 
  select(eventID, decimalLatitude, decimalLongitude,
         individualCount, family, verbatimIdentification) 


# 6. Crear funcion para verificar que las columnas no tengan vacíos
has_vacio <- function(tabla){
  
  # DESCRIPCION:
  # Función que toma un tibble o dataframe y analiza si tiene vacíos
      
  # CODIGO:
  if(is.matrix(tabla)) stop("Solo se admiten tibbles o dataframes")
      
  for (columna in 1:ncol(tabla)) {
  #>> Se establece la cantidad de columnas y se pasa por cada uno de ellas
        
  nombre_col <- colnames(tabla)[columna]
  vacio <- sum(tabla[[nombre_col]] == "")
  #>> Se obtiene el nombre de la columna
  #>> y luego se suma la cantidad de NA's
        
  #> Definir acciones si hay vacios o si no hay
  if (vacio > 0) {
    donde <- which(tabla[[nombre_col]] == "")
    cat(nombre_col, "tiene", sum(vacio), "espacios vacíos en las filas:", donde, "\n")
    } else {
    cat(nombre_col, "no tiene vacíos", "\n")
    }
  
  # RESULTADO:
  # Se entrega una cadena de texto por cada una de las columnas existentes
  # indicando si: 
  # (a) no tiene vacío ni nans
  # (b) tiene x espacios vacios, junto con el número de las filas en que se presentó

    }
}

has_vacio(registros_sitio)
  
  #> Como columna family tiene 15 vacios, se analiza su distribucion
  (no_familia <- table(registros_sitio[registros_sitio$family == "", 
                                       "verbatimIdentification"]))
  #>> Corresponde a tres morfotipos
  
  #> Averiguar por qué no fue identificado hasta familia
  morfos <- names(no_familia)
  
  sapply(X = morfos,
         FUN = function(x) unique(registros[registros$verbatimIdentification == x, 
                         c("verbatimTaxonRank", "taxonRank", "identificationRemarks")])
  )
  #>> para cada uno de los nombres identificados como morfo, se saca sus filas y tres
  #>> columnas, para luego saber los valores únicos
  
  # NOTA: dado que para los tres morfos se debe a que no se pudo una mayor resolucion
  #       por el especimen, entonces se puede dejar como "Morfo..."

  
# 7. Modificar los vacios por los morfotipos respectivos
  registros_sitioOK <- registros_sitio %>% 
    mutate(family = case_when(verbatimIdentification == "MF 151" ~ "Morfotipo151",
                              verbatimIdentification == "MF 555" ~ "Morfotipo555",
                              verbatimIdentification == "MF 9" ~ "Morfotipo9",
                              .default = family))
  #>> Con mutate se modifica la columna familia para que
  #>> (a) si verbatimIdentification tiene un morfotipo, se copiara extenso (condicion ~ valor)
  #>> (b) si no esta especificada ninguna condicion (default) entonces ponga el valor normal
  #>>     de la columna family
  
  #> Comprobar que haya quedado bien
  has_vacio(registros_sitioOK) # ya no tiene vacíos


# 8. Expandir los registros para que quede sitios x familia

especies_col <- registros_sitioOK %>% 
  pivot_wider(names_from = family, values_from = individualCount,
              values_fn = function(x)sum(x), #suma los valores de individualCount
              values_fill = 0)

  # ¿Como funciona ese values_fn?
  # Primero considera que un morfotipo puede estar en varios eventos de muestreo
    (cant <- registros_sitio[registros_sitio$verbatimIdentification == "MF 9", 
                             c("eventID", "individualCount")])
    #>> este, por ejemplo, estuvo en 12. Cada uno con una abundancia diferente
    #>> (aunque, por ejemplo ENJR_7_TP, siendo el mismo evento, esta en tres filas)
  
  # Luego, habria que hacer algo con aquellos que tiene valores repetidos en dos
  # columnas, porque por ejemplo: perteneciendo al mismo eventID, estan en la misma
  # familia ya sus géneros son distintos (si no se comprende, revisar siguiente filtro)
  # registros[registros$eventID == "ZUQ_9_RED" & registros$family == "Acrididae"]

    cant %>% 
      group_by(eventID) %>% # Todos aquellos que tengan mismo eventID se agrupan
      summarise(individualCount = sum(individualCount)) %>%  # se hace suma de individualCount
      table() # 1 de 1 individuo, 3 de 2 ind, 2 de 4 ind, 2 de 6 ind, 1 de 8 ind 
    
  # Eso es lo que hace values_fn
    especies_col[,"Morfotipo9"] %>% table()


# 9. Subdividir la matriz en dos tablas, la de registros y las coordenadas
  
  # Matriz de registros
  matriz_sitio_fila <- especies_col %>% 
    select(-c(decimalLatitude, decimalLongitude, verbatimIdentification))

  setwd("G:/.shortcut-targets-by-id/1lY-4uHdphyVr4Vb11idLDHXDF1E4rAmt/TrabajoGrado_DavidMurillo/Ejecucion_proyecto/templetes/word/Secciones")
  
  saveRDS(matriz_sitio_fila, "./codigo_r_y_desarrollo/paquetes_probados_rmd/Git/Paquetes-Riqueza/datos_sitio_fila.rds")

  # matriz de coordenadas
  matriz_coord <- especies_col %>% 
    select(eventID, y = decimalLatitude , x = decimalLongitude)
  
  saveRDS(matriz_coord, "./codigo_r_y_desarrollo/paquetes_probados_rmd/Git/Paquetes-Riqueza/matriz_coord.rds")
