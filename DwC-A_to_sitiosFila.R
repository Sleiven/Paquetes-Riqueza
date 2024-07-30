# script para transformar el conjunto de datos dwc-a
# de entomofauna en cuatro sitios en Bogotá a una tabla de 
# sitios x especie + tabla de coordenadas

# se toma la tabla original 
# https://ipt.biodiversidad.co/sib/resource?r=sda_artropofauna_aie-montana
# doi 10.15472/cwrg3e

library(tidyverse)

# 1. importar el archivo (ubicado en una subruta del directorio de trabajo)
# Nota, no se utilizó read.table porque la base de datos es muy grande
# así que se usara fread {data.table} que es más eficiente con grandes 
# conjuntos de datos

registros <- fread('./codigo_r_y_desarrollo/comparaciones_categorias/predatos/occurrence.txt')

dim(registros)

  # una formita de verificar que se haya copiado bien los nombres de columna
    
    # copiar manualmente la primera fila de los datos, copiarlos entre comilla
    ori <- "id	type	language	accessRights	institutionID	institutionCode	basisOfRecord	occurrenceID	recordNumber	recordedBy	recordedByID	individualCount	sex	lifeStage	reproductiveCondition	establishmentMeans	occurrenceStatus	preparations	otherCatalogNumbers	occurrenceRemarks	organismRemarks	eventID	parentEventID	eventDate	eventTime	year	month	day	habitat	samplingProtocol	sampleSizeValue	sampleSizeUnit	samplingEffort	eventRemarks	continent	country	countryCode	stateProvince	county	municipality	locality	verbatimLocality	minimumElevationInMeters	maximumElevationInMeters	verbatimElevation	locationAccordingTo	locationRemarks	decimalLatitude	decimalLongitude	geodeticDatum	coordinateUncertaintyInMeters	coordinatePrecision	verbatimCoordinates	verbatimLatitude	verbatimLongitude	verbatimCoordinateSystem	verbatimSRS	georeferencedBy	verbatimIdentification	identificationQualifier	identifiedBy	identifiedByID	dateIdentified	identificationRemarks	scientificName	higherClassification	kingdom	phylum	class	order	family	subfamily	genus	specificEpithet	infraspecificEpithet	taxonRank	verbatimTaxonRank	scientificNameAuthorship	vernacularName	taxonomicStatus"
    
    # se separan individualmente y se comparan con los nombres de columna importados
    names <- ori %>% strsplit("\t") %>% unlist()
    sum(colnames(registros) == names)
    
# 2. Tomar las columnas de importancia

registros_sitio <- registros %>% 
  select(eventID, decimalLatitude, decimalLongitude,
         individualCount, family) 

  # verificar que las columnas no tengan vacíos o NA
    has_nans_vacio <- function(matriz) {
      for (colu in 1:ncol(matriz)) {
        
        nombre_col <- colnames(matriz)[colu]
        vacio <- sum(matriz[[nombre_col]] == "")
        nans <- sum(is.na(matriz[[nombre_col]]))
        
        if (vacio > 0) {
          donde <- which(matriz[[nombre_col]] == "")
          cat(nombre_col, "tiene", sum(vacio), "espacios vacíos en las filas:", donde, "\n")
        } else if (nans > 0) {
          donde <- which(is.na(matriz[[nombre_col]]))
          cat(nombre_col, "tiene", sum(nans), "nans en las filas:", donde, "\n")
        } else {
          cat(nombre_col, "no tiene vacíos ni nans", "\n")
        }
      }
    }
    
  has_nans_vacio(registros_sitio)
  
  # como si tiene vacios, se analiza que tan distribuidos estan
  table(registros_sitio$family) # todos pertenecen a un mismo
  # registro, el cual, mirando la tabla original, corresponde
  # a un morfotipo (Morfotipo9) que no pudo ser identificado al nivel de familia

  registros_sitio <- registros_sitio %>% 
    mutate(family = ifelse(family == "", "Morfotipo9", family))

# 3. Expandir los registros para que quede sitios x familia

especies_col <- registros_sitio %>% 
  pivot_wider(names_from = family, values_from = individualCount,
              values_fn = function(x)sum(x),
              values_fill = 0)
dim(especies_col)

# 4. subdividir la matriz en dos tablas, la de registros y las coordenadas
  
  # Matriz de registros
  matriz_sitio_fila <- especies_col %>% 
    select(-c(decimalLatitude, decimalLongitude))
  dim(especies_col)
  
  saveRDS(matriz_sitio_fila, "./codigo_r_y_desarrollo/comparaciones_categorias/datos_sitio_fila.rds")

  # matriz de coordenadas
  matriz_coord <- especies_col %>% 
    select(eventID, y = decimalLatitude , x = decimalLongitude)
  
  saveRDS(matriz_coord, "./codigo_r_y_desarrollo/comparaciones_categorias/coord_datos.rds")

