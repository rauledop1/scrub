#########################################################################################
#######Monitoring natural competition of Pine and Eucalyptus plantation##################
#############script develop by @raulperez########Biobio Chile############################
#######################raul.perez.astorga@gmail.com######################################
#########################################################################################

library(sf)
library(rgee)
library(cptcity)
library(tidyverse)
library(reshape2)
ee_Initialize(drive = TRUE) #required google earth engine acount


#area of interest 
token <- '92QetvsbMf8zCT_deKo5Qp_ugjXOtxK-KscMUGeWA6wl0bG-Zt9aRm2gwdT8ulf7WzABOkvv-vZxLWFnodX6OHP9jOw65loh9SnURBgZt60NBdPNYKmc8mDPdajEBHSipk3ktmbajJMDjcLCUhL5RfcMPwrjDS1SfExJeMXB3oRVkR2lOgUJvGJGzT9NruC7yiMHRU0c1XuKrbaKJfqzYW4ZGW7lSixTytEqBcr-no3LHclfNAAo9xAg4AC3liSk'
NUMSOLPED <-'0400035203'
url <-paste0('https://araucaria.arauco.com/server/rest/services/Silvicola/Contratadas/FeatureServer/0/query?where=SZONA%3D%27Arauco%27+and+NUMSOLPED%3D%27',
             NUMSOLPED,
             '%27&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Meter&relationParam=&outFields=IDPREDIO%2CTEMPORADA%2CIDTIPOOPERACIONSILVICOLA%2CNUMSOLPED%2CIDSERVICIOSILVICOLA+%2C+SSERVICIO%2C+IDESPECIESILVICOLA%2CANOPLANTACION%2CCANTIDADCONTRATADA+%2C+COSTOUNITARIO&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&having=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=xyFootprint&resultOffset=&resultRecordCount=&returnTrueCurves=false&returnExceededLimitFeatures=false&quantizationParameters=&returnCentroid=false&sqlFormat=none&resultType=&featureEncoding=esriDefault&f=geojson&token=',
             token)
solped=read_sf(url) %>%
  st_transform(32718) %>%
  sf_as_ee()# a metric CRS
#st_geometry() #extract only the geometry


list_modis <-
  ee$ImageCollection('COPERNICUS/S2')$ filterDate(
    '2017-11-01','2020-11-01')$
  filterMetadata('CLOUDY_PIXEL_PERCENTAGE','less_than',1)$
  filterBounds(solped)

addNdwi <- function(x){   
  #add NDWI index
  ndwi_values <- x$normalizedDifference(c("B8A", "B11"))$
    copyProperties(x,list("system:time_start"))
}

#Cloud masking function Sentinel 2
s2_clean <- function(x){
  #Cloud clean
  cloudShadowBitMask <- bitwShiftL(1,10)
  cloudBitMask <- bitwShiftL(1,11)
  qa <- x$select("QA60")
  
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudBitMask)$eq(0))
  
  x$updateMask(mask)$
    divide(10000)$
    copyProperties(x, list("system:time_start"))
}

NDWI<-list_modis$map(s2_clean)$
  map(addNdwi)$
  select("nd")

list<-ee_get_date_ic(NDWI)

ndvi_ts <- ee_extract(NDWI$toBands(),
                      solped,
                      fun = ee$Reducer$mean())

ndvi_ts2<- melt(ndvi_ts)
colnames(ndvi_ts2) <- c("date","ndwi")
ndvi_ts2$date <- as.Date(substr(ndvi_ts2$date,2,9),format = "%Y%m%d")
plot(ndvi_ts2$date,ndvi_ts2$ndwi, xlab = "Fecha", ylab = "NDWI")
