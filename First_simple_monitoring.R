#########################################################################################
#######Monitoring natural competition of Pine and Eucalyptus plantation##################
#############script develop by @raulperez########Biobio Chile############################
#######################raul.perez.astorga@gmail.com######################################
#########################################################################################

library(sf)
library(rgee)
library(cptcity)

ee_Initialize(drive = TRUE) #required google earth engine acount


#area of interest 
token <- '_HoXx7GZDb88ScZjRNIi6GHJLNHjGiJKlyOWAeUf0nIX_Hq31t8kSfetXOq6n3qEm1wZKP6Ebyshoo87Krq7GzHqCf6Mae6BFvoCiZxGLBDw8CshE-PdGqWAecuGzisgd_dfxb3aUzyL7apkpkKuPrV4ePwOdcFFlYG2nYdwpFthZsOQpUnjuBvGiBFBsjT7ao_-fWxXydmczgSMkTMg7N_wcv1Nh1PnDFyTVqVsZKWkFzj6YaEfITIRWdtELZ9M'
NUMSOLPED <-'0400035203'
url <-paste0('https://araucaria.arauco.com/server/rest/services/Silvicola/Contratadas/FeatureServer/0/query?where=SZONA%3D%27Arauco%27+and+NUMSOLPED%3D%27',
             NUMSOLPED,
             '%27&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Meter&relationParam=&outFields=IDPREDIO%2CTEMPORADA%2CIDTIPOOPERACIONSILVICOLA%2CNUMSOLPED%2CIDSERVICIOSILVICOLA+%2C+SSERVICIO%2C+IDESPECIESILVICOLA%2CANOPLANTACION%2CCANTIDADCONTRATADA+%2C+COSTOUNITARIO&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&having=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=xyFootprint&resultOffset=&resultRecordCount=&returnTrueCurves=false&returnExceededLimitFeatures=false&quantizationParameters=&returnCentroid=false&sqlFormat=none&resultType=&featureEncoding=esriDefault&f=geojson&token=',
             token)
solped=read_sf(url) %>%
  st_transform(32718) %>% # a metric CRS
  st_geometry() 


grid_spacing <- 30 # size of squares, in units of the CRS (i.e. meters for 32718)

polygony <- st_make_grid(solped, square = T, cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later

i <- st_intersection(solped, polygony)

gridto<- i %>%
  sf_as_ee()

ee_roi <- solped %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01","2016-01-31")$
  filterBounds(ee_roi)$
  filterMetadata('CLOUDY_PIXEL_PERCENTAGE','less_than'
                 ,40)$
  map(
    function(img){
      ndvi_values <- img$normalizedDifference(c("B8A","B4"))$rename("NDVI")
      ndwi_values <- img$normalizedDifference(c("B8A","B11"))$rename("NDWI")
    }
  )$
median()
ee_get_date_ic(ee_s2)
#bandNames<-ee_s2$bandNames()
#cat("Bands: ",paste(bandNames$getInfo(),"\n",collapse=" "))
ee_print(ee_s2[1])
ndvi <- ee_s2$select("NDVI")$reproject("EPSG:4326")

Map$centerObject(ee_roi)
Map$addLayer(ee_s2,
             visParams = list(
               min = -0.2 ,
               max = 0.9 ,
               palette = cpt("grass_ndvi", 10)
             )
) + Map$addLayer(ee_roi)

ndvi_mean_sf <- ee_extract(
  x = ee_s2,
  y = gridto,
  fun = ee$Reducer$mean(),
  scale = 20,
  sf = TRUE
)

plot(ndvi_mean_sf["NDWI"])
