######################
#
library(googledrive)
library(googlesheets4)
library(readxl)
library(sf)
library(leaflet)
library(stringr)
drive_download(file = as_id("1pwn_NaY9CnfhhCZGU90--J27gq71J9JD"), path = paste0("data/database/BaseDatos_Master.xlsx"), overwrite = TRUE)
res_db <- read_excel("data/database/BaseDatos_Master.xlsx", sheet="Base de datos master") 

#############################################################################
######################## Download all shapefiles available - Be more tactical
#### 1 - Farms for nature
files_f4n <- drive_ls(as_id("1iSWbvDHykoPopQ6JVvnDUs56YumSGFP8"), recursive=F)
dir.create("data/shapefile/F4N")
j <- 1
i <- 1
for(j in 1:nrow(files_f4n))
  {
    tmp <- drive_ls(as_id(files_f4n$id[j]), recursive=T)
    #Remove spaces from the names 
    tmp_name <- str_replace(files_f4n$name[j], " ", "_")
      for(i in 1:nrow(tmp))
      {
        # Remove folders
        if(length(str_split(tmp$name[i], "[.]")[[1]])>1)
        {
        drive_download(file = as_id(tmp$id[i]), path = paste0("data/shapefile/F4N/",tmp_name,"-",tmp$name[i]), overwrite = TRUE)
        }
      }
  }
# Read in all the files
f4n_shp <- list.files("data/shapefile/F4N", full.names=T, pattern =".shp")

f4n_shp_wgs <- list()
i <-2
for(i in 1:length(f4n_shp))
{
  f4n_shp_wgs[[i]] <- st_read(f4n_shp[i])
  # if it isnt wgs 1984 convert it
  f4n_shp_wgs[[i]] <- st_transform(f4n_shp_wgs[[i]], 4326)
  # add a location column 
  f4n_shp_wgs[[i]]$location <- substr(str_split(f4n_shp[i], "-")[[1]][1], 20, 100)
  # Add a type column
  f4n_shp_wgs[[i]]$type <- st_geometry_type(f4n_shp_wgs[[i]])
  # Remove the zm issues
  f4n_shp_wgs[[i]]<- st_zm(f4n_shp_wgs[[i]], drop = TRUE, what = "ZM")
}

m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

i <-1
for(i in 1:length(f4n_shp_wgs))
{
  if(f4n_shp_wgs[[i]]$type[1]=="POLYGON")
  {
  # The last point
  m <-   m %>%   addPolygons(data=f4n_shp_wgs[[i]], label=f4n_shp_wgs[[i]]$location[1])
  }
}
m

# To send to planet we need to merge each by location and send a single polygon

aoi_f4n <- list()
for(i in 1:length(f4n_shp_wgs))
{
  if(f4n_shp_wgs[[i]]$type[1]=="POLYGON")
  {
    tmp <- f4n_shp_wgs[[i]]
    tmp <- st_make_valid(tmp)
    aoi_f4n[[i]] <- tmp[, "location"]
  }
}
library(dplyr)
library(MetBrewer)
aoi_f4n <- bind_rows(aoi_f4n)
#plot(st_geometry(aoi_f4n))
aoi_f4n <- st_make_valid(aoi_f4n)
aoi_f4n <- aoi_f4n %>% group_by(location) %>% summarise()

# Give unique colours
aoi_f4n$colour <- as.character(met.brewer(name="Homer1", n=nrow(aoi_f4n)))

m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

m <-   m %>%   addPolygons(data=aoi_f4n, label=aoi_f4n$location, color = aoi_f4n$colour, opacity=1)

m

###########################################################################
###########################################################################
# 2 - Pending - 2024
#https://drive.google.com/drive/folders/?usp=drive_link
# Highlands
files_rest_high <- drive_ls(as_id("1ifnFwP-1_H4fwTXEQpc1agISQuFG6as_"), recursive=F)
files_rest_low <- drive_ls(as_id("1TSEkMOPHSU2jgg_kd7gokffwMYFuPdrs"), recursive=F)
files_rest <- rbind(files_rest_high, files_rest_low)

dir.create("data/shapefile/planned")
j <- 1
i <- 1
for(j in 1:nrow(files_rest))
{
  tmp <- drive_ls(as_id(files_rest$id[j]), recursive=T)
  #Remove spaces from the names 
  tmp_name <- str_replace(files_rest$name[j], " ", "_")
  for(i in 1:nrow(tmp))
  {
    # Remove folders
    if(length(str_split(tmp$name[i], "[.]")[[1]])>1)
    {
      drive_download(file = as_id(tmp$id[i]), path = paste0("data/shapefile/planned/",tmp_name,"-",tmp$name[i]), overwrite = TRUE)
    }
  }
}

# Read in all the files
res_shp <- list.files("data/shapefile/planned", full.names=T, pattern =".shp")

# Remove ones that dont run
res_shp <- res_shp[res_shp!="data/shapefile/planned/Fundación_Corcovado-Fundacion Corcovado.shp"]



res_shp_wgs <- list()
i <-2

for(i in 1:length(res_shp))
{
  res_shp_wgs[[i]] <- st_read(res_shp[i])
  # if it isnt wgs 1984 convert it
  res_shp_wgs[[i]] <- st_transform(res_shp_wgs[[i]], 4326)
  # add a location column 
  res_shp_wgs[[i]]$location <- substr(str_split(res_shp[i], "-")[[1]][1], 24, 100)
  # Add a type column
  res_shp_wgs[[i]]$type <- st_geometry_type(res_shp_wgs[[i]])
  # Remove the zm issues
  res_shp_wgs[[i]]<- st_zm(res_shp_wgs[[i]], drop = TRUE, what = "ZM")
}

m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

i <-1
for(i in 1:length(res_shp_wgs))
{
  if(res_shp_wgs[[i]]$type[1]=="POLYGON")
  {
    # The last point
    m <-   m %>%   addPolygons(data=res_shp_wgs[[i]], label=res_shp_wgs[[i]]$location[1])
  }
}
m

###############################################################################
# To send to planet we need to merge each by location and send a single polygon

aoi_res <- list()
for(i in 1:length(res_shp_wgs))
{
  if(res_shp_wgs[[i]]$type[1]=="POLYGON")
  {
    tmp <- res_shp_wgs[[i]]
    tmp <- st_make_valid(tmp)
    aoi_res[[i]] <- tmp[, "location"]
  }
}
aoi_res <- bind_rows(aoi_res)
#plot(st_geometry(aoi_res))
aoi_res <- st_make_valid(aoi_res)
aoi_res <- aoi_res %>% group_by(location) %>% summarise()

# Give unique colours
aoi_res$colour <- as.character(met.brewer(name="Homer1", n=nrow(aoi_res)))

m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

m <-   m %>%   addPolygons(data=aoi_res, label=aoi_res$location, color = "red", opacity=1)

m

###################################################
# Historical 
files_his_2020 <- drive_ls(as_id("1ixobdP1jAF3upq1NCVQIWLIELhFtF_35"), recursive=F)
files_his_2020$year <- 2020
files_his_2021 <- drive_ls(as_id("1iwNHOty5nyCF5q34ruMemTmWd3sN5aIG"), recursive=F)
files_his_2021$year <- 2021
files_his_2022 <- drive_ls(as_id("1iuPXcZsfGH8TIH_g0IZTnVyMbUpN2okN"), recursive=F)
files_his_2022$year <- 2022
files_his_2023 <- drive_ls(as_id("1isiCI0PylPxJ-8MwdVUeCkU1GQ1AGUIN"), recursive=F)
files_his_2023$year <- 2023
files_his_enrich <- drive_ls(as_id("1inDLw-ipH8y69SlsQKzvzFJ2maDtWXbg"), recursive=F)
files_his_enrich$year <- "enrich"

files_his <- rbind(files_his_2020, files_his_2021,files_his_2022,
                   files_his_2023,files_his_enrich)

dir.create("data/shapefile/historical")
j <- 1
i <- 1
for(j in 1:nrow(files_his))
{
  tmp <- drive_ls(as_id(files_his$id[j]), recursive=T)
  #Remove spaces from the names 
  tmp_name <- str_replace(files_his$name[j], " ", "_")
  for(i in 1:nrow(tmp))
  {
    # Remove folders
    if(length(str_split(tmp$name[i], "[.]")[[1]])>1)
    {
      drive_download(file = as_id(tmp$id[i]), path = paste0("data/shapefile/historical/",tmp_name,"-",tmp$name[i]), overwrite = TRUE)
    }
  }
}


# Read in all the files
his_shp <- list.files("data/shapefile/historical", full.names=T, pattern =".shp")

# Remove ones that dont run
#his_shp <- his_shp[his_shp!="data/shapefile/historical/Fundación_Corcovado-Fundacion Corcovado.shp"]



his_shp_wgs <- list()
for(i in 1:length(his_shp))
{
  his_shp_wgs[[i]] <- st_read(his_shp[i])
  # if it isnt wgs 1984 convert it
  his_shp_wgs[[i]] <- st_transform(his_shp_wgs[[i]], 4326)
  # add a location column 
  his_shp_wgs[[i]]$location <- substr(str_split(his_shp[i], "-")[[1]][1], 27, 100)
  # Add a type column
  his_shp_wgs[[i]]$type <- st_geometry_type(his_shp_wgs[[i]])
  # Remove the zm issues
  his_shp_wgs[[i]]<- st_zm(his_shp_wgs[[i]], drop = TRUE, what = "ZM")
}

m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

i <-1
for(i in 1:length(his_shp_wgs))
{
  if(his_shp_wgs[[i]]$type[1]=="POLYGON")
  {
    # The last point
    m <-   m %>% addPolygons(data=his_shp_wgs[[i]], label=his_shp_wgs[[i]]$location[1])
  }
}
m

###############################################################################
# To send to planet we need to merge each by location and send a single polygon

aoi_his <- list()
for(i in 1:length(his_shp_wgs))
{
  if(his_shp_wgs[[i]]$type[1]=="POLYGON")
  {
    tmp <- his_shp_wgs[[i]]
    tmp <- st_make_valid(tmp)
    aoi_his[[i]] <- tmp[, "location"]
  }
}

aoi_his <- bind_rows(aoi_his)
#plot(st_geometry(aoi_his))
aoi_his <- st_make_valid(aoi_his)
aoi_his <- aoi_his %>% group_by(location) %>% summarise()

# Add in the year

files_his <- as.data.frame(files_his)
files_his$id <- NULL
files_his$drive_resource <- NULL

files_his$name <- str_replace(files_his$name, " ", "_")

aoi_his  <-left_join(aoi_his, files_his, by=c("location" = "name"))

# Asada brasilia is 2023
aoi_his$year[aoi_his$location=="ASADA_Brasilia "] <- 2023

# Colour by year
aoi_his$colour <- met.brewer(name="Austria", n=length(unique(aoi_his$year)))[as.factor(aoi_his$year)]


# Deal with the geometry collections
geom_tmp <- list()
geom_tmp[[1]] <- st_collection_extract(aoi_his[aoi_his$location=="Ronald_Gamboa",], "POLYGON" )
geom_tmp[[2]] <- st_collection_extract(aoi_his[aoi_his$location=="Vilma_Garcia 1",], "POLYGON" )

aoi_his <- aoi_his[!(aoi_his$location %in% c("Ronald_Gamboa", "Vilma_Garcia 1")),]
aoi_his <- bind_rows(aoi_his, geom_tmp)



m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

m <-   m %>%   addPolygons(data=aoi_his, label=aoi_his$location, color = aoi_his$colour, opacity=1)
m


##########################################################################
# Add meta data and plot them all on the same map ########################

head(aoi_his)
aoi_his$type <- "planted"
head(aoi_f4n)
aoi_f4n$type <- "farms4nature"
aoi_f4n$year <- "2024"
aoi_res$type <- "planned"
aoi_res$year <- "2024"


aoi_his <- aoi_his[, c("location","geometry","year","colour","type")]
aoi_f4n <- aoi_f4n[, c("location","geometry","year","colour","type")]
# Update colour
aoi_f4n$colour <- "#3EFAEE"
aoi_res <- aoi_res[, c("location","geometry","year","colour","type")]
aoi_res$colour <- "#FB991D"

aoi_all <- bind_rows(aoi_f4n, aoi_res, aoi_his)

tmp <- aoi_his[,c("year", "colour")]
tmp$geometry <- NULL
tmp[duplicated(tmp)==F,]

m <- leaflet() %>%
  # Add a satellite image layer  
  addProviderTiles(providers$CartoDB.PositronNoLabels, group="Simple") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group="OS")%>%
  addLayersControl(
    baseGroups = c("OS", "Simple", "Satellite"),
    overlayGroups = c(unique(aoi_all$type), "biodiversity"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
addLegend(position="bottomleft", labels=c("Farms4Nature", "Planned - '24", "Planted - '23", 
                                        "Planted - '22","Planted - '21","Planted - '20",
                                        "Planted - Enriched"), 
                colors=c("#3EFAEE","#FB991D","#ffcd12","#007e2f","#16317d", "#a40000","#b86092")  
                         , title="Type", opacity=1)
m <-   m %>%   addPolygons(data=aoi_all, label=paste(aoi_all$location, aoi_all$type, aoi_all$year), 
                           color = aoi_all$colour,group=aoi_all$type, opacity=1)

# Import the biodiversity sirvey locations
bio_surveys <- read_sheet("https://docs.google.com/spreadsheets/d/1TwzLJFRQgyZgaf28r4ac3kib2iQaybFasbTJAxoe3cw")

m <-   m %>%   addMarkers(lng=bio_surveys$Longitud, lat=bio_surveys$Latitud, label=bio_surveys$farmer,
                                group="biodiversity")
m


# Save the derived files
dir.create("data/processed")
st_write(aoi_all, "data/processed/restoration_locations.shp")
write.csv(bio_surveys, "data/processed/biodiversity_surveys.csv", row.names=F)


dir.create("docs")

















