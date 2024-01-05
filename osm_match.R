
# /************************************************************************************/
# /**                                                                                **/
# /** Project Traffic Accidents                                                      **/
# /**                                                                                **/
# /** Download and macth OSM data with accidents                                     **/
# /**                                                                                **/
# /**                                                                                **/
# /************************************************************************************/

# load libraries
if(!require(osmextract)) install.packages("osmextract")
if(!require(sf)) install.packages("sf")
if(!require(data.table)) install.packages("data.table")

library(osmextract)
library(sf)
library(data.table)

# Check whether accidents data is already loaded into memory 
# => run first part of TrafficAccidents.R otherweise
if (!exists("accidents") | !is.data.frame(get("accidents"))) {
  stop("Accidents data must be loaded in data frame 'accidents'.")
}

# state names in accidents used for filtering
ger_state_filters <- c("Baden-Württemberg",
                      "Bayern",
                      "",
                      "Bremen",
                      "Hamburg",
                      "Hessen",
                      "Mecklenburg-Vorpommern",
                      "Niedersachsen",
                      "Nordrhein-Westfalen",
                      "Rheinland-Pfalz",
                      "Saarland",
                      "Sachsen",
                      "Sachsen-Anhalt",
                      "Schleswig-Holstein",
                      "Thüringen")

# file names from https://download.geofabrik.de/europe/germany.html
# make sure all files are downloaded before running this

ger_state_pbfs <- c("baden-wuerttemberg",
                    "bayern",
                    "brandenburg",
                    "bremen",
                    "hamburg",
                    "hessen",
                    "mecklenburg-vorpommern",
                    "niedersachsen",
                    "nordrhein-westfalen",
                    "rheinland-pfalz",
                    "saarland",
                    "sachsen",
                    "sachsen-anhalt",
                    "schleswig-holstein",
                    "thueringen")

# file names for intermediate results
ger_state_csvs <- c("baden",
                    "bayern",
                    "brandenburg",
                    "bremen",
                    "hamburg",
                    "hessen",
                    "mecklenburg",
                    "niedersachsen",
                    "nordrhein",
                    "rheinland",
                    "saarland",
                    "sachsen",
                    "anhalt",
                    "schleswig",
                    "thueringen")

# query string for relevant roads in OSM
my_vectortranslate <- c(
  # SQL-like query where we select only the following fields
  "-select", "osm_id, highway, surface, lanes, maxspeed", 
  # SQL-like query where we filter only the features where highway is equal to footway or cycleway
  "-where", "highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'residential', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link')"
)

folder <- "TrafficAccidents/"
URL <- "https://download.geofabrik.de/europe/germany/"

# Check if the folder exists, if it doesn't exist, create the folder
if (!file.exists(folder)) {
  dir.create(folder)
}

# for each state
for (state_nr in 1:length(ger_state_filters)) {

  print(ger_state_pbfs[state_nr])

  # download if file does not yet exist
  if (!file.exists(paste(folder, ger_state_pbfs[state_nr], "-latest.osm.pbf", sep = ""))) {
    download.file(paste(URL, ger_state_pbfs[state_nr], "-latest.osm.pbf", sep = ""),
                  paste(folder, ger_state_pbfs[state_nr], "-latest.osm.pbf", sep = ""))
  }
  
  # convert pbf file to .gpkg file
  osm_gpkg <- oe_vectortranslate(paste(folder, ger_state_pbfs[state_nr], "-latest.osm.pbf", sep = ""), 
                                 extra_tags = c("surface", "lanes", "maxspeed"),
                                 vectortranslate_options = my_vectortranslate,
                                 layer="lines")

  # read gpkg file and convert to sf
  ger_st <- st_read(osm_gpkg, layer="lines")
  ger_sf <- sf::st_as_sf(ger_st)

  # calculate bounding box for each road (speeds up later intersections)
  # initialize variables
  ger_sf$xmin <- 0
  ger_sf$xmax <- 0
  ger_sf$ymin <- 0
  ger_sf$ymax <- 0

  # for all roads
  for (i in 1:nrow(ger_sf)) {
    
    #calculate bounding box and save to dataframe
    coords <- st_bbox(ger_sf[i,])
    ger_sf$xmin[i]  <- coords["xmin"]
    ger_sf$xmax[i]  <- coords["xmax"]
    ger_sf$ymin[i]  <- coords["ymin"]
    ger_sf$ymax[i]  <- coords["ymax"]
    
    # message progress
    if (round(i/5000)*5000==i) print(i)
  }

  # extract data from state to accidents_tmp (Berlin and Brandenburg where lumped together)
  if (ger_state_filters[state_nr] == "") {
    accidents_tmp <- accidents |> filter(state %in% c("Brandenburg", "Berlin"))
  } else {
    accidents_tmp <- accidents |> filter(state == ger_state_filters[state_nr])
  }
  
  # select geo coordinates and initialize variables  
  roads <- accidents_tmp |> select(WGSX, WGSY)
  roads$highway <- 0
  roads$lanes <- 0
  roads$maxspeed <- 0
  roads$surface <- 0

  # what are the surface types?
  fact <- factor(ger_sf$surface)
  surfaces <- levels(fact[1])

  # for all accident locations
  for(i in 1:nrow(roads)) {
    
    # create a small box around accident location and convert coordinate reference system
    lon <- roads$WGSX[i]
    lat <- roads$WGSY[i]
    d <- .0001
    bbox <- st_polygon(list(rbind( c(lon - d, lat - d),
                                   c(lon - d, lat + d),
                                   c(lon + d, lat + d),
                                   c(lon + d, lat - d),
                                   c(lon - d, lat - d))))
    bbox <- st_sfc(bbox, crs = 4326)
  
    # select all roads that could have an intersection
    possible <- ger_sf |> filter(xmin <= lon + d) |> 
                          filter (xmax >= lon - d) |>
                          filter (ymin <= lat + d) |>
                          filter (ymax >= lat - d) 
    
    # calculate exact intersections with roads
    suppressWarnings({
       osm_data <- st_intersection(possible, bbox)
    })
    
    # result could be several roads (e.g. accidents on junctions)
    # mean of road category
    roads$highway[i] <- mean(c(match(osm_data$highway, highways), 
                               match(osm_data$highway, links)), 
                             na.rm=TRUE)
    
    # mean of max speed
    roads$maxspeed[i] <- mean(as.integer(str_replace(
                                         str_replace(
                                         str_replace(
                                         str_replace(osm_data$maxspeed,
                                                     "signals", "50"),
                                                     "none", "250"),
                                                     "DE:urban", "50"),
                                                     "DE:rural", "100")), 
                                                     na.rm=TRUE)
    
    # mean of lanes
    roads$lanes[i] <- mean(as.integer(osm_data$lanes), na.rm=TRUE)
    
    # "mean" of surfaces - will be rounded
    roads$surface[i] <- mean(match(osm_data$surface, surfaces), na.rm=TRUE)

    if (round(i/500)*500==i) print(i)
  }

  fwrite(roads, paste(folder, "road_", ger_state_csvs[state_nr], ".csv", sep = ""))

}


# cncatenate all files, initialize allroads
all_roads <- data.frame()

# for each state load and append file
for (state_nr in 1:length(ger_state_csvs)) {

    # read file
    roads <- fread(paste(folder, "road_", ger_state_csvs[state_nr], ".csv", sep = ""))

    # round all variables
    roads <- roads |> 
      mutate(highway = round(highway),
             lanes = round(lanes),
             surface = round(surface),
             maxspeed = round(maxspeed))

    # read gpkg file and convert to sf
    ger_st <- st_read(paste(folder, ger_state_pbfs[state_nr], "-latest.gpkg", sep = ""), layer="lines") 
    ger_sf <- sf::st_as_sf(ger_st)
    
    # what are the surface types?
    fact <- factor(ger_sf$surface)
    surfaces <- levels(fact[1])
    
    # convert to character string
    roads$surface <- surfaces[roads$surface]
    
    # combine with previous data
    all_roads <- rbind(all_roads, roads)
}

half <- round(nrow(all_roads)/2)

# write first half of data into file
fwrite(all_roads[1:half], paste(folder, "road_all1.csv", sep = ""))

# write second half of  data into file
fwrite(all_roads[half+1:nrow(all_roads)], paste(folder, "road_all2.csv", sep = ""))

