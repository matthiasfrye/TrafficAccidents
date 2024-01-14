# /************************************************************************************/
# /**                                                                                **/
# /** Project Traffic Accidents                                                      **/
# /**                                                                                **/
# /** OpenStreetMap provides useful information about all roads in Germany.          **/
# /** OpenStreetMap data is provided under the                                       **/
# /** [Open Database License](openstreetmap.org/copyright)                           **/
# /** [Geofabrik](https://download.geofabrik.de/) provides data files for download.  **/
# /**                                                                                **/
# /** This script downloads OSM data and matches it with traffic accident locations. **/
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

# query string for relevant roads in OSM
my_vectortranslate <- c(
  # SQL-like query where we select only the following fields
  "-select", "osm_id, highway, lanes, maxspeed, lit, oneway, sidewalk, cycleway " 
)

folder <- "TrafficAccidents/"
URL <- "https://download.geofabrik.de/europe/germany/"

# Check if the folder exists, if it doesn't exist, create the folder
if (!file.exists(folder)) {
  dir.create(folder)
}

# download germany if file does not yet exist
if (!file.exists(paste(folder,"germany-latest.osm.pbf", sep = ""))) {
  download.file(paste(URL, "germany-latest.osm.pbf", sep = ""),
                paste(folder, "germany-latest.osm.pbf", sep = ""))
}

# convert germany pbf file to .gpkg file
osm_gpkg <- oe_vectortranslate(paste(folder, "germany-latest.osm.pbf", sep = ""), 
                               extra_tags = c("lanes", "maxspeed", "lit", "oneway", "sidewalk", "cycleway"),
                               vectortranslate_options = my_vectortranslate,
                               layer="lines")

# read gpkg file and convert to sf
ger_sf <- sf::st_as_sf(st_read(osm_gpkg, layer="lines"))


# read geometry of all roads
geom <- ger_sf$geometry

# initialize dataframe for all bounding boxes
bbox_all <- data.frame(c())

# 449*42407 rows in germany

for (j in 0:448) {
    bbox <- data.frame( xmin = rep(0, 42407),
                        xmax = rep(0, 42407),
                        ymin = rep(0, 42407),
                        ymax = rep(0, 42407))

    # for all roads
    for (i in 1:42407) {
      
        #calculate bounding box and save to dataframe
        coords <- geom[42407 * j + i][[1]]
        bbox$xmin[i]  <- min(coords[,1])
        bbox$xmax[i]  <- max(coords[,1])
        bbox$ymin[i]  <- min(coords[,2])
        bbox$ymax[i]  <- max(coords[,2])
    }
    
    # save results in bbox_all
    bbox_all <- rbind(bbox_all, bbox)
    print(paste("iteration", as.character(j), "complete"))
}

# copy results to ger_sf
ger_sf$xmin <- bbox_all$xmin
ger_sf$xmax <- bbox_all$xmax
ger_sf$ymin <- bbox_all$ymin
ger_sf$ymax <- bbox_all$ymax

##############################

# 11 * 11 * 5879 accidents


# select geo coordinates and sort by x cordinate
points <- accidents |> 
  mutate(key = paste(format(WGSX, digits = 13, scientific = FALSE, trim = TRUE),
                     format(WGSY, digits = 13, scientific = FALSE, trim = TRUE),
                     sep = "-")) |>
  group_by(key) |>
  summarize(WGSX = first(WGSX),
            WGSY = first(WGSY),
            .groups = "keep") |>
  ungroup() |>
  arrange(WGSX)

# Split accidents into smaller chunks
# 3 * 7 * 13 * 2579 = 273 * 2579 points exist in accidents

# find all possible values in highway
highway_levels <- levels(factor(ger_sf$highway))

# put important levels by size
road_categories <- union( c("motorway", "motorway_link", "trunk", "trunk_link", "primary", "primary_link", 
                            "secondary", "secondary_link", "tertiary", "tertiary_link", "unclassified", 
                             "residential", "living_street", "service", "road"),
                          highway_levels)

# convert to factor with defined levels
highway_factor <- factor(ger_sf$highway, levels = road_categories)

# convert to factor with defined levels
ger_sf$highway_n <- as.integer(highway_factor)
rm(highway_factor)

# initialize variable that collects all road data
roads_all <- c()

# 273 times
for (j in 0:272) { 

  # get portion of points  
  m <- j * 2579 + 1
  n <- (j +1) * 2579 
  points_tmp <- points[m:n,]
  points_tmp$highway <- NA
  points_tmp$lanes <- NA 
  points_tmp$maxspeed <- NA 
  points_tmp$lit <- NA
  points_tmp$oneway <- NA 
  points_tmp$sidewalk <- NA 
  points_tmp$cycleway <- NA
  
  # distance for box around accident location
  d <- .0001
 
  # compute min and max of coordinates
  points_xmin <- min(points_tmp$WGSX) - d * 10
  points_xmax <- max(points_tmp$WGSX) + d * 10
  
  # filter roads that might fit to these coordinates
  slice_sf <- ger_sf[(ger_sf$xmax >= points_xmin) & (ger_sf$xmin <= points_xmax),]

  # for all accident locations  
  for(i in 1:2579) { 

    # create a small box around accident location and convert coordinate reference system
    lon <- points_tmp$WGSX[i]
    lat <- points_tmp$WGSY[i]

    bbox <- st_sfc(st_polygon(list(rbind( c(lon - d, lat - d),
                                          c(lon - d, lat + d),
                                          c(lon + d, lat + d),
                                          c(lon + d, lat - d),
                                          c(lon - d, lat - d)))), crs = 4326)

    # select all roads that could have an intersection
    possible <- slice_sf[(slice_sf$xmin <= lon + d) & 
                           (slice_sf$xmax >= lon - d) &
                           (slice_sf$ymin <= lat + d) &
                           (slice_sf$ymax >= lat - d),]

    # calculate exact intersections with point
    suppressWarnings({
      # calculate exact intersections with point
      osm_data <- st_intersection(possible, bbox)
      
      # if nothing found, make a wider box
      if (n_roads == 0){
        bbox <- st_sfc(st_polygon(list(rbind(c(lon - d*4, lat - d*4),
                                             c(lon - d*4, lat + d*4),
                                             c(lon + d*4, lat + d*4),
                                             c(lon + d*4, lat - d*4),
                                             c(lon - d*4, lat - d*4)))), crs = 4326)
        osm_data <- st_intersection(possible, bbox)
        
        # if still nothing found, make an even wider box
        if (nrow(osm_data) == 0){
          bbox <- st_sfc(st_polygon(list(rbind(c(lon - d*10, lat - d*10),
                                               c(lon - d*10, lat + d*10),
                                               c(lon + d*10, lat + d*10),
                                               c(lon + d*10, lat - d*10),
                                               c(lon - d*10, lat - d*10)))), crs = 4326)
          osm_data <- st_intersection(possible, bbox)
        }
      }  
    })
    
    # result could be several roads (e.g. accidents on junctions)
    # find most important street
    k <- which.min(osm_data$highway_n)
    if (!is_empty(k)) { 
        
        # save information of most imprtant road
        points_tmp$highway[i] <- osm_data$highway[k]
        points_tmp$lanes[i] <- osm_data$lanes[k] 
        points_tmp$maxspeed[i] <- osm_data$maxspeed[k] 
        points_tmp$lit[i] <- osm_data$lit[k] 
        points_tmp$oneway[i] <- osm_data$oneway[k] 
        points_tmp$sidewalk[i] <- osm_data$sidewalk[k] 
        
        # check whether tag clceway is != no or cycleway close by
        points_tmp$cycleway[i] <- ((!is.na(osm_data$cycleway[k]) & 
                                    !(osm_data$cycleway[k] %in% c("no", "none", "no:lane", "n"))) |
                                   ("cycleway" %in% osm_data$highway))
    }
    else
    {
        # could not find a road, sett all to NA
        points_tmp$highway[i] <- NA
        points_tmp$lanes[i] <- NA 
        points_tmp$maxspeed[i] <- NA 
        points_tmp$lit[i] <- NA 
        points_tmp$oneway[i] <- NA 
        points_tmp$sidewalk[i] <- NA 
        points_tmp$cycleway[i] <- NA
    }
  }
  # save results in roads_all
  roads_all <- rbind(roads_all, points_tmp)
  print(paste("iteration", as.character(j), "complete"))
}

# correct some uncommon road categories
roads_all$highway[roads_all$highway %in% c("busway", "bridleway", "steps", "no", "proposed", "abandoned", 
                                           "bus_stop", "corridor", "elevator", "raceway", "rest_area")] <- "other" 
roads_all$highway[is.na(roads_all$highway)] <- "unknown" 

# correct some unusual lane values
roads_all$lanes[roads_all$lanes==70] <- 2
roads_all$lanes[roads_all$lanes==1.5] <- 2
roads_all$lanes[roads_all$lanes==0] <- 1

# set lanes if is.na
roads_all$lanes[is.na(roads_all$lanes) & 
                  (roads_all$highway == "motorway")] <- 4
roads_all$lanes[is.na(roads_all$lanes) & 
                  (roads_all$highway %in% c("trunk", "primary", "secondary", "tertiary"))] <- 2
roads_all$lanes[is.na(roads_all$lanes)] <- 1

# correct some unusual maxspeed values
roads_all$maxspeed[roads_all$maxspeed=="none"] <- 250
roads_all$maxspeed[roads_all$maxspeed=="signals"] <- 50
roads_all$maxspeed[roads_all$maxspeed=="DE:urban"] <- 50
roads_all$maxspeed[roads_all$maxspeed=="walk"] <- 7
roads_all$maxspeed[roads_all$maxspeed=="DE:rural"] <- 100
roads_all$maxspeed[roads_all$maxspeed=="70; 50"] <- 50
roads_all$maxspeed[roads_all$maxspeed=="50; 30"] <- 30
roads_all$maxspeed[roads_all$maxspeed=="30;(Mo-Fr 07:00-17:00),"] <- 30
roads_all$maxspeed[roads_all$maxspeed=="30;50"] <- 30
roads_all$maxspeed[roads_all$maxspeed=="60;80"] <- 60
roads_all$maxspeed[roads_all$maxspeed=="DE:zone:30"] <- 30
roads_all$maxspeed[roads_all$maxspeed=="de:rural"] <- 100
roads_all$maxspeed[roads_all$maxspeed=="frei"] <- 100
roads_all$maxspeed[roads_all$maxspeed=="maxspeed:foreward=100, maxspeed:backward=70"] <- 70
roads_all$maxspeed[roads_all$maxspeed=="maxspeed:forward=50 + maxspeed:backward=70"] <- 50
roads_all$maxspeed[roads_all$maxspeed=="maxspeed:forward=70"] <- 70
roads_all$maxspeed[roads_all$maxspeed=="DE:living_street"] <- 7

# set maxspeed if is.na
roads_all$maxspeed[is.na(roads_all$maxspeed) & 
                     (roads_all$highway %in% c("motorway", "trunk", "primary"))] <- 100
roads_all$maxspeed[is.na(roads_all$maxspeed) & 
                     (roads_all$highway == "secondary")] <- 70
roads_all$maxspeed[is.na(roads_all$maxspeed)] <- 50


# correct some unusual lit values
roads_all$lit[roads_all$lit %in% c("2", "no;yes", "disused")] <- NA
roads_all$lit[roads_all$lit == "no;limited"] <- "no"
roads_all$lit[roads_all$lit %in% c("24/7", "limited", "sunset-sunrise", "automatic",
                                   "interval", "dusk_dawn", "dusk-22:00", "twilight")] <- "yes"
roads_all$lit[is.na(roads_all$lit)] <- "unknown"

# correct some unusual oneway values
roads_all$oneway[roads_all$oneway %in% c("-1", "Neuer Mast")] <- NA
roads_all$oneway[roads_all$oneway %in% c("reversible", "alternating")] <- "yes"
roads_all$oneway[is.na(roads_all$oneway)] <- "unknown"

# correct some unusual sidewalk values
roads_all$sidewalk[is.na(roads_all$sidewalk)] <- "unknown"
roads_all$sidewalk[roads_all$sidewalk %in% c("e", "none")] <- "no"
roads_all$sidewalk[!(roads_all$sidewalk %in% c("yes", "no"))] <- "yes"

# correct some unusual cycleway values
roads_all$cycleway[is.na(roads_all$cycleway)] <- "no"
roads_all$cycleway[roads_all$cycleway == TRUE] <- "yes"
roads_all$cycleway[roads_all$cycleway == FALSE] <- "no"

# remove the coordinates (key is sufficient)
roads_all <- roads_all |> select(-WGSX, -WGSY)

# Split data int two files, so that files have less than 32MB
nrow <- nrow(roads_all)
half <- round(nrow/2)

first_half <- roads_all[1:half,]
half <- half+1
second_half <- roads_all[half:nrow,]

# write first half of data into file
fwrite(first_half, paste(folder, "road_all1.csv", sep = ""))

# write second half of  data into file
fwrite(second_half, paste(folder, "road_all2.csv", sep = ""))

rm(first_half, second_half)


