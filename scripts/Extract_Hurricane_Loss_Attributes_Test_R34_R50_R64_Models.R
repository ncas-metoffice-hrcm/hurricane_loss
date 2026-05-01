
## suppress warning messages ##
options( warn = -1 )


# Set the display format for numbers
options(scipen = 999)

setwd("~/Hurricane_Attributes_and_Loss_Relationships")


library(readxl)
library(tidyr)
library(purrr)
library(lubridate)
library(dplyr)
library(rworldmap)
library(writexl)
library(sp)
library(sf)
# library(rgdal)
library(rgeos)
library(maps)
library(stringr)
library(stringi)
library(geosphere)
library(lwgeom)
library(ggplot2)
library(readr)
library(caret)
# library(ranger)
library(raster)
library(randomForest)


Reconstruction_yes_or_no <- "No"   # Yes No
number_of_timesteps_post_landfall_included <- 40 # 6 12 40

if (number_of_timesteps_post_landfall_included == 6) {
  post_landfall <- "12hr"
}
if (number_of_timesteps_post_landfall_included == 12) {
  post_landfall <- "24hr"
}
if (number_of_timesteps_post_landfall_included == 40) {
  post_landfall <- "Fullhr"
}


print(Reconstruction_yes_or_no)
print(number_of_timesteps_post_landfall_included)


#### function to determining landfalling country ####
## used to determine U.S. landfalls ##
determine_landfall_country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  ## converting points to a SpatialPoints object ##
  ## setting CRS directly to that from rworldmap ##
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP))) 
  
  ## use 'over' to get indices of the Polygons object containing each point ## 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   ## returns the continent (6 continent model)
  #indices$REGION   ## returns the continent (7 continent model)
  #indices$ADMIN  ## returns country name
  test <- indices$ISO3 ## returns the ISO3 code 
}

determine_landfall_country_bypassing = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  ## converting points to a SpatialPoints object ##
  ## setting CRS directly to that from rworldmap ##
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP))) 
  
  ### --- Inlcudes bypassing storms --- ###
  ### --- Units need to be in degrees --- ###
  # Create a buffer around the point
  buffer <- gBuffer(pointsSP, width = 1.0, byid = TRUE)
  
  ## use 'over' to get indices of the Polygons object containing each point ## 
  indices = over(buffer, countriesSP)
  
  #indices$continent   ## returns the continent (6 continent model)
  #indices$REGION   ## returns the continent (7 continent model)
  #indices$ADMIN  ## returns country name
  test <- indices$ISO3 ## returns the ISO3 code 
}




# Function to sum raster values within buffers of specified radii
compute_raster_values_within_radius <- function(lon, lat, radius, raster_layer) {
  
  # Create a SpatialPoints object for the coordinate
  point <- SpatialPoints(cbind(lon, lat), proj4string = crs(raster_layer))
  
  # Create a buffer around the point
  buffer <- gBuffer(point, width = radius, byid = TRUE)
  
  # Mask and crop raster to the extent of the buffer
  cropped_raster <- crop(raster_layer, buffer)
  #masked_raster <- mask(cropped_raster, buffer)
  
  # Sum the raster values within the buffer
  cellStats(cropped_raster, stat = 'sum', na.rm = TRUE)
  
}




#### function to determining landfalling state ####
## used to determine U.S. landfall State ##
determine_landfall_state <- function(pointsDF,
                                     states = spData::us_states,
                                     name_col = "NAME") 
{
  ## Convert points data.frame to an sf POINTS object ##
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system ##
  ## (e.g. Web Mercator) as required for geometric operations ##
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point ##
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}



#### ---- 1) Download and read ibtracs ---- ####

basin <- "NA"

url <- paste("https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.", basin, ".list.v04r01.csv", sep = "")
destination <- paste("HURRICANE_DATA/ibtracs.", basin, ".list.v04r01.csv", sep = "")
# download.file(url, destination, mode="wb")
ibtracs_all <- read.csv(destination)
## subset by column ##
ibtracs_all <- ibtracs_all[ , c("USA_ATCF_ID", "NAME", "LAT", "LON", "ISO_TIME", "DIST2LAND", "USA_WIND", "USA_PRES", "USA_R34_NE", "USA_R50_NE", "USA_R64_NE", "USA_RMW", "STORM_SPEED")] 
## delete first row with units ##
ibtracs_all <- ibtracs_all[-c(1),]


print("Read in data")


## convert number columns to numeric columns ##
ibtracs_all$USA_ATCF_ID <- as.character(ibtracs_all$USA_ATCF_ID)
ibtracs_all$NAME        <- as.character(ibtracs_all$NAME)

## ISO_TIME changes format from 1900 ##
ibtracs_all <- ibtracs_all %>%
  dplyr::mutate(ISO_TIME = coalesce(
    ymd_hms(ISO_TIME, quiet = TRUE),  # Parsing "YYYY-MM-DD HH:MM:SS"
    dmy_hm(ISO_TIME, quiet = TRUE)    # Parsing "DD/MM/YYYY HH:MM"
  ))
ibtracs_all$ISO_TIME    <- as.character(ibtracs_all$ISO_TIME)
ibtracs_all$Year        <- as.numeric(as.character(strtoi(substr(ibtracs_all$ISO_TIME, 1, 4))))
ibtracs_all$Year_Name   <- paste(ibtracs_all$Year, "_", ibtracs_all$NAME, sep = "")


ibtracs_all$LON         <- as.numeric(as.character(ibtracs_all$LON))
ibtracs_all$LAT         <- as.numeric(as.character(ibtracs_all$LAT))
ibtracs_all$DIST2LAND   <- as.numeric(as.character(ibtracs_all$DIST2LAND))
ibtracs_all$USA_WIND    <- as.numeric(as.character(ibtracs_all$USA_WIND))
ibtracs_all$USA_PRES    <- as.numeric(as.character(ibtracs_all$USA_PRES))
ibtracs_all$USA_R34_NE  <- as.numeric(as.character(ibtracs_all$USA_R34_NE))
ibtracs_all$USA_R50_NE  <- as.numeric(as.character(ibtracs_all$USA_R50_NE))
ibtracs_all$USA_R64_NE  <- as.numeric(as.character(ibtracs_all$USA_R64_NE))
ibtracs_all$USA_RMW     <- as.numeric(as.character(ibtracs_all$USA_RMW))
ibtracs_all$STORM_SPEED <- as.numeric(as.character(ibtracs_all$STORM_SPEED))


#### ---- 2) Filter ibtracs  ---- ####

## filter by year ##
# ibtracs_all <- ibtracs_all[ibtracs_all$Year >= 2020, ]
# ibtracs_all <- subset(ibtracs_all, Year == 1970 | Year == 2021)


# ibtracs_all <- ibtracs_all[ibtracs_all$USA_ATCF_ID == "AL162005", ]

## Filter out "NOT_NAMED" ##
ibtracs_all <- ibtracs_all[ibtracs_all$NAME != "NOT_NAMED", ]



#### ---- Filter ibtracs to 3-hourly per storm ---- ####

# Convert ISO_TIME to POSIXct if it isn't already
ibtracs_all <- ibtracs_all %>%
  dplyr::mutate(ISO_TIME = as.POSIXct(ISO_TIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# Filter data to every 3 hours ##
ibtracs_3hourly <- ibtracs_all %>% 
  filter(as.numeric(format(ISO_TIME, "%H")) %% 3 == 0)

# Floor time to every 3 hours 
ibtracs_3hourly$ISO_TIME <- floor_date(ibtracs_3hourly$ISO_TIME, "3 hours")


# Create the points data frame
points <- data.frame(lon = ibtracs_3hourly$LON, lat = ibtracs_3hourly$LAT)

# Pre-calculate landfall country and state for all points
ibtracs_3hourly$LANDFALL_COUNTRY <- as.character(determine_landfall_country(points))
ibtracs_3hourly$LANDFALL_STATE   <- as.character(determine_landfall_state(points))
ibtracs_3hourly$BYPASS_COUNTRY   <- as.character(determine_landfall_country_bypassing(points))
remove(points)




#### ---- 3) Get landfall information per storm ---- ####



### -- Determine Maximum landfall / bypassing points from ibtracs -- ### 
# Pre-calculate unique storm IDs and prepare the result data frame
unique_storm_ids <- unique(ibtracs_3hourly$USA_ATCF_ID)
ibtracs_max_landfall_info <- data.frame()

landfall_number <- "multiple" 

ibtracs_3hourly_with_landfall_marker <- c()

# Iterate through each unique storm ID
for (storm_id in unique_storm_ids) {
  
  # # ## Katrina ##
  # storm_id <- "AL122005"
  # 
  # ## Milton ##
  # storm_id <- "AL142024"
  
  # Subset the data for the current storm
  one_storm <- ibtracs_3hourly %>% filter(USA_ATCF_ID == storm_id)
  
  # Initialize Landfall_Marker column
  one_storm$Landfall_Marker <- NA
  
  ## if there are no landfall points, don't bother running next code ##
  if ("USA" %in% unlist(one_storm$LANDFALL_COUNTRY) == TRUE) {
    
    if (landfall_number == "multiple") {
      
      ### -- Determine number of multiple Landfalls -- ###
      values <- is.na(one_storm$LANDFALL_STATE)
      
      # Find the indices of FALSE values
      false_indices <- which(!values)
      
      # Find the landfall state indexes separated by at least 4
      first_landfall_index_of_each_landfall <- false_indices[c(TRUE, diff(false_indices) >= 4)]
      
      # which is the max landfall #
      landfall_order <- data.frame(USA_WIND = one_storm$USA_WIND[first_landfall_index_of_each_landfall],
                                   Index = first_landfall_index_of_each_landfall)
      landfall_order <- landfall_order[order(-landfall_order$USA_WIND), ]
      landfall_order$Rank <- seq(1, nrow(landfall_order))
      
      if (nrow(landfall_order) == 1) {
        landfall_order$Rank[1] <- "1st Max Landfall"
      }
      if (nrow(landfall_order) == 2) {
        landfall_order$Rank[1] <- "1st Max Landfall"
        landfall_order$Rank[2] <- "2nd Max Landfall"
      }
      if (nrow(landfall_order) == 3) {
        landfall_order$Rank[1] <- "1st Max Landfall"
        landfall_order$Rank[2] <- "2nd Max Landfall"        
        landfall_order$Rank[3] <- "3rd Max Landfall"
      }
      if (nrow(landfall_order) == 4) {
        landfall_order$Rank[1] <- "1st Max Landfall"
        landfall_order$Rank[2] <- "2nd Max Landfall"        
        landfall_order$Rank[3] <- "3rd Max Landfall"
        landfall_order$Rank[4] <- "4th Max Landfall"      
      }
      
      ## Get one timestep before landfall point and append this 
      one_storm$LANDFALL_STATE[first_landfall_index_of_each_landfall-1] <- one_storm$LANDFALL_STATE[first_landfall_index_of_each_landfall]
      one_storm$LANDFALL_COUNTRY[first_landfall_index_of_each_landfall-1] <- one_storm$LANDFALL_COUNTRY[first_landfall_index_of_each_landfall]
      one_storm$Landfall_Marker[landfall_order$Index-1] <- paste("Pre - ", landfall_order$Rank, sep = "")
      
      for (multiple_landfall_point in first_landfall_index_of_each_landfall) {
        time_lag <- 0
        while (multiple_landfall_point + time_lag <= nrow(one_storm) & !is.na(one_storm$LANDFALL_STATE[multiple_landfall_point + time_lag])) {
          one_storm$Landfall_Marker[multiple_landfall_point + time_lag] <- paste0("LANDFALL +", 3*time_lag, "hours")
          time_lag <- time_lag + 1
        }
      }

      ## filter to landfall points ##
      one_storm_landfall <- one_storm[grepl("Pre", one_storm$Landfall_Marker), ]
      
      ## Append to landfall information dataframe
      ibtracs_max_landfall_info <- bind_rows(ibtracs_max_landfall_info, one_storm_landfall)
      
      ibtracs_3hourly_with_landfall_marker <- rbind(ibtracs_3hourly_with_landfall_marker, one_storm)
    }
    
    if (landfall_number == "single") {
      
      ## Obtain USA landfalling points ##
      USA_landfall <- one_storm %>% filter(LANDFALL_COUNTRY == "USA") %>% drop_na(USA_WIND)
      
      # Determine max landfall point
      max_landfall_index <- which(USA_landfall$USA_WIND == max(USA_landfall$USA_WIND))
      
      # Determine if USA landfall point is greater than tropical storm intensity
      # USA_landfall <- USA_landfall[USA_landfall$USA_WIND > 34.0, ]
      
      if (nrow(USA_landfall) > 0) {
        
        ## if several indexes have same wind, take the first one ##
        if (length(max_landfall_index)) {
          max_landfall_index <- max_landfall_index[1]
        }
        
        # Mark the maximum landfall point
        max_landfall_index_in_master <- which(one_storm$ISO_TIME == USA_landfall$ISO_TIME[max_landfall_index])
        one_storm$Landfall_Marker[max_landfall_index_in_master - 1] <- "Max. LANDFALL"
        
        ## append landfall point to master landfall attribute data frame ##
        ibtracs_max_landfall_info <- bind_rows(ibtracs_max_landfall_info, one_storm[max_landfall_index_in_master-1, ])
        
        ibtracs_max_landfall_info_index <- which(ibtracs_max_landfall_info$USA_ATCF_ID == storm_id)
        
        ## Append Landfall Country to all
        ibtracs_max_landfall_info$LANDFALL_COUNTRY[ibtracs_max_landfall_info_index] <- "USA"
        
        ## Append Landfall Country to all
        ibtracs_max_landfall_info$LANDFALL_STATE[ibtracs_max_landfall_info_index] <- USA_landfall$LANDFALL_STATE[1]
      
        ibtracs_3hourly_with_landfall_marker <- rbind(ibtracs_3hourly_with_landfall_marker, one_storm)
        
      }
    }
  }  
  
  ## if there are no landfall points, do following code running next code ##
  if ("USA" %in% unlist(one_storm$LANDFALL_COUNTRY) != TRUE) {
    
    if ("USA" %in% unlist(one_storm$BYPASS_COUNTRY) == TRUE) {
      
      ## Obtain USA landfalling points ##
      USA_Bypass <- one_storm %>% filter(BYPASS_COUNTRY == "USA") %>% drop_na(USA_WIND)
      
      # Determine max landfall point
      max_landfall_index <- which(USA_Bypass$USA_WIND == max(USA_Bypass$USA_WIND))
      
      # Determine closest bypassing point
      closest_bypass_index <- which(USA_Bypass$DIST2LAND == min(USA_Bypass$DIST2LAND))
      
      ## if several indexes have same wind, take the first one ##
      if (length(closest_bypass_index)) {
        closest_bypass_index <- closest_bypass_index[1]
      }
      
      # # Mark the maximum bypasser point
      # one_storm$Landfall_Marker[max_landfall_index - 1] <- "Max. Bypasser"
      
      # Mark the maximum bypasser point
      closest_bypass_index_in_master <- which(one_storm$ISO_TIME == USA_Bypass$ISO_TIME[closest_bypass_index])
      one_storm$Landfall_Marker[closest_bypass_index_in_master] <- "Closest Bypasser"
      
      ## append landfall point to master landfall attribute data frame ##
      ibtracs_max_landfall_info <- bind_rows(ibtracs_max_landfall_info, one_storm[closest_bypass_index_in_master, ])
      
      ibtracs_3hourly_with_landfall_marker <- rbind(ibtracs_3hourly_with_landfall_marker, one_storm)
      
    }
  }
}

ibtracs_3hourly <- ibtracs_3hourly_with_landfall_marker
ibtracs_max_landfall_info <- subset(ibtracs_max_landfall_info, select = -c(DIST2LAND))
length(unique(ibtracs_max_landfall_info$USA_ATCF_ID))





### Filter to specified time after landfall ###

if (number_of_timesteps_post_landfall_included == 6) {
  
  ibtracs_3hourly <- ibtracs_3hourly %>%
    filter(Landfall_Marker %in% c("Pre - 1st Max Landfall", "Pre - 2nd Max Landfall", "Pre - 3rd Max Landfall",
                                  "Closest Bypasser",
                                  "LANDFALL +0hours", "LANDFALL +3hours",
                                  "LANDFALL +6hours", "LANDFALL +9hours", "LANDFALL +12hours"))
}

if (number_of_timesteps_post_landfall_included == 12) {
  
  ibtracs_3hourly <- ibtracs_3hourly %>%
    filter(Landfall_Marker %in% c("Pre - 1st Max Landfall", "Pre - 2nd Max Landfall", "Pre - 3rd Max Landfall",
                                  "Closest Bypasser",
                                  "LANDFALL +0hours", "LANDFALL +3hours",
                                  "LANDFALL +6hours", "LANDFALL +9hours", "LANDFALL +12hours",
                                  "LANDFALL +15hours", "LANDFALL +18hours",
                                  "LANDFALL +21hours", "LANDFALL +24hours"))
}





####### ------ Extend ibtracs R34 using HURDAT2 and R34 Random Forest Model ------ #######




# ibtracs_3hourly <- tail(ibtracs_3hourly, 100)







##### ---- Append HURDAT2 per storm ---- #####


##### ---- Append HURDAT2 per storm ---- #####


HURDAT_data <- read_xlsx("HURRICANE_DATA/HURDAT2_Reanalysis.xlsx")

HURDAT_data$Latitude <- gsub("N", "", HURDAT_data$Latitude)
HURDAT_data$Longitude <- gsub("W", "", HURDAT_data$Longitude)
HURDAT_data$Latitude <- as.numeric(HURDAT_data$Latitude)
HURDAT_data$Longitude <- as.numeric(HURDAT_data$Longitude)
HURDAT_data$Longitude <- HURDAT_data$Longitude * -1

HURDAT_data$Storm_Names <- gsub("\"", "", HURDAT_data$Storm_Names)
HURDAT_data$Storm_Names <- gsub(" ", "_", HURDAT_data$Storm_Names)
HURDAT_data$Storm_Names <- ifelse(HURDAT_data$Storm_Names == "---------------", "NO_NAME", HURDAT_data$Storm_Names)
HURDAT_data$RMW_nm      <- as.numeric(HURDAT_data$RMW_nm)
HURDAT_data$Central_Pressure_mb <- abs(as.numeric(HURDAT_data$Central_Pressure_mb))

HURDAT_data$Storm_Names <- as.character(HURDAT_data$Storm_Names)
HURDAT_data$Year_Name   <- paste(HURDAT_data$Year, "_", toupper(HURDAT_data$Storm_Names), sep = "")

## There are multiple points in HURDAT if the storm made mutiple landfalls ##
ibtracs_max_landfall_info$HURDAT_RMW <- NA
ibtracs_3hourly$HURDAT_RMW <- NA

Storm_Name_Index <- which(ibtracs_max_landfall_info$Year_Name == "2005_KATRINA")

## remove NA RMW ##
HURDAT_data <- HURDAT_data %>% drop_na(RMW_nm)


## Append HURDAT Landfall RMW to ibtracs_max_landfall_info ##

ibtracs_max_landfall_info$Year_Name_Landfall_Marker <- paste(ibtracs_max_landfall_info$Year_Name, ibtracs_max_landfall_info$Landfall_Marker, sep = "_")

for (Storm_Name_Index in seq(1, length(ibtracs_max_landfall_info$Year_Name), 1)) {
  
  HURDAT_per_storm <- HURDAT_data[HURDAT_data$Year_Name == ibtracs_max_landfall_info$Year_Name[Storm_Name_Index], ]
  ibtracs_max_landfall_info_per_storm <- ibtracs_max_landfall_info[ibtracs_max_landfall_info$Year_Name == ibtracs_max_landfall_info$Year_Name[Storm_Name_Index], ]
  
  if (nrow(HURDAT_per_storm) == 1) {
    
    common_matches <- intersect(ibtracs_max_landfall_info$Year_Name_Landfall_Marker, paste(HURDAT_per_storm$Year_Name, "Pre - 1st Max Landfall", sep = "_"))
    
    if (length(common_matches) > 0) {
      
      ibtracs_match_index  <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Pre - 1st Max Landfall", sep = "_"))
      
      ibtracs_max_landfall_info$HURDAT_RMW[ibtracs_match_index] <- HURDAT_per_storm$RMW_nm[1]
      
      if (is.na(ibtracs_max_landfall_info$USA_PRES[ibtracs_match_index]) == TRUE) {
        ibtracs_max_landfall_info$USA_PRES[ibtracs_match_index] <- HURDAT_per_storm$Central_Pressure_mb[1]
      }
    }
  }
  
  if (nrow(HURDAT_per_storm) > 1 && nrow(ibtracs_max_landfall_info_per_storm) == 1) {
    
    # Example target coordinate
    target_lon <- ibtracs_max_landfall_info$LON
    target_lat <- ibtracs_max_landfall_info$LAT
    
    # Find closest index based on distance
    distances <- sqrt((HURDAT_per_storm$Longitude - target_lon)^2 +
                        (HURDAT_per_storm$Latitude - target_lat)^2)
    
    # Find the index of the minimum distance
    closest_index <- which.min(distances)
    
    ibtracs_match_index  <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Pre - 1st Max Landfall", sep = "_"))
    
    if (length(ibtracs_match_index) == 0) {
      ibtracs_match_index  <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Closest Bypasser", sep = "_"))
    }
    if (length(ibtracs_match_index) == 0) {
      ibtracs_match_index  <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Pre - 2nd Max Landfall", sep = "_"))
    }
    if (length(ibtracs_match_index) == 0) {
      ibtracs_match_index  <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Pre - 3rd Max Landfall", sep = "_"))
    }
    
    ibtracs_max_landfall_info$HURDAT_RMW[ibtracs_match_index] <- HURDAT_per_storm$RMW_nm[closest_index]
    
    if (is.na(ibtracs_max_landfall_info$USA_RMW[ibtracs_match_index]) == TRUE) {
      ibtracs_max_landfall_info$USA_RMW[ibtracs_match_index] <- HURDAT_per_storm$RMW_nm[closest_index]
    }
    
    if (is.na(ibtracs_max_landfall_info$USA_PRES[ibtracs_match_index]) == TRUE) {
      ibtracs_max_landfall_info$USA_PRES[ibtracs_match_index] <- HURDAT_per_storm$Central_Pressure_mb[closest_index]
    }
  }
}



## Append HURDAT Landfall RMW to ibtracs_3hourly ##

ibtracs_3hourly$Year_Name_Landfall_Marker <- paste(ibtracs_3hourly$Year_Name, ibtracs_3hourly$Landfall_Marker, sep = "_")

for (Storm_Name_Index in seq(1, length(unique(ibtracs_3hourly$Year_Name)), 1)) {
  
  HURDAT_per_storm <- HURDAT_data[HURDAT_data$Year_Name == unique(ibtracs_3hourly$Year_Name)[Storm_Name_Index], ]
  HURDAT_per_storm <- HURDAT_per_storm %>% drop_na(Longitude)
  ibtracs_3hourly_per_storm <- ibtracs_3hourly[ibtracs_3hourly$Year_Name == unique(ibtracs_3hourly$Year_Name)[Storm_Name_Index], ]
  
  if (nrow(HURDAT_per_storm) == 1) {
    
    common_matches <- intersect(ibtracs_3hourly$Year_Name_Landfall_Marker, paste(HURDAT_per_storm$Year_Name, "Pre - 1st Max Landfall", sep = "_"))
    
    if (length(common_matches) > 0) {
      
      ibtracs_match_index  <- which(ibtracs_3hourly$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Pre - 1st Max Landfall", sep = "_"))
      
      ibtracs_3hourly$HURDAT_RMW[ibtracs_match_index] <- HURDAT_per_storm$RMW_nm[1]
      
      if (is.na(ibtracs_3hourly$USA_PRES[ibtracs_match_index]) == TRUE) {
        ibtracs_3hourly$USA_PRES[ibtracs_match_index] <- HURDAT_per_storm$Central_Pressure_mb[1]
      }
    }
  }
  
  if (nrow(HURDAT_per_storm) > 1) {
    
    ibtracs_landfall <-  ibtracs_3hourly_per_storm[ibtracs_3hourly_per_storm$Landfall_Marker == "Pre - 1st Max Landfall", ]
    ibtracs_landfall <-  ibtracs_landfall %>% drop_na(LON)
    
    if (nrow(ibtracs_landfall) > 0) {
      
      # Example target coordinate
      target_lon <- ibtracs_landfall$LON
      target_lat <- ibtracs_landfall$LAT
      
      # Find closest index based on distance
      distances <- sqrt((HURDAT_per_storm$Longitude - target_lon)^2 +
                          (HURDAT_per_storm$Latitude - target_lat)^2)
      
      # Find the index of the minimum distance
      closest_index <- which.min(distances)
      
      ibtracs_match_index  <- which(ibtracs_3hourly$Year_Name_Landfall_Marker == paste(HURDAT_per_storm$Year_Name, "Pre - 1st Max Landfall", sep = "_"))
      
      ibtracs_3hourly$HURDAT_RMW[ibtracs_match_index] <- HURDAT_per_storm$RMW_nm[closest_index]
      
      if (is.na(ibtracs_3hourly$USA_PRES[ibtracs_match_index]) == TRUE && is.na(HURDAT_per_storm$Central_Pressure_mb[closest_index]) == FALSE) {
        ibtracs_3hourly$USA_PRES[ibtracs_match_index] <- HURDAT_per_storm$Central_Pressure_mb[closest_index]
      }
      
      if (is.na(ibtracs_3hourly$USA_RMW[ibtracs_match_index]) == TRUE) {
        ibtracs_3hourly$USA_RMW[ibtracs_match_index] <- HURDAT_per_storm$RMW_nm[closest_index]
      }
    }
  }
}

test <- ibtracs_3hourly[ibtracs_3hourly$Year_Name == "1998_GEORGES", ]



#### Add Gori et al. RMW to USA_RMW ####
Gori_et_al_data <- read_xlsx("HURRICANE_DATA/timesteps_per_storm_in_Gori_RMW_Pmin.xlsx")

## Convert time to consistent format ##
Gori_et_al_data$DATE <- as.Date(substr(Gori_et_al_data$Timestep, 1, 8), format = "%Y%m%d")
Gori_et_al_data$TIME <- paste(substr(Gori_et_al_data$Timestep, 9, 10), ":00:00", sep = "")
Gori_et_al_data$DATETIME <- paste(Gori_et_al_data$DATE, " ", Gori_et_al_data$TIME, sep = "")
Gori_et_al_data$DATETIME <- as.POSIXct(Gori_et_al_data$DATETIME, tz = "UTC")

## Convert from km to nm ##
Gori_et_al_data$RMW_100 <- Gori_et_al_data$RMW_100 * 0.53996

# Filter data to every 3 hours ##
Gori_et_al_data <- Gori_et_al_data %>%
  filter(as.numeric(format(DATETIME, "%H")) %% 3 == 0)

## Append Gori et al. RMW to ibtracs_3hourly ##
ibtracs_3hourly$Gori_RMW <- rep(NA, nrow(ibtracs_3hourly))


for (Storm_Name_Index in seq(1, nrow(ibtracs_3hourly), 1)) {
  
  # Gori_et_al_data$Storm_ID[match(ibtracs_max_landfall_info$USA_ATCF_ID, Gori_et_al_data$Storm_ID)]
  
  if (is.na(match(ibtracs_3hourly$USA_ATCF_ID[Storm_Name_Index], Gori_et_al_data$Storm_ID)) == FALSE) {
    
    Gori_et_al_data_per_storm <- Gori_et_al_data[Gori_et_al_data$Storm_ID == ibtracs_3hourly$USA_ATCF_ID[Storm_Name_Index], ]
    
    # Format without timezone
    Gori_et_al_data_per_storm$DATETIME <- paste(Gori_et_al_data_per_storm$DATE, Gori_et_al_data_per_storm$TIME, sep = " ")
    
    if (nrow(Gori_et_al_data_per_storm) >= 1) {
      
      # Ensure both are character
      dt_ibtracs <- as.character(ibtracs_3hourly$ISO_TIME[Storm_Name_Index])
      dt_gori    <- as.character(Gori_et_al_data_per_storm$DATETIME)
      
      common_matches <- intersect(dt_ibtracs, dt_gori)
      
      if (length(common_matches) > 0) {
        # print(ibtracs_max_landfall_info$USA_ATCF_ID[Storm_Name_Index])
        ibtracs_match_index  <- which(dt_gori == dt_ibtracs)
        ibtracs_3hourly$Gori_RMW[ibtracs_match_index] <- Gori_et_al_data_per_storm$RMW_100[ibtracs_match_index]
      }
    }
  }
}


## Replace USA_RMW if missing with Gori et al. ##
for (index in seq(1, nrow(ibtracs_3hourly), 1)) {
  if (is.na(ibtracs_3hourly$USA_RMW[index]) == TRUE && is.na(ibtracs_3hourly$Gori_RMW[index]) == FALSE) {
    ibtracs_3hourly$USA_RMW[index] <- ibtracs_3hourly$Gori_RMW[index]
  }
}


## Append Gori et al. Landfall RMW to ibtracs_max_landfall_info ##

ibtracs_max_landfall_info$Gori_RMW <- NA

for (Storm_Name_Index in seq(1, length(ibtracs_max_landfall_info$Year_Name), 1)) {
  
  # Gori_et_al_data$Storm_ID[match(ibtracs_max_landfall_info$USA_ATCF_ID, Gori_et_al_data$Storm_ID)]
  
  if (is.na(match(ibtracs_max_landfall_info$USA_ATCF_ID[Storm_Name_Index], Gori_et_al_data$Storm_ID)) == FALSE) {
    
    Gori_et_al_data_per_storm <- Gori_et_al_data[Gori_et_al_data$Storm_ID == ibtracs_max_landfall_info$USA_ATCF_ID[Storm_Name_Index], ]
    
    # Format without timezone
    Gori_et_al_data_per_storm$DATETIME <- paste(Gori_et_al_data_per_storm$DATE, Gori_et_al_data_per_storm$TIME, sep = " ")
    
    if (nrow(Gori_et_al_data_per_storm) >= 1) {
      
      # Ensure both are character
      dt_ibtracs <- as.character(ibtracs_max_landfall_info$ISO_TIME[Storm_Name_Index])
      dt_gori    <- as.character(Gori_et_al_data_per_storm$DATETIME)
      
      common_matches <- intersect(dt_ibtracs, dt_gori)
      
      if (length(common_matches) > 0) {
        # print(ibtracs_max_landfall_info$USA_ATCF_ID[Storm_Name_Index])
        ibtracs_match_index  <- which(dt_gori == dt_ibtracs)
        ibtracs_max_landfall_info$Gori_RMW[Storm_Name_Index] <- Gori_et_al_data_per_storm$RMW_100[ibtracs_match_index]
      }
    }
  }
}


## If USA_RMW is missing, replace it with (first choice) HURDAT2 RMW or (second choice) Gori et al. RMW ##

for (index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
  if (is.na(ibtracs_max_landfall_info$USA_RMW[index]) == TRUE && is.na(ibtracs_max_landfall_info$HURDAT_RMW[index]) == FALSE) {
    ibtracs_max_landfall_info$USA_RMW[index] <- ibtracs_max_landfall_info$HURDAT_RMW[index]
  }
  else if (is.na(ibtracs_max_landfall_info$USA_RMW[index]) == TRUE && is.na(ibtracs_max_landfall_info$Gori_RMW[index]) == FALSE) {
    ibtracs_max_landfall_info$USA_RMW[index] <- ibtracs_max_landfall_info$Gori_RMW[index]
  }
}





#### Replace USA_RMW with previous RMW #####

## for each storm in ibtracs_3hourly append previous R34 to next timestep ##

storm_ids <- unique(ibtracs_3hourly$USA_ATCF_ID)

ibtracs_3hourly_new_RMW <- c()

replacement <- 0

for (storm_index in seq(1, length(storm_ids), 1)) {
  
  per_storm <- ibtracs_3hourly[ibtracs_3hourly$USA_ATCF_ID == storm_ids[storm_index], ]
  
  which(!is.na(per_storm$USA_RMW))
  
  if (nrow(per_storm) > 1) {
    
    for (row_index in seq(2, nrow(per_storm), 1)) {
      
      if (is.na(per_storm$USA_RMW[row_index]) == TRUE) {
        if (is.na(per_storm$USA_RMW[row_index-1]) == FALSE) {
          per_storm$USA_RMW[row_index] <- per_storm$USA_RMW[row_index-1]
          replacement <- replacement + 1
        }
      }
    }
  }
  
  ibtracs_3hourly_new_RMW <- rbind(ibtracs_3hourly_new_RMW, per_storm)
}

ibtracs_3hourly <- ibtracs_3hourly_new_RMW




#### Random Forest Model to reconstruct R34 ####

print("Calculating Models")

## -- Verifying Model -- ##
# Subset model training data
ibtracs_3hourly_new_R34 <- ibtracs_3hourly ## when not replacing with previous R34

R34_Model_Test_Data <- subset(ibtracs_3hourly_new_R34, select = c("USA_R34_NE", "USA_WIND", "USA_PRES", "LAT",
                                                                  "USA_RMW"))
R50_Model_Test_Data <- subset(ibtracs_3hourly_new_R34, select = c("USA_R50_NE", "USA_WIND", "USA_PRES", "LAT",
                                                                  "USA_RMW"))
R64_Model_Test_Data <- subset(ibtracs_3hourly_new_R34, select = c("USA_R64_NE", "USA_WIND", "USA_PRES", "LAT",
                                                                  "USA_RMW"))

## Modelled Reconstructed R34 using HURDAT2 RMW ##
R34_Model_Test_Data <- R34_Model_Test_Data %>% drop_na(USA_R34_NE)
R34_Model_Test_Data <- R34_Model_Test_Data %>% drop_na(USA_WIND)
R34_Model_Test_Data <- R34_Model_Test_Data %>% drop_na(USA_PRES)
R34_Model_Test_Data <- R34_Model_Test_Data %>% drop_na(USA_RMW)

R34_Model_Test_Data <- R34_Model_Test_Data[R34_Model_Test_Data$USA_R34_NE > 10, ]


R50_Model_Test_Data <- R50_Model_Test_Data %>% drop_na(USA_R50_NE)
R50_Model_Test_Data <- R50_Model_Test_Data %>% drop_na(USA_WIND)
R50_Model_Test_Data <- R50_Model_Test_Data %>% drop_na(USA_PRES)
R50_Model_Test_Data <- R50_Model_Test_Data %>% drop_na(USA_RMW)

R50_Model_Test_Data <- R50_Model_Test_Data[R50_Model_Test_Data$USA_R50_NE > 5, ]


R64_Model_Test_Data <- R64_Model_Test_Data %>% drop_na(USA_R64_NE)
R64_Model_Test_Data <- R64_Model_Test_Data %>% drop_na(USA_WIND)
R64_Model_Test_Data <- R64_Model_Test_Data %>% drop_na(USA_PRES)
R64_Model_Test_Data <- R64_Model_Test_Data %>% drop_na(USA_RMW)

R64_Model_Test_Data <- R64_Model_Test_Data[R64_Model_Test_Data$USA_R64_NE > 5, ]


results_R34 <- data.frame(Observed = R34_Model_Test_Data$USA_R34_NE,
                          Predicted = rep(NA, nrow(R34_Model_Test_Data)))
results_R50 <- data.frame(Observed = R50_Model_Test_Data$USA_R50_NE,
                          Predicted = rep(NA, nrow(R50_Model_Test_Data)))
results_R64 <- data.frame(Observed = R64_Model_Test_Data$USA_R64_NE,
                          Predicted = rep(NA, nrow(R64_Model_Test_Data)))

for (i in seq(1, nrow(R34_Model_Test_Data), 1)) {  # nrow(R34_Model_Test_Data)
  # print(i)
  train_data <- R34_Model_Test_Data[-i, ]
  test_data  <- R34_Model_Test_Data[i, ]
  model <- ranger(USA_R34_NE ~ LAT + USA_WIND + USA_PRES + USA_RMW,
             data = train_data)

  results_R34$Predicted[i] <- predict(model, test_data)
}

print("Done R34")

# for (i in seq(1, nrow(R50_Model_Test_Data), 1)) {  # nrow(R34_Model_Test_Data)
#   # print(i)
#   train_data <- R50_Model_Test_Data[-i, ]
#   test_data  <- R50_Model_Test_Data[i, ]
#   model <- ranger(USA_R50_NE ~ LAT + USA_WIND + USA_PRES + USA_RMW,
#                   data = train_data)
# 
#   results_R50$Predicted[i] <- predict(model, test_data)
# }
# 
# print("Done R50")
# 
# for (i in seq(1, nrow(R64_Model_Test_Data), 1)) {  # nrow(R34_Model_Test_Data)
#   # print(i)
#   train_data <- R64_Model_Test_Data[-i, ]
#   test_data  <- R64_Model_Test_Data[i, ]
#   model <- ranger(USA_R64_NE ~ LAT + USA_WIND + USA_PRES + USA_RMW,
#                   data = train_data)
#   results_R64$Predicted[i] <- predict(model, test_data)
# }
# 
# print("Done R64")




results_R34 <- results_R34 %>% unnest(Predicted)
results_R50 <- results_R50 %>% unnest(Predicted)
results_R64 <- results_R64 %>% unnest(Predicted)



# results_R34 <- data.frame(Predicted = runif(100, min = 10, max = 300),
#                           Observed = runif(100, min = 10, max = 300))
# R34_Model_Test_Data <- data.frame(Predicted = runif(100, min = 10, max = 300),
#                                   Observed = runif(100, min = 10, max = 300))



## R34 ##
MAE_result <- mean(abs(results_R34$Observed - results_R34$Predicted))
MAE <- sprintf("%.1f", MAE_result)
correlation_result <- cor(results_R34$Observed, results_R34$Predicted)
correlation <- sprintf("%.2f", correlation_result)

print(max(results_R34, na.rm=T))

# Create a scatter plot with a line of best fit
ggplot(data = results_R34, aes(x = Predicted, y = Observed)) +

  geom_label(
    label=paste("Correlation: ", correlation, "\n", "MAE: ", MAE,
                "\nN Points: ", nrow(R34_Model_Test_Data), sep = ""),
    x = 465, y = 75,
    size = 4,  # Adjust the text size here
    # label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.5,
    color = "black",
    fill=alpha("white", 0.5)
  ) +
  
  geom_point(color = "red3", size=0.75, alpha = 0.15) + 
  # geom_point(shape = 1,size = 1,colour = "black", alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid", linewidth = 0.25) +
  # geom_density_2d(color = "black", size = 0.5, linetype = "dashed", bins=4) +  # Add this line for the density plot
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", linewidth = 1.1) +

  # labs(title = "Random Forest Model to Predict R34: \nGiven Vmax, Cp and Latitude",
  labs(x = "R34 Model Prediction (nm)",    # title = "Random Forest Model to Predict R34: \nGiven Vmax, RMW, Cp and Latitude",
       y = "R34 \nObserved \n(nm)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.y = element_text(angle = 0, hjust = 1.0)
  ) +
  scale_x_continuous(breaks = seq(0, 600, 50),
                     limits = c(0, 600),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 600, 50),
                     limits = c(0, 600),
                     expand = c(0,0)) +
  coord_fixed(ratio = 1)

ggsave(paste("PLOTS/R34_R50_R64_Model/R34_Prediction_Model_Vmax_Cp_Lat_all_ibtracs.png"), width = 5.5, height = 5)


print(jj)

## R50 ##

title <- expression(bold("a)") * " " * "Random Forest Model to Predict R50")

MAE_result <- mean(abs(results_R50$Observed - results_R50$Predicted))
MAE <- sprintf("%.1f", MAE_result)
correlation_result <- cor(results_R50$Observed, results_R50$Predicted)
correlation <- sprintf("%.2f", correlation_result)

# Create a scatter plot with a line of best fit
ggplot(data = results_R50, aes(x = Predicted, y = Observed)) +

  geom_label(
    label=paste("Correlation: ", correlation, "\n", "MAE: ", MAE,
                "\nN Points: ", nrow(R34_Model_Test_Data), sep = ""),
    x = 200, y = 35,
    size = 4,  # Adjust the text size here
    # label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.5,
    color = "black",
    fill=alpha("white", 0.5)
  ) +

  geom_point(color = "red3", size=0.75, alpha = 0.15) + 
  # geom_point(shape = 1,size = 1,colour = "black", alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid", linewidth = 0.25) +
  # geom_density_2d(color = "black", size = 0.5, linetype = "dashed", bins=4) +  # Add this line for the density plot
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", linewidth = 1.1) +
  
  # labs(title = "Random Forest Model to Predict R34: \nGiven Vmax, Cp and Latitude",
  labs(x = "R50 Model Prediction (nm)",
       y = "R50 \nObserved \n(nm)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.y = element_text(angle = 0, hjust = 1.0)
  ) +
  scale_x_continuous(breaks = seq(0, 250, 50),
                     limits = c(0, 250),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 250, 50),
                     limits = c(0, 250),
                     expand = c(0,0)) +
  coord_fixed(ratio = 1)

ggsave(paste("PLOTS/R34_R50_R64_Model/R50_Prediction_Model_Vmax_Cp_Lat_all_ibtracs.png"), width = 5.5, height = 5)


## R64 ##

title <- expression(bold("b)") * " " * "Random Forest Model to Predict R64")

MAE_result <- mean(abs(results_R64$Observed - results_R64$Predicted))
MAE <- sprintf("%.1f", MAE_result)
correlation_result <- cor(results_R64$Observed, results_R64$Predicted)
correlation <- sprintf("%.2f", correlation_result)

# Create a scatter plot with a line of best fit
ggplot(data = results_R64, aes(x = Predicted, y = Observed)) +

  geom_label(
    label=paste("Correlation: ", correlation, "\n", "MAE: ", MAE,
                "\nN Points: ", nrow(R34_Model_Test_Data), sep = ""),
    x = 200, y = 25,
    size = 4,  # Adjust the text size here
    # label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.5,
    color = "black",
    fill=alpha("white", 0.5)
  ) +

  geom_point(color = "red3", size=0.75, alpha = 0.15) + 
  # geom_point(shape = 1,size = 1,colour = "black", alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid", linewidth = 0.25) +
  # geom_density_2d(color = "black", size = 0.5, linetype = "dashed", bins=4) +  # Add this line for the density plot
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", linewidth = 1.1) +
  
  # labs(title = "Random Forest Model to Predict R34: \nGiven Vmax, Cp and Latitude",
  labs(x = "R64 Model Prediction (nm)",
       y = "R64 \nObserved \n(nm)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.y = element_text(angle = 0, hjust = 1.0)
  ) +
  scale_x_continuous(breaks = seq(0, 250, 50),
                     limits = c(0, 250),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 250, 50),
                     limits = c(0, 250),
                     expand = c(0,0)) +
  coord_fixed(ratio = 1)

ggsave(paste("PLOTS/R34_R50_R64_Model/R64_Prediction_Model_Vmax_Cp_Lat_all_ibtracs.png"), width = 5.5, height = 5)



