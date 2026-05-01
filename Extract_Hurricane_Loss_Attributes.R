
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
library(ranger)
library(readr)
library(caret)
library(ranger)
library(raster)
library(randomForest)
library(RColorBrewer)




number_of_timesteps_post_landfall_included <- 40 # 6 12 40
r_value         <- "USA_R34_NE" # "USA_R50_NE", "USA_R64_NE", "USA_R34_NE"
r_value_colname <- "R34" # "R50", "R64", "R34"
loss_type <- "Average_Damage"  # Average_Damage NOAA_Billion
with_other_loss <- "yes"  # Average_Damage NOAA_Billion


# test <- read_xlsx(paste("OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
#                  "12hr", "_ERA5_Storm_Surge_", r_value, "_", loss_type, "_All_Loss_Estimates.xlsx", sep = ""))
# 
# colnames(test)
# # test <- na.omit(test)
# # Weinkle_Unadjusted_Loss Grinsted_Unadjusted_Loss Muller_Unadjusted_Loss EMDAT_Unadjusted_Loss NOAA_Unadjusted_Loss
# test <- subset(test,
#                                            select = c(USA_ATCF_ID, Landfall_Marker, Year_Name, USA_WIND, SS_Category, USA_PRES, USA_R34_NE, USA_RMW, STORM_SPEED, Storm_Tide,
#                                                       Rainfall_Accumilation, Rainfall_Max_rate, Max_Rain_3hour_Accumilation,
#                                                       
#                                                       R34_Sum_Hurricane_Building_Value, R34_Mean_Hurricane_Historic_Loss_Ratio_Building,
#                                                       R34_Mean_Risk_Score, R34_Median_Risk_Score,
#                                                       R34_Mean_Percent_Not_Resistant, R34_Median_Percent_Not_Resistant,
#                                                       R34_Sum_PresentDay_Building_Density, R34_Sum_Past_Building_Density,
#                                                       R34_Sum_PresentDay_Population_Density, R34_Sum_Past_Population_Density,
#                                                       R34_Sum_Past_Housing_Units, R34_Sum_Present_Housing_Units,
#                                                       R34_Mean_Building_Year, R34_Median_Building_Year, R34_Sum_LitPOP_Exposure_Value,
#                                                       
#                                                       NOAA_Unadjusted_Loss
#                                                       
#                                                       ))
# 
# test <- na.omit(test)
# nrow(test)
# Year_Name_missing_MSWEP <- unique((test$Year_Name))







if (number_of_timesteps_post_landfall_included == 6) {
  post_landfall <- "12hr"
}
if (number_of_timesteps_post_landfall_included == 12) {
  post_landfall <- "24hr"
}
if (number_of_timesteps_post_landfall_included == 40) {
  post_landfall <- "Fullhr"
}


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

ibtracs_all$USA_R34_NE[ibtracs_all$USA_R34_NE == 0] <- NA
ibtracs_all$USA_R34_NE[ibtracs_all$USA_R50_NE == 0] <- NA
ibtracs_all$USA_R34_NE[ibtracs_all$USA_R64_NE == 0] <- NA


#### ---- 2) Filter ibtracs  ---- ####

## filter by year ##
ibtracs_all <- ibtracs_all[ibtracs_all$Year >= 1979, ]
# ibtracs_all <- subset(ibtracs_all, Year == 2005 | Year == 2007)
# ibtracs_all <- ibtracs_all[ibtracs_all$Year <= 1982, ]


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

# # Floor time to every 3 hours
# ibtracs_3hourly$ISO_TIME <- floor_date(ibtracs_3hourly$ISO_TIME, "3 hours")




# Create the points data frame
points <- data.frame(lon = ibtracs_3hourly$LON, lat = ibtracs_3hourly$LAT)

# Pre-calculate landfall country and state for all points
ibtracs_3hourly$LANDFALL_COUNTRY <- as.character(determine_landfall_country(points))
ibtracs_3hourly$LANDFALL_STATE   <- as.character(determine_landfall_state(points))
ibtracs_3hourly$BYPASS_COUNTRY   <- as.character(determine_landfall_country_bypassing(points))
remove(points)





#### ---- Filter to storms with a loss estimates ---- ####

if (loss_type == "NOAA_Billion") {
  loss_data <- read_xlsx("LOSS_DATA/Billion_Dollar_USA_Hurricane_Loss.xlsx", sheet = "Unadjusted")
}
if (loss_type == "Average_Damage") {
  loss_data <- read_xlsx("LOSS_DATA/Average_Hurricane_Loss_Data_1979_2024.xlsx", sheet = "Sheet1")
}

loss_data <- loss_data %>% drop_na(AVERAGE_Damage)


# Filter rows where category is in my_list
ibtracs_3hourly <- ibtracs_3hourly %>%
  filter(USA_ATCF_ID %in% loss_data$USA_ATCF_ID)


# unique((ibtracs_3hourly$Year_Name))
# 
# ERA5_Storm_Surge   <- read_xlsx("HURRICANE_DATA/Average_Damage_Max_Landfall_Timesteps_1979_2024_with_ERA5_Storm_Surge.xlsx")
# 
# # Values in list2 but not in list1
# missing_in_list1 <- setdiff(ibtracs_3hourly$Year_Name, ERA5_Storm_Surge$Year_Name)


## Filter ibtracs_3hourly to istance to land less than 500 ##
ibtracs_3hourly <- ibtracs_3hourly[ibtracs_3hourly$DIST2LAND < 500, ]




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
  # storm_id <- "AL012014"

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

      # Determine if USA landfall point is greater than tropical storm (CAT0+) intensity
      USA_landfall <- USA_landfall[USA_landfall$USA_WIND > 34.0, ]

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
      if (length(closest_bypass_index) > 2) {
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

# ## Filter out secondary landfalls ##
# ibtracs_max_landfall_info <- ibtracs_max_landfall_info[ibtracs_max_landfall_info$Landfall_Marker != "Pre - 2nd Max Landfall", ]
# ibtracs_max_landfall_info <- ibtracs_max_landfall_info[ibtracs_max_landfall_info$Landfall_Marker != "Pre - 3rd Max Landfall", ]



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

if (number_of_timesteps_post_landfall_included == 40) {
  
  ibtracs_3hourly <- ibtracs_3hourly %>%
    filter(Landfall_Marker %in% c("Pre - 1st Max Landfall", "Pre - 2nd Max Landfall", "Pre - 3rd Max Landfall",
                                  "Closest Bypasser",
                                  "LANDFALL +0hours", "LANDFALL +3hours",
                                  "LANDFALL +6hours", "LANDFALL +9hours", "LANDFALL +12hours",
                                  "LANDFALL +15hours", "LANDFALL +18hours",
                                  "LANDFALL +21hours", "LANDFALL +24hours",
                                  "LANDFALL +27hours", "LANDFALL +30hours",
                                  "LANDFALL +33hours", "LANDFALL +36hours",
                                  "LANDFALL +39hours", "LANDFALL +42hours",
                                  "LANDFALL +45hours", "LANDFALL +48hours",
                                  "LANDFALL +51hours", "LANDFALL +54hours",
                                  "LANDFALL +57hours", "LANDFALL +60hours",
                                  "LANDFALL +63hours", "LANDFALL +66hours",
                                  "LANDFALL +69hours", "LANDFALL +72hours"))
}






# ### Save File for MSWEP Download ###
# year <- format(ibtracs_3hourly$ISO_TIME, "%Y")
# doy  <- format(ibtracs_3hourly$ISO_TIME, "%j")
# hour <- format(ibtracs_3hourly$ISO_TIME, "%H")
# ibtracs_3hourly$MSWEP_FILE <- paste(year, doy, ".", hour, ".nc", sep = "")
# 
# ### Save output ###
# if (number_of_timesteps_post_landfall_included == 6) {
#   ibtracs_3hourly <- subset(ibtracs_3hourly, select = -c(USA_PRES, USA_R34_NE, USA_R50_NE, USA_R64_NE, USA_RMW, STORM_SPEED))
# 
#   # ibtracs_3hourly <- ibtracs_3hourly[ibtracs_3hourly$Year_Name %in% Year_Name_missing_MSWEP, ]
#   # # Write to a text file
#   # writeLines(unique(ibtracs_3hourly$MSWEP_FILE), "OUTPUT/Avergae_Damage_Missing_MSWEP.txt")
#   #
# 
#   write_xlsx(ibtracs_3hourly, "OUTPUT/Average_Damage_12hr_Landfall_Timesteps_1979_2024_MSWEP_FILE.xlsx")
# 
# }
# if (number_of_timesteps_post_landfall_included == 12) {
#   ibtracs_3hourly <- subset(ibtracs_3hourly, select = -c(USA_PRES, USA_R34_NE, USA_R50_NE, USA_R64_NE, USA_RMW, STORM_SPEED))
#   write_xlsx(ibtracs_3hourly, "OUTPUT/NOAA_Billion_24hr_Landfall_Timesteps_1979_2024_MSWEP_FILE.xlsx")
# }
# if (number_of_timesteps_post_landfall_included == 40) {
#   ibtracs_3hourly <- subset(ibtracs_3hourly, select = -c(USA_PRES, USA_R34_NE, USA_R50_NE, USA_R64_NE, USA_RMW, STORM_SPEED))
#   write_xlsx(ibtracs_3hourly, "OUTPUT/NOAA_Billion_Fullhr_Landfall_Timesteps_1979_2024_MSWEP_FILE.xlsx")
# }


# # ### Save output for Storm Surge ###
# ibtracs_max_landfall_info <- subset(ibtracs_max_landfall_info, select = c(USA_ATCF_ID, NAME, LAT, LON, ISO_TIME, USA_WIND, Year, Year_Name, LANDFALL_COUNTRY, LANDFALL_STATE, BYPASS_COUNTRY, Landfall_Marker))
# write_xlsx(ibtracs_max_landfall_info, "OUTPUT/Average_Damage_Max_Landfall_Timesteps_1979_2024.xlsx")






#### ---- Append Loss ---- ####

if (loss_type == "Average_Damage") {
  loss_data_hurricane <- read_xlsx("LOSS_DATA/Average_Hurricane_Loss_Data_1979_2024.xlsx", sheet = "Sheet1")
  names(loss_data_hurricane)[names(loss_data_hurricane) == "AVERAGE_Damage"] <- "Unadjusted_Loss"
}

loss_data_hurricane$Year_Name_Landfall_Marker       <- paste(loss_data_hurricane$Year_Name, loss_data_hurricane$Landfall_Marker, sep = "")
ibtracs_max_landfall_info$Year_Name_Landfall_Marker <- paste(ibtracs_max_landfall_info$Year_Name, ibtracs_max_landfall_info$Landfall_Marker, sep = "")

matching_indexes <- match(ibtracs_max_landfall_info$Year_Name_Landfall_Marker, loss_data_hurricane$Year_Name_Landfall_Marker)

# matching_indexes <- matching_indexes[!is.na(matching_indexes)]

ibtracs_max_landfall_info$Unadjusted_Loss <- loss_data_hurricane$Unadjusted_Loss[matching_indexes]

ibtracs_max_landfall_info <- subset(ibtracs_max_landfall_info, select = c(-Year_Name_Landfall_Marker))













####### ------ Extend ibtracs R34 using HURDAT2 and R34 Random Forest Model ------ #######

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





ibtracs_3hourly$Year_Name_Time <- paste(ibtracs_3hourly$Year_Name, ibtracs_3hourly$ISO_TIME, sep = "_")

for (Storm_Name_Index in seq(1, length(unique(ibtracs_3hourly$USA_ATCF_ID)), 1)) {  # length(unique(ibtracs_3hourly$USA_ATCF_ID))
  
  # Gori_et_al_data$Storm_ID[match(ibtracs_max_landfall_info$USA_ATCF_ID, Gori_et_al_data$Storm_ID)]
  
  if (is.na(match(unique(ibtracs_3hourly$USA_ATCF_ID)[Storm_Name_Index], Gori_et_al_data$Storm_ID)) == FALSE) {
    
    Gori_et_al_data_per_storm <- Gori_et_al_data[Gori_et_al_data$Storm_ID == unique(ibtracs_3hourly$USA_ATCF_ID)[Storm_Name_Index], ]
    ibtracs_3hourly_per_storm <- ibtracs_3hourly[ibtracs_3hourly$USA_ATCF_ID == unique(ibtracs_3hourly$USA_ATCF_ID)[Storm_Name_Index], ]
    
    # Format without timezone
    Gori_et_al_data_per_storm$DATETIME <- paste(Gori_et_al_data_per_storm$DATE, Gori_et_al_data_per_storm$TIME, sep = " ")
    Gori_et_al_data_per_storm <- Gori_et_al_data_per_storm %>%
      dplyr::mutate(DATETIME = as.POSIXct(DATETIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    
    ibtracs_3hourly_per_storm <- ibtracs_3hourly_per_storm %>%
      dplyr::mutate(ISO_TIME = as.POSIXct(ISO_TIME, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    
    if (nrow(Gori_et_al_data_per_storm) >= 1) {
      
      # Ensure both are character
      dt_ibtracs <- as.character(ibtracs_3hourly_per_storm$ISO_TIME)
      dt_gori    <- as.character(Gori_et_al_data_per_storm$DATETIME)
      
      common_matches <- intersect(dt_ibtracs, dt_gori)
      
      if (length(common_matches) > 0) {
        # print(ibtracs_max_landfall_info$USA_ATCF_ID[Storm_Name_Index])
        ibtracs_match_index  <- match(dt_ibtracs, dt_gori)
        ibtracs_3hourly_per_storm$Gori_RMW <- Gori_et_al_data_per_storm$RMW_100[ibtracs_match_index]
      }
    }
    
    ## Append Gori et al to master ibtracs_3hourly ##
    ibtracs_3hourly_per_storm$Year_Name_Time <- paste(ibtracs_3hourly_per_storm$Year_Name, ibtracs_3hourly_per_storm$ISO_TIME, sep = "_")
    ibtracs_match_index  <- match(ibtracs_3hourly_per_storm$Year_Name_Time, ibtracs_3hourly$Year_Name_Time)
    ibtracs_3hourly$Gori_RMW[ibtracs_match_index] <- ibtracs_3hourly_per_storm$Gori_RMW
  }
}

ibtracs_3hourly <- subset(ibtracs_3hourly, select = -c(Year_Name_Time))





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

## Final random forest model ##
final_R34_model_input_data <- subset(ibtracs_3hourly, select = c("USA_R34_NE", "USA_WIND", "USA_PRES", "LAT", 
                                                                 "USA_RMW"))
## Final random forest model ##
final_R50_model_input_data <- subset(ibtracs_3hourly, select = c("USA_R50_NE", "USA_WIND", "USA_PRES", "LAT", 
                                                                 "USA_RMW"))
## Final random forest model ##
final_R64_model_input_data <- subset(ibtracs_3hourly, select = c("USA_R64_NE", "USA_WIND", "USA_PRES", "LAT", 
                                                                 "USA_RMW"))

## Modeled Reconstructed R34 using HURDAT2 RMW ##
final_R34_model_input_data <- final_R34_model_input_data %>% drop_na(USA_R34_NE)
final_R34_model_input_data <- final_R34_model_input_data %>% drop_na(USA_WIND)
final_R34_model_input_data <- final_R34_model_input_data %>% drop_na(USA_PRES)
final_R34_model_input_data <- final_R34_model_input_data %>% drop_na(USA_RMW)

## Modeled Reconstructed R34 using HURDAT2 RMW ##
final_R50_model_input_data <- final_R50_model_input_data %>% drop_na(USA_R50_NE)
final_R50_model_input_data <- final_R50_model_input_data %>% drop_na(USA_WIND)
final_R50_model_input_data <- final_R50_model_input_data %>% drop_na(USA_PRES)
final_R50_model_input_data <- final_R50_model_input_data %>% drop_na(USA_RMW)

## Modeled Reconstructed R34 using HURDAT2 RMW ##
final_R64_model_input_data <- final_R64_model_input_data %>% drop_na(USA_R64_NE)
final_R64_model_input_data <- final_R64_model_input_data %>% drop_na(USA_WIND)
final_R64_model_input_data <- final_R64_model_input_data %>% drop_na(USA_PRES)
final_R64_model_input_data <- final_R64_model_input_data %>% drop_na(USA_RMW)

Final_R34_Model <- randomForest(USA_R34_NE ~ LAT + USA_WIND + USA_PRES + USA_RMW,
                                data = final_R34_model_input_data)
Final_R50_Model <- randomForest(USA_R50_NE ~ LAT + USA_WIND + USA_PRES + USA_RMW,
                                data = final_R50_model_input_data)
Final_R64_Model <- randomForest(USA_R64_NE ~ LAT + USA_WIND + USA_PRES + USA_RMW,
                                data = final_R64_model_input_data)



### Max. Landfall table ###
## If there is a HURDAT RMW, append this to USA_RMW
for (index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
  if (is.na(ibtracs_max_landfall_info$USA_RMW[index]) == TRUE && is.na(ibtracs_max_landfall_info$HURDAT_RMW[index]) == FALSE) {
    ibtracs_max_landfall_info$USA_RMW[index] <- ibtracs_max_landfall_info$HURDAT_RMW[index]
  }
}

## If there is a Gori et al. RMW, append this to USA_RMW
for (index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
  if (is.na(ibtracs_max_landfall_info$USA_RMW[index]) == TRUE && is.na(ibtracs_max_landfall_info$Gori_RMW[index]) == FALSE) {
    ibtracs_max_landfall_info$USA_RMW[index] <- ibtracs_max_landfall_info$Gori_RMW[index]
  }
}

### All Landfall table ###
## If there is a HURDAT RMW, append this to USA_RMW
for (index in seq(1, nrow(ibtracs_3hourly), 1)) {
  if (is.na(ibtracs_3hourly$USA_RMW[index]) == TRUE && is.na(ibtracs_3hourly$HURDAT_RMW[index]) == FALSE) {
    ibtracs_3hourly$USA_RMW[index] <- ibtracs_3hourly$HURDAT_RMW[index]
  }
}

## If there is a Gori et al. RMW, append this to USA_RMW
for (index in seq(1, nrow(ibtracs_3hourly), 1)) {
  if (is.na(ibtracs_3hourly$USA_RMW[index]) == TRUE && is.na(ibtracs_3hourly$Gori_RMW[index]) == FALSE) {
    ibtracs_3hourly$USA_RMW[index] <- ibtracs_3hourly$Gori_RMW[index]
  }
}


# ## Omit NAs from ibtracs_max_landfall_info for R34 Model ##
# ibtracs_max_landfall_info <- ibtracs_max_landfall_info %>% drop_na(USA_RMW)
# ibtracs_max_landfall_info <- ibtracs_max_landfall_info %>% drop_na(USA_PRES)

## landfall ibtracs ##
for (storm_index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
  
  if (is.na(ibtracs_max_landfall_info$USA_WIND[storm_index]) == FALSE && is.na(ibtracs_max_landfall_info$USA_PRES[storm_index]) == FALSE && 
      is.na(ibtracs_max_landfall_info$LAT[storm_index]) == FALSE && is.na(ibtracs_max_landfall_info$USA_R34_NE[storm_index]) == TRUE) {
    
    storm_data <- ibtracs_max_landfall_info[storm_index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    ibtracs_max_landfall_info$USA_R34_NE[storm_index] <- predict(Final_R34_Model, newdata = newdata)
    
  }
  
  if (is.na(ibtracs_max_landfall_info$USA_WIND[storm_index]) == FALSE && is.na(ibtracs_max_landfall_info$USA_PRES[storm_index]) == FALSE && 
      is.na(ibtracs_max_landfall_info$LAT[storm_index]) == FALSE && is.na(ibtracs_max_landfall_info$USA_R50_NE[storm_index]) == TRUE) {
    
    storm_data <- ibtracs_max_landfall_info[storm_index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    ibtracs_max_landfall_info$USA_R50_NE[storm_index] <- predict(Final_R50_Model, newdata = newdata)
    
  }
  
  if (is.na(ibtracs_max_landfall_info$USA_WIND[storm_index]) == FALSE && is.na(ibtracs_max_landfall_info$USA_PRES[storm_index]) == FALSE && 
      is.na(ibtracs_max_landfall_info$LAT[storm_index]) == FALSE && is.na(ibtracs_max_landfall_info$USA_R64_NE[storm_index]) == TRUE) {
    
    storm_data <- ibtracs_max_landfall_info[storm_index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    ibtracs_max_landfall_info$USA_R64_NE[storm_index] <- predict(Final_R64_Model, newdata = newdata)
    
  }
}


## 3hourly ibtracs ##
## filter ibtracs_3hourly to just landfalling points ##
ibtracs_3hourly <- ibtracs_3hourly[is.na(ibtracs_3hourly$Landfall_Marker) == FALSE, ]
for (storm_index in seq(1, nrow(ibtracs_3hourly), 1)) {
  
  if (is.na(ibtracs_3hourly$USA_WIND[storm_index]) == FALSE && is.na(ibtracs_3hourly$USA_PRES[storm_index]) == FALSE && 
      is.na(ibtracs_3hourly$LAT[storm_index]) == FALSE && is.na(ibtracs_3hourly$USA_R34_NE[storm_index]) == TRUE) {
    
    storm_data <- ibtracs_3hourly[storm_index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    ibtracs_3hourly$USA_R34_NE[storm_index] <- predict(Final_R34_Model, newdata = newdata)
    
  }
  
  if (is.na(ibtracs_3hourly$USA_WIND[storm_index]) == FALSE && is.na(ibtracs_3hourly$USA_PRES[storm_index]) == FALSE && 
      is.na(ibtracs_3hourly$LAT[storm_index]) == FALSE && is.na(ibtracs_3hourly$USA_R50_NE[storm_index]) == TRUE) {
    
    storm_data <- ibtracs_3hourly[storm_index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    ibtracs_3hourly$USA_R50_NE[storm_index] <- predict(Final_R50_Model, newdata = newdata)
    
  }
  
  if (is.na(ibtracs_3hourly$USA_WIND[storm_index]) == FALSE && is.na(ibtracs_3hourly$USA_PRES[storm_index]) == FALSE && 
      is.na(ibtracs_3hourly$LAT[storm_index]) == FALSE && is.na(ibtracs_3hourly$USA_R64_NE[storm_index]) == TRUE) {
    
    storm_data <- ibtracs_3hourly[storm_index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    ibtracs_3hourly$USA_R64_NE[storm_index] <- predict(Final_R64_Model, newdata = newdata)
    
  }
  
}


## Replace 0 with NAs ##
ibtracs_3hourly$USA_R34_NE[ibtracs_3hourly$USA_R34_NE <= 0] <- NA
ibtracs_3hourly$USA_R50_NE[ibtracs_3hourly$USA_R50_NE <= 0] <- NA
ibtracs_3hourly$USA_R64_NE[ibtracs_3hourly$USA_R64_NE <= 0] <- NA



## REPLACE MISSING STORM SIZE WITH PREVIOUS STORM SIZE ##

# for (row in seq(2, nrow(ibtracs_3hourly), 1)) {
# 
#   previous_row <- row-1
#   
#   # # } && is.na(ibtracs_3hourly$USA_R34_NE[previous_row]) == FALSE && ibtracs_3hourly$USA_ATCF_ID[row] == ibtracs_3hourly$USA_ATCF_ID[previous_row]) {
#   #   print(jj)
#   #   ibtracs_3hourly$USA_R34_NE[row] <- ibtracs_3hourly$USA_R34_NE[previous_row]
#   # }
#   # if (is.na(ibtracs_3hourly$USA_R50_NE[row]) == TRUE && is.na(ibtracs_3hourly$USA_R50_NE[previous_row]) == FALSE && ibtracs_3hourly$USA_ATCF_ID[row] == ibtracs_3hourly$USA_ATCF_ID[previous_row]) {
#   #   ibtracs_3hourly$USA_R50_NE[row] <- ibtracs_3hourly$USA_R50_NE[previous_row]
#   # }
#   # if (is.na(ibtracs_3hourly$USA_R64_NE[row]) == TRUE && is.na(ibtracs_3hourly$USA_R64_NE[previous_row]) == FALSE && ibtracs_3hourly$USA_ATCF_ID[row] == ibtracs_3hourly$USA_ATCF_ID[previous_row]) {
#   #   ibtracs_3hourly$USA_R64_NE[row] <- ibtracs_3hourly$USA_R64_NE[previous_row]
#   # }
# 
# }






#### ---- 4) Append Saffir Simpson Scale, Rainfall and Storm Surge ---- ####



### --- Add SS_Category --- ###

ibtracs_max_landfall_info <- ibtracs_max_landfall_info %>%
  mutate(
    SS_Category = case_when(
      USA_WIND < 64 ~ 0,
      between(USA_WIND, 64, 84) ~ 1,
      between(USA_WIND, 84.00001, 96) ~ 2,
      between(USA_WIND, 96.00001, 114) ~ 3,
      between(USA_WIND, 114.00001, 135) ~ 4,
      USA_WIND >= 135 ~ 5,
      TRUE ~ NA_real_  # In case of missing or unexpected values
    )
  )








##### ---- Append max rainfall accumulation per storm ---- #####


if (loss_type == "NOAA_Billion") {
  
  if (post_landfall == "12hr") {
    rainfall_data_MSWEP   <- read_xlsx("HURRICANE_DATA/NOAA_Billion_12hr_Landfall_Timesteps_1979_2024_MSWEP_FILE_with_MSWEP_Rain.xlsx")
  }
  if (post_landfall == "Fullhr") {
    rainfall_data_MSWEP   <- read_xlsx("HURRICANE_DATA/NOAA_Billion_Fullhr_Landfall_Timesteps_1979_2024_MSWEP_FILE_with_MSWEP_Rain.xlsx")
  } 
  
}

if (loss_type == "Average_Damage") {
  
  if (post_landfall == "12hr") {
    rainfall_data_MSWEP   <- read_xlsx("HURRICANE_DATA/Average_Damage_12hr_Landfall_Timesteps_1979_2024_MSWEP_FILE_with_MSWEP_Rain.xlsx")
  }
  if (post_landfall == "Fullhr") {
    rainfall_data_MSWEP   <- read_xlsx("HURRICANE_DATA/Average_Damage_12hr_Landfall_Timesteps_1979_2024_MSWEP_FILE_with_MSWEP_Rain.xlsx")
  } 
}

rainfall_data_MSWEP$Previous_landfall_Type <- NA
for (row in seq(1, nrow(rainfall_data_MSWEP), 1)) {
  
  if (substring(rainfall_data_MSWEP$Landfall_Marker[row], 1, 6) != "LANDFA") {
    previous_landfall_type <- rainfall_data_MSWEP$Landfall_Marker[row]
  }
  rainfall_data_MSWEP$Previous_landfall_Type[row] <- previous_landfall_type
}

## Remove NAs ##
rainfall_data_MSWEP <- rainfall_data_MSWEP %>% drop_na(Max_Radius_3hour_Rain_Rate__MSWEP_mm_per_10km)

##  Get storm names like master landfall table ##
rainfall_data_MSWEP$ID_Landfall_Marker       <- paste(rainfall_data_MSWEP$USA_ATCF_ID, rainfall_data_MSWEP$Previous_landfall_Type, sep = "_")
ibtracs_max_landfall_info$ID_Landfall_Marker <- paste(ibtracs_max_landfall_info$USA_ATCF_ID, ibtracs_max_landfall_info$Landfall_Marker, sep = "_")

## Append to Master Landfall Table ##
matching_indexes <- match(ibtracs_max_landfall_info$ID_Landfall_Marker, rainfall_data_MSWEP$ID_Landfall_Marker)

ibtracs_max_landfall_info$Rainfall_Sum_Accumilation   <- rainfall_data_MSWEP$Max_Rain_Accumilation_Gridpoint_from_Footprint_MSWEP_mm_per_10km[matching_indexes]
ibtracs_max_landfall_info$Rainfall_Max_Rate           <- rainfall_data_MSWEP$Max_Radius_3hour_Rain_Rate__MSWEP_mm_per_10km[matching_indexes] 
ibtracs_max_landfall_info$Max_Rain_3hour_Accumilation <- rainfall_data_MSWEP$Sum_Radius_3hour_Rain_Rate__MSWEP_mm_per_10km[matching_indexes]






##### ---- Append NOAA max rainfall accumulation per storm ---- #####


rainfall_data_NOAA   <- read_xlsx("HURRICANE_DATA/NOAA_Max_Rainfall_Accumilation_1979_2024.xlsx")

##  Get storm names like master landfall table ##
rainfall_data_NOAA$Year_Name_Landfall_Marker        <- paste(rainfall_data_NOAA$Year_Name, rainfall_data_NOAA$Landfall_Marker, sep = "_")
ibtracs_max_landfall_info$Year_Name_Landfall_Marker <- paste(ibtracs_max_landfall_info$Year_Name, ibtracs_max_landfall_info$Landfall_Marker, sep = "_")

## Append to Master Landfall Table ##
matching_indexes <- match(ibtracs_max_landfall_info$Year_Name, rainfall_data_NOAA$Year_Name)

## Even if there are multiple matches, same value append to 1st and 2nd landfall ##
ibtracs_max_landfall_info$NOAA_Max_Rain_Accumilation <- rainfall_data_NOAA$NOAA_Max_Rainfall[matching_indexes]






# ##### ---- Add Max Storm Surge ---- #####

#### ---- 5) Append Storm Surge ---- ####

## Max tide from GORI et al.
# historical_storm_tide_data <- read_xlsx("HURRICANE_DATA/USA_Landfall_Times_1950_2023_with_GORI_Storm_Surge.xlsx")

# historical_storm_tide_data <- read_xlsx("HURRICANE_DATA/USA_Landfall_Times_1950_2023_with_ERA5_Storm_Surge.xlsx")
# historical_storm_tide_data$Storm_ID_Landfall_Marker   <- paste(historical_storm_tide_data$USA_ATCF_ID, historical_storm_tide_data$Landfall_Marker, sep = "")
# ibtracs_max_landfall_info$USA_ATCF_ID_Landfall_Marker <- paste(ibtracs_max_landfall_info$USA_ATCF_ID, ibtracs_max_landfall_info$Landfall_Marker, sep = "")
# 
# ## Append storm surge per storm to master ##
# matching_indexes <- match(ibtracs_max_landfall_info$USA_ATCF_ID_Landfall_Marker, historical_storm_tide_data$Storm_ID_Landfall_Marker)
# ibtracs_max_landfall_info$Storm_Tide <- historical_storm_tide_data$ERA5_Storm_Surge_Residual[c(matching_indexes)]

if (loss_type == "NOAA_Billion") {
  ERA5_Storm_Surge   <- read_xlsx("HURRICANE_DATA/NOAA_Billion_Max_Landfall_Timesteps_1979_2024_with_ERA5_Storm_Surge.xlsx")
  
  ERA5_Storm_Surge$ID_Landfall_Marker          <- paste(ERA5_Storm_Surge$USA_ATCF_ID, ERA5_Storm_Surge$Landfall_Marker, sep = "")
  ibtracs_max_landfall_info$ID_Landfall_Marker <- paste(ibtracs_max_landfall_info$USA_ATCF_ID, ibtracs_max_landfall_info$Landfall_Marker, sep = "")
  
  ibtracs_max_landfall_info$Storm_Tide   <- rep(NA, nrow(ibtracs_max_landfall_info))
  
  for (storm_index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
    
    ERA5_Storm_Surge_per_storm   <- ERA5_Storm_Surge[ERA5_Storm_Surge$ID_Landfall_Marker == ibtracs_max_landfall_info$ID_Landfall_Marker[storm_index], ]
    
    ibtracs_max_landfall_info$Storm_Tide[storm_index]   <- ERA5_Storm_Surge_per_storm$ERA5_Storm_Surge_Residual
    
  }
  
  ibtracs_max_landfall_info <- subset(ibtracs_max_landfall_info, select = c(-ID_Landfall_Marker))
}

if (loss_type == "Average_Damage") {
  
  ERA5_Storm_Surge   <- read_xlsx("HURRICANE_DATA/Average_Damage_Max_Landfall_Timesteps_1979_2024_with_ERA5_Storm_Surge.xlsx")
  
  ERA5_Storm_Surge$ID_Landfall_Marker          <- paste(ERA5_Storm_Surge$USA_ATCF_ID, ERA5_Storm_Surge$Landfall_Marker, sep = "")
  ibtracs_max_landfall_info$ID_Landfall_Marker <- paste(ibtracs_max_landfall_info$USA_ATCF_ID, ibtracs_max_landfall_info$Landfall_Marker, sep = "")
  
  ibtracs_max_landfall_info$Storm_Tide   <- rep(NA, nrow(ibtracs_max_landfall_info))
  
  ## Append to Master Landfall Table ##
  matching_indexes <- match(ibtracs_max_landfall_info$ID_Landfall_Marker, ERA5_Storm_Surge$ID_Landfall_Marker)
  
  ## Even if there are multiple matches, same value append to 1st and 2nd landfall ##

  ibtracs_max_landfall_info$Storm_Tide <- ERA5_Storm_Surge$ERA5_Storm_Surge_Residual[matching_indexes]
  
  ibtracs_max_landfall_info <- subset(ibtracs_max_landfall_info, select = c(-ID_Landfall_Marker))
  
}







##### ---- Add GDP Per USA Landfall State ---- #####


GDP_Per_State <- read_xlsx("SOCIAL_DATA/USA_State_GDP_1998_2024.xlsx", sheet = "Table")

# ibtracs_max_landfall_info$GDP_Per_State <- rep(NA, nrow(ibtracs_max_landfall_info))
# 
# for (storm_index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
#   
#   matching_index <- which(GDP_Per_State$GeoName == ibtracs_max_landfall_info$LANDFALL_STATE[storm_index])
#   
#   storm_year <- ibtracs_max_landfall_info$Year[storm_index]
#   
#   if (storm_year < 1998) {
#     storm_year <- 1998
#   }
#   
#   storm_year <- as.character(storm_year)
#                   
#   if (ibtracs_max_landfall_info$Landfall_Marker[storm_index] != "Closest Bypasser") {
#     ibtracs_max_landfall_info$GDP_Per_State[storm_index] <- GDP_Per_State[[storm_year]][matching_index]
#   }
#   ## If cloest byypasser, just append the national avergae GDP ##
#   if (ibtracs_max_landfall_info$Landfall_Marker[storm_index] == "Closest Bypasser") {
#     ibtracs_max_landfall_info$GDP_Per_State[storm_index] <- mean(GDP_Per_State[[storm_year]][2:nrow(GDP_Per_State)], na.rm=T)
#   }
# }







#### ---- 5) Obtain Social Footprint Impact Information ---- ####

# ibtracs_max_landfall_info <- ibtracs_max_landfall_info[ibtracs_max_landfall_info$Year == 1979, ]
# ibtracs_3hourly <- ibtracs_3hourly[ibtracs_max_landfall_info$Year == 1979, ]




## --- Loop through storms and accumilate FEMA Risk Scores --- ##

# Function to get State and County within radius
get_counties_in_radius <- function(longitude, latitude, radius) {
  
  # Convert central point to an sf object
  central_point <- st_as_sf(data.frame(longitude, latitude), coords = c("longitude", "latitude"), crs = 4326)
  # Get state and county boundaries from the maps package
  county_data <- maps::map("county", fill = TRUE, plot = FALSE)
  county_sf <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
  
  # Projected CRS: UTM zone, adjust as necessary for your area
  crs_projected <- 32615 # Example: UTM zone 15N, adjust based on your location
  
  # Transform data to projected CRS
  county_sf <- st_transform(county_sf, crs = crs_projected)
  central_point <- st_transform(central_point, crs = crs_projected)
  
  # Create a buffer around the central point (distance in meters)
  # convert radius from nm to km
  radius <- radius * 1.852
  buffer <- st_buffer(central_point, dist = radius * 1000) # Convert radius to meters
  
  # Check for valid geometries
  buffer <- st_make_valid(buffer)
  county_sf <- st_make_valid(county_sf)
  
  # Perform spatial intersection
  counties_within_radius <- st_intersection(county_sf, buffer)
  
  # Extract and display county names
  county_names <- counties_within_radius$ID
  
  return(county_names)
}




# Function to sum raster values within a specified radius around a coordinate
sum_raster_values_within_radius <- function(lon, lat, radius, raster_layer) {
  
  result <- tryCatch({
    
    # Convert the lon/lat to a spatial point
    point <- SpatialPoints(cbind(lon, lat), proj4string = CRS(projection(raster_layer)))
    
    # Convert radius from nautical miles to meters
    radius <- radius * 1852
    
    # Buffer the point by the specified radius (in meters)
    buffered_area <- buffer(point, width = radius)
    
    # Crop the raster to the buffered area
    cropped_raster <- crop(raster_layer, buffered_area)
    
    # Mask the cropped raster to get values only within the buffer
    masked_raster <- mask(cropped_raster, buffered_area)
    
    # Calculate the sum of the raster values within the buffer
    return(sum(values(masked_raster), na.rm = TRUE))
    
  }, error = function(e) {
    # Handle the specific error "extents do not overlap"
    if (grepl("extents do not overlap", e$message) || grepl("NA is not a valid rownumber", e$message)) {
      NA
    } else {
      stop(e)  # Re-throw the error if it's not the one we are expecting
    }
  })
  
  # Convert raster to data frame
  raster_df <- as.data.frame(rasterToPoints(raster_layer), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("lon", "lat", "value")

  # Assuming 'combined_buffer' is an SpatialPolygons object
  combined_buffer_sf <- st_as_sf(buffered_area)

  ggplot() +
    geom_raster(data = raster_df, aes(x = lon, y = lat, fill = value / 10 ** 9)) +
    scale_fill_viridis_c() +  # or any other color scale
    geom_sf(data = combined_buffer_sf, fill = NA, color = "red", size = 1) +
    coord_sf() +
    theme_minimal() +
    labs(title = "Raster with Buffer Polygons",
         fill = "Raster Value")
  
}




### PLOT KATRINA LITPOP EXPOSURE IMPACT ###

ibtracs_3hourly <- ibtracs_all[ibtracs_all $USA_ATCF_ID == "AL122005", ]

lon <- ibtracs_3hourly$LON
lat <- ibtracs_3hourly$LAT
radius <- ibtracs_3hourly$USA_R34_NE

# ## Load LitPOP Exposure Value ##
# presentdayLitPOP_Exposure_Raster <- raster(paste("~/DATA/LitPOP/Extrapolated/LITPOP_Exposure_Value_USA_10km_2020.tif", sep = ""))
presentdayLitPOP_Exposure_Raster <- raster(paste("~/DATA/LitPOP/LitPop_pc_30arcsec_Mainland_USA_5km.tif", sep = ""))
# presentdayLitPOP_Exposure_Raster <- raster(paste("~/DATA/LitPOP/LitPop_pc_30arcsec_USA_1km.tif", sep = ""))

# Convert the lon/lat to a spatial point
point <- SpatialPoints(cbind(lon, lat), proj4string = CRS(projection(presentdayLitPOP_Exposure_Raster)))

# Convert radius from nautical miles to meters
radius <- radius * 1852

# Buffer the point by the specified radius (in meters)
buffered_area <- buffer(point, width = radius)

# Crop the raster to the buffered area
cropped_raster <- crop(presentdayLitPOP_Exposure_Raster, buffered_area)

# Mask the cropped raster to get values only within the buffer
masked_raster <- mask(cropped_raster, buffered_area)

# Convert raster to data frame
raster_df <- as.data.frame(rasterToPoints(presentdayLitPOP_Exposure_Raster), stringsAsFactors = FALSE)
colnames(raster_df) <- c("lon", "lat", "value")

# Assuming 'combined_buffer' is an SpatialPolygons object
combined_buffer_sf <- st_as_sf(buffered_area)

## Load world map ##
world_map <-  map_data("world")

## Filter the world_map for mainland USA
mainland_usa_map <- world_map %>%
  filter(region %in% c("USA") & !grepl("Hawaii", subregion) & !grepl("Alaska", subregion))
# Get the state borders data
states <- map_data("state")

ggplot() +
  # geom_raster(data = raster_df, aes(x = lon, y = lat, fill = value / 10 ** 9)) +
  # scale_fill_viridis_c() +  # or any other color scale
  # geom_sf(data = combined_buffer_sf, fill = NA, color = "red", size = 1) +
  
  geom_point(data = ibtracs_3hourly, aes(x = LON, y = LAT), size = 0.5, color = "black") +

  geom_point(data = ibtracs_3hourly, aes(x = LON+5, y = LAT), size = 0.5, color = "red") +
  
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.05) +

  geom_polygon(data = mainland_usa_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.3) +  # Plot the map of Europe

  # scale_fill_stepsn(colours= c("white", rev(brewer.pal(7, "Spectral"))),
  #                   name = "LitPOP \nExposure \nValue \n(US$ bn) \n(1x1km)",
  #                   breaks = c(0.1, 1, 2, 4, 6, 8, 10) / 10,
  #                   limits = c(0, 10) / 10,
  #                   # breaks = c(seq(0, 5, 0.5), 10),
  #                   # limits = c(0, 5),
  #                   na.value = NA) +

  # coord_sf() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  # labs(title = "Hurricane Katrina (2005) LitPOP Exposure Value Footprint") +

  guides(fill = guide_colorbar(bar = "vertical",
                               barwidth = 1,
                               barheight = 6,
                               title.position = "top",
                               ticks = TRUE,
                               ticks.colour = "black",
                               frame.colour = "black",
                               frame.linewidth = 1)) +

  # geom_label(aes(x = -124, y = 27.5,
  #                # geom_label(aes(x = -125, y = 24.5,
  # 
  #                label = paste("Total Exposure Value Impacted:\nUS$22,259 bn", sep ="")),
  #            fill = "white",  # Fill color of the box
  #            color = "black",  # Text color
  #            size = 3,  # Size of the text
  #            label.padding = unit(0.5, "lines"),  # Padding around the label
  #            label.size = 0.5,  # Size of the label border
  #            color = "black",
  #            hjust = 0) + # Outline color of the box

  coord_sf(xlim = c(-100, -70), ylim = c(23.5, 40.0))

  # coord_sf(xlim = c(-123, -69), ylim = c(23.5, 48.8))

ggsave("PLOTS/Katrina_2005_Impact_Footprint_1.png", width = 8, height = 4)







# Function to sum raster values within a specified radius around a coordinate
sum_raster_values_within_radius_individual <- function(lon, lat, radius, raster_layer) {
  
  all_raster_values <- 0
  
  for (index in seq(1, length(lon), 1)) {
    
    result <- tryCatch({
      
      # Convert the lon/lat to a spatial point
      point <- SpatialPoints(cbind(lon, lat), proj4string = CRS(projection(raster_layer)))
      
      # Convert radius from nautical miles to meters
      radius <- radius * 1852
      
      # Buffer the point by the specified radius (in meters)
      buffered_area <- buffer(point, width = radius)
      
      # Crop the raster to the buffered area
      cropped_raster <- crop(raster_layer, buffered_area)
      
      # Mask the cropped raster to get values only within the buffer
      masked_raster <- mask(cropped_raster, buffered_area)
      
      # Calculate the sum of the raster values within the buffer
      all_raster_values <- all_raster_values + sum(values(masked_raster), na.rm = TRUE)
      
    }, error = function(e) {
      # Handle the specific error "extents do not overlap"
      if (grepl("extents do not overlap", e$message) || grepl("NA is not a valid rownumber", e$message)) {
        return(c(NA))
      } else {
        stop(e)  # Re-throw the error if it's not the one we are expecting
      }
    })
  }
  
  return(all_raster_values)
  
}



# Function to process a single storm
sum_raster_values <- function(LON, LAT, RADIUS, input_raster, sum_yes_or_no) {
  # Ensure CRS is set correctly
  crs(input_raster) <- CRS("+proj=longlat +datum=WGS84")
  all_raster_values <- mapply(sum_raster_values_within_radius,
                              one_storm$LON,
                              one_storm$LAT,
                              RADIUS,
                              MoreArgs = list(raster_layer = input_raster))
  
  # plot(input_raster)
  
  if (sum_yes_or_no == "Yes") {
    if (length(all_raster_values) == 0 || sum(all_raster_values, na.rm = TRUE) == 0) {
      return(NA)
    } else {
      return(sum(round(all_raster_values), na.rm = TRUE))
    }
  }
  if (sum_yes_or_no == "No") {
    if (length(all_raster_values) == 0 || sum(all_raster_values, na.rm = TRUE) == 0) {
      return(NA)
    } else {
      return(all_raster_values)
    }
  }
}



## Load FEMA Risk Score Data ##
FEMA_Vulnerability_Data <- read.csv("SOCIAL_DATA/FEMA_NRI_Table_Counties.csv")
FEMA_Vulnerability_Data$STATE_AND_COUNTY <- str_to_lower(paste(FEMA_Vulnerability_Data$STATE, ",", FEMA_Vulnerability_Data$COUNTY, sep = ""))

## Load FEMA Building Resilience Data ##
FEMA_Building_Code_Adoption <- read.csv("SOCIAL_DATA/FEMA_Building_Code_Adoption_Tracking_2023.csv")
FEMA_Building_Code_Adoption <- subset(FEMA_Building_Code_Adoption, select = c("State_Name", "NAME", "Hurricane_Resistance"))
FEMA_Building_Code_Adoption$STATE_AND_COUNTY <- str_to_lower(paste(FEMA_Building_Code_Adoption$State_Name, ",", FEMA_Building_Code_Adoption$NAME, sep = ""))



## Load GEM Exposure ##
GEM_Exposure_Raster <- raster("SOCIAL_DATA/USA_GEM_Exposure_Value_1km.tif")


## Load Building Density Exposure ##
presentday_GHSL_Building_Density_Raster <- raster("SOCIAL_DATA/USA_GHSL_Building_Density_Data_10km_2020_not_pixels.tif")


# ## Load Population Exposure ##
presentday_Population_Density_Raster <- raster(paste("SOCIAL_DATA/WorldPop/usa_ppp_", 2020, "_1km_Aggregated.tif", sep = ""))



# ## Load LitPOP Exposure Value ##
presentdayLitPOP_Exposure_Raster <- raster(paste("~/DATA/LitPOP/Extrapolated/LITPOP_Exposure_Value_USA_10km_2020.tif", sep = ""))




## Housing Units Data ##
Housing_Units_Data <- read_xlsx("SOCIAL_DATA/All_USA_Housing_Units_Per_County.xlsx")

Housing_Units_Data <- subset(Housing_Units_Data, select = c("State", "County", "Total housing units", 
                                                            "Housing units built 2020 or later",
                                                            "Housing units built 2010 to 2019",
                                                            "Housing units built 2000 to 2009",
                                                            "Housing units built 1990 to 1999",
                                                            "Housing units built 1980 to 1989",
                                                            "Housing units built 1970 to 1979",
                                                            "Housing units built 1960 to 1969",
                                                            "Housing units built 1950 to 1959",
                                                            "Housing units built 1940 to 1949",
                                                            "Housing units built 1939 or earlier",
                                                            "Median year housing units built"))
colnames(Housing_Units_Data)[colnames(Housing_Units_Data) == "Total housing units"] <- "Total_housing_units"
colnames(Housing_Units_Data)[colnames(Housing_Units_Data) == "Median year housing units built"] <- "Median_Build_Year"

Housing_Units_Data$Housing_Units_Upto_1950 <- Housing_Units_Data$`Housing units built 1939 or earlier` + Housing_Units_Data$`Housing units built 1940 to 1949`
Housing_Units_Data$Housing_Units_Upto_1960 <- Housing_Units_Data$Housing_Units_Upto_1950 + Housing_Units_Data$`Housing units built 1950 to 1959`
Housing_Units_Data$Housing_Units_Upto_1970 <- Housing_Units_Data$Housing_Units_Upto_1960 + Housing_Units_Data$`Housing units built 1960 to 1969`
Housing_Units_Data$Housing_Units_Upto_1980 <- Housing_Units_Data$Housing_Units_Upto_1970 + Housing_Units_Data$`Housing units built 1970 to 1979`
Housing_Units_Data$Housing_Units_Upto_1990 <- Housing_Units_Data$Housing_Units_Upto_1980 + Housing_Units_Data$`Housing units built 1980 to 1989`
Housing_Units_Data$Housing_Units_Upto_2000 <- Housing_Units_Data$Housing_Units_Upto_1990 + Housing_Units_Data$`Housing units built 1990 to 1999`
Housing_Units_Data$Housing_Units_Upto_2010 <- Housing_Units_Data$Housing_Units_Upto_2000 + Housing_Units_Data$`Housing units built 2000 to 2009`
Housing_Units_Data$Housing_Units_Upto_2020 <- Housing_Units_Data$Housing_Units_Upto_2010 + Housing_Units_Data$`Housing units built 2010 to 2019`
Housing_Units_Data$Housing_Units_Upto_Present <- Housing_Units_Data$Housing_Units_Upto_2020 + Housing_Units_Data$`Housing units built 2020 or later`

Housing_Units_Data <- subset(Housing_Units_Data, select = c(State, County, Total_housing_units, Median_Build_Year,
                                                            Housing_Units_Upto_1950, Housing_Units_Upto_1960,
                                                            Housing_Units_Upto_1970, Housing_Units_Upto_1980,
                                                            Housing_Units_Upto_1990, Housing_Units_Upto_2000,
                                                            Housing_Units_Upto_2010, Housing_Units_Upto_2020,
                                                            Housing_Units_Upto_Present))

Housing_Units_Data$STATE_AND_COUNTY <- paste(tolower(Housing_Units_Data$State), ",", tolower(gsub(" County", "", Housing_Units_Data$County)), sep = "")        

# Remove 'parish' and any following text from items ending with 'parish'
Housing_Units_Data$STATE_AND_COUNTY <- gsub("(.*) parish$", "\\1", Housing_Units_Data$STATE_AND_COUNTY, ignore.case = TRUE)



# Pre-define the raster years based on conditions
get_raster_year <- function(year, raster_type) {
  if (raster_type == "building") {
    return(cut(year, breaks = c(-Inf, 1978, 1983, 1988, 1993, 1998, 2003, 2008, 2013, 2018, 2025, 2028), labels = c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)))
  } else if (raster_type == "housing_previous") {
    return(cut(year, breaks = c(-Inf, 1955, 1965, 1975, 1985, 1995, 2005, 2015, 2030), labels = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)))
  } else if (raster_type == "housing_next") {
    return(cut(year, breaks = c(-Inf, 1955, 1965, 1975, 1985, 1995, 2005, 2015, 2030), labels = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2020)))
  }
}



## Initialize columns
ibtracs_max_landfall_info <- ibtracs_max_landfall_info %>%
  dplyr::mutate( 
    R34_Sum_Hurricane_Building_Value = NA,
    R34_Mean_Hurricane_Historic_Loss_Ratio_Building = NA,
    R34_Mean_Risk_Score = NA,
    R34_Median_Risk_Score = NA,
    R34_Mean_Percent_Not_Resistant = NA,
    R34_Median_Percent_Not_Resistant = NA,
    
    R34_Sum_GEM_Exposure = NA,
    R34_Sum_PresentDay_Building_Density = NA,
    R34_Sum_PresentDay_Population_Density = NA,
    
    R34_Max_PresentDay_Building_Density = NA,
    R34_Max_PresentDay_Population_Density = NA,
    
    R34_Sum_Past_Building_Density = NA,
    R34_Sum_Past_Population_Density = NA,
    R34_Sum_Past_Housing_Units = NA,
    R34_Sum_Present_Housing_Units = NA,
    
    R34_Sum_LitPOP_Exposure_Value = NA,
    
    R34_Mean_Building_Year = NA,
    R34_Median_Building_Year = NA,
    
    GDP_Per_State = NA,
    
    LitPOP_Percent_Damage = NA,
    GHSL_Percent_Damage = NA,

    Mean_Percent_Damage = NA
    )

# ibtracs_max_landfall_info$GDP_Per_State <- rep(NA, nrow(ibtracs_max_landfall_info))
# 
# for (storm_index in seq(1, nrow(ibtracs_max_landfall_info), 1)) {
#   
#   matching_index <- which(GDP_Per_State$GeoName == ibtracs_max_landfall_info$LANDFALL_STATE[storm_index])
#   
#   storm_year <- ibtracs_max_landfall_info$Year[storm_index]
#   
#   if (storm_year < 1998) {
#     storm_year <- 1998
#   }
#   
#   storm_year <- as.character(storm_year)
#   
#   if (ibtracs_max_landfall_info$Landfall_Marker[storm_index] != "Closest Bypasser") {
#     ibtracs_max_landfall_info$GDP_Per_State[storm_index] <- GDP_Per_State[[storm_year]][matching_index]
#   }
#   ## If cloest byypasser, just append the national avergae GDP ##
#   if (ibtracs_max_landfall_info$Landfall_Marker[storm_index] == "Closest Bypasser") {
#     ibtracs_max_landfall_info$GDP_Per_State[storm_index] <- mean(GDP_Per_State[[storm_year]][2:nrow(GDP_Per_State)], na.rm=T)
#   }
# }



## Emanuel Wind Vulnerability Curve: https://www.atmos.albany.edu/daes/atmclasses/atm305/2013/files/Emanuel%20(2011).pdf ##
## Vhalf: 110 knots, Vthres: 50 knots ##

# Set up % property destroyed equation using Emanuel 2011
# V_thres set to minimum max. landfall wind speed to aviod 0 values
# V_Half is unknown, but should be around 140 according to literature: 
# https://www.researchgate.net/publication/363600651_Estimating_Tropical_Cyclone_Vulnerability_A_Review_of_Different_Open-Source_Approaches/figures?lo=1
# https://ascelibrary.org/doi/10.1061/%28ASCE%291527-6988%282006%297%3A2%2894%29 
# -> https://www.google.com/search?safe=active&sca_esv=7550878c098e0420&sxsrf=AE3TifNxWZ71Yj5drUxkcMI8WN5T1DxBUg:1757000989626&udm=2&fbs=AIIjpHxU7SXXniUZfeShr2fp4giZ1Y6MJ25_tmWITc7uy4KIeuYzzFkfneXafNx6OMdA4MQRJc_t_TQjwHYrzlkIauOKKs0YI23S9EeDMNq-iJuNE9kbk_tqcCBI4NkmSFsStjsRXP1Xs0hqcA8NrOIiNJKwjGLFWru1jr_OtncT3qc7s2sIkptL5fddmgm0rryFypvi7c4_TB9BquBpaSsdhJm_WiwfDA&q=hazus+mean+hurricane+wind+vulnerability+function&sa=X&ved=2ahUKEwj7gu3eur-PAxV7QkEAHcu6FwgQtKgLegQIGBAB&biw=2390&bih=1342&dpr=0.8#vhid=W0Q8OqC4BAHCxM&vssid=mosaic
get_percent_damage <- function(wind) {

  V_Half  <- 140  # knots
  V_thres <- 45   # knots
  
  Vn <- max(c((wind-V_thres)/(V_Half - V_thres), 0.0))
  Fn <- (Vn**3) / (1+Vn**3)

  return(Fn)
}



ibtracs_max_landfall_info$Year_Name_Landfall_Marker <- paste(ibtracs_max_landfall_info$Year_Name, ibtracs_max_landfall_info$Landfall_Marker, sep = "_")

unique_storms <- unique(ibtracs_max_landfall_info$Year_Name_Landfall_Marker)

loss_data_second_landfall <- loss_data[loss_data$Landfall_Marker == "Pre - 2nd Max Landfall", ]



ibtracs_max_landfall_info$USA_RMW[which(ibtracs_max_landfall_info$Year_Name == "2004_ALEX")] <- 10.0

## Loop through unique storms and append social footprint ##
for (i in seq(1, length(unique_storms), 1)) { # length(unique_storms)
  
  print(paste(unique_storms[i], sep = ""))
  
  per_storm  <- ibtracs_max_landfall_info[ibtracs_max_landfall_info$Year_Name_Landfall_Marker == unique_storms[i], ]
  storm_name <- per_storm$Year_Name
  storm_year <- per_storm$Year
  
  landfalls_per_Year_Name <- ibtracs_max_landfall_info[ibtracs_max_landfall_info$Year_Name == per_storm$Year_Name, ]
  
  one_storm <- ibtracs_3hourly %>%
    filter(Year_Name == storm_name)
  
  ## If there is loss data for 1st and 2nd landfalls, split ibtracs_3hourly per landfall ##
  ## There are no 3rd landfalls with loss estimate ##
  
  one_storm <- ibtracs_3hourly %>%
    filter(Year_Name == storm_name)
  
  if (all(c("Pre - 2nd Max Landfall", "Pre - 1st Max Landfall") %in% one_storm$Landfall_Marker)) {
    max_landfall_index        <- which(one_storm$Landfall_Marker == "Pre - 1st Max Landfall")
    second_max_landfall_index <- which(one_storm$Landfall_Marker == "Pre - 2nd Max Landfall")

    if (per_storm$Landfall_Marker == "Pre - 1st Max Landfall") {
      if (max_landfall_index > second_max_landfall_index) {
        one_storm <- one_storm[max_landfall_index:nrow(one_storm),]
      }
      if (max_landfall_index < second_max_landfall_index) {
        one_storm <- one_storm[max_landfall_index:(second_max_landfall_index-1),]
      }
    }
    if (per_storm$Landfall_Marker == "Pre - 2nd Max Landfall") {
      if (second_max_landfall_index > max_landfall_index) {
        one_storm <- one_storm[second_max_landfall_index:nrow(one_storm),]
      }
      if (second_max_landfall_index < max_landfall_index) {
        one_storm <- one_storm[second_max_landfall_index:(max_landfall_index-1),]
      }
    }
  }
  
  ## If "Pre - 1st Max Landfall" isnt first index, make it so ##
  if (one_storm$Landfall_Marker[1] != "Pre - 1st Max Landfall" && !any(one_storm$Landfall_Marker %in% "Pre - 2nd Max Landfall") && !any(one_storm$Landfall_Marker %in% "Closest Bypasser")) {
    max_landfall_index        <- which(one_storm$Landfall_Marker == "Pre - 1st Max Landfall")
    one_storm <- one_storm[max_landfall_index:(nrow(one_storm)),]
  }
  
  
  if (one_storm$Landfall_Marker[1] != "Pre - 1st Max Landfall" && any(one_storm$Landfall_Marker %in% "Closest Bypasser")) {
    max_landfall_index        <- which(one_storm$Landfall_Marker == "Closest Bypasser")
    one_storm <- one_storm[max_landfall_index:(nrow(one_storm)),]
  }
    
  ## Replace missigng RMWs with previous RMW ## 
  first_non_na_RMW <- one_storm$USA_RMW[which(!is.na(one_storm$USA_RMW))[1]]
  one_storm$USA_RMW[is.na(one_storm$USA_RMW)]       <- first_non_na_RMW
  
  ## Get NaN indexes ##  
  USA_R34_NE_NaN_Index <- which(is.na(one_storm$USA_R34_NE))
  USA_R50_NE_NaN_Index <- which(is.na(one_storm$USA_R50_NE))
  USA_R64_NE_NaN_Index <- which(is.na(one_storm$USA_R64_NE))
  
  ## Run R34 Model ##
  if (length(USA_R34_NE_NaN_Index) > 0) {
    storm_data <- one_storm[USA_R34_NE_NaN_Index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    one_storm$USA_R34_NE[USA_R34_NE_NaN_Index] <- predict(Final_R34_Model, newdata = newdata)
  }
  
  ## Run R50 Model ##
  if (length(USA_R50_NE_NaN_Index) > 0) {
    storm_data <- one_storm[USA_R50_NE_NaN_Index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    one_storm$USA_R50_NE[USA_R50_NE_NaN_Index] <- predict(Final_R50_Model, newdata = newdata)
  }
  
  ## Run R64 Model ##
  if (length(USA_R64_NE_NaN_Index) > 0) {
    storm_data <- one_storm[USA_R64_NE_NaN_Index, ]
    newdata <- subset(storm_data, select = c(LAT, USA_WIND, USA_PRES, USA_RMW))
    one_storm$USA_R64_NE[USA_R64_NE_NaN_Index] <- predict(Final_R64_Model, newdata = newdata)
  }
  
  ## Replace NaN in master landfall table ##
  storm_index_in_master <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == one_storm$Year_Name_Landfall_Marker[1])
  
  if (is.na(ibtracs_max_landfall_info$USA_R34_NE[storm_index_in_master]) == TRUE) {
    ibtracs_max_landfall_info$USA_R34_NE[storm_index_in_master] <- one_storm$USA_R34_NE[1]
  }
  if (is.na(ibtracs_max_landfall_info$USA_R50_NE[storm_index_in_master]) == TRUE) {
    ibtracs_max_landfall_info$USA_R50_NE[storm_index_in_master] <- one_storm$USA_R50_NE[1]
  }
  if (is.na(ibtracs_max_landfall_info$USA_R64_NE[storm_index_in_master]) == TRUE) {
    ibtracs_max_landfall_info$USA_R64_NE[storm_index_in_master] <- one_storm$USA_R64_NE[1]
  }
  if (is.na(ibtracs_max_landfall_info$USA_RMW[storm_index_in_master]) == TRUE) {
    ibtracs_max_landfall_info$USA_RMW[storm_index_in_master]    <- one_storm$USA_RMW[1]
  }

  
  one_storm <- one_storm %>% drop_na(!!sym(r_value))
  
  
  raster_year_population <- get_raster_year(storm_year, "population")
  raster_year_building   <- get_raster_year(storm_year, "building")
  
  housing_year_previous       <- as.numeric(as.list(as.character(get_raster_year(storm_year, "housing_previous"))))
  housing_year_next           <- as.numeric(as.list(as.character(get_raster_year(storm_year, "housing_next"))))
  
  housing_column_previous <- paste("Housing_Units_Upto_", housing_year_previous, sep = "")
  housing_column_next     <- paste("Housing_Units_Upto_", housing_year_next, sep = "")
  
  ### Load time period specific population / building data ##
  past_raster_building <- raster(paste("SOCIAL_DATA/Building_Density_Raster/USA_GHSL_Building_Density_Data_10km_", raster_year_building, "_not_pixels.tif", sep = ""))
  if (storm_year <= 1999) {
    population_year <- 2000
  }
  if (storm_year >= 2020) {
    population_year <- 2020
  }
  if (between(storm_year, 2000, 2020)) {
    population_year <- storm_year
  }
  past_raster_population <- raster(paste("SOCIAL_DATA/WorldPop/usa_ppp_", population_year, "_1km_Aggregated.tif", sep = ""))
  
  
  
  ## GDP of States Impacted ##
  # Get GDP year
  if (storm_year < 1998) {
    GDP_Year <- as.character(1998)
  }
  if (storm_year >= 1998) {
    GDP_Year <- as.character(storm_year)
  }
  # Determine landfall States
  points <- data.frame(lon = one_storm$LON, lat = one_storm$LAT)
  one_storm$LANDFALL_STATE <- as.character(determine_landfall_state(points))
  # Get unique States
  landfall_states <- unique(one_storm$LANDFALL_STATE[!is.na(one_storm$LANDFALL_STATE)])
  # Find matching State indexes in GDP file
  matching_index_State_GDP_index <- match(landfall_states, GDP_Per_State$GeoName)
  # Find matching State indexes in master landfall table
  storm_index_in_master <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == one_storm$Year_Name_Landfall_Marker[1])
  # Append mean GDP impacted o master landfall table
  if (any(one_storm$Landfall_Marker == "Closest Bypasser") == FALSE) {
    ibtracs_max_landfall_info$GDP_Per_State[storm_index_in_master] <- mean(GDP_Per_State[[GDP_Year]][matching_index_State_GDP_index], na.rm=T)
  }
  # If closest bypasser, just append the national avergae GDP ##
  if (any(one_storm$Landfall_Marker == "Closest Bypasser") == TRUE) {
    ibtracs_max_landfall_info$GDP_Per_State[storm_index_in_master] <- mean(GDP_Per_State[[GDP_Year]][2:nrow(GDP_Per_State)], na.rm=T)
  }
  

  
  if (nrow(one_storm) > 0) {
  
    ## Initialise lists
    all_Hurricane_Building_Value <- rep(NA, nrow(one_storm))
    all_Hurricane_Historic_Loss_Ratio <- rep(NA, nrow(one_storm))
    all_Hurricane_Risk_Score <- rep(NA, nrow(one_storm))
    all_GEM_Exposure <- rep(NA, nrow(one_storm))
    all_Building_Year <- rep(NA, nrow(one_storm))
    
    all_presentday_Building_Density <- rep(NA, nrow(one_storm))
    all_presentday_Population_Density <- rep(NA, nrow(one_storm))
    all_present_Housing_Units <- rep(NA, nrow(one_storm))
    
    all_LitPOP_Exposure_Value <- rep(NA, nrow(one_storm))
    
    all_past_Population_Density <- rep(NA, nrow(one_storm))
    all_past_Building_Density <- rep(NA, nrow(one_storm))
    all_past_Housing_Units <- rep(NA, nrow(one_storm))
    
    all_Exposure_damaged <- rep(NA, nrow(one_storm))
    all_Percent_Damage   <- rep(NA, nrow(one_storm))
    
    counties_impacted <- get_counties_in_radius(
      one_storm$LON,
      one_storm$LAT,
      one_storm[[r_value]])
    
    FEMA_Counties <- FEMA_Vulnerability_Data %>% filter(STATE_AND_COUNTY %in% counties_impacted)
    Housing_Units_Data_Counties <- Housing_Units_Data %>% filter(STATE_AND_COUNTY %in% counties_impacted)
    
    
    if (nrow(FEMA_Counties) == 0) {
      all_Hurricane_Building_Value               <- 0.0
      all_Hurricane_Historic_Loss_Ratio          <- 0.0
      all_Hurricane_Risk_Score                   <- 0.0
    }
    
    if (nrow(FEMA_Counties) > 0) {
      all_Hurricane_Building_Value               <- sum(FEMA_Counties$HRCN_EXPB, na.rm = T)
      all_Hurricane_Historic_Loss_Ratio          <- mean(FEMA_Counties$HRCN_HLRB, na.rm = T)
      all_Hurricane_Risk_Score                   <- mean(FEMA_Counties$HRCN_RISKS, na.rm = T)
    }
    
    ## Raster Data - GEM Exposure, GHSL and Population Density ##
    # all_GEM_Exposure                             <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], GEM_Exposure_Raster)
    all_presentday_Building_Density              <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], presentday_GHSL_Building_Density_Raster, "Yes")
    all_presentday_Population_Density            <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], presentday_Population_Density_Raster, "Yes")
    
    LitPOP_Exposure_Value                        <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], presentdayLitPOP_Exposure_Raster, "No")
    all_LitPOP_Exposure_Value                    <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], presentdayLitPOP_Exposure_Raster, "Yes")
    
    all_past_Building_Density                    <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], past_raster_building, "Yes")
    all_past_Building_Density_not_sum            <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], past_raster_building, "No")
    all_past_Population_Density                  <- sum_raster_values(one_storm$LON, one_storm$LAT, one_storm[[r_value]], past_raster_population, "Yes")
    
    ## Emanuel Damagability ##
    mean_Percent_Damage          <- mean(sapply(one_storm$USA_WIND, get_percent_damage), na.rm = T)
    all_Percent_Damage           <- sapply(one_storm$USA_WIND, get_percent_damage)
    sum_LitPOP_Exposure_damaged  <- sum(LitPOP_Exposure_Value * all_Percent_Damage, na.rm = T)
    sum_GHSL_Exposure_damaged    <- sum(all_past_Building_Density_not_sum * all_Percent_Damage, na.rm = T)

    ## Housing Data ##
    all_previous_decade_Housing_Units     <- sum(Housing_Units_Data_Counties[[housing_column_previous]], na.rm = T)
    all_next_decade_Housing_Units         <- sum(Housing_Units_Data_Counties[[housing_column_next]], na.rm = T)
    ## Linear interpolate between decade housing unit density from Census data ##
    housing_unit_dataframe <- data.frame(Year = c(1, 10),
                                         Density = c(all_previous_decade_Housing_Units, all_next_decade_Housing_Units))
    ## Create a linear model
    model <- lm(Density ~ Year, data = housing_unit_dataframe)
    new_data <- data.frame(Year = housing_year_next - storm_year)
    ## Do linear interpolation
    all_past_Housing_Units <- predict(model, newdata = new_data)
    
    if (nrow(Housing_Units_Data_Counties) > 0) {
      all_present_Housing_Units  <- sum(Housing_Units_Data_Counties$Housing_Units_Upto_Present, na.rm = T)
      all_present_Housing_Units  <- sum(Housing_Units_Data_Counties$Housing_Units_Upto_Present, na.rm = T)
      all_Building_Year          <- mean(Housing_Units_Data_Counties$Median_Build_Year, na.rm = T)
    }
    if (nrow(Housing_Units_Data_Counties) == 0) {
      all_present_Housing_Units  <- 0.0
      all_present_Housing_Units  <- 0.0
      all_Building_Year          <- mean(Housing_Units_Data$Median_Build_Year, na.rm = T)
    }
    
    ## -- Building Resistance -- ##
    Building_Resistance_in_Counties <- FEMA_Building_Code_Adoption$Hurricane_Resistance[match(counties_impacted, FEMA_Building_Code_Adoption$STATE_AND_COUNTY)]
    Building_Resistance_in_Counties <- data.frame(Building_Resistance = na.omit(Building_Resistance_in_Counties))
    if (all(Building_Resistance_in_Counties$Building_Resistance == "Not at Risk")) {
      Percent_Not_Resistant <- 0.0
    }
    if (!all(Building_Resistance_in_Counties$Building_Resistance == "Not at Risk")) {
      Building_Resistance_in_Counties <- data.frame(Building_Resistance = Building_Resistance_in_Counties[Building_Resistance_in_Counties$Building_Resistance != "Not at Risk", ])
      Percent_Not_Resistant <- length(Building_Resistance_in_Counties[Building_Resistance_in_Counties$Building_Resistance != "Resistant", ]) /
        nrow(Building_Resistance_in_Counties)
    }
    
    ## Append Mean / Sum FEMA Data to Master storm list ##
    storm_index_in_master <- which(ibtracs_max_landfall_info$Year_Name_Landfall_Marker == one_storm$Year_Name_Landfall_Marker[1])
    
    ### FEMA ###
    ibtracs_max_landfall_info$R34_Sum_Hurricane_Building_Value[storm_index_in_master]                   <- sum(all_Hurricane_Building_Value, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Mean_Hurricane_Historic_Loss_Ratio_Building[storm_index_in_master]    <- sum(all_Hurricane_Risk_Score, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Mean_Risk_Score[storm_index_in_master]                                <- mean(all_Hurricane_Risk_Score, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Median_Risk_Score[storm_index_in_master]                              <- median(all_Hurricane_Risk_Score, na.rm = TRUE)
    
    ### Building Resistance ###
    ibtracs_max_landfall_info$R34_Mean_Percent_Not_Resistant[storm_index_in_master]    <- mean(Percent_Not_Resistant, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Median_Percent_Not_Resistant[storm_index_in_master]  <- median(Percent_Not_Resistant, na.rm = TRUE)
    
    ### Raster Values - GHSL, GEM & Population ###
    ibtracs_max_landfall_info$R34_Sum_GEM_Exposure[storm_index_in_master]                   <- sum(all_GEM_Exposure, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Sum_PresentDay_Building_Density[storm_index_in_master]    <- sum(all_presentday_Building_Density, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Sum_PresentDay_Population_Density[storm_index_in_master]  <- sum(all_presentday_Population_Density, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Sum_Past_Building_Density[storm_index_in_master]          <- sum(all_past_Building_Density, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Sum_Past_Population_Density[storm_index_in_master]        <- sum(all_past_Population_Density, na.rm = TRUE)
    
    ibtracs_max_landfall_info$R34_Sum_LitPOP_Exposure_Value[storm_index_in_master]          <- sum(all_LitPOP_Exposure_Value, na.rm = TRUE)
    
    ibtracs_max_landfall_info$R34_Max_PresentDay_Building_Density[storm_index_in_master]          <- max(all_past_Building_Density, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Max_PresentDay_Population_Density[storm_index_in_master]        <- max(all_past_Population_Density, na.rm = TRUE)
    
    ### Housing Units ###
    ibtracs_max_landfall_info$R34_Sum_Past_Housing_Units[storm_index_in_master]       <- sum(all_past_Housing_Units, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Sum_Present_Housing_Units[storm_index_in_master]    <- sum(all_present_Housing_Units, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Mean_Building_Year[storm_index_in_master]           <- mean(all_Building_Year, na.rm = TRUE)
    ibtracs_max_landfall_info$R34_Median_Building_Year[storm_index_in_master]         <- median(all_Building_Year, na.rm = TRUE)

    
    ## Emanuel Damagability ##
    ibtracs_max_landfall_info$LitPOP_Percent_Damage[storm_index_in_master]       <- sum_LitPOP_Exposure_damaged
    ibtracs_max_landfall_info$GHSL_Percent_Damage[storm_index_in_master]         <- sum_GHSL_Exposure_damaged

    ibtracs_max_landfall_info$Mean_Percent_Damage[storm_index_in_master]         <- mean_Percent_Damage
  }
}

ibtracs_max_landfall_info$R34_Sum_Past_Housing_Units
ibtracs_max_landfall_info$R34_Sum_Present_Housing_Units


ibtracs_max_landfall_info_output <- subset(ibtracs_max_landfall_info,
                                           select = c(USA_ATCF_ID, Landfall_Marker, Year_Name, USA_WIND, SS_Category, USA_PRES, USA_R34_NE, USA_RMW, STORM_SPEED, Storm_Tide,
                                                      Rainfall_Sum_Accumilation, Rainfall_Max_Rate, Max_Rain_3hour_Accumilation,
                                                      NOAA_Max_Rain_Accumilation,
                                                      
                                                      R34_Sum_Hurricane_Building_Value, R34_Mean_Hurricane_Historic_Loss_Ratio_Building,
                                                      R34_Mean_Risk_Score, R34_Median_Risk_Score,
                                                      R34_Mean_Percent_Not_Resistant, R34_Median_Percent_Not_Resistant,
                                                      R34_Sum_PresentDay_Building_Density, R34_Sum_Past_Building_Density,
                                                      R34_Sum_PresentDay_Population_Density, R34_Sum_Past_Population_Density,
                                                      R34_Sum_Past_Housing_Units, R34_Sum_Present_Housing_Units,
                                                      R34_Mean_Building_Year, R34_Median_Building_Year, R34_Sum_LitPOP_Exposure_Value,
                                                      GDP_Per_State,
                                                      LitPOP_Percent_Damage, Mean_Percent_Damage, GHSL_Percent_Damage))

## Housing Unit Factor is: 1 + (NEW - OLD) / OLD - i.e., what is the change required to get to present day
ibtracs_max_landfall_info_output$Housing_Factor <- 1 + (ibtracs_max_landfall_info_output$R34_Sum_Present_Housing_Units - ibtracs_max_landfall_info_output$R34_Sum_Past_Housing_Units) / ibtracs_max_landfall_info_output$R34_Sum_Past_Housing_Units

# Change 'R34' to 'R50' in all column names
colnames(ibtracs_max_landfall_info_output) <- gsub("R34", r_value_colname, colnames(ibtracs_max_landfall_info_output))


#### ---- 12) Append Normalised Loss Data ---- ####

if (loss_type == "NOAA_Billion") {
  loss_data_hurricane <- read_xlsx("LOSS_DATA/Billion_Dollar_USA_Hurricane_Loss.xlsx", sheet = "Unadjusted")
  names(loss_data_hurricane)[names(loss_data_hurricane) == "Unadjusted_Cost_USbn"] <- "Unadjusted_Loss"
}
if (loss_type == "Average_Damage") {
  loss_data_hurricane <- read_xlsx("LOSS_DATA/Average_Hurricane_Loss_Data_1979_2024.xlsx", sheet = "Sheet1")
  names(loss_data_hurricane)[names(loss_data_hurricane) == "AVERAGE_Damage"] <- "Unadjusted_Loss"
}

loss_data_hurricane$Year_Name_Landfall_Marker              <- paste(loss_data_hurricane$Year_Name, loss_data_hurricane$Landfall_Marker, sep = "")
ibtracs_max_landfall_info_output$Year_Name_Landfall_Marker <- paste(ibtracs_max_landfall_info_output$Year_Name, ibtracs_max_landfall_info_output$Landfall_Marker, sep = "")

matching_indexes <- match(ibtracs_max_landfall_info_output$Year_Name_Landfall_Marker, loss_data_hurricane$Year_Name_Landfall_Marker)

# matching_indexes <- matching_indexes[!is.na(matching_indexes)]

ibtracs_max_landfall_info_output$Unadjusted_Loss <- loss_data_hurricane$Unadjusted_Loss[matching_indexes]

ibtracs_max_landfall_info_output <- subset(ibtracs_max_landfall_info_output, select = c(-Year_Name_Landfall_Marker))

if (with_other_loss == "yes") {
  ibtracs_max_landfall_info_output$Weinkle_Unadjusted_Loss  <- loss_data_hurricane$Weinkle_UnNormalised_Damage[matching_indexes]
  ibtracs_max_landfall_info_output$Grinsted_Unadjusted_Loss <- loss_data_hurricane$Grinsted_UnNormalised_Damage[matching_indexes]
  ibtracs_max_landfall_info_output$Muller_Unadjusted_Loss   <- loss_data_hurricane$Muller_UnNormalised_Damage[matching_indexes]
  ibtracs_max_landfall_info_output$EMDAT_Unadjusted_Loss    <- loss_data_hurricane$EMDAT_UnNormalised_Damage[matching_indexes]
  ibtracs_max_landfall_info_output$NOAA_Unadjusted_Loss     <- loss_data_hurricane$NOAA_UnNormalised_Damage[matching_indexes]
}


##### --- Normalise Losses --- #####

Inflation <- read.csv("LOSS_DATA/FRED__GDP_Implicit_Price_Deflator_1979_2024.csv")

## Only keep January values from quarterly numbers ##
Inflation$observation_date <- as.Date(Inflation$observation_date)
Inflation <- Inflation %>%
  filter(format(observation_date, "%m") == "01")

## Inflation factor is 2024 value / year value
Inflation$Inflation_Factor <- Inflation$GDPDEF[length(Inflation$GDPDEF)] / Inflation$GDPDEF

Inflation$Year <- format(Inflation$observation_date, "%Y")

### Inflation factor is applied as it is ###
ibtracs_max_landfall_info_output$Inflation_Factor <- Inflation$Inflation_Factor[match(substring(ibtracs_max_landfall_info_output$Year_Name, 1, 4), Inflation$Year)]



# ### Housing Factor ####
# ## Muller et al. 2025 ##
# muller_housing_data <- read.csv("LOSS_DATA/05_USA Housing Units.csv")
# colnames(muller_housing_data) <- c("Year", "Housing_Units", "Housing_Units_Multiplier")
# 
# ibtracs_max_landfall_info_output$Housing_Factor <- muller_housing_data$Housing_Units_Multiplier[match(substring(ibtracs_max_landfall_info_output$Year_Name, 1, 4), muller_housing_data$Year)]
# ibtracs_max_landfall_info_output$Housing_Factor <- as.numeric(as.character((ibtracs_max_landfall_info_output$Housing_Factor)))
# # ibtracs_max_landfall_info_output$Housing_Factor <- c(ibtracs_max_landfall_info_output$Housing_Factor[1:177], c(1.0, 1.0, 1.0, 1.0))

# ibtracs_max_landfall_info_output$Housing_Factor <- ibtracs_max_landfall_info_output$R34_Sum_Present_Housing_Units / ibtracs_max_landfall_info_output$R34_Sum_Past_Housing_Units





### Wealth factor is (real wealth in 2024 / real wealth in year) / housing adjustment factor
Wealth <- read.csv("LOSS_DATA/BEA__Fixed_Assets_and_Consumer_Durable_Goods_Index_1950_2024.csv")
Wealth <- Wealth[Wealth$Year >= 1979, ]
Wealth$Multipler <-  Wealth$Wealth_Metric[length(Wealth$Wealth_Metric)] / Wealth$Wealth_Metric

## Calc wealth factor adjusted by inflation ##
Wealth$Inflation_Adjusted_Wealth_Multipler <-  Wealth$Multipler / Inflation$Inflation_Factor[match(Wealth$Year, Inflation$Year)]
ibtracs_max_landfall_info_output$Inflation_Adjusted_Wealth_Multipler <- Wealth$Inflation_Adjusted_Wealth_Multipler[match(substring(ibtracs_max_landfall_info_output$Year_Name, 1, 4), Wealth$Year)]

## Calc wealth factor adjusted by housing units ##
ibtracs_max_landfall_info_output$Wealth_Factor <- ibtracs_max_landfall_info_output$Inflation_Adjusted_Wealth_Multipler / ibtracs_max_landfall_info_output$Housing_Factor


### Normalised Loss ### 
ibtracs_max_landfall_info_output$Adjusted_Economic_Loss  <- ibtracs_max_landfall_info_output$Unadjusted_Loss * ibtracs_max_landfall_info_output$Inflation_Factor * ibtracs_max_landfall_info_output$Wealth_Factor * ibtracs_max_landfall_info_output$Housing_Factor


if (with_other_loss == "no") {
  write_xlsx(ibtracs_max_landfall_info_output,
             paste("OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
                   post_landfall, "_ERA5_Storm_Surge_", r_value, "_", loss_type, ".xlsx", sep = ""))
}

if (with_other_loss == "yes") {
  write_xlsx(ibtracs_max_landfall_info_output,
             paste("OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
                   post_landfall, "_ERA5_Storm_Surge_", r_value, "_", loss_type, "_All_Loss_Estimates_test.xlsx", sep = ""))
}



