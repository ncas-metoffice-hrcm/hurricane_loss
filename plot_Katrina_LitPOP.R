
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
library(rgdal)
library(rgeos)
library(maps)
library(stringr)
library(stringi)
library(geosphere)
library(ggplot2)
library(readr)
library(raster)
library(RColorBrewer)





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





### PLOT KATRINA LITPOP EXPOSURE IMPACT ###

ibtracs_3hourly <- ibtracs_3hourly[ibtracs_3hourly$USA_ATCF_ID == "AL122005", ]

lon <- ibtracs_3hourly$LON
lat <- ibtracs_3hourly$LAT
radius <- ibtracs_3hourly$USA_R34_NE

# ## Load LitPOP Exposure Value ##
# presentdayLitPOP_Exposure_Raster <- raster(paste("~/DATA/LitPOP/Extrapolated/LITPOP_Exposure_Value_USA_10km_2020.tif", sep = ""))
presentdayLitPOP_Exposure_Raster <- raster(paste("~/DATA/LitPOP/LitPop_pc_30arcsec_USA_1km.tiff", sep = ""))

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
  geom_raster(data = raster_df, aes(x = lon, y = lat, fill = value / 10 ** 9)) +
  scale_fill_viridis_c() +  # or any other color scale
  geom_sf(data = combined_buffer_sf, fill = NA, color = "red", size = 1) +
  geom_point(data = ibtracs_3hourly, aes(x = LON, y = LAT), size = 0.5, color = "black") +
  
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.05) +
  
  geom_polygon(data = mainland_usa_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.3) +  # Plot the map of Europe
  
  scale_fill_stepsn(colours= c("white", rev(brewer.pal(7, "Spectral"))),
                    name = "LitPOP \nExposure \nValue \n(US$ bn) \n(1x1km)",
                    breaks = c(0.1, 1, 2, 4, 6, 8, 10) / 10,
                    limits = c(0, 10) / 10,
                    # breaks = c(seq(0, 5, 0.5), 10),
                    # limits = c(0, 5),
                    na.value = NA) +
  
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
  
  geom_label(aes(x = -124, y = 27.5,
                 # geom_label(aes(x = -125, y = 24.5,
                 
                 label = paste("Total Exposure Value Impacted:\nUS$22,259 bn", sep ="")),
             fill = "white",  # Fill color of the box
             color = "black",  # Text color
             size = 3,  # Size of the text
             label.padding = unit(0.5, "lines"),  # Padding around the label
             label.size = 0.5,  # Size of the label border
             color = "black",
             hjust = 0) + # Outline color of the box
  
  coord_sf(xlim = c(-123, -69), ylim = c(23.5, 48.8))

ggsave("PLOTS/Katrina_2005_Impact_Footprint.png", width = 8, height = 4)





