

setwd("~/Hurricane_Attributes_and_Loss_Relationships")

###### Set working directory ######
directory <- "~/Hurricane_Attributes_and_Loss_Relationships"


###### stop R from outputting Warning Messages ###### 
options(warn=-1)




###### load R libraries for analysis ###### 
library("leaflet")
#library("mapview")
library("stringr")
library("readxl")
library("dplyr")
library("sp")
library("maptools")
library("maps")
library("webshot")
library("ggplot2")
library("ggmap")
library("rworldmap")
library("writexl")
library("tidyr")



###### ----- 1) Read in Model Input Data ----- ###### 

loss_data_type <- "Economic"  # Economic or Insured




## read in blended loss data ##
loss_data <- read_xlsx("OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_12hr_ERA5_Storm_Surge_USA_R34_NE_Average_Damage_All_Loss_Estimates.xlsx")

loss_data <- subset(loss_data, select = c(Year_Name, USA_ATCF_ID, Landfall_Marker, USA_WIND, USA_R34_NE,
                                          
                                          Inflation_Factor, Wealth_Factor, Housing_Factor,
                                          
                                          Unadjusted_Loss,
                                          
                                          Adjusted_Economic_Loss,
                                          
                                          Weinkle_Unadjusted_Loss, Muller_Unadjusted_Loss,
                                          EMDAT_Unadjusted_Loss, Grinsted_Unadjusted_Loss, NOAA_Unadjusted_Loss
                                          
                                          ))

colnames(loss_data)[colnames(loss_data) == "Unadjusted_Loss"]        <- "Unadjusted_Average_Economic_Loss"
colnames(loss_data)[colnames(loss_data) == "Adjusted_Economic_Loss"] <- "Adjusted_Average_Economic_Loss"

loss_data$Unadjusted_Average_Economic_Loss <- as.numeric(loss_data$Unadjusted_Average_Economic_Loss)
loss_data$Adjusted_Average_Economic_Loss   <- as.numeric(loss_data$Adjusted_Average_Economic_Loss)



loss_data$Adjusted_Weinkle_Economic_Loss  <- loss_data$Weinkle_Unadjusted_Loss * loss_data$Inflation_Factor * loss_data$Wealth_Factor * loss_data$Housing_Factor
loss_data$Adjusted_Muller_Economic_Loss   <- loss_data$Muller_Unadjusted_Loss * loss_data$Inflation_Factor * loss_data$Wealth_Factor * loss_data$Housing_Factor
loss_data$Adjusted_Grinsted_Economic_Loss <- loss_data$Grinsted_Unadjusted_Loss * loss_data$Inflation_Factor * loss_data$Wealth_Factor * loss_data$Housing_Factor
loss_data$Adjusted_EMDAT_Economic_Loss    <- loss_data$EMDAT_Unadjusted_Loss * loss_data$Inflation_Factor * loss_data$Wealth_Factor * loss_data$Housing_Factor
loss_data$Adjusted_NOAA_Economic_Loss     <- loss_data$NOAA_Unadjusted_Loss * loss_data$Inflation_Factor * loss_data$Wealth_Factor * loss_data$Housing_Factor



# colnames(df)[colnames(df) == "b"] <- "new_b"


loss_data <- loss_data %>% drop_na(Unadjusted_Average_Economic_Loss)

loss_data_all_vars <- na.omit(loss_data)

loss_data_with_na <- loss_data[rowSums(is.na(loss_data)) > 0, ]

loss_data_with_na <- loss_data_with_na %>% drop_na(Unadjusted_Average_Economic_Loss)
# 
# write_xlsx(loss_data, "loss_data.xlsx")




## assign landfall Saffir Simpson category ##
loss_data$SS_Category <- rep(NA, nrow(loss_data))

for (storm_index in seq(1, nrow(loss_data), 1)) {
  
  storm_vmax <- loss_data$USA_WIND[storm_index]
  
  if (storm_vmax < 64.0) {
    loss_data$SS_Category[storm_index] <- as.numeric(0)
  }
  if (between(storm_vmax, 64, 84.0)) {
    loss_data$SS_Category[storm_index] <- as.numeric(1)
  }
  if (between(storm_vmax, 84.00001, 96.0)) {
    loss_data$SS_Category[storm_index] <- as.numeric(2)
  }
  if (between(storm_vmax, 96.00001, 114.0)) {
    loss_data$SS_Category[storm_index] <- as.numeric(3)
  }
  if (between(storm_vmax, 114.00001, 135.0)) {
    loss_data$SS_Category[storm_index] <- as.numeric(4)
  }
  if (storm_vmax >= 135.0) {
    loss_data$SS_Category[storm_index] <- as.numeric(5)
  }
}




loss_data$Year <- as.numeric(substring(loss_data$Year_Name, 1, 4))

## Replace NA with 0
loss_data[is.na(loss_data)] <- 0


## Convert landfall marker to another string ##
loss_data$Landfall_Marker <- gsub("Closest Bypasser", "CB", loss_data$Landfall_Marker)
loss_data$Landfall_Marker <- gsub("Pre - 1st Max Landfall", "LF Max", loss_data$Landfall_Marker)
loss_data$Landfall_Marker <- gsub("Pre - 2nd Max Landfall", "LF 2", loss_data$Landfall_Marker)
loss_data$Landfall_Marker <- gsub("Pre - 3rd Max Landfall", "LF 3", loss_data$Landfall_Marker)

## Create column with Year_Name_LandfallMarker ##
loss_data$Year_Name_LandfallMarker <- paste(loss_data$Year_Name, "_", loss_data$Landfall_Marker, sep = "")

loss_data$storm_names <- loss_data$Year_Name


## Create Year column ##
loss_data$Year <- as.numeric(substring(loss_data$Year_Name, 1, 4))

## drop NAs ##
loss_data <- loss_data %>% drop_na(Unadjusted_Average_Economic_Loss)



# ## Subset by year ##
# loss_data <- loss_data[loss_data$Year >= 1950, ]

## Convert to billions ##
loss_data$Unadjusted_Average_Economic_Loss        <- loss_data$Unadjusted_Average_Economic_Loss / 10 ** 9 

loss_data$Adjusted_Average_Economic_Loss   <- loss_data$Adjusted_Average_Economic_Loss / 10 ** 9
loss_data$Adjusted_Weinkle_Economic_Loss   <- loss_data$Adjusted_Weinkle_Economic_Loss / 10 ** 9
loss_data$Adjusted_Grinsted_Economic_Loss  <- loss_data$Adjusted_Grinsted_Economic_Loss / 10 ** 9
loss_data$Adjusted_Muller_Economic_Loss    <- loss_data$Adjusted_Muller_Economic_Loss / 10 ** 9
loss_data$Adjusted_EMDAT_Economic_Loss     <- loss_data$Adjusted_EMDAT_Economic_Loss / 10 ** 9
loss_data$Adjusted_NOAA_Economic_Loss      <- loss_data$Adjusted_NOAA_Economic_Loss / 10 ** 9


# # Replace all 0 values with NA
# loss_data[loss_data == 0] <- NA


##### Plot timeseries of loss #####

years <- seq(1979, 2024, 1)
Year_Loss_Unadjusted_Aggregate <- data.frame(Year = years,
                                             Loss = rep(NA, length(years)))
Year_Loss_Adjusted_Aggregate   <- data.frame(Year = years,
                                             Loss = rep(NA, length(years)))

## Aggregate losses per year ##
for (year_index in seq(1, length(years), 1)) {
  
  per_year <- loss_data[loss_data$Year == years[year_index], ]
  
  if (nrow(per_year) > 0) {
    Year_Loss_Unadjusted_Aggregate$Loss[year_index] <- sum(per_year$Unadjusted_Average_Economic_Loss, na.rm=T)
    Year_Loss_Adjusted_Aggregate$Loss[year_index]   <- sum(per_year$Adjusted_Average_Economic_Loss, na.rm=T)
  }
  
  if (nrow(per_year) == 0) {
    Year_Loss_Unadjusted_Aggregate$Loss[year_index] <- 0
    Year_Loss_Adjusted_Aggregate$Loss[year_index]   <- 0
  }
}

# Year_Loss_Unadjusted_Aggregate <- aggregate(Unadjusted_Average_Economic_Loss ~ Year, data = loss_data, sum)
# Year_Loss_Adjusted_Aggregate   <- aggregate(Adjusted_Average_Economic_Loss ~ Year, data = loss_data, sum)
# 
# colnames(Year_Loss_Unadjusted_Aggregate) <- c("Year", "Loss")
# colnames(Year_Loss_Adjusted_Aggregate)   <- c("Year", "Loss")

## If you minus eachother, you can stack in the below bar graph without adjusted being stacked ontop of unadjusted
Year_Loss_Adjusted_Aggregate_og <- Year_Loss_Adjusted_Aggregate

Year_Loss_Adjusted_Aggregate$Loss <- Year_Loss_Adjusted_Aggregate$Loss #- Year_Loss_Unadjusted_Aggregate$Loss

Year_Loss_Unadjusted_Aggregate$Group <- "Un-Normalised"
Year_Loss_Adjusted_Aggregate$Group   <- "Normalised"

Year_Loss_Unadjusted_and_Adjusted_Aggregate <- rbind(Year_Loss_Unadjusted_Aggregate, Year_Loss_Adjusted_Aggregate)


if (loss_data_type == "Economic"){
  fill <- "cornflowerblue"
  color <- "black"
  title <- expression(bold("a)") * " " * "Historical North Atlantic Hurricane Economic Loss (1979 - 2023)")
  title <- "Historical North Atlantic Hurricane Economic Loss (1979 - 2024)"
  
  ymax <- 375.0
  x_intervals <- 25
  y_title <- "Observed\nEconomic\nLoss\n(US$ bn)"
}
if (loss_data_type == "Insured"){
  fill <- "coral1"
  color <- "black"
  title <- expression(bold("b)") * " " * "Historical North Atlantic Hurricane Insured Loss (1979 - 2023)")
  ymax <- 140.0
  x_intervals <- 20
  y_title <- "Observed\nEconomic\nLoss\n(US$ bn)"
}

# Create the sequence
xticks <- seq(1979, 2024, 1)
# Set every number that is not every 5th number to NA
xticks <- ifelse((xticks - 1979) %% 5 == 0, xticks, "")


model_unadjusted <- lm(Loss ~ Year, Year_Loss_Unadjusted_Aggregate)
model_adjusted   <- lm(Loss ~ Year, Year_Loss_Adjusted_Aggregate_og)


ggplot() + 
  
  geom_bar(data = Year_Loss_Unadjusted_and_Adjusted_Aggregate, 
           aes(x = Year, y = Loss, group = Group, fill = Group),
           stat = "identity", position = "dodge", width = 0.5, color = color) +
  
  # geom_bar(data = Year_Loss_Unadjusted_and_Adjusted_Aggregate, 
  #          aes(x = Year, y = Loss, group = Group, fill = Group),
  #          stat = "identity", position =  position_dodge(width = 0.7), width = 0.5, color = color) +
  
  # geom_bar(stat = "identity", size = 0.5, color = color) +
  
  geom_smooth(data = Year_Loss_Unadjusted_Aggregate, aes(x = Year, y = Loss),
              se = FALSE, method = lm, color = "royalblue", size = 1.0, linetype = "dashed") +
  
  geom_smooth(data = Year_Loss_Adjusted_Aggregate_og, aes(x = Year, y = Loss),
              se = FALSE, method = lm, color = "coral2", size = 1.0, linetype = "dashed") +
  
  labs(x = "Year", y = "Loss", fill = "Loss Type:") + 
  
  scale_fill_manual(values = c("coral2", "royalblue")) +
  
  theme_minimal(base_size = 16) +
  # labs(title = "Multi Linear Regression Model") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey80", size=0.02),
        panel.grid.major.y = element_line(colour="grey60", size=0.05),
        panel.grid.minor.x = element_line(colour="grey80", size=0.02),
        panel.grid.major.x = element_line(colour="grey60", size=0.05),
        legend.text = element_text(size = 16),
        # legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = c(0.09, 0.86),
        axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
        axis.ticks = element_line(color = "black", size = 0.8),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1.0),
        axis.text.y = element_text(angle = 0, hjust = 1.0)) + # Black ticks on the axis)
  
  scale_y_continuous(name = "Annual\nHurricane\nEconomic\nLoss\n(US$ bn)",
                     breaks = seq(0, ymax, x_intervals),
                     minor_breaks = seq(0, ymax, 10),
                     expand = c(0.0, 0.0),
                     limits=c(0, ymax)) +
  
  scale_x_continuous(name = "Year",
                     breaks = seq(1979, 2024, 1),
                     labels = xticks,
                     expand = c(0.0, 0.0),
                     limits = c(1978, 2025))
  
  # labs(title = title) +
  
ggsave(paste(directory, "/PLOTS/Figure_4.png", sep = ""), width = 16, height = 6, dpi=150)


# ggsave(paste(directory, "/PLOTS/", loss_data_type, "_Loss/Timeseries_", loss_data_type,"_Loss_1979_2024.png", sep = ""), width = 16, height = 6, dpi=150)





#### Ordered Storm Loss Plot ####


### --- UNADJUSTED --- ###
# loss_data_billion_plus <- loss_data
# 
# if (loss_data_type == "Economic") {
# 
#   loss_data_billion_plus <- loss_data_billion_plus[loss_data_billion_plus$Unadjusted_Average_Economic_Loss > 5.0, ]
# }
# if (loss_data_type == "Insured") {
# 
#   loss_data_billion_plus <- loss_data_billion_plus[loss_data_billion_plus$Loss > 5.0, ]
# }
# 
# loss_data_billion_plus$Year_Name_LandfallMarker <- gsub("_", "\n", loss_data_billion_plus$Year_Name_LandfallMarker)
# 
# 
# if (loss_data_type == "Economic"){
#   fill <- "cornflowerblue"
#   color <- "black"
#   title <- expression(bold("a)") * " " * "Historical Un-normalised Economic Loss Per Storm, where Loss >US$5 bn (1979 - 2023)")
#   title <- "Historical Un-normalised Economic Loss Per Storm, where Loss >US$5 bn (1979 - 2023)"
#   ymax <- 140.0
#   x_intervals <- 20
#   y_title <- "Financial Loss (US$ bn)"
# }
# if (loss_data_type == "Insured"){
#   fill <- "coral1"
#   color <- "black"
#   title <- expression(bold("b)") * " " * "Historical Normalised (to 2023) Industry-wide Insured Loss Per Storm, where Loss >US$5 bn (1950 - 2023)")
#   ymax <- 100.0
#   x_intervals <- 20
#   y_title <- "Financial Loss (US$ bn)"
# }
# 
# ## Replace NA with 0
# loss_data_billion_plus$Adjusted_Weinkle_Economic_Loss[loss_data_billion_plus$Adjusted_Weinkle_Economic_Loss == 0] <- NA
# loss_data_billion_plus$Adjusted_Grinsted_Economic_Loss[loss_data_billion_plus$Adjusted_Grinsted_Economic_Loss == 0] <- NA
# loss_data_billion_plus$Adjusted_Muller_Economic_Loss[loss_data_billion_plus$Adjusted_Muller_Economic_Loss == 0] <- NA
# loss_data_billion_plus$Adjusted_EMDAT_Economic_Loss[loss_data_billion_plus$Adjusted_EMDAT_Economic_Loss == 0] <- NA
# 
# 
# # Calculate min and max for each year
# loss_data_summary_unadjusted <- loss_data_billion_plus %>%
#   group_by(Year_Name_LandfallMarker) %>%
#   summarise(
#     Loss = mean(Unadjusted_Average_Economic_Loss),  # Assuming you want to keep the average Loss for the bar
#     ymin = pmin(Unadjusted_Weinkle_Economic_Loss, Unadjusted_Grinsted_Economic_Loss, Unadjusted_Muller_Economic_Loss, Unadjusted_EMDAT_Economic_Loss, na.rm=T),  # Minimum value
#     ymax = pmax(Unadjusted_Weinkle_Economic_Loss, Unadjusted_Grinsted_Economic_Loss, Unadjusted_Muller_Economic_Loss, Unadjusted_EMDAT_Economic_Loss, na.rm=T)   # Maximum value
#   )
# 
# loss_data_summary_unadjusted <- na.omit(loss_data_summary_unadjusted)
# 
# ## Assign Saffir Simpson Scale ##
# loss_data$Year_Name_LandfallMarker <- gsub("_", "\n", loss_data$Year_Name_LandfallMarker)
# match_indexes <- match(loss_data_summary_unadjusted$Year_Name_LandfallMarker, loss_data$Year_Name_LandfallMarker)
# match_indexes <- match_indexes[!is.na(match_indexes)]
# loss_data_summary_unadjusted$Saffir_Simpson_Scale <- loss_data$SS_Category[match_indexes]
# 
# ## Replace NA with 0
# loss_data_summary_unadjusted$Loss[loss_data_summary_unadjusted$Loss == 0] <- NA
# loss_data_summary_unadjusted$ymin[loss_data_summary_unadjusted$ymin == 0] <- NA
# loss_data_summary_unadjusted$ymax[loss_data_summary_unadjusted$ymax == 0] <- NA
# 
# ## Round to 1 decimal place
# loss_data_summary_unadjusted$Loss <- round(loss_data_summary_unadjusted$Loss, 1)
# 
# ## Drop NAs ##
# loss_data_summary_unadjusted <- loss_data_summary_unadjusted %>% drop_na(Loss)
# 
# ## round to 1 decimal place ##
# loss_data_summary_unadjusted$Loss <- round(loss_data_summary_unadjusted$Loss, 1)
# 
# # Convert Saffir_Simpson_Scale to a factor (if it should be categorical)
# loss_data_summary_unadjusted$Saffir_Simpson_Scale <- as.factor(loss_data_summary_unadjusted$Saffir_Simpson_Scale)
# 
# # Create a color palette for discrete values
# ylorRd_colors <- c("lightyellow", "yellow", "orange", "coral", "red2", "red4")  # Example colors
# 
# # Create the plot
# ggplot(data = loss_data_summary_unadjusted, 
#        aes(x = reorder(Year_Name_LandfallMarker, Loss), y = Loss,
#        fill = Saffir_Simpson_Scale)) +
#   geom_col(color = "black") +
#   geom_errorbar(data = loss_data_summary_unadjusted,
#                 aes(ymin = ymin, ymax = ymax), width = 0.2) +  # Add whiskers
#   
#   geom_text(aes(label = Loss),  # Use Loss as the label
#             position = position_stack(vjust = 0.5),  # Position the labels in the middle of the bars
#             color = "black") +  # Set label color
#   
#   scale_fill_manual(values = ylorRd_colors) +
# 
#   theme_minimal(base_size = 16) +
#   # labs(title = "Multi Linear Regression Model") +
#   theme(panel.background = element_rect(fill = "white", color = NA),
#         plot.background = element_rect(fill = "white", color = NA),
#         panel.grid.minor.y = element_line(colour="grey90", size=0.1),
#         panel.grid.major.y = element_line(colour="grey80", size=0.2),
#         panel.grid.minor.x = element_line(colour="grey90", size=0.1),
#         panel.grid.major.x = element_line(colour="grey80", size=0.2),
#         legend.text = element_text(size = 16),
#         legend.title = element_blank(),
#         legend.key = element_rect(fill = "white", colour = "black"),
#         legend.position = "none",
#         axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
#         axis.ticks = element_line(color = "black", size = 0.8),
#         axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 12)) + # Black ticks on the axis)
#   
#   scale_y_continuous(name = y_title,
#                      breaks = seq(0, ymax, x_intervals),
#                      minor_breaks = seq(0, ymax, 5),
#                      expand = c(0.0, 0.0),
#                      limits=c(0, ymax)) +
# 
#   scale_x_discrete(name = "",
#                      expand = c(0.0, 0.0)) +
#   
#   labs(title = title, fill = "Saffir \nSimpson \nScale")
# 
# ggsave(paste(directory, "/PLOTS/", loss_data_type, "_Loss/Ordered_Storm_Unadjusted_", loss_data_type,"_Loss_1979_2023.png", sep = ""), width = 16, height = 6, dpi=150)
# 


## Adjusted ##

loss_data_billion_plus <- loss_data

loss_data_billion_plus <- loss_data_billion_plus %>% arrange(Adjusted_Average_Economic_Loss)


if (loss_data_type == "Economic") {
  
  loss_data_billion_plus <- loss_data_billion_plus[loss_data_billion_plus$Adjusted_Average_Economic_Loss > 5.0, ]
}
if (loss_data_type == "Insured") {
  
  loss_data_billion_plus <- loss_data_billion_plus[loss_data_billion_plus$Loss > 5.0, ]
}

loss_data_billion_plus$Year_Name_LandfallMarker <- gsub("_", "\n", loss_data_billion_plus$Year_Name_LandfallMarker)


if (loss_data_type == "Economic"){
  fill <- "cornflowerblue"
  color <- "black"
  title <- expression(bold("a)") * " " * "Historical Hurricane Normalised (to 2024) Economic Loss, where Loss >US$5 bn (1979 - 2024)")
  title <- "Historical Averaged Normalised (to 2024) Economic Loss Per Storm, where Loss >US$5 bn (1979 - 2024)"
  ymax <- 300.0
  x_intervals <- 20
  y_title <- "Observed\nEconomic\nLoss\n(US$ bn)"
}
if (loss_data_type == "Insured"){
  fill <- "coral1"
  color <- "black"
  title <- expression(bold("b)") * " " * "Historical Normalised (to 2023) Industry-wide Insured Loss Per Storm, where Loss >US$5 bn (1950 - 2023)")
  ymax <- 100.0
  x_intervals <- 20
  y_title <- "Observed\nEconomic\nLoss\n(US$ bn)"
}

## Replace NA with 0
loss_data_billion_plus$Adjusted_Weinkle_Economic_Loss[loss_data_billion_plus$Adjusted_Weinkle_Economic_Loss == 0]   <- NA
loss_data_billion_plus$Adjusted_Grinsted_Economic_Loss[loss_data_billion_plus$Adjusted_Grinsted_Economic_Loss == 0] <- NA
loss_data_billion_plus$Adjusted_Muller_Economic_Loss[loss_data_billion_plus$Adjusted_Muller_Economic_Loss == 0]     <- NA
loss_data_billion_plus$Adjusted_EMDAT_Economic_Loss[loss_data_billion_plus$Adjusted_EMDAT_Economic_Loss == 0]       <- NA
loss_data_billion_plus$Adjusted_NOAA_Economic_Loss[loss_data_billion_plus$Adjusted_NOAA_Economic_Loss == 0]         <- NA

# Calculate min and max for each year
loss_data_summary_adjusted <- loss_data_billion_plus %>%
  group_by(Year_Name_LandfallMarker) %>%
  summarise(
    Loss = mean(Adjusted_Average_Economic_Loss),  # Assuming you want to keep the average Loss for the bar
    ymin = pmin(Adjusted_Weinkle_Economic_Loss, Adjusted_Grinsted_Economic_Loss, Adjusted_Muller_Economic_Loss, Adjusted_EMDAT_Economic_Loss, Adjusted_NOAA_Economic_Loss, na.rm=T),  # Minimum value
    ymax = pmax(Adjusted_Weinkle_Economic_Loss, Adjusted_Grinsted_Economic_Loss, Adjusted_Muller_Economic_Loss, Adjusted_EMDAT_Economic_Loss, Adjusted_NOAA_Economic_Loss, na.rm=T)   # Maximum value
  )




loss_data_summary_adjusted <- na.omit(loss_data_summary_adjusted)

## Assign Saffir Simpson Scale ##
loss_data$Year_Name_LandfallMarker <- gsub("_", "\n", loss_data$Year_Name_LandfallMarker)
match_indexes <- match(loss_data_summary_adjusted$Year_Name_LandfallMarker, loss_data$Year_Name_LandfallMarker)
match_indexes <- match_indexes[!is.na(match_indexes)]
loss_data_summary_adjusted$Saffir_Simpson_Scale <- loss_data$SS_Category[match_indexes]


## Assign storm names ##
loss_data_summary_adjusted$storm_names <- loss_data$storm_names[match_indexes]

## Round to 1 decimal place
loss_data_summary_adjusted$Loss <- round(loss_data_summary_adjusted$Loss, 1)

## Drop NAs ##
loss_data_summary_adjusted <- loss_data_summary_adjusted %>% drop_na(Loss)

## round to 1 decimal place ##
loss_data_summary_adjusted$Loss <- round(loss_data_summary_adjusted$Loss, 0)

# Convert Saffir_Simpson_Scale to a factor (if it should be categorical)
loss_data_summary_adjusted$Saffir_Simpson_Scale <- as.factor(loss_data_summary_adjusted$Saffir_Simpson_Scale)

# Create a color palette for discrete values
ylorRd_colors <- c("lightyellow", "yellow", "orange", "coral", "red2", "red4")  # Example colors


loss_data_summary_adjusted$storm_names <- gsub("_", " ", loss_data_summary_adjusted$storm_names)


# Create the plot
ggplot(data = loss_data_summary_adjusted, 
       aes(x = reorder(storm_names, -Loss), y = Loss,
       fill = Saffir_Simpson_Scale)) +
  geom_col(color = "black") +
  geom_errorbar(data = loss_data_summary_adjusted,
                aes(ymin = ymin, ymax = ymax), width = 0.2) +  # Add whiskers
  
  geom_label(
    aes(label = Loss, y = 10),
    fill = alpha("white", 0.5),  # White with 50% transparency
    color = "black",
    size = 4,
    label.size = NA,  # Remove border if not needed
    # position = position_nudge(y = max(loss_data_summary_adjusted$Loss) * 0.05)
  ) +
  
  scale_fill_manual(values = ylorRd_colors) +
  
  theme_minimal(base_size = 16) +
  # labs(title = "Multi Linear Regression Model") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey90", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey90", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),
        legend.text = element_text(size = 16),
        # legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        # legend.position = "none",
        axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
        axis.ticks = element_line(color = "black", size = 0.8),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1, size = 10)) + # Black ticks on the axis)
  
  scale_y_continuous(name = y_title,
                     breaks = seq(0, ymax+10, x_intervals),
                     minor_breaks = seq(0, ymax, 5),
                     expand = c(0.0, 0.0),
                     limits=c(0, ymax)) +
  
  scale_x_discrete(name = "",
                   expand = c(0.02, 0.02)) +
  
  labs(fill = "Saffir\nSimpson\nScale\nCategory")

  
ggsave(paste(directory, "/PLOTS/Figure_5.png", sep = ""), width = 18, height = 6, dpi=150)


print("/PLOTS/Figure_5.png")
# ggsave(paste(directory, "/PLOTS/", loss_data_type, "_Loss/Ordered_Storm_Adjusted_", loss_data_type,"_Loss_1979_2023.png", sep = ""), width = 18, height = 6, dpi=150)



