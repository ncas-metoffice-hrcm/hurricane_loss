



## suppress warning messages ##
options( warn = -1 )


setwd("~/Hurricane_Attributes_and_Loss_Relationships")


library(readxl)
library(lubridate)
library(tidyr)
library(purrr)
library(dplyr)
library(rworldmap)
library(writexl)
library(raster)
# library(sp)
# library(sf)
# library(rgdal)
library(rgeos)
library(maps)
library(stringr)
library(stringi)
library(geosphere)
# library(lwgeom)
library(caret)
library(randomForest)
library(mgcv)





### Read in Data ### 

loss_data_type <- "Economic"  # Economic  Insured
number_of_timesteps_post_landfall_included <- 6  # 6 12 40

year_range <- "(1979 - 2024)"

if (number_of_timesteps_post_landfall_included == 6) {
  landfall_hour_extension <- "12hr"
}
if (number_of_timesteps_post_landfall_included == 12) {
  landfall_hour_extension <- "24hr"
}
if (number_of_timesteps_post_landfall_included == 40) {
  landfall_hour_extension <- "Fullhr"
}
r_value <- "USA_R34_NE" # "USA_R50_NE", "USA_R64_NE", "USA_R34_NE"
r_value_colname <- "R34" # "R50", "R64", "R34"


loss_type <- "Average_Damage"  # Average_Damage NOAA_Billion
# 
# for (vhalf in seq(90, 170, 10)) {
# 
#   if (loss_type == "Average_Damage") {
#     hurricane_landfall_attributes <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships",
#                                                      "/OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
#                                                      landfall_hour_extension, "_ERA5_Storm_Surge_", r_value, "_Average_Damage_All_Loss_Estimates_Vhalf", vhalf, ".xlsx", sep = ""))
#   }
#   
#   if (loss_type == "NOAA_Billion") {
#     hurricane_landfall_attributes <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships", 
#                                                      "/OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
#                                                      landfall_hour_extension, "_ERA5_Storm_Surge_", r_value, "_NOAA_Damage.xlsx", sep = ""))
#   }
#   
#   
#   
#   test <- subset(hurricane_landfall_attributes, select = c(Year_Name, USA_WIND, Adjusted_Economic_Loss, LitPOP_Percent_Damage, Mean_Percent_Damage, GHSL_Percent_Damage))
#   
#   test <- na.omit(test)
#   
#   # Order by 'ID' column in ascending order
#   test_sorted <- test[order(test$GHSL_Percent_Damage), ]
#   
#   
#   comp <- data.frame(observed = rank(-test_sorted$Adjusted_Economic_Loss),
#                      predict = rank(-test_sorted$GHSL_Percent_Damage))
#   
#   print(vhalf)
#   print(round(cor(comp$observed, comp$predict), 2))
#   
# }
# 
# ggplot(data = comp, aes(x = observed, y = predict)) +
#   geom_point()




if (loss_type == "Average_Damage") {
  hurricane_landfall_attributes <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships",
                                                   "/OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
                                                   landfall_hour_extension, "_ERA5_Storm_Surge_", r_value, "_Average_Damage_All_Loss_Estimates_test.xlsx", sep = ""))
}

if (loss_type == "NOAA_Billion") {
  hurricane_landfall_attributes <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships",
                                                   "/OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
                                                   landfall_hour_extension, "_ERA5_Storm_Surge_", r_value, "_NOAA_Damage.xlsx", sep = ""))
}




## Convert landfall marker to another string ##
hurricane_landfall_attributes$Landfall_Marker <- gsub("Closest Bypasser", "CB", hurricane_landfall_attributes$Landfall_Marker)
hurricane_landfall_attributes$Landfall_Marker <- gsub("Pre - 1st Max Landfall", "LF Max", hurricane_landfall_attributes$Landfall_Marker)
hurricane_landfall_attributes$Landfall_Marker <- gsub("Pre - 2nd Max Landfall", "LF 2", hurricane_landfall_attributes$Landfall_Marker)
hurricane_landfall_attributes$Landfall_Marker <- gsub("Pre - 3rd Max Landfall", "LF 3", hurricane_landfall_attributes$Landfall_Marker)

## Create column with Year_Name_LandfallMarker ##
hurricane_landfall_attributes$Year_Name_LandfallMarker <- paste(hurricane_landfall_attributes$Year_Name, "_", hurricane_landfall_attributes$Landfall_Marker, sep = "")

### Subset data ###
if (loss_data_type == "Economic") {
  
  ## subset data ##
  
  if (loss_type == "Average_Damage") {
    ## Using normalised and present-day data ##
    original_columns <- c("USA_WIND", "SS_Category", "USA_PRES", "USA_R34_NE", "USA_RMW", "STORM_SPEED", "Storm_Tide",
                          "Rainfall_Sum_Accumilation", "Rainfall_Max_Rate", "Max_Rain_3hour_Accumilation",
                          "NOAA_Max_Rain_Accumilation",
                          "R34_Mean_Hurricane_Historic_Loss_Ratio_Building", "R34_Mean_Risk_Score",
                          "R34_Mean_Percent_Not_Resistant", "R34_Mean_Building_Year", 
                          "R34_Sum_Past_Building_Density",
                          "R34_Sum_Past_Population_Density",
                          "R34_Sum_Past_Housing_Units", 
                          "R34_Sum_Hurricane_Building_Value", "R34_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                          "LitPOP_Percent_Damage", "Mean_Percent_Damage", "GHSL_Percent_Damage",
                          "Adjusted_Economic_Loss")
    type_of_loss <- "Adjusted_Economic_Loss" # Weinkle_CL18 Weinkle_PL18 Unadjusted_Weinkle_Loss
  }
  if (loss_type == "NOAA_Billion") {
    ## Using normalised and present-day data ##
    original_columns <- c("USA_WIND", "SS_Category", "USA_PRES", "USA_R34_NE", "USA_RMW", "STORM_SPEED", "Storm_Tide",
                          "Rainfall_Sum_Accumilation", "Rainfall_Max_Rate", "Max_Rain_3hour_Accumilation",
                          "NOAA_Max_Rain_Accumilation",
                          "R34_Mean_Hurricane_Historic_Loss_Ratio_Building", "R34_Mean_Risk_Score",
                          "R34_Mean_Percent_Not_Resistant", "R34_Mean_Building_Year", 
                          "R34_Sum_Past_Building_Density",
                          "R34_Sum_Past_Population_Density",
                          "R34_Sum_Past_Housing_Units", 
                          "R34_Sum_Hurricane_Building_Value", "R34_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                          "LitPOP_Percent_Damage", "Mean_Percent_Damage",
                          "Adjusted_NOAA_Economic_Loss")
    type_of_loss <- "Adjusted_NOAA_Economic_Loss" # Weinkle_CL18 Weinkle_PL18 Unadjusted_Weinkle_Loss
  }
  
  # Replace "R34" with the value in r_value_colname
  modified_columns <- gsub("R34", r_value_colname, original_columns)
  # Subset the data frame using the modified column names
  hurricane_landfall_attributes_adjusted_loss <- subset(hurricane_landfall_attributes, select = modified_columns)
  
  hurricane_landfall_attributes <- hurricane_landfall_attributes_adjusted_loss
  
  ## Rename loss column ##
  colnames(hurricane_landfall_attributes)[colnames(hurricane_landfall_attributes) == type_of_loss] <- "Loss" 
  
}




#### --- Run Analysis --- ####

## omit storms with no loss estimate ##
hurricane_landfall_attributes <- hurricane_landfall_attributes %>% drop_na(Loss)

## make all cells in data frame as.numeric
hurricane_landfall_attributes$Storm_Tide <- as.numeric(hurricane_landfall_attributes$Storm_Tide)

## Replace -Inf with NA
# hurricane_landfall_attributes[hurricane_landfall_attributes == -Inf] <- NA

## remove NAs
hurricane_landfall_with_na <- hurricane_landfall_attributes[rowSums(is.na(hurricane_landfall_attributes)) > 0, ]
hurricane_landfall_attributes <- na.omit(hurricane_landfall_attributes)

hurricane_landfall_attributes_data <- hurricane_landfall_attributes








### ---- Correlation of each variable to loss ---- ###

## initialise data frames and lists
variables <- colnames(hurricane_landfall_attributes_data)


variables_short_name <- c("IBTrACS: Vmax", "IBTrACS: Saffir Simspon Cat.", "IBTrACS: Cp", paste("IBTrACS: ", r_value_colname, sep = ""), "IBTrACS: RMW", "IBTrACS: Storm Speed", "ERA5: Storm Surge",
                          "MSWEP: Total Rain Acc.", "MSWEP: Max Rain Rate", "MSWEP: Max Rain Acc.",
                          "NOAA: Max Rain Acc.",
                          "FEMA: HLR", "FEMA: Mean Risk Value", "FEMA: Mean Build Res.",
                          "ACS: Mean Build Year", "GHSL: Build Density", "WorldPOP: Pop Density", 
                          "ACS: Housing Units", "FEMA: Building Value", "LitPOP: Exposure Value", "Landfall State GDP",
                          "LitPOP Damaged", "Mean Damageability", "GHSL Damaged")

variables_minus_loss <- variables[1:length(variables)-1]
correlation_data_frame     <- data.frame(Variables = variables_minus_loss,
                                         Variables_Name = variables_short_name,
                                         Correlation = rep(NA, length(variables_minus_loss)))




# Calculate the 25th, 50th, and 75th percentiles
percentiles <- quantile(hurricane_landfall_attributes_data$Loss, probs = c(38.9/100, 63.9/100, 85.2/100, 97.2/100))
print(round(percentiles/10**9, 2))






#### --- Raw Values --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  historical_loss <- hurricane_landfall_attributes_data$Loss
  var_data <- hurricane_landfall_attributes_data[,var_index]
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss, method = "pearson"))) == TRUE) {
    print(var_index)
  }
}


## Sort correlation data frame for veiwing ## 
correlation_data_frame_sorted <- correlation_data_frame %>%
  arrange(desc(Correlation))

## Convert USA_Pressure to negative correlation ## 
USA_Pres_index <- which(correlation_data_frame_sorted$Variables == "USA_PRES")
correlation_data_frame_sorted$Correlation[USA_Pres_index] <- correlation_data_frame_sorted$Correlation[USA_Pres_index] * -1

if (loss_data_type == "Economic") {
  title <- expression(bold("a)") * " " * "Raw Hurricane Values")
}
if (loss_data_type == "Insured") {
  title <- expression(bold("c)") * " " * "Historical Normalised (to 2024) Industry-wide Insured Loss (1979 - 2024)")
}

## Sort correlation data frame for veiwing ## 
correlation_data_frame_sorted <- correlation_data_frame_sorted %>%
  arrange(desc(Correlation))

order_variables <- correlation_data_frame_sorted$Variables

ggplot(data = correlation_data_frame_sorted, aes(x = reorder(Variables_Name, Correlation), y = Correlation, fill = Correlation)) +
  
  scale_fill_gradient2(low = "darkblue", high = "darkred", midpoint = 0) +
    geom_label(
    aes(label = "      ",
        vjust = ifelse(Correlation > 0, 0.0, 1.0)),
    fill = "white",
    color = NA,
    size = 3.5,
    alpha = 0.8,
  ) +
  geom_text(aes(label = round(Correlation, 2), vjust = ifelse(Correlation > 0, -0.5, 1.5)), size = 3.5) +
  geom_col(color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
  guides(fill = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                   color = c("gold4", "brown3", "brown3", "gold4", "gold4", "brown3", "brown3", "brown3", "gold4",
                                             "turquoise4", "gold4", "turquoise4", "turquoise4", "turquoise4", "gold4", "turquoise4", "gold4",
                                             "gold4", "gold4", "gold4", "dodgerblue3", "gold4", "dodgerblue3", "brown3")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5, hjust = 0.5),
        axis.text = element_text(size=13),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 13),
        panel.grid.major.x  = element_line(color = "grey60",
                                           size = 0.3,
                                           linetype = 1),
        panel.grid.major.y  = element_line(color = "grey60",
                                           size = 0.3,
                                           linetype = 1),
        panel.grid.minor.y = element_line(color = "grey70",
                                          size = 0.1,
                                          linetype = 1),        
        plot.title = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
        axis.ticks.margin = unit(0.5, "cm"),
        axis.ticks = element_line(color = "black"),
        # plot.margin = unit(c(10, 10, 10, 30), "pt")
  ) +  # Set the margin of the ticks
  labs(x = "", y = "Correlation", title = title) +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2),
                     minor_breaks = seq(-1.0, 1.0, 0.1),
                     limits = c(-0.9, 0.9),
                     expand = c(0,0))

ggsave(paste("PLOTS/Figure_8a.png", sep = ""),
       width = 10, height = 6)

# ggsave(paste("PLOTS/Correlation/FINAL/Variable_Correlation_",
#              "_Loss__", landfall_hour_extension,
#              "_", r_value, "_", loss_type, "_raw_values.png", sep = ""),
#         width = 10, height = 6)


correlation_data_frame_sorted_raw_values <- correlation_data_frame_sorted




#### --- Rank Values --- ####
## loop through all variables and calc. correlation

correlation_data_frame     <- data.frame(Variables = variables_minus_loss,
                                         Variables_Name = variables_short_name,
                                         Correlation = rep(NA, length(variables_minus_loss)))

for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  historical_loss <- rank(hurricane_landfall_attributes_data$Loss)
  var_data <- rank(hurricane_landfall_attributes_data[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  dataframe_test <- data.frame(loss = historical_loss,
                               var = var_data)
  dataframe_test <- na.omit(dataframe_test)
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(dataframe_test$var, dataframe_test$loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss, method = "pearson"))) == TRUE) {
    print(var_index)
  }
}



## Sort Alphabetically ##
correlation_data_frame <- correlation_data_frame[order(correlation_data_frame$Variables_Name), ]

# correlation_data_frame_sorted <- correlation_data_frame %>%
#   arrange(desc(Correlation))

# Convert 'category' to a factor with this order
correlation_data_frame$Variables <- factor(correlation_data_frame$Variables, levels = order_variables)

# Order the data frame based on this ordered factor
correlation_data_frame_sorted <- correlation_data_frame[order(correlation_data_frame$Variables), ]

correlation_data_frame_sorted$custom_order_num <- seq(nrow(correlation_data_frame_sorted), 1, -1)



USA_Pres_index <- which(correlation_data_frame_sorted$Variables == "USA_PRES")
correlation_data_frame_sorted$Correlation[USA_Pres_index] <- correlation_data_frame_sorted$Correlation[USA_Pres_index] * -1


if (loss_data_type == "Economic") {
  title <- expression(bold("b)") * " " * "Ranked Hurricane Values")
}
if (loss_data_type == "Insured") {
  title <- expression(bold("d)") * " " * "Historical Normalised (to 2024) Industry-wide Insured Loss (1979 - 2024)")
}



ggplot(data = correlation_data_frame_sorted, aes(x = reorder(Variables_Name, Correlation), y = Correlation, fill = Correlation)) +
  
  scale_fill_gradient2(low = "darkblue", high = "darkred", midpoint = 0) +
  geom_label(
    aes(label = "      ",
        vjust = ifelse(Correlation > 0, 0.0, 1.0)),
    fill = "white",
    color = NA,
    size = 3.5,
    alpha = 0.8,
  ) +
  geom_text(aes(label = round(Correlation, 2), vjust = ifelse(Correlation > 0, -0.5, 1.5)), size = 3.5) +
  geom_col(color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
  guides(fill = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                   color = c("gold4", "gold4", "brown3", "brown3", "gold4", "brown3", "brown3", "brown3", "gold4",
                                             "gold4", "gold4", "gold4", "turquoise4", "turquoise4", "gold4", "turquoise4", "turquoise4",
                                             "turquoise4", "gold4", "brown3", "gold4", "gold4", "dodgerblue3", "dodgerblue3")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5, hjust = 0.5),
        axis.text = element_text(size=13),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 13),
        panel.grid.major.x  = element_line(color = "grey60",
                                           size = 0.3,
                                           linetype = 1),
        panel.grid.major.y  = element_line(color = "grey60",
                                           size = 0.3,
                                           linetype = 1),
        panel.grid.minor.y = element_line(color = "grey70",
                                          size = 0.1,
                                          linetype = 1), 
        plot.title = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
        axis.ticks.margin = unit(0.5, "cm"),
        axis.ticks = element_line(color = "black"),
        # plot.margin = unit(c(10, 10, 10, 30), "pt")
  ) +  # Set the margin of the ticks
  labs(x = "", y = "Correlation", title = title) +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2),
                     limits = c(-0.9, 0.9))

ggsave(paste("PLOTS/Figure_8b.png", sep = ""),
       width = 10, height = 6)

# ggsave(paste("PLOTS/Correlation/FINAL/Variable_Correlation_",
#              "_Loss__", landfall_hour_extension,
#              "_", r_value, "_", loss_type, "_rank_values.png", sep = ""),
#        width = 10, height = 6)

correlation_data_frame_sorted_rank_values <- correlation_data_frame_sorted





#### ---- Difference Plot: Raw Vs. Rank ---- ####

matching_indexes <- match(correlation_data_frame_sorted_raw_values$Variables_Name, correlation_data_frame_sorted_rank_values$Variables_Name)

correlation_diff_data_frame <- data.frame(Variables = correlation_data_frame_sorted_raw_values$Variables,
                                          Variables_Name = correlation_data_frame_sorted_raw_values$Variables_Name,
                                          Difference = rep(NA, length(correlation_data_frame_sorted_raw_values$Variables_Name)))

correlation_diff_data_frame$Difference <- correlation_data_frame_sorted_rank_values$Correlation[matching_indexes] - correlation_data_frame_sorted_raw_values$Correlation

USA_Pres_index <- which(correlation_diff_data_frame$Variables == "USA_PRES")
correlation_diff_data_frame$Difference[USA_Pres_index] <- correlation_diff_data_frame$Difference[USA_Pres_index] * -1

correlation_diff_data_frame_sorted <- correlation_diff_data_frame %>%
  arrange(desc(Difference))


if (loss_data_type == "Economic") {
  title <- expression(bold("c)") * " " * "Difference in Correlation: Ranked Minus Raw Hurricane Values")
}
if (loss_data_type == "Insured") {
  title <- expression(bold("e)") * " " * "Difference in Correlation: Raw Vs. Rank Values")
}

absolute_max_value <- max(abs(correlation_diff_data_frame_sorted$Difference))

if (absolute_max_value <= 0.2) {
  ymin <- -0.4
  ymax <- 0.4
}
if (between(absolute_max_value, 0.201, 0.4)) {
  ymin <- -0.6
  ymax <- 0.6
}
if (between(absolute_max_value, 0.401, 0.6)) {
  ymin <- -0.8
  ymax <- 0.8
}
if (absolute_max_value > 0.6) {
  ymin <- -1.0
  ymax <- 1.0
}


# Convert 'category' to a factor with this order
correlation_diff_data_frame_sorted$Variables <- factor(correlation_diff_data_frame_sorted$Variables, levels = order_variables)

# Order the data frame based on this ordered factor
correlation_diff_data_frame_sorted <- correlation_diff_data_frame_sorted[order(correlation_diff_data_frame_sorted$Variables), ]

correlation_diff_data_frame_sorted$custom_order_num <- seq(nrow(correlation_diff_data_frame_sorted), 1, -1)



ggplot(data = correlation_diff_data_frame_sorted, aes(x = reorder(Variables_Name, Difference), y = Difference, fill = Difference)) +
  
  scale_fill_gradient2(low = "darkblue", high = "darkred", midpoint = 0) +
  geom_label(
    aes(label = "      ",
        vjust = ifelse(Difference > 0, 0.0, 1.0)),
    fill = "white",
    color = NA,
    size = 3.5,
    alpha = 0.8,
  ) +
  geom_text(aes(label = round(Difference, 2), vjust = ifelse(Difference > 0, -0.5, 1.5)), size = 3.5) +
  geom_col(color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
  guides(fill = FALSE) +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                   color = c("gold4", "brown3", "brown3", "brown3", "brown3", "gold4", "gold4", "gold4", "gold4",
                                             "brown3", "gold4", "gold4", "gold4", "gold4", "gold4", "dodgerblue3", "dodgerblue3",
                                             "gold4", "gold4", "turquoise4", "turquoise4", "turquoise4", "turquoise4", "turquoise4")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5, hjust = 0.5),
        axis.text = element_text(size=13),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 13),
        panel.grid.major.x  = element_line(color = "grey60",
                                           size = 0.3,
                                           linetype = 1),
        panel.grid.major.y  = element_line(color = "grey60",
                                           size = 0.3,
                                           linetype = 1),
        panel.grid.minor.y = element_line(color = "grey70",
                                          size = 0.1,
                                          linetype = 1), 
        plot.title = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
        axis.ticks.margin = unit(0.5, "cm"),
        axis.ticks = element_line(color = "black"),
        # plot.margin = unit(c(10, 10, 10, 30), "pt")
        ) +  # Set the margin of the ticks
  labs(x = "", y = "Correlation\nDifference", title = title) +
  scale_y_continuous(
    breaks =  round(seq(-0.4, 0.4, 0.1), 2),
    limits = c(-0.4, 0.4))

ggsave(paste("PLOTS/Figure_8c.png", sep = ""),
       width = 10, height = 6)

# ggsave(paste("PLOTS/Correlation/FINAL//Difference_Variable_Correlation_",
#              "_Loss__", landfall_hour_extension,
#              "_", r_value, "_", loss_type, "_raw_values.png", sep = ""),
#        width = 10, height = 6)




















# #### --- Normalised Rank Values --- ####
# ### Normalise rank is the same as having the raw values, as you keep the distances between the values ###
# which(variables_minus_loss == "R34_Sum_Present_Housing_Units")
# 
# variables_minus_loss <- variables[1:length(variables)-1]
# correlation_data_frame     <- data.frame(Variables = variables_minus_loss,
#                                          Variables_Name = variables_short_name,
#                                          Correlation = rep(NA, length(variables_minus_loss)))
# 
# 
# ## Loop through all variables and calculate correlation
# for (var_index in seq(1, length(variables_minus_loss), 1)) {
#   
#   # Normalize the Loss column between 0 and 1
#   historical_loss_ranked <- (hurricane_landfall_attributes_data$Loss - min(hurricane_landfall_attributes_data$Loss)) / 
#     (max(hurricane_landfall_attributes_data$Loss) - min(hurricane_landfall_attributes_data$Loss))
#   
#   # Normalize the variable column between 0 and 1
#   var_data_ranked <- (hurricane_landfall_attributes_data[, var_index] - 
#                                min(hurricane_landfall_attributes_data[, var_index], na.rm = TRUE)) / 
#     (max(hurricane_landfall_attributes_data[, var_index], na.rm = TRUE) - 
#        min(hurricane_landfall_attributes_data[, var_index], na.rm = TRUE))
#   
#   # # Rank the normalized values
#   # historical_loss_ranked <- rank(historical_loss)
#   # var_data_ranked <- rank(var_data)
#   
#   # Calculate the correlation
#   correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data_ranked, historical_loss_ranked)), 2)
#   correlation_data_frame$Variables[var_index] <- colnames(hurricane_landfall_attributes_data[, var_index])
#   
#   
#   if (is.na(correlation_data_frame$Correlation[var_index])) {
#     print(var_index)
#   }
# }
# 
# 
# ## Sort Alphabetically ##
# type_of_loss
# correlation_data_frame <- correlation_data_frame[order(correlation_data_frame$Variables_Name), ]
# 
# 
# 
# correlation_data_frame_sorted <- correlation_data_frame %>%
#   arrange(desc(Correlation))
# 
# USA_Pres_index <- which(correlation_data_frame_sorted$Variables == "USA_PRES")
# correlation_data_frame_sorted$Correlation[USA_Pres_index] <- correlation_data_frame_sorted$Correlation[USA_Pres_index] * -1
# 
# if (loss_data_type == "Economic") {
#   title <- expression(bold("c)") * " " * "Historical Normalised (to 2024) Economic Loss (1979 - 2024)")
# }
# if (loss_data_type == "Insured") {
#   title <- expression(bold("f)") * " " * "Historical Normalised (to 2024) Industry-wide Insured Loss (1979 - 2024)")
# }
# 
# 
# ggplot(data = correlation_data_frame_sorted, aes(x = reorder(Variables_Name, Correlation), y = Correlation, fill = Correlation)) +
#   geom_col(color = "black") +
#   guides(fill = FALSE) +
#   geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
#   scale_fill_gradient2(low = "darkblue", high = "darkred", midpoint = 0) +
#   geom_text(aes(label = round(Correlation, 2), vjust = ifelse(Correlation > 0, -0.5, 1.5)), size = 3.5) +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         axis.title.x = element_blank(),
#         axis.text = element_text(size=13),
#         panel.background = element_rect(fill = "white", color = "black"),
#         plot.background = element_rect(fill = "white", color = "white"),
#         axis.line.y = element_line(color = "black"),
#         axis.line.x = element_line(color = "black"),
#         axis.text.y = element_text(color = "black", size = 13),
#         panel.grid.minor.y = element_blank(),
#         plot.title = element_text(size = 15),
#         axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
#         axis.ticks.margin = unit(0.5, "cm"),
#         axis.ticks = element_line(color = "black")) +  # Set the margin of the ticks
#   labs(x = "", y = "Correlation: Percentile Rank", title = title) +
#   scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2),
#                      limits = c(-0.9, 0.9))
# 
# 
# ggsave(paste("PLOTS/Correlation/Variable_Correlation_", loss_data_type,
#              "_Loss_Normalised_Rank_", landfall_hour_extension, "_", normalise_or_unnormalised, 
#              "_", type_of_loss, ".png", sep = ""),
#        width = 8, height = 6)










##### ---- UNCERTAINTY ---- #####


## Length of track for social footprints ##
Fullhr <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                          "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                          "Fullhr", "_ERA5_Storm_Surge", "_", "USA_R34_NE_Average_Damage_All_Loss_Estimates",
                          ".xlsx", sep = ""))

Half_Day_Footprint   <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                                        "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                                        "12hr", "_ERA5_Storm_Surge", "_", "USA_R34_NE_Average_Damage_All_Loss_Estimates",
                                        ".xlsx", sep = ""))

colnames(Fullhr)[colnames(Fullhr) == "Adjusted_Economic_Loss"]                         <- "Loss"
colnames(Half_Day_Footprint)[colnames(Half_Day_Footprint) == "Adjusted_Economic_Loss"] <- "Loss" 

Fullhr               <- Fullhr %>% drop_na(Loss)
Half_Day_Footprint   <- Half_Day_Footprint %>% drop_na(Loss)

Fullhr <- subset(Fullhr, select = c(R34_Mean_Hurricane_Historic_Loss_Ratio_Building, R34_Mean_Risk_Score,
                                    R34_Mean_Percent_Not_Resistant, R34_Mean_Building_Year,
                                    R34_Sum_PresentDay_Building_Density,
                                    R34_Sum_PresentDay_Population_Density, 
                                    R34_Sum_Past_Housing_Units, R34_Sum_Present_Housing_Units, 
                                    R34_Sum_LitPOP_Exposure_Value, GDP_Per_State,
                                    LitPOP_Percent_Damage, Mean_Percent_Damage, GHSL_Percent_Damage,
                                    
                                    Loss))

Half_Day_Footprint <- subset(Half_Day_Footprint, select = c(R34_Mean_Hurricane_Historic_Loss_Ratio_Building, R34_Mean_Risk_Score,
                                                            R34_Mean_Percent_Not_Resistant, R34_Mean_Building_Year,
                                                            R34_Sum_PresentDay_Building_Density,
                                                            R34_Sum_PresentDay_Population_Density, 
                                                            R34_Sum_Past_Housing_Units, R34_Sum_Present_Housing_Units, 
                                                            R34_Sum_LitPOP_Exposure_Value, GDP_Per_State,
                                                            LitPOP_Percent_Damage, Mean_Percent_Damage, GHSL_Percent_Damage,
                                                            
                                                            Loss))

## initialise data frames and lists
variables <- colnames(Fullhr)

variables_short_name <- c("FEMA: HLR", "FEMA: Mean RV", "FEMA: Mean Build Res.",
                          "ACS: Mean Build Year", "GHSL: Build Density", "WorldPop: Pop Density", 
                          "ACS: Housing Units", "FEMA: Building Value", "LitPOP: Exposure Value", "Landfall State GDP",
                          "LitPOP: Exposure Damage", "Mean Damageability", "GHSL: Buildings Damaged")

## Day_Footprint ##
variables_minus_loss       <- variables[1:length(variables)-1]
correlation_data_frame     <- data.frame(Variables = variables_minus_loss,
                                         Variables_Name = variables_short_name,
                                         Correlation = rep(NA, length(variables_minus_loss)))

#### --- Rank Values - 6hr --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  historical_loss <- rank(Half_Day_Footprint$Loss)
  var_data <- rank(Half_Day_Footprint[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "12hr"
correlation_data_frame_all <- correlation_data_frame

#### --- Rank Values - Fullhr --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  ## reconstructed ##
  historical_loss <- rank(Fullhr$Loss)
  var_data <- rank(Fullhr[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "Fullhr"
correlation_data_frame_all <- rbind(correlation_data_frame_all, correlation_data_frame)

# Order the data frame by 'Correlation', keeping groups together
ordered_data <- correlation_data_frame_all %>%
  arrange(Group, Correlation)  # Arrange by Group and then by Correlation in descending order

# Create a new factor for Variables_Name based on the ordered data
ordered_data$Variables_Name <- factor(ordered_data$Variables_Name, 
                                      levels = unique(ordered_data$Variables_Name))

title <- expression(bold("a)") * " " * "Correlation Test: Length of Hurricane Track")


# ordered_data <- ordered_data[ordered_data$Group != "24hr", ]


ggplot(data = ordered_data, aes(x = Variables_Name, y = Correlation, fill = Group)) +
  
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  # guides(fill = FALSE) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
  geom_text(aes(label = Correlation), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  # Adjust vertical position of labels
            size = 3.5) +   # Adjust size of labels
  
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.y = element_text(color = "black", size = 13, angle = 0, hjust=0.5, vjust=0.5),
        legend.text = element_text(size = 15),
        # legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = c(0.09, 0.82),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
        axis.ticks.margin = unit(0.5, "cm"),
        axis.ticks = element_line(color = "black")) +  # Set the margin of the ticks
  
  labs(x = "", y = "Correlation: \nRank", title = title, fill = "Track Length:") +
  scale_y_continuous(breaks = seq(0.0, 1.0, 0.2),
                     limits = c(0.0, 0.9)) +
  # scale_fill_manual(values = c("12hr" = "coral", "24hr" = "dodgerblue3", "Fullhr" = "darkseagreen"))  # Replace with your actual group names and desired colors
  scale_fill_manual(values = c("12hr" = "coral", "Fullhr" = "dodgerblue3"))  # Replace with your actual group names and desired colors


ggsave(paste("PLOTS/Correlation/FINAL/Uncertainty/Uncertainty_Variable_Correlation_", loss_data_type,
             "_Loss_Rank_", "_Track_Length.png", sep = ""),
       width = 14, height = 6)



## --------- Unadjusted Vs. Adjusted --------- ##

hurricane_landfall_attributes   <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                                                   "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                                                   "12hr", "_ERA5_Storm_Surge", "_", "USA_R34_NE_Average_Damage_All_Loss_Estimates",
                                                   ".xlsx", sep = ""))

## Using un-normalised and historical data ##
unadjusted <- subset(hurricane_landfall_attributes, select = c("USA_WIND", "SS_Category", "USA_PRES", "USA_R34_NE", "USA_RMW", "STORM_SPEED", "Storm_Tide",
                                                               "Rainfall_Sum_Accumilation", "Rainfall_Max_Rate", "Max_Rain_3hour_Accumilation",
                                                               "NOAA_Max_Rain_Accumilation",
                                                               "R34_Mean_Hurricane_Historic_Loss_Ratio_Building", "R34_Mean_Risk_Score",
                                                               "R34_Mean_Percent_Not_Resistant", "R34_Mean_Building_Year", 
                                                               "R34_Sum_Past_Building_Density",
                                                               "R34_Sum_Past_Population_Density",
                                                               "R34_Sum_Past_Housing_Units", 
                                                               "R34_Sum_Hurricane_Building_Value", "R34_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                                                               "LitPOP_Percent_Damage", "Mean_Percent_Damage", "GHSL_Percent_Damage",
                                                               "Unadjusted_Loss"))
## Using normalised and present-day data ##
adjusted   <- subset(hurricane_landfall_attributes, select = c("USA_WIND", "SS_Category", "USA_PRES", "USA_R34_NE", "USA_RMW", "STORM_SPEED", "Storm_Tide",
                                                               "Rainfall_Sum_Accumilation", "Rainfall_Max_Rate", "Max_Rain_3hour_Accumilation",
                                                               "NOAA_Max_Rain_Accumilation",
                                                               "R34_Mean_Hurricane_Historic_Loss_Ratio_Building", "R34_Mean_Risk_Score",
                                                               "R34_Mean_Percent_Not_Resistant", "R34_Mean_Building_Year", 
                                                               "R34_Sum_Past_Building_Density",
                                                               "R34_Sum_Past_Population_Density",
                                                               "R34_Sum_Past_Housing_Units", 
                                                               "R34_Sum_Hurricane_Building_Value", "R34_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                                                               "LitPOP_Percent_Damage", "Mean_Percent_Damage", "GHSL_Percent_Damage",
                                                               "Adjusted_Economic_Loss"))

colnames(unadjusted)[colnames(unadjusted) == "Unadjusted_Loss"]        <- "Loss"
colnames(adjusted)[colnames(adjusted) == "Adjusted_Economic_Loss"]     <- "Loss"

unadjusted <- unadjusted %>% drop_na(Loss)
adjusted   <- adjusted %>% drop_na(Loss)

unadjusted <- na.omit(unadjusted)
adjusted   <- na.omit(adjusted)

## initialise data frames and lists
variables <- colnames(unadjusted)
variables_short_name <- c("IBTrACS: Vmax", "Saffir Simspon Cat.", "IBTrACS: Cp", paste("IBTrACS: ", r_value_colname, sep = ""), "IBTrACS: RMW", "IBTrACS: Storm Speed", "ERA5: Storm Surge",
                          "MSWEP: Total Rain Acc.", "MSWEP: Max Rain Rate", "MSWEP: Max Rain Acc.",
                          "NOAA: Max Rain Acc.",
                          "FEMA: HLR", "FEMA: Mean RV", "FEMA: Mean Build Res.",
                          "ACS: Mean Build Year", "GHSL: Build Density", "WorldPop: Pop Density", 
                          "ACS: Housing Units", "FEMA: Building Value", "LitPOP: Exposure Value", "Landfall State GDP",
                          "LitPOP: Exposure Damage", "Mean Damageability", "GHSL: Buildings Damaged")

## Unadjusted ##
variables_minus_loss       <- variables[1:length(variables)-1]
correlation_data_frame     <- data.frame(Variables = variables_minus_loss,
                                         Variables_Name = variables_short_name,
                                         Correlation = rep(NA, length(variables_minus_loss)))

#### --- Rank Values - Unadjusted --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  historical_loss <- rank(unadjusted$Loss)
  var_data <- rank(unadjusted[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "Un-Normalised"
correlation_data_frame_all <- correlation_data_frame


#### --- Rank Values - Adjusted --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  ## reconstructed ##
  historical_loss <- rank(adjusted$Loss)
  var_data <- rank(adjusted[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "Normalised"
correlation_data_frame_all <- rbind(correlation_data_frame_all, correlation_data_frame)

Cp_indexes <- which(correlation_data_frame_all$Variables_Name == "IBTrACS: Cp")
correlation_data_frame_all$Correlation[Cp_indexes] <- correlation_data_frame_all$Correlation[Cp_indexes] * -1


# Order the data frame by 'Correlation', keeping groups together
ordered_data <- correlation_data_frame_all %>%
  arrange(Group, Correlation)  # Arrange by Group and then by Correlation in descending order

# Create a new factor for Variables_Name based on the ordered data
ordered_data$Variables_Name <- factor(ordered_data$Variables_Name, 
                                      levels = unique(ordered_data$Variables_Name))

title <- expression(bold("b)") * " " * "Correlation Test: Normalised Vs. Un-Normalised")


print(mean(abs(ordered_data[ordered_data$Group == "Normalised",]$Correlation)))
print(mean(abs(ordered_data[ordered_data$Group == "Un-Normalised",]$Correlation)))


ggplot(data = ordered_data, aes(x = Variables_Name, y = Correlation, fill = Group)) +
  
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  # guides(fill = FALSE) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
  geom_text(aes(label = Correlation), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  # Adjust vertical position of labels
            size = 2.5) +   # Adjust size of labels
  
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.y = element_text(color = "black", size = 13, angle = 0, hjust=0.5, vjust=0.5),
        legend.text = element_text(size = 15),
        # legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = c(0.905, 0.16),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
        axis.ticks.margin = unit(0.5, "cm"),
        axis.ticks = element_line(color = "black")) +  # Set the margin of the ticks
  labs(x = "", y = "Correlation: \nRank", title = title, fill = "Nomalisation:") +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.2),
                     limits = c(-0.9, 0.9)) +
  scale_fill_manual(values = c("Normalised" = "pink", "Un-Normalised" = "darkorchid1"))  # Replace with your actual group names and desired colors


ggsave(paste("PLOTS/Correlation/FINAL/Uncertainty/Uncertainty_Variable_Correlation_", loss_data_type,
             "_Loss_Rank_", "Normalise_Vs_Unnormalised", ".png", sep = ""),
       width = 18, height = 6)







## Storm_Size ##
R34 <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                       "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                       "12hr", "_ERA5_Storm_Surge", "_", "USA_R34_NE_Average_Damage_All_Loss_Estimates",
                       ".xlsx", sep = ""))
R50 <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                       "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                       "12hr", "_ERA5_Storm_Surge", "_", "USA_R50_NE_Average_Damage_All_Loss_Estimates",
                       ".xlsx", sep = ""))
R64 <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                       "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                       "12hr", "_ERA5_Storm_Surge", "_", "USA_R64_NE_Average_Damage_All_Loss_Estimates",
                       ".xlsx", sep = ""))

colnames(R34)[colnames(R34) == "Adjusted_Economic_Loss"] <- "Loss"
colnames(R50)[colnames(R50) == "Adjusted_Economic_Loss"] <- "Loss" 
colnames(R64)[colnames(R64) == "Adjusted_Economic_Loss"] <- "Loss" 

R34   <- R34 %>% drop_na(Loss)
R50   <- R50 %>% drop_na(Loss)
R64   <- R64 %>% drop_na(Loss)



R34 <- subset(R34, select = c("USA_R34_NE",
                              "R34_Mean_Hurricane_Historic_Loss_Ratio_Building", "R34_Mean_Risk_Score",
                              "R34_Mean_Percent_Not_Resistant", "R34_Mean_Building_Year", 
                              "R34_Sum_Past_Building_Density",
                              "R34_Sum_Past_Population_Density",
                              "R34_Sum_Past_Housing_Units", 
                              "R34_Sum_Hurricane_Building_Value", "R34_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                              "LitPOP_Percent_Damage", "Mean_Percent_Damage", "GHSL_Percent_Damage",
                              
                              "Loss"))

R50 <- subset(R50, select = c("USA_R50_NE",
                              "R50_Mean_Hurricane_Historic_Loss_Ratio_Building", "R50_Mean_Risk_Score",
                              "R50_Mean_Percent_Not_Resistant", "R50_Mean_Building_Year", 
                              "R50_Sum_Past_Building_Density",
                              "R50_Sum_Past_Population_Density",
                              "R50_Sum_Past_Housing_Units", 
                              "R50_Sum_Hurricane_Building_Value", "R50_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                              "LitPOP_Percent_Damage", "Mean_Percent_Damage", "GHSL_Percent_Damage",
                              
                              "Loss"))

R64 <- subset(R64, select = c("USA_R64_NE",
                              "R64_Mean_Hurricane_Historic_Loss_Ratio_Building", "R64_Mean_Risk_Score",
                              "R64_Mean_Percent_Not_Resistant", "R64_Mean_Building_Year", 
                              "R64_Sum_Past_Building_Density",
                              "R64_Sum_Past_Population_Density",
                              "R64_Sum_Past_Housing_Units", 
                              "R64_Sum_Hurricane_Building_Value", "R64_Sum_LitPOP_Exposure_Value", "GDP_Per_State",
                              "LitPOP_Percent_Damage", "Mean_Percent_Damage", "GHSL_Percent_Damage",
                              
                              "Loss"))

## initialise data frames and lists
variables <- colnames(R34)

variables_short_name <- c("IBTrACS: Storm Size", "FEMA: HLR", "FEMA: Mean RV", "FEMA: Mean Build Res.",
                          "ACS: Mean Build Year", "GHSL: Build Density", "WorldPop: Pop Density", 
                          "ACS: Housing Units", "FEMA: Building Value", "LitPOP: Exposure Value", "Landfall State GDP",
                          "LitPOP: Exposure Damage", "Mean Damageability", "GHSL: Buildings Damaged")

## Day_Footprint ##
variables_minus_loss       <- variables[1:length(variables)-1]
correlation_data_frame     <- data.frame(Variables = variables_minus_loss,
                                         Variables_Name = variables_short_name,
                                         Correlation = rep(NA, length(variables_minus_loss)))

#### --- Rank Values - R34 --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  historical_loss <- rank(R34$Loss)
  var_data <- rank(R34[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss, method = "pearson"))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "R34"
correlation_data_frame_all <- correlation_data_frame

#### --- Rank Values - R50 --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  ## reconstructed ##
  historical_loss <- rank(R50$Loss)
  var_data <- rank(R50[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "R50"
correlation_data_frame_all <- rbind(correlation_data_frame_all, correlation_data_frame)

#### --- Rank Values - R64 --- ####
## loop through all variables and calc. correlation
for (var_index in seq(1, length(variables_minus_loss), 1)) {
  
  ## reconstructed ##
  historical_loss <- rank(R64$Loss)
  var_data <- rank(R64[,var_index])
  
  correlation_data_frame$Correlation[var_index] <- round(abs(cor(var_data, historical_loss, method = "pearson")), 2)
  
  if (is.na(abs(cor(var_data, historical_loss))) == TRUE) {
    print(var_index)
  }
}

correlation_data_frame$Group <- "R64"
correlation_data_frame_all <- rbind(correlation_data_frame_all, correlation_data_frame)

# Order the data frame by 'Correlation', keeping groups together
ordered_data <- correlation_data_frame_all %>%
  arrange(Group, Correlation)  # Arrange by Group and then by Correlation in descending order

# Create a new factor for Variables_Name based on the ordered data
ordered_data$Variables_Name <- factor(ordered_data$Variables_Name, 
                                      levels = unique(ordered_data$Variables_Name))

title <- expression(bold("c)") * " " * "Correlation Test: Storm Size")



ggplot(data = ordered_data, aes(x = Variables_Name, y = Correlation, fill = Group)) +
  
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  # guides(fill = FALSE) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size  = 1) +
  geom_text(aes(label = Correlation), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,  # Adjust vertical position of labels
            size = 2.5) +   # Adjust size of labels
  
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(color = "black", size = 13),
        axis.title.y = element_text(color = "black", size = 13, angle = 0, hjust=0.5, vjust=0.5),
        legend.text = element_text(size = 15),
        # legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = c(0.07, 0.8),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 15),
        axis.ticks.length = unit(0.3, "cm"),  # Set the length of the ticks
        axis.ticks.margin = unit(0.5, "cm"),
        axis.ticks = element_line(color = "black")) +  # Set the margin of the ticks
  
  labs(x = "", y = "Correlation:\nRank", title = title, fill = "Storm Size:") +
  scale_y_continuous(breaks = seq(0.0, 1.0, 0.2),
                     limits = c(0.0, 0.9)) +
  scale_fill_manual(values = c("R34" = "orange2", "R50" = "brown4", "R64" = "chocolate"))  # Replace with your actual group names and desired colors


ggsave(paste("PLOTS/Correlation/FINAL/Uncertainty/Uncertainty_Variable_Correlation_", loss_data_type,
             "_Loss_Rank_", "12hr", "_Storm_Size.png", sep = ""),
       width = 14, height = 6)













test <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                        "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                        "Fullhr", "_ERA5_Storm_Surge", "_", "USA_R34_NE",
                        ".xlsx", sep = ""))

test_1 <- read_xlsx(paste("~/Hurricane_Attributes_and_Loss_Relationships/OUTPUT/Storm_Size/", 
                          "USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_", 
                          "Fullhr", "_ERA5_Storm_Surge", "_", "USA_R34_NE_NO_REPLACEMENT",
                          ".xlsx", sep = ""))

test <- subset(test, select = c(USA_WIND, USA_PRES, USA_R34_NE, Rainfall_Accumilation,
                                Storm_Tide,
                                R34_Mean_Hurricane_Historic_Loss_Ratio_Building,
                                R34_Mean_Building_Year, 
                                
                                R34_Sum_PresentDay_Building_Density,
                                R34_Sum_PresentDay_Population_Density,
                                R34_Sum_Present_Housing_Units, 
                                R34_Sum_Hurricane_Building_Value, R34_Sum_LitPOP_Exposure_Value,
                                
                                Adjusted_Average_Economic_Loss))

test_1 <- subset(test_1, select = c(USA_WIND, USA_PRES, USA_R34_NE, Rainfall_Accumilation,
                                    Storm_Tide,
                                    R34_Mean_Hurricane_Historic_Loss_Ratio_Building,
                                    R34_Mean_Building_Year, 
                                    
                                    R34_Sum_PresentDay_Building_Density,
                                    R34_Sum_PresentDay_Population_Density,
                                    R34_Sum_Present_Housing_Units, 
                                    R34_Sum_Hurricane_Building_Value, R34_Sum_LitPOP_Exposure_Value,
                                    
                                    Adjusted_Average_Economic_Loss))

test   <- na.omit(test)
test_1 <- na.omit(test_1)


test$R34_Sum_LitPOP_Exposure_Value - test_1$R34_Sum_LitPOP_Exposure_Value
