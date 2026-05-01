

setwd("~/Hurricane_Attributes_and_Loss_Relationships")

###### Set working directory ######
directory <- "~/Hurricane_Attributes_and_Loss_Relationships"



###### stop R from outputting Warning Messages ###### 
options(warn=-1)




###### load R libraries for analysis ###### 
library("leaflet")
#library("mapview")
library("stringr")
# library("sf")
library("htmlwidgets")
library("readxl")
library("dplyr")
# library("sp")
library("maptools")
library("maps")
library("webshot")
library("ggmap")
library("rworldmap")
# library("rgdal")
library("writexl")
# library("pracma")
# library("NLP")
library("tidyr")



###### ----- 1) Read in Model Input Data ----- ###### 

loss_data_type <- "Economic"  # Economic or Insured
landfall_hour_extension <- "Fullhr"
r_value <- "USA_R34_NE" # "USA_R50_NE", "USA_R64_NE", "USA_R34_NE"
r_value_colname <- "R34" # "R50", "R64", "R34"

###### ------- Read in data ##

input_file <- "OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_12hr_ERA5_Storm_Surge_USA_R34_NE_Average_Damage_All_Loss_Estimates.xlsx"

# input_file <- (paste("~/Hurricane_Attributes_and_Loss_Relationships",
#                                                  "/OUTPUT/Storm_Size/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_",
#                                                  landfall_hour_extension, "_ERA5_Storm_Surge_", r_value, "_Average_Damage.xlsx", sep = ""))

# input_file <- paste(directory, "/OUTPUT/USA_Hurricane_Landfall_Attributes_Multiple_Landfalls_WINDONLY.xlsx", sep = "")
data <- read_xlsx(input_file)


data <- subset(data, select = c(Year_Name, Landfall_Marker, Adjusted_Economic_Loss, Unadjusted_Loss,
                         
                                  USA_WIND, SS_Category,
                                  
                                  USA_PRES, USA_R34_NE, USA_RMW, STORM_SPEED,
                                  Storm_Tide,
                                  Rainfall_Sum_Accumilation, Rainfall_Max_Rate, Max_Rain_3hour_Accumilation,
                                  NOAA_Max_Rain_Accumilation,
                                  R34_Sum_Hurricane_Building_Value, R34_Mean_Hurricane_Historic_Loss_Ratio_Building, R34_Mean_Risk_Score,
                                  R34_Mean_Percent_Not_Resistant, R34_Mean_Building_Year,
                                  R34_Sum_PresentDay_Building_Density, R34_Sum_Past_Building_Density,
                                  R34_Sum_PresentDay_Population_Density, R34_Sum_Past_Population_Density,
                                  R34_Sum_Past_Housing_Units, R34_Sum_Present_Housing_Units,
                                  R34_Sum_LitPOP_Exposure_Value, GDP_Per_State
                                  ))







# data <- subset(data, select = c(Adjusted_Economic_Loss, SS_Category, USA_WIND))
 
loss_data_with_na <- data[rowSums(is.na(data)) > 0, ]

loss_data <- data %>% drop_na(Adjusted_Economic_Loss)


data$Year <- substring(data$Year_Name, 1, 4)

## Convert loss to $ bn and change Weinkle / PCS colname to a common colname ##
if (loss_data_type == "Economic") {

  data             <- subset(data, select = c(Year, Year_Name, USA_WIND, Adjusted_Economic_Loss, USA_WIND))
  colnames(data)[colnames(data) == "Adjusted_Economic_Loss"] <- "Loss"
  data$Loss             <- data$Loss/ 10 ** 9
  
}

if (loss_data_type == "Insured") {
  data$Loss             <- data$Normalised_2023_PCS_Loss / 10 ** 9
  ## Remove PCS Column ##
  data             <- subset(data, select = c(Year, Year_Name, USA_WIND, Normalised_2023_PCS_Loss, USA_WIND))
  colnames(data)[colnames(data) == "Normalised_2023_PCS_Loss"] <- "Loss"
}





###### ----- Plot SS Against Loss ----- ###### 

data$SS_Category <- rep(NA, nrow(data))


## assign landfall category ##
for (storm_index in seq(1, nrow(data), 1)) {
  
  storm_vmax <- data$USA_WIND[storm_index]
  
  if (storm_vmax < 64.0) {
    data$SS_Category[storm_index] <- as.numeric(0)
  }
  if (between(storm_vmax, 64, 84.0)) {
    data$SS_Category[storm_index] <- as.numeric(1)
  }
  if (between(storm_vmax, 84.00001, 96.0)) {
    data$SS_Category[storm_index] <- as.numeric(2)
  }
  if (between(storm_vmax, 96.00001, 114.0)) {
    data$SS_Category[storm_index] <- as.numeric(3)
  }
  if (between(storm_vmax, 114.00001, 135.0)) {
    data$SS_Category[storm_index] <- as.numeric(4)
  }
  if (storm_vmax >= 135.0) {
    data$SS_Category[storm_index] <- as.numeric(5)
  }
}



#### Percentage of historical loss per Saffir Simpson Cat. ####

sum(data[data$SS_Category == 0, ]$Loss, na.rm=T) / sum(data$SS_Category, na.rm=T)
sum(data[data$SS_Category == 1, ]$Loss, na.rm=T) / sum(data$SS_Category, na.rm=T)
sum(data[data$SS_Category == 2, ]$Loss, na.rm=T) / sum(data$SS_Category, na.rm=T)
sum(data[data$SS_Category == 3, ]$Loss, na.rm=T) / sum(data$SS_Category, na.rm=T)
sum(data[data$SS_Category == 4, ]$Loss, na.rm=T) / sum(data$SS_Category, na.rm=T)
sum(data[data$SS_Category == 5, ]$Loss, na.rm=T) / sum(data$SS_Category, na.rm=T)


if (loss_data_type == "Economic") {
  title_SS <- expression(bold("a)") * " " * "Saffir Simpson Scale Vs. Normalised Economic Loss (1979 - 2024)")
  title_Vmax <- expression(bold("b)") * " " * "Vmax Vs. Normalised Economic Loss (1979 - 2024)")
  y_title <- "Financial\nLoss\n(US$ bn)"
  save_file_SS <- paste(directory, "/PLOTS/Economic_Loss/Saffir_Simpson_Scale_Versus_Economic_Loss.png", sep = "")
  save_file_Vmax <- paste(directory, "/PLOTS/Economic_Loss/Landfall_Vmax_Versus_Economic_Loss.png", sep = "")
  ymax <- 280.0
  x_intervals <- 20
  RMSE_label_position_y <- 235
  RMSE_label_position_x_SS <- 0.55
  RMSE_label_position_x_Vmax <- 50
  color <- "dodgerblue3"
}
if (loss_data_type == "Insured") {
  title_SS <- expression(bold("b)") * " " * "Historical Normalised (to 2023) Industry-wide Insured Loss (1950 - 2023)")
  title_Vmax <- expression(bold("d)") * " " * "Historical Normalised (to 2023) Indsury-wide Insured Loss (1950 - 2023)")
  y_title <- "Financial Loss (US$ bn)"
  save_file_SS <- paste(directory, "/PLOTS/Insured_Loss/Saffir_Simpson_Scale_Versus_Insured_Loss.png", sep = "")
  save_file_Vmax <- paste(directory, "/PLOTS/Insured_Loss/Landfall_Vmax_Versus_Insured_Loss.png", sep = "")
  ymax <- 100.0
  x_intervals <- 10
  RMSE_label_position_y <- 81
  RMSE_label_position_x_SS <- 0.4
  RMSE_label_position_x_Vmax <- 50
  color <- "brown1"
}


# Custom rounding function
round_with_decimal <- function(x, digits = 0) {
  rounded <- round(x, digits)
  if (rounded == floor(rounded)) {
    return(sprintf("%0.*f", digits, rounded))
  } else {
    return(as.character(rounded))
  }
}


## Saffir Simpson Linear Model ##

data <- subset(data, select = c(Loss, SS_Category, USA_WIND))
data <- data %>% drop_na(Loss)


linear_model = lm(Loss ~ SS_Category, data = data) #Create a linear regression with two variables
linear_predictions <- predict(linear_model, new_data = data$SS_Category)

MAE <- mean(abs(data$Loss - linear_predictions))
max_abs_error <- round_with_decimal(max(abs(data$Loss - linear_predictions), na.rm = T), 1)
correlation <- cor(data$SS_Category, data$Loss)
number_of_storms <- nrow(data)


data_linear_projection <- data.frame(Loss = linear_predictions,
                                     SS_Category = data$SS_Category)
# Assuming data_linear_projection is your data frame
filtered_data <- data_linear_projection %>%
  filter(SS_Category %in% c(0, 5))
# Get unique rows
filtered_data <- filtered_data %>%
  distinct()

# 
# my_colors <- c("#FFCC00", "#FF9900", "#FF6600", "#FF3300")  # Replace with your desired colors
# my_pal <- colorRampPalette(my_colors)(8)
# data$SS_Category <- factor(data$SS_Category)  # Convert SS_Category to a factor
# ggplot(data, aes(Loss, SS_Category, color = SS_Category, fill = SS_Category)) +
#   # coord_flip() +
#   scale_y_discrete(expand = c(.07, .07)) +
#   scale_color_manual(values = my_pal, guide = "none") +
#   scale_fill_manual(values = my_pal, guide = "none") +
# 
#   theme_minimal(base_size = 20) +
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
#         axis.text.x = element_text(angle = 0, hjust = 1.0)) + # Black ticks on the axis)
# 
#   labs(title = title_SS) +
#   xlab(y_title) +
#   ylab("Saffir Simpson Scale Category at Landfall") +
#   # scale_y_continuous(name = y_title,
#   #                    breaks = seq(0, ymax, x_intervals),
#   #                    minor_breaks = seq(0, ymax, 5),
#   #                    expand = c(0.0, 0.0),
#   #                    limits=c(0, ymax)) +
#   ggridges::geom_density_ridges(
#     alpha = .7, size = 1.5
#   ) +
#   ggridges::stat_density_ridges(
#     quantile_lines = TRUE, quantiles = 2,
#     color = "black", alpha = .8, size = 1.5
#   )









## PLOT - Saffir Simpson Scale ##
ggplot() + 
  
  geom_hline(yintercept = 0.0, color = "grey40", size = 0.3, linetype = "dashed") +
  
  geom_boxplot(data = data, 
               aes(x = SS_Category, y = Loss, group = SS_Category),
               size = 0.4,
               color = "black",
               fill = "coral2",
               coef = Inf,
               width = 0.5) +
  
  geom_label(aes(x = RMSE_label_position_x_SS, y = RMSE_label_position_y, 
                 label = paste("Correlation: ", round(correlation, 2), "\n",
                               "MAE: US$ ", round_with_decimal(MAE, 1), " bn\n",
                               "Max. AE: US$ ", max_abs_error, " bn\n",
                               "N Storms: ", number_of_storms, sep = "")),
             size = 5, fill = "white", color = "black") +
  
  geom_line(data = filtered_data, aes(x = SS_Category, y = Loss),
            color = "blue3", size = 1.0) +

  theme_minimal(base_size = 14) +
  # labs(title = "Multi Linear Regression Model") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey90", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
        axis.ticks = element_line(color = "black", size = 0.8),
        plot.title = element_text(size = 15)) + # Black ticks on the axis)
  
  scale_y_continuous(name = y_title,  # Adjust name as needed
                     breaks = seq(0, ymax, x_intervals),
                     minor_breaks = seq(0, ymax, 5),
                     expand = c(0.0, 0.0),
                     limits=c(-10, ymax)) +
  scale_x_continuous(name = "Landfall Saffir Simpson Scale Category", 
                     breaks = seq(0, 5, 1),
                     expand = c(0.0, 0.0)) +
  coord_cartesian(ylim = c(-10, ymax), xlim = c(-0.5, 5.5)) +  # Adjust axis limits
  labs(title = title_SS)


ggsave("PLOTS/Figure_6a.png", width = 9, height = 6, dpi=150)



## PLOT - Landfall Vmax ##



## Landfall Vmax Linear Model ##

linear_model = lm(Loss ~ USA_WIND, data = data) #Create a linear regression with two variables
linear_predictions <- predict(linear_model, new_data = data$USA_WIND)

MAE <- mean(abs(data$Loss - linear_predictions))
max_abs_error <- round_with_decimal(max(abs(data$Loss - linear_predictions), na.rm = T), 1)
correlation <- cor(data$USA_WIND, data$Loss)
number_of_storms <- nrow(data)

# Custom rounding function
round_with_decimal <- function(x, digits = 0) {
  rounded <- round(x, digits)
  if (rounded == floor(rounded)) {
    return(sprintf("%0.*f", digits, rounded))
  } else {
    return(as.character(rounded))
  }
}


data_linear_projection <- data.frame(Loss = linear_predictions,
                                     USA_WIND = data$USA_WIND)

## PLOT - V max ##
ggplot() + 
  
  geom_hline(yintercept = 0.0, color = "grey40", size = 0.3, linetype = "dashed") +
  
  geom_point(data = data, 
             aes(x = USA_WIND, y = Loss, group = USA_WIND),
               size = 2,
               color = "coral2") +
  geom_point(data = data, 
             aes(x = USA_WIND, y = Loss, group = USA_WIND),
             shape = 1,size = 2,colour = "black") +
  
  geom_label(aes(x = RMSE_label_position_x_Vmax, y = RMSE_label_position_y,
                 label = paste("Correlation: ", round(correlation, 2), "\n",
                               "MAE: US$ ", round_with_decimal(MAE, 1), " bn\n",
                               "Max. AE: US$ ", max_abs_error, " bn\n",
                               "N Storms: ", number_of_storms, sep = "")),
             size = 5, fill = "white", color = "black") +
  
  geom_line(data = data_linear_projection, aes(x = USA_WIND, y = Loss),
            color = "blue3", size = 1.0) +
  
  theme_minimal(base_size = 14) +
  # labs(title = "Multi Linear Regression Model") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey90", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey90", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
        axis.ticks = element_line(color = "black", size = 0.8),
        axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        plot.title = element_text(size = 15)) + # Black ticks on the axis)
  
  coord_cartesian(ylim = c(-10, ymax)) +
  coord_cartesian(xlim = c(30, 150)) +
  
  scale_y_continuous(name = y_title,
                     breaks = seq(0, ymax, x_intervals),
                     minor_breaks = seq(0, ymax, 5),
                     expand = c(0.0, 0.0),
                     limits=c(-10, ymax)) +
  # scale_x_continuous(name = y_title,
  #                    trans = "log",
  #                    breaks = 10^(-3:9),
  #                    # minor_breaks = seq(10, 10^9, by = 100),
  #                    labels = function(x) {
  #                      ifelse(x >= 1, sprintf("%.0f", x), sprintf("%.3f", x))
  #                    },
  #                    limits = c(10^-3, 10^3)) +
  
  scale_x_continuous(name = "Landfall Vmax (knots)", 
                     breaks = seq(30, 150, 10),
                     minor_breaks = seq(0, 150, 5),
                     expand = c(0.01,0.01),
                     limits=c(30, 150)) +
  labs(title = title_Vmax)

ggsave("PLOTS/Figure_6b.png", width = 9, height = 6, dpi=150)


# ggsave(save_file_Vmax, width = 9, height = 6, dpi=150)
