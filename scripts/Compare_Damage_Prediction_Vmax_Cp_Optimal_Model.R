

## suppress warning messages ##
options(warn = -1)

setwd("/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model")

###### Set working directory ######
directory <- "/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model"

## Load Libraries ##
library(readxl)
library(lubridate)
library(tidyr)
library(purrr)
library(dplyr)
library(writexl)
library(stringr)
library(stringi)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(randomForest)
library(mgcv)
library(scales)      # For rescaling numeric values
library(caret)
library(patchwork)  # for combining plots
library(DEoptim)



# List all files in the directory that match a pattern
matching_files <- list.files(path = "/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT", 
                             pattern = "Best_Variable", 
                             full.names = TRUE, 
                             recursive = TRUE)  # set to FALSE if not recursive










#for (file in matching_files) {
#  file_data_frame <- read_xlsx(file)
#  print(file)
#  print(file_data_frame[1,])
#  #print(file_data_frame[3,])
#  #print(file_data_frame[4,])
#  print("-------------------------")
#}
#
#print(jj)








args <- commandArgs(trailingOnly = TRUE)

fractions   <- as.numeric(args[1])
model_type  <- (args[2])
eval_Metric <- (args[3])
rank_type   <- (args[4])


frac <- fractions
normalised_yes_no <- "yes"


all_data <- read_xlsx("/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/USA_Landfall_Data_combined.xlsx")



landfall_marker <- all_data$Landfall_Marker_12hr
year_name       <- all_data$Year_Name_12hr
Year_Name_Landfall_Marker       <- all_data$Year_Name_Landfall_Marker
storm_names     <- gsub("_", " ", paste(all_data$Year_Name_12hr, sep = ""))


## Convert landfall marker to another string ##
all_data$Landfall_Marker_12hr <- gsub("Closest Bypasser", "CB", landfall_marker)
all_data$Landfall_Marker_12hr <- gsub("Pre - 1st Max Landfall", "LF Max", landfall_marker)
all_data$Landfall_Marker_12hr <- gsub("Pre - 2nd Max Landfall", "LF 2", landfall_marker)
all_data$Landfall_Marker_12hr <- gsub("Pre - 3rd Max Landfall", "LF 3", landfall_marker)

all_data$Year_Name_Landfall_Marker <- paste(year_name, "_", landfall_marker, sep = "")
all_data$Year_Name_Landfall_Marker <- gsub("_", "\n", all_data$Year_Name_Landfall_Marker)

storm_Year_Name_Landfall_Marker <- all_data$Year_Name_LandfallMarker
storm_names    <- gsub("_", " ", paste(all_data$Year_Name_12hr, sep = ""))


all_data <- subset(all_data, select = c(-Year_Name_12hr, -Landfall_Marker_12hr, -Year_Name_Landfall_Marker, -Unadjusted_Loss))
loss_type <- "Adjusted_Loss"    # "Unadjusted_Loss" "Adjusted_Loss"
names(all_data)[names(all_data) == loss_type] <- "Loss"


loss_data <- all_data$Loss




#print(colnames(all_data))



#### ------- Rank Processing ------- ######
hurricane_landfall_attributes_data <- all_data
hurricane_landfall_attributes_data$USA_PRES_Fullhr   <- 1020 - hurricane_landfall_attributes_data$USA_PRES_12hr
hurricane_landfall_attributes_data$USA_PRES_12hr   <- 1020 - hurricane_landfall_attributes_data$USA_PRES_12hr


## Linear Rank ##
renamed_hurricane_data <- hurricane_landfall_attributes_data %>% mutate(across(everything(), ~rank(-.)))


## Normalised Rank  ##
## Divide each item by max in each column ##
df_scaled <- hurricane_landfall_attributes_data %>%
  mutate(across(everything(), ~ .x / max(.x)))

## Get onto a scale up where 1 (the maximum) has a value of: nrow(df_scaled) ##
df_scaled <- df_scaled * nrow(df_scaled)

## Reverse the scaling so that the max has rank of 1 ##
df_ranked <- as.data.frame(
  lapply(df_scaled, function(x) {
    1 + (max(x) - x) * 105 / (max(x) - min(x))
  })
)

renamed_hurricane_data_no_rank_vars_rank_loss <- df_ranked

renamed_hurricane_data_no_rank_vars_rank_loss$Loss <- rank(-loss_data, ties.method = "min")


renamed_hurricane_data <- renamed_hurricane_data %>% rename_all(~paste0(., "_rank"))
renamed_hurricane_data_no_rank_vars_rank_loss <- renamed_hurricane_data_no_rank_vars_rank_loss %>% rename_all(~paste0(., "_rank"))






#### ------- Second-Order Feature Engineering - renamed_hurricane_data ------- ######

renamed_hurricane_data_second_order <- subset(renamed_hurricane_data, select = -c(Loss_rank))
## Get number of columns
n <- ncol(renamed_hurricane_data_second_order)

## Loop over all pairs in original order
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    col1 <- names(renamed_hurricane_data_second_order)[i]
    col2 <- names(renamed_hurricane_data_second_order)[j]
    new_col_name <- paste0(col1, "_", col2, "_combined_rank")
    renamed_hurricane_data_second_order[[new_col_name]] <- 
      rank(renamed_hurricane_data_second_order[[col1]] + renamed_hurricane_data_second_order[[col2]])
  }
}
renamed_hurricane_data_second_order$Loss_rank <- renamed_hurricane_data$Loss_rank


### --- Rename missing column name --- ###
if ("R34_Sum_LitPOP_Exposure_Value_Fullhr_rank" %in% names(renamed_hurricane_data)) {
  print("Yes")
}
if ("R34_Sum_PresentDay_Population_Density_Fullhr_rank" %in% names(renamed_hurricane_data)) {
  print("Yes")
}

if (paste0("R34_Sum_PresentDay_Population_Density_Fullhr_rank", "_", "R34_Sum_LitPOP_Exposure_Value_Fullhr_rank", "_combined_rank") %in% names(renamed_hurricane_data_second_order)) {
  print("Yes")
}

colnames(renamed_hurricane_data_second_order)[
  colnames(renamed_hurricane_data_second_order) == "R34_Sum_PresentDay_Population_Density_Fullhr_rank_R34_Sum_LitPOP_Exposure_Value_Fullhr_rank_combined_rank"
] <- "R34_Sum_LitPOP_Exposure_Value_Fullhr_rank_R34_Sum_PresentDay_Population_Density_Fullhr_rank_combined_rank"

correlation_data_frame <- data.frame(Variables = colnames(renamed_hurricane_data_second_order)[-ncol(renamed_hurricane_data_second_order)])
for (var_index in 1:nrow(correlation_data_frame)) {
  correlation_data_frame$Correlation[var_index] <- abs(cor(renamed_hurricane_data_second_order[[var_index]], renamed_hurricane_data_second_order$Loss_rank))
}
correlation_data_frame_sorted_filter <- correlation_data_frame %>% filter(Correlation > 0.3) %>% arrange(desc(Correlation))
var_names <- c(correlation_data_frame_sorted_filter$Variables, "Loss_rank")
renamed_hurricane_data_second_order <- renamed_hurricane_data_second_order[, var_names]



#### ------- Second-Order Feature Engineering - renamed_hurricane_data_no_rank_vars_rank_loss ------- ######

renamed_hurricane_data_second_order_no_rank_vars_rank_loss <- subset(renamed_hurricane_data_no_rank_vars_rank_loss, select = -c(Loss_rank))
## Get number of columns
n <- ncol(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)

## Loop over all pairs in original order
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    col1 <- names(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)[i]
    col2 <- names(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)[j]
    new_col_name <- paste0(col1, "_", col2, "_combined_rank")
    renamed_hurricane_data_second_order_no_rank_vars_rank_loss[[new_col_name]] <- 
      rank(renamed_hurricane_data_second_order_no_rank_vars_rank_loss[[col1]] + renamed_hurricane_data_second_order_no_rank_vars_rank_loss[[col2]])
  }
}
renamed_hurricane_data_second_order_no_rank_vars_rank_loss$Loss_rank <- renamed_hurricane_data$Loss_rank


### --- Rename missing column name --- ###
if ("R34_Sum_LitPOP_Exposure_Value_Fullhr_rank" %in% names(renamed_hurricane_data)) {
  print("Yes")
}
if ("R34_Sum_PresentDay_Population_Density_Fullhr_rank" %in% names(renamed_hurricane_data)) {
  print("Yes")
}

if (paste0("R34_Sum_PresentDay_Population_Density_Fullhr_rank", "_", "R34_Sum_LitPOP_Exposure_Value_Fullhr_rank", "_combined_rank") %in% names(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)) {
  print("Yes")
}

colnames(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)[
  colnames(renamed_hurricane_data_second_order_no_rank_vars_rank_loss) == "R34_Sum_PresentDay_Population_Density_Fullhr_rank_R34_Sum_LitPOP_Exposure_Value_Fullhr_rank_combined_rank"
] <- "R34_Sum_LitPOP_Exposure_Value_Fullhr_rank_R34_Sum_PresentDay_Population_Density_Fullhr_rank_combined_rank"

correlation_data_frame <- data.frame(Variables = colnames(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)[-ncol(renamed_hurricane_data_second_order_no_rank_vars_rank_loss)])
for (var_index in 1:nrow(correlation_data_frame)) {
  correlation_data_frame$Correlation[var_index] <- abs(cor(renamed_hurricane_data_second_order_no_rank_vars_rank_loss[[var_index]], renamed_hurricane_data_second_order_no_rank_vars_rank_loss$Loss_rank))
}
correlation_data_frame_sorted_filter <- correlation_data_frame %>% filter(Correlation > 0.3) %>% arrange(desc(Correlation))
var_names <- c(correlation_data_frame_sorted_filter$Variables, "Loss_rank")
renamed_hurricane_data_second_order_no_rank_vars_rank_loss <- renamed_hurricane_data_second_order_no_rank_vars_rank_loss[, var_names]




storm_raw_loss <- all_data$Loss
storm_loss_rank <- rank(-all_data$Loss, ties.method = "min")








eval_Metric <- "RMSE"


rmse <- function(predicted, observed) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))  # RMSE
}












#### ---- Determine New Cp and Combined Rank Scale --- ####


## Determine Cp and Combined Risk Rank thresholds based on Saffir Simpson ratios ##

model_type   <- "Weighted_Rank"   # Weighted_Rank
second_order <- "Not_Second_Order"    # "Not_Second_Order"  "Second_Order"
rank_type    <- "no_vars_yes_loss"

input_file <- "/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT_Damage/Best_Variable_Combo_Loss_Weighted_Rank_20pct_Not_Second_Order_RMSE_no_vars_yes_loss_hybrid_optim.xlsx"

optimal_file_data_frame <- read_xlsx(input_file)

optimal_variables <- optimal_file_data_frame[3, 2]
optimal_weights   <- optimal_file_data_frame[4, 2]

if (rank_type == "rank") {
  if (second_order == "Not_Second_Order") {
    df <- renamed_hurricane_data
  }
  if (second_order == "Second_Order") {
    df <- renamed_hurricane_data_second_order
  }
}

if (rank_type == "no_vars_yes_loss") {
  if (second_order == "Not_Second_Order") {
    df <- renamed_hurricane_data_no_rank_vars_rank_loss
  }
  if (second_order == "Second_Order") {
    df <- renamed_hurricane_data_second_order_no_rank_vars_rank_loss
  }
}

X_full <- df[, !names(df) %in% "Loss_rank"]
y <- df$Loss_rank

vars <- strsplit(optimal_variables$Value, ",\\s*")[[1]]

X <- X_full[, vars, drop = FALSE]

weights   <- as.numeric(strsplit(optimal_weights$Value, ",\\s*")[[1]])

X <- X_full[, vars, drop = FALSE]
X <- as.data.frame(sapply(X, as.numeric))

# Force X into a numeric matrix
X <- as.matrix(sapply(X_full[, vars, drop = FALSE], as.numeric))

## Apply Weights ##
preds <- as.vector(X %*% weights)




cat(optimal_variables$Value, sep = "\n")


variable_rank <- rank(preds, ties.method = "min")


observed_Vs_predicted <- data.frame(observed = renamed_hurricane_data$Loss_rank,
                                    predicted = variable_rank)

observed_Vs_predicted$Loss  <- all_data$Loss
observed_Vs_predicted$Index <- seq(1, nrow(observed_Vs_predicted), 1)
observed_Vs_predicted$Storm_Names <- storm_names

closest_indices <- sapply(observed_Vs_predicted$predicted, function(x) which.min(abs(observed_Vs_predicted$observed - x)))

observed_Vs_predicted$Predicted_Loss <- observed_Vs_predicted$Loss[closest_indices]


RMSE        <- round(sqrt(mean((observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)^2)), 1)

#print(observed_Vs_predicted)
#print(RMSE)
#print(jj)


category_data <- data.frame(Year_Name_Landfall_Marker = storm_names,
                            USA_PRES_12hr_rank = renamed_hurricane_data$USA_PRES_12hr_rank,
                            USA_WIND = all_data$USA_WIND_12hr,
                            Predicted_Loss = observed_Vs_predicted$Predicted_Loss,
                            Observed_Loss = observed_Vs_predicted$Loss)

## Loss Scale ##
for (storm_index in seq(1, nrow(category_data), 1)) {

  ## Loss Scale ##  
  if (category_data$Predicted_Loss[storm_index] <= 1.3) {
    category_data$Loss_Category[storm_index] <- as.character(1)
  }
  if (between(category_data$Predicted_Loss[storm_index], 1.3, 5.3)) {
    category_data$Loss_Category[storm_index] <- as.character(2)
  }
  if (between(category_data$Predicted_Loss[storm_index], 5.3, 29.5)) {
    category_data$Loss_Category[storm_index] <- as.character(3)
  }
  if (between(category_data$Predicted_Loss[storm_index], 29.5, 119.4)) {
    category_data$Loss_Category[storm_index] <- as.character(4)
  }
  if (category_data$Predicted_Loss[storm_index] >= 119.4) {
    category_data$Loss_Category[storm_index] <- as.character(5)
  }
  ## Loss Scale ##  
  if (category_data$Observed_Loss[storm_index] <= 1.3) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(1)
  }
  if (between(category_data$Observed_Loss[storm_index], 1.3, 5.3)) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(2)
  }
  if (between(category_data$Observed_Loss[storm_index], 5.3, 29.5)) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(3)
  }
  if (between(category_data$Observed_Loss[storm_index], 29.5, 119.4)) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(4)
  }
  if (category_data$Observed_Loss[storm_index] >= 119.4) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(5)
  }
}

## Saffir Simpson Scale ##
for (storm_index in seq(1, nrow(category_data), 1)) {

  ## Loss Scale ##  
  if (category_data$USA_WIND[storm_index] <= 83) {
    category_data$SSHWS_Category[storm_index] <- as.character(1)
  }
  if (between(category_data$USA_WIND[storm_index], 83, 96)) {
    category_data$SSHWS_Category[storm_index] <- as.character(2)
  }
  if (between(category_data$USA_WIND[storm_index], 96, 113)) {
    category_data$SSHWS_Category[storm_index] <- as.character(3)
  }
  if (between(category_data$USA_WIND[storm_index], 113, 137)) {
    category_data$SSHWS_Category[storm_index] <- as.character(4)
  }
  if (category_data$USA_WIND[storm_index] >= 137) {
    category_data$SSHWS_Category[storm_index] <- as.character(5)
  }
}



diffs <- as.numeric(category_data$SSHWS_Category) -
         as.numeric(category_data$Correct_Loss_Category)

#print(table(diffs))

#print(jj)



#### ---- PLOTTING ---- ####


### Loss Scale ###

#title <- expression(bold("c)") * " " * "Historical Normalised Economic Loss Per Hurricane and its Loss Scale Category (1979-2023)")
title <- "Historical Hurricane Averaged Normalised (to 2024) Economic Loss and its Predicted Loss Scale Category (1979-2024)"

ymax <- 270.0
x_intervals <- 20
y_title <- "Observed\nEconomic\nLoss\n(US$ bn)"

# Create a color palette for discrete values
ylorRd_colors <- c("yellow", "orange", "coral", "red2", "red4")  # Example colors

## order data frame by column value ##
category_data <- category_data[order(category_data$Observed_Loss), ]

category_data <- category_data[category_data$Observed_Loss > 5.0, ]

print(category_data)

# Create the plot
ggplot(data = category_data, 
       aes(x = reorder(Year_Name_Landfall_Marker, -Observed_Loss), y = Observed_Loss,
       fill = Loss_Category)) +
  geom_col(color = "black") +

  #geom_text(aes(label = round(Observed_Loss)),  # Use Loss as the label
  #          position = position_stack(vjust = 0.5),  # Position the labels in the middle of the bars
  #          color = "black", size = 3) +  # Set label color
  
  geom_label(aes(label = round(Observed_Loss), y = 10), fill = alpha("white", 0.5), color = "black", size = 3, label.size = NA) +

  scale_fill_manual(values = ylorRd_colors) +

  theme_minimal(base_size = 14) +
  # labs(title = "Multi Linear Regression Model") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey90", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey90", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),
        legend.text = element_text(size = 16),
        legend.key = element_rect(fill = "white", colour = "black"),
        axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
        axis.ticks = element_line(color = "black", size = 0.8),
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, size = 10),
        axis.title.y = element_text(angle=0, vjust=0.5)) + # Black ticks on the axis)
  
  scale_y_continuous(name = y_title,
                     breaks = seq(0, ymax, x_intervals),
                     minor_breaks = seq(0, ymax, 5),
                     expand = c(0.0, 0.0),
                     limits=c(0, ymax)) +

  scale_x_discrete(name = "Hurricane Landfall (US$ 5+ bn)",
                     expand = c(0.02, 0.02)) +
  
  labs(title = title, fill = "Predicted\nLoss\nScale\nCategory")

ggsave(paste("PLOTS/Loss_Rank_Scale_Damage_Representation_1.png", sep = ""), width = 16, height = 6, dpi=150)

print(paste("PLOTS/Loss_Rank_Scale_Damage_Representation_1.png", sep = ""))

print(jj)







































#### ---- Damage ---- ####

#### --- USA_WIND --- ####

variable_rank <- renamed_hurricane_data$USA_WIND_12hr_rank
#title <- expression(bold("b)") * " " * "Landfall Cp Normalised Rank")
#title <- expression(bold("a)") * " " * "Landfall Vmax ("All Hurricanes)")


observed_Vs_predicted <- data.frame(observed = renamed_hurricane_data$Loss_rank,
                                    predicted = variable_rank)


observed_Vs_predicted$Loss  <- all_data$Loss
observed_Vs_predicted$Index <- seq(1, nrow(observed_Vs_predicted), 1)
observed_Vs_predicted$Storm_Names <- storm_names

observed_Vs_predicted <- observed_Vs_predicted[order(-observed_Vs_predicted$Loss), ]

closest_indices <- sapply(observed_Vs_predicted$predicted, function(x) which.min(abs(observed_Vs_predicted$observed - x)))

observed_Vs_predicted$Predicted_Loss <- observed_Vs_predicted$Loss[closest_indices]



RMSE        <- round(sqrt(mean((observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)^2)), 1)
Max_Error   <- round(max(abs(observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)), 1)
Correlation <- round(cor(observed_Vs_predicted$Loss, observed_Vs_predicted$Predicted_Loss, method = "spearman"), 2)

RMSE <- sprintf("%.1f", RMSE)
Max_Error <- sprintf("%.1f", Max_Error)
Correlation <- sprintf("%.2f", Correlation)
Correlation_top <- round(
  cor(
    observed_Vs_predicted$Loss[observed_Vs_predicted$Loss > 20],
    observed_Vs_predicted$Predicted_Loss[observed_Vs_predicted$Loss > 20],
    method = "spearman"
  ),
  2
)

color <- "salmon"

observed_Vs_predicted$Index <- as.character(observed_Vs_predicted$Index)

# Calculate relative difference
relative_diff <- abs(observed_Vs_predicted$predicted - observed_Vs_predicted$observed) / observed_Vs_predicted$observed

# Count where difference > 20%
num_more_than_20_percent <- sum(relative_diff > 0.20, na.rm = TRUE)

## Filter to US$ 5+ bn ##
observed_Vs_predicted <- observed_Vs_predicted[observed_Vs_predicted$Loss > 3, ]


## Scatter ##
# Calculate Spearman correlation
spearman_corr <- cor(
  observed_Vs_predicted$Loss,
  observed_Vs_predicted$Predicted_Loss,
  method = "spearman"
)

ggplot() +

  geom_point(data = observed_Vs_predicted, aes(x = Loss, y = Predicted_Loss)) +
  labs(
    title = paste("Spearman Correlation =", round(spearman_corr, 2)),
    x = "Observed Loss",
    y = "Predicted Loss"
  )

#ggsave(paste(directory, "/PLOTS/Scatter_Loss_Prediction_Vmax", ".png", sep = ""), width = 6, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Scatter_Loss_Prediction_Vmax", ".png", sep = ""))



ggplot() +

  geom_col(data = observed_Vs_predicted,
           aes(x = reorder(Index, -Loss), y = Loss, fill = "Observed Loss"),
           color = "black") +
  geom_point(data = observed_Vs_predicted,
             aes(x = reorder(Index, -Loss), y = Predicted_Loss, fill = "Predicted Loss"),
             color = "black", shape = 23, size = 3) +
  scale_fill_manual(
    name = "Legend",   # Legend title
    values = c("Observed Loss" = "goldenrod1", "Predicted Loss" = color)
  ) +

  geom_label(aes(x = 41, y = 240, 
                 label = paste("RMSE: US$ ", RMSE, " bn\n",
                               "Max Error: US$ ", Max_Error, " bn\n",
                               "Correlation: ", Correlation, "\n",
                               "Correlation (>US$ 20 bn): ", Correlation_top,
                               #"\n# +/- 20% Error:", num_more_than_20_percent,
                               sep = "")),
             size = 5, fill = "white", color = "black") +

  labs(title = title, x = "Historical Hurricane (US$ 1+ bn)") +

  scale_y_continuous(name = "Economic Loss (US$ bn)",
                     breaks = seq(0, 280, 20),
                     minor_breaks = seq(0, 280, 5),
                     expand = c(0.0, 0.0),
                     limits = c(0.0, 280)) +

  scale_y_continuous(name = "Economic \nLoss \n(US$ bn)",
                     breaks = seq(0, 280, 20),
                     minor_breaks = seq(0, 280, 5),
                     expand = c(0.0, 0.0),
                     limits = c(0.0, 280)) +

  scale_x_discrete(breaks = observed_Vs_predicted$Index, labels = observed_Vs_predicted$Storm_Names) + 
  
  theme_minimal(base_size = 16) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey80", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey80", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),

        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.23, 0.87),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white"),

        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.line = element_line(color = "black"),  # Axis lines
        axis.ticks = element_line(color = "black"),  # Tick marks
        axis.text = element_text(color = "black"),   # Axis text
        axis.title = element_text(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, size = 10),
        )

#ggsave(paste(directory, "/PLOTS/Loss_Prediction_Vmax", ".png", sep = ""), width = 18, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Loss_Prediction_Vmax", ".png", sep = ""))



print(jj)





### --- USA_PRES --- ###


variable_rank <- renamed_hurricane_data$USA_PRES_12hr_rank
title <- expression(bold("b)") * " " * "Landfall Cp Normalised Rank")

## Build Data frame 
observed_Vs_predicted <- data.frame(observed = renamed_hurricane_data$Loss_rank,
                                    predicted = variable_rank)

observed_Vs_predicted$Loss  <- all_data$Loss
observed_Vs_predicted$Index <- seq(1, nrow(observed_Vs_predicted), 1)
observed_Vs_predicted$Storm_Names <- storm_names


closest_indices <- sapply(observed_Vs_predicted$predicted, function(x) which.min(abs(observed_Vs_predicted$observed - x)))

observed_Vs_predicted$Predicted_Loss <- observed_Vs_predicted$Loss[closest_indices]


RMSE        <- round(sqrt(mean((observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)^2)), 1)
Max_Error   <- round(max(abs(observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)), 1)
Correlation <- round(cor(observed_Vs_predicted$Loss, observed_Vs_predicted$Predicted_Loss, method = "spearman"), 2)
Correlation_top <- round(
  cor(
    observed_Vs_predicted$Loss[observed_Vs_predicted$Loss > 20],
    observed_Vs_predicted$Predicted_Loss[observed_Vs_predicted$Loss > 20],
    method = "spearman"
  ),
  2
)

# Sort by Loss
observed_Vs_predicted <- observed_Vs_predicted[order(-observed_Vs_predicted$Loss), ]


RMSE <- sprintf("%.1f", RMSE)
Max_Error <- sprintf("%.1f", Max_Error)
Correlation <- sprintf("%.2f", Correlation)

color <- "cornflowerblue"

observed_Vs_predicted$Index <- as.character(observed_Vs_predicted$Index)

## Filter to US$ 5+ bn ##
observed_Vs_predicted <- observed_Vs_predicted[observed_Vs_predicted$Loss > 3, ]

# Calculate relative difference
relative_diff <- abs(observed_Vs_predicted$predicted - observed_Vs_predicted$observed) / observed_Vs_predicted$observed

# Count where difference > 20%
num_more_than_20_percent <- sum(relative_diff > 0.20, na.rm = TRUE)



## Scatter ##
# Calculate Spearman correlation
spearman_corr <- cor(
  observed_Vs_predicted$Loss,
  observed_Vs_predicted$Predicted_Loss,
  method = "spearman"
)

ggplot() +

  geom_point(data = observed_Vs_predicted, aes(x = Loss, y = Predicted_Loss)) +
  labs(
    title = paste("Spearman Correlation =", round(spearman_corr, 2)),
    x = "Observed Loss",
    y = "Predicted Loss"
  )

#ggsave(paste(directory, "/PLOTS/Scatter_Loss_Prediction_Cp", ".png", sep = ""), width = 6, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Scatter_Loss_Prediction_Cp", ".png", sep = ""))



ggplot() +

  geom_col(data = observed_Vs_predicted,
           aes(x = reorder(Index, -Loss), y = Loss, fill = "Observed Loss"),
           color = "black") +
  geom_point(data = observed_Vs_predicted,
             aes(x = reorder(Index, -Loss), y = Predicted_Loss, fill = "Predicted Loss"),
             color = "black", shape = 23, size = 3) +
  scale_fill_manual(
    name = "Legend",   # Legend title
    values = c("Observed Loss" = "goldenrod1", "Predicted Loss" = color)
  ) +

  geom_label(aes(x = 41, y = 240, 
                 label = paste("RMSE: US$ ", RMSE, " bn\n",
                               "Max Error: US$ ", Max_Error, " bn\n",
                               "Correlation: ", Correlation, "\n",
                               "Correlation (>US$ 20 bn): ", Correlation_top,
                               #"\n# +/- 20% Error:", num_more_than_20_percent,
                               sep = "")),
             size = 5, fill = "white", color = "black") +

  labs(title = title, x = "Historical Hurricane (US$ 1+ bn)") +

  scale_y_continuous(name = "Economic \nLoss \n(US$ bn)",
                     breaks = seq(0, 280, 20),
                     minor_breaks = seq(0, 280, 5),
                     expand = c(0.0, 0.0),
                     limits = c(0.0, 280)) +

  scale_x_discrete(breaks = observed_Vs_predicted$Index, labels = observed_Vs_predicted$Storm_Names) + 
  
  theme_minimal(base_size = 16) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey80", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey80", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),

        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.23, 0.87),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white"),

        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.line = element_line(color = "black"),  # Axis lines
        axis.ticks = element_line(color = "black"),  # Tick marks
        axis.text = element_text(color = "black"),   # Axis text
        axis.title = element_text(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, size = 10),
        )

#ggsave(paste(directory, "/PLOTS/Loss_Prediction_Cp", ".png", sep = ""), width = 18, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Loss_Prediction_Cp", ".png", sep = ""))






### --- Combined Var --- ###


input_file <- paste("/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT_Damage/", 
                           "Best_Variable_Combo_Loss_", "Weighted_Rank", "_", "60", "pct_Not_Second_Order_Correl_", "rank", ".xlsx", sep = "")

#print(input_file)
optimal_file_data_frame <- read_xlsx(input_file)

optimal_variables <- optimal_file_data_frame[3, 2]
optimal_weights   <- optimal_file_data_frame[4, 2]

#print(optimal_variables)
#print(optimal_weights)

#print(jj)


cols_both <- grep("(?=.*RMW)(?=.*Mean_Percent_Damage)", 
                  colnames(renamed_hurricane_data_second_order), 
                  value = TRUE, perl = TRUE)

#print(cols_both)
#print(jj)

for (second_order in c("Not_Second_Order")) {  # "100", "90", "80", "70", "60", "50", "40", "30", "20", "10"

  for (model_type in c("RF")) {    # "Weighted_Rank", "Linear_Regression", "RF"

    for (rank_type in c("rank", "no_vars_yes_loss")) {   # "rank", "no_vars_yes_loss"

      for (pct in c("100")) {  # "100", "90", "80", "70", "60", "50", "40", "30", "20", "10"

        input_file <- paste("/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT_Damage/", 
                             "Best_Variable_Combo_Loss_", model_type, "_", pct, "pct_", second_order, "_Correl_", rank_type, ".xlsx", sep = "")  # _hybrid_optim

        optimal_file_data_frame <- read_xlsx(input_file)

        optimal_variables <- optimal_file_data_frame[3, 2]
        optimal_weights   <- optimal_file_data_frame[4, 2]

        if (rank_type == "rank") {
          if (second_order == "Not_Second_Order") {
            df <- renamed_hurricane_data
          }
          if (second_order == "Second_Order") {
            df <- renamed_hurricane_data_second_order
          }
        }

        if (rank_type == "no_vars_yes_loss") {
          if (second_order == "Not_Second_Order") {
            df <- renamed_hurricane_data_no_rank_vars_rank_loss
          }
          if (second_order == "Second_Order") {
            df <- renamed_hurricane_data_second_order_no_rank_vars_rank_loss
          }
        }

        X_full <- df[, !names(df) %in% "Loss_rank"]
        y <- df$Loss_rank

        vars <- strsplit(optimal_variables$Value, ",\\s*")[[1]]

        X <- X_full[, vars, drop = FALSE]

        if (model_type == "Weighted_Rank") {
          weights   <- as.numeric(strsplit(optimal_weights$Value, ",\\s*")[[1]])

          X <- X_full[, vars, drop = FALSE]
          X <- as.data.frame(sapply(X, as.numeric))

          # Force X into a numeric matrix
          X <- as.matrix(sapply(X_full[, vars, drop = FALSE], as.numeric))

          ## Apply Weights ##
          preds <- as.vector(X %*% weights)

        }

        if (model_type == "GAM") {

          n <- nrow(X)
          preds <- numeric(n)
          append_index <- 1

          for (i in seq(1, nrow(X), 1)) {
            train_X <- X[-i, , drop = FALSE]
            train_y <- y[-i]
            test_X  <- X[i, , drop = FALSE]

            # Construct smooth GAM formula dynamically
            smooth_terms <- paste0("s(", names(train_X), ")", collapse = " + ")
            formula_gam <- as.formula(paste("train_y ~", colnames(train_X)))
 
            train_X <- as.data.frame(train_X)   # ensure it's a data.frame
            test_X  <- as.data.frame(test_X)    # same for test

            train_data <- cbind(train_y = train_y, train_X)

            model <- gam(formula_gam, data = train_data, method = "REML")

            test_data <- data.frame(test_X)
            preds[append_index] <- predict(model, newdata = test_data)
            append_index <- append_index + 1
          }
        }

        if (model_type == "Linear_Regression") {

          n <- nrow(X)
          preds <- numeric(n)
          append_index <- 1

          for (i in seq(1, nrow(X), 1)) {
            train_X <- X[-i, , drop = FALSE]
            train_y <- y[-i]
            test_X  <- X[i, , drop = FALSE]

            train_data <- data.frame(train_y = train_y, train_X)
            model <- lm(train_y ~ ., data = train_data)

            test_data <- data.frame(test_X)
            preds[append_index] <- predict(model, newdata = test_data)
            append_index <- append_index + 1
          }
        }

        if (model_type == "RF") {
          ## Find the best model for the top percentile of data ##
          n <- nrow(X)
          preds <- numeric(n)
          append_index <- 1

          for (i in seq(1, nrow(X), 1)) {
            train_X <- X[-i, , drop = FALSE]
            train_y <- y[-i]
            test_X  <- X[i, , drop = FALSE]

            train_data <- data.frame(train_y = train_y, train_X)

            set.seed(123)
            model <- tryCatch(
              randomForest(x = train_X, y = train_y, ntree = 500),
              error = function(e) NULL
            )

            test_data <- data.frame(test_X)
            preds[append_index] <- predict(model, newdata = test_data)
            append_index <- append_index + 1
          }
        }

        variable_rank <- rank(preds, ties.method = "min")

        observed_Vs_predicted <- data.frame(observed = renamed_hurricane_data$Loss_rank,
                                            predicted = variable_rank)

        observed_Vs_predicted$Loss  <- all_data$Loss
        observed_Vs_predicted$Index <- seq(1, nrow(observed_Vs_predicted), 1)
        observed_Vs_predicted$Storm_Names <- storm_names

        closest_indices <- sapply(observed_Vs_predicted$predicted, function(x) which.min(abs(observed_Vs_predicted$observed - x)))

        observed_Vs_predicted$Predicted_Loss <- observed_Vs_predicted$Loss[closest_indices]

        RMSE        <- round(sqrt(mean((observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)^2)), 1)
        Max_Error   <- round(max(abs(observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)), 1)
        Correlation_spearman <- round(cor(observed_Vs_predicted$Loss, observed_Vs_predicted$Predicted_Loss, method = "spearman"), 2)
        Correlation_pearson <- round(cor(observed_Vs_predicted$Loss, observed_Vs_predicted$Predicted_Loss, method = "pearson"), 2)
        Correlation_kendall <- round(cor(observed_Vs_predicted$Loss, observed_Vs_predicted$Predicted_Loss, method = "kendall"), 2)

        RMSE <- sprintf("%.1f", RMSE)
        Max_Error <- sprintf("%.1f", Max_Error)
        Correlation <- sprintf("%.2f", Correlation_spearman)
   
        #print(input_file)
        print(paste(model_type, " ", rank_type, " ", pct, " ", second_order, sep = ""))
        #print(vars)
        #print(weights)
        print(paste(RMSE, " ", Correlation, " ", Max_Error, sep = ""))
        #print('---------------------------------')
      }
    }
  }
}


#print(jj)


## -- Rank predictions -- ##

cols_both <- grep("(?=.*PRES)(?=.*LitPOP)", 
                  colnames(renamed_hurricane_data_second_order), 
                  value = TRUE, perl = TRUE)
#print(cols_both)

#print(colnames(renamed_hurricane_data_second_order))
#print(jj)
model_type   <- "Weighted_Rank"   # Weighted_Rank
second_order <- "Not_Second_Order"    # "Not_Second_Order"  "Second_Order"


if (model_type == "Weighted_Rank") {
  input_file <- paste("/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT_Damage/", 
                      "Best_Variable_Combo_Loss_", model_type, "_", "20", "pct_", second_order, "_RMSE_", "no_vars_yes_loss", "_hybrid_optim.xlsx", sep = "")

  input_file <- "/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT_Damage/Best_Variable_Combo_Loss_Weighted_Rank_20pct_Not_Second_Order_RMSE_no_vars_yes_loss_hybrid_optim.xlsx"

  optimal_file_data_frame <- read_xlsx(input_file)

  optimal_variables <- optimal_file_data_frame[3, 2]
  optimal_weights   <- optimal_file_data_frame[4, 2]

  if (rank_type == "rank") {
      if (second_order == "Not_Second_Order") {
        df <- renamed_hurricane_data
    }
    if (second_order == "Second_Order") {
      df <- renamed_hurricane_data_second_order
    }
  }

  if (rank_type == "no_vars_yes_loss") {
    if (second_order == "Not_Second_Order") {
      df <- renamed_hurricane_data_no_rank_vars_rank_loss
    }
    if (second_order == "Second_Order") {
      df <- renamed_hurricane_data_second_order_no_rank_vars_rank_loss
    }
  }

  X_full <- df[, !names(df) %in% "Loss_rank"]
  y <- df$Loss_rank

  vars <- strsplit(optimal_variables$Value, ",\\s*")[[1]]

  X <- X_full[, vars, drop = FALSE]

  weights   <- as.numeric(strsplit(optimal_weights$Value, ",\\s*")[[1]])

  X <- X_full[, vars, drop = FALSE]
  X <- as.data.frame(sapply(X, as.numeric))

  # Force X into a numeric matrix
  X <- as.matrix(sapply(X_full[, vars, drop = FALSE], as.numeric))

  ## Apply Weights ##
  preds <- as.vector(X %*% weights)

}



cat(optimal_variables$Value, sep = "\n")


variable_rank <- rank(preds, ties.method = "min")


observed_Vs_predicted <- data.frame(observed = renamed_hurricane_data$Loss_rank,
                                    predicted = variable_rank)

observed_Vs_predicted$Loss  <- all_data$Loss
observed_Vs_predicted$Index <- seq(1, nrow(observed_Vs_predicted), 1)
observed_Vs_predicted$Storm_Names <- storm_names

closest_indices <- sapply(observed_Vs_predicted$predicted, function(x) which.min(abs(observed_Vs_predicted$observed - x)))

observed_Vs_predicted$Predicted_Loss <- observed_Vs_predicted$Loss[closest_indices]


title <- expression(bold("c)") * " " * "Optimal Variable Normalised Rank Selection & Weighting")

RMSE        <- round(sqrt(mean((observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)^2)), 1)
Max_Error   <- round(max(abs(observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)), 1)
Correlation <- round(cor(observed_Vs_predicted$Loss, observed_Vs_predicted$Predicted_Loss, method = "spearman"), 2)
Correlation_top <- round(
  cor(
    observed_Vs_predicted$Loss[observed_Vs_predicted$Loss > 20],
    observed_Vs_predicted$Predicted_Loss[observed_Vs_predicted$Loss > 20],
    method = "spearman"
  ),
  2
)

RMSE <- sprintf("%.1f", RMSE)
Max_Error <- sprintf("%.1f", Max_Error)
Correlation <- sprintf("%.2f", Correlation)


## Scatter ##
# Calculate Spearman correlation
spearman_corr <- cor(
  observed_Vs_predicted$Loss,
  observed_Vs_predicted$Predicted_Loss,
  method = "spearman"
)


ggplot() +

  geom_point(data = observed_Vs_predicted, aes(x = Loss, y = Predicted_Loss)) +
  labs(
    title = paste("Spearman Correlation =", round(spearman_corr, 2)),
    x = "Observed Loss",
    y = "Predicted Loss"
  )

#ggsave(paste(directory, "/PLOTS/Scatter_Loss_Prediction_Optimal_Vars_", model_type, ".png", sep = ""), width = 6, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Scatter_Loss_Prediction_Optimal_Vars_", model_type, ".png", sep = ""))





observed_Vs_predicted$Index <- as.character(observed_Vs_predicted$Index)

## Filter to US$ 5+ bn ##
observed_Vs_predicted <- observed_Vs_predicted[observed_Vs_predicted$Loss > 3, ]

# Calculate relative difference
relative_diff <- abs(observed_Vs_predicted$predicted - observed_Vs_predicted$observed) / observed_Vs_predicted$observed

# Count where difference > 20%
num_more_than_20_percent <- sum(relative_diff > 0.20, na.rm = TRUE)


color <- "darkseagreen"




#print(jj)


## Add optimal labels to plot ##
optimal_variables_label <- gsub(",", "\n", optimal_variables$Value)

optimal_weights_label <- round(as.numeric(optimal_weights$Value), 3)


print(optimal_variables$Value)
print(optimal_weights$Value)

optimal_variables_label <- paste("Landfall Cp, \nMax. Rainfall Accumilation, \n% GHSL R64 12hr Building Density Damage", sep = "") 
optimal_weights_label <- paste("6.9099, 2.5347, 1.9525", sep = "") 

ggplot() +

  geom_col(data = observed_Vs_predicted,
           aes(x = reorder(Index, -Loss), y = Loss, fill = "Observed Loss"),
           color = "black") +
  geom_point(data = observed_Vs_predicted,
             aes(x = reorder(Index, -Loss), y = Predicted_Loss, fill = "Predicted Loss"),
             color = "black", shape = 23, size = 3) +
  scale_fill_manual(
    name = "Legend",   # Legend title
    values = c("Observed Loss" = "goldenrod1", "Predicted Loss" = "darkseagreen")
  ) +

  geom_label(aes(x = 41, y = 240, 
                 label = paste("RMSE: US$ ", RMSE, " bn\n",
                               "Max Error: US$ ", Max_Error, " bn\n",
                               "Correlation: ", Correlation, "\n",
                               "Correlation (>US$ 20 bn): ", Correlation_top,
                               #"\n# +/- 20% Error:", num_more_than_20_percent,
                               sep = "")),
             size = 5, fill = "white", color = "black") +

  geom_label(aes(x = 26, y = 220,    # 200
                 label = paste("Weighted Rank Sum:\n",
                               "Optimal Variables: \n",
                               optimal_variables_label,
                               "\n", "Optimals Weights: ", optimal_weights_label,
                               sep = "")),
             size = 5, fill = "white", color = "black") +

  labs(title = title, x = "Historical Hurricane (US$ 3+ bn)") +

  scale_y_continuous(name = "Economic \nLoss \n(US$ bn)",
                     breaks = seq(0, 280, 20),
                     minor_breaks = seq(0, 280, 5),
                     expand = c(0.0, 0.0),
                     limits = c(0.0, 280)) +

  scale_x_discrete(breaks = observed_Vs_predicted$Index, labels = observed_Vs_predicted$Storm_Names) + 
  
  theme_minimal(base_size = 16) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey80", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey80", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),

        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.23, 0.87),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white"),

        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.line = element_line(color = "black"),  # Axis lines
        axis.ticks = element_line(color = "black"),  # Tick marks
        axis.text = element_text(color = "black"),   # Axis text
        axis.title = element_text(color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, size = 10),
        )

#ggsave(paste(directory, "/PLOTS/Loss_Prediction_Optimal_Vars_", model_type, ".png", sep = ""), width = 18, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Loss_Prediction_Optimal_Vars_", model_type, ".png", sep = ""))





#print(jj)







#### ---- Compare Damage Predictions Models --- ####

data <- data.frame(
  Model = rep(c("Vmax Rank", "Cp Rank", "Random Forest \n(Optimal Risk Vars)", "Linear Regression \n(Optimal Risk Vars)", "Weighted Rank Sum \n(Optimal Risk Vars)"), 2),
  Skill_Value = c(0.54, 0.78, 0.85, 0.87, 0.89,
                  35.6, 26.4, 18.7, 17.1, 7.0),
  Skill_Metric = c(rep("Spearman Correlation", 5), 
                   rep("RMSE", 5))
)

# Reorder Model factor
data$Model <- factor(
  data$Model,
  levels = c("Vmax Rank", "Cp Rank", "Random Forest \n(Optimal Risk Vars)", "Linear Regression \n(Optimal Risk Vars)", "Weighted Rank Sum \n(Optimal Risk Vars)")
)


# Function to make a plot for a single metric
plot_metric <- function(metric, limits, labels) {

  y_label <- ifelse(metric == "RMSE", "RMSE\n(US$ bn)", "Spearman\nCorrelation")
  title   <- ifelse(metric == "RMSE", "", "Observed Vs. Predicted Hurricane Loss")

  ggplot(filter(data, Skill_Metric == metric), 
         aes(x = Model, y = Skill_Value, fill = Model)) +
    geom_col(color = "black") +
    geom_text(aes(label = format(round(Skill_Value, 2), nsmall = 1)), vjust = -0.5, size = 4) +

    scale_fill_manual(
      values = c(
                 "Linear Regression \n(Optimal Risk Vars)" = "cadetblue", 
                 "Weighted Rank Sum \n(Optimal Risk Vars)"                      = "olivedrab",
                 "Random Forest \n(Optimal Risk Vars)"     = "royalblue",
                 "Vmax Rank"                              = "grey20",
                 "Cp Rank"                                = "grey60")) +

    coord_cartesian(ylim = limits) +        # zoom instead of scale_y
    scale_y_continuous(labels = labels, expand = c(0,0)) +  # keep ticks & labels formatting

    theme_minimal(base_size = 12) +
    theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey80", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey80", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.length = unit(5, "pt"),      # make ticks extend outward
        axis.line = element_line(color = "black", linewidth = 1)  # black solid axis lines
    ) +
    labs(x = "Prediction Model", y = y_label, title = title)
}

# Create two plots
p1 <- plot_metric("Spearman Correlation", limits = c(0.0, 1), labels = label_number(accuracy = 0.01))
p2 <- plot_metric("RMSE", limits = c(0, 40), labels = label_number(accuracy = 1))

# Combine vertically
combined_plot <- p1 | p2  # patchwork: stack vertically


#ggsave(paste(directory, "/PLOTS/Loss_Prediction_Comparison", ".png", sep = ""), combined_plot, width = 14, height = 6, dpi=150)

print(paste(directory, "/PLOTS/Loss_Prediction_Comparison", ".png", sep = ""))











#### ---- Determine New Cp and Combined Rank Scale --- ####


## Determine Cp and Combined Risk Rank thresholds based on Saffir Simpson ratios ##

model_type   <- "Weighted_Rank"   # Weighted_Rank
second_order <- "Not_Second_Order"    # "Not_Second_Order"  "Second_Order"
rank_type    <- "no_vars_yes_loss"

input_file <- "/home/users/afvessey/Hurricane_Loss_Model/Find_Best_Variation_Model/OUTPUT_Damage/Best_Variable_Combo_Loss_Weighted_Rank_20pct_Not_Second_Order_RMSE_no_vars_yes_loss_hybrid_optim.xlsx"

optimal_file_data_frame <- read_xlsx(input_file)

optimal_variables <- optimal_file_data_frame[3, 2]
optimal_weights   <- optimal_file_data_frame[4, 2]

if (rank_type == "rank") {
  if (second_order == "Not_Second_Order") {
    df <- renamed_hurricane_data
  }
  if (second_order == "Second_Order") {
    df <- renamed_hurricane_data_second_order
  }
}

if (rank_type == "no_vars_yes_loss") {
  if (second_order == "Not_Second_Order") {
    df <- renamed_hurricane_data_no_rank_vars_rank_loss
  }
  if (second_order == "Second_Order") {
    df <- renamed_hurricane_data_second_order_no_rank_vars_rank_loss
  }
}

X_full <- df[, !names(df) %in% "Loss_rank"]
y <- df$Loss_rank

vars <- strsplit(optimal_variables$Value, ",\\s*")[[1]]

X <- X_full[, vars, drop = FALSE]

weights   <- as.numeric(strsplit(optimal_weights$Value, ",\\s*")[[1]])

X <- X_full[, vars, drop = FALSE]
X <- as.data.frame(sapply(X, as.numeric))

# Force X into a numeric matrix
X <- as.matrix(sapply(X_full[, vars, drop = FALSE], as.numeric))

## Apply Weights ##
preds <- as.vector(X %*% weights)




cat(optimal_variables$Value, sep = "\n")


variable_rank <- rank(preds, ties.method = "min")


observed_Vs_predicted <- data.frame(observed = renamed_hurricane_data$Loss_rank,
                                    predicted = variable_rank)

observed_Vs_predicted$Loss  <- all_data$Loss
observed_Vs_predicted$Index <- seq(1, nrow(observed_Vs_predicted), 1)
observed_Vs_predicted$Storm_Names <- storm_names

closest_indices <- sapply(observed_Vs_predicted$predicted, function(x) which.min(abs(observed_Vs_predicted$observed - x)))

observed_Vs_predicted$Predicted_Loss <- observed_Vs_predicted$Loss[closest_indices]


RMSE        <- round(sqrt(mean((observed_Vs_predicted$Loss - observed_Vs_predicted$Predicted_Loss)^2)), 1)

#print(observed_Vs_predicted)
#print(RMSE)
#print(jj)


category_data <- data.frame(Year_Name_Landfall_Marker = storm_names,
                            USA_PRES_12hr_rank = renamed_hurricane_data$USA_PRES_12hr_rank,
                            USA_WIND = all_data$USA_WIND_12hr,
                            Predicted_Loss = observed_Vs_predicted$Predicted_Loss,
                            Observed_Loss = observed_Vs_predicted$Loss)

## Loss Scale ##
for (storm_index in seq(1, nrow(category_data), 1)) {

  ## Loss Scale ##  
  if (category_data$Predicted_Loss[storm_index] <= 1.3) {
    category_data$Loss_Category[storm_index] <- as.character(1)
  }
  if (between(category_data$Predicted_Loss[storm_index], 1.3, 5.3)) {
    category_data$Loss_Category[storm_index] <- as.character(2)
  }
  if (between(category_data$Predicted_Loss[storm_index], 5.3, 29.5)) {
    category_data$Loss_Category[storm_index] <- as.character(3)
  }
  if (between(category_data$Predicted_Loss[storm_index], 29.5, 119.4)) {
    category_data$Loss_Category[storm_index] <- as.character(4)
  }
  if (category_data$Predicted_Loss[storm_index] >= 119.4) {
    category_data$Loss_Category[storm_index] <- as.character(5)
  }
  ## Loss Scale ##  
  if (category_data$Observed_Loss[storm_index] <= 1.3) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(1)
  }
  if (between(category_data$Observed_Loss[storm_index], 1.3, 5.3)) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(2)
  }
  if (between(category_data$Observed_Loss[storm_index], 5.3, 29.5)) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(3)
  }
  if (between(category_data$Observed_Loss[storm_index], 29.5, 119.4)) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(4)
  }
  if (category_data$Observed_Loss[storm_index] >= 119.4) {
    category_data$Correct_Loss_Category[storm_index] <- as.character(5)
  }
}

## Saffir Simpson Scale ##
for (storm_index in seq(1, nrow(category_data), 1)) {

  ## Loss Scale ##  
  if (category_data$USA_WIND[storm_index] <= 83) {
    category_data$SSHWS_Category[storm_index] <- as.character(1)
  }
  if (between(category_data$USA_WIND[storm_index], 83, 96)) {
    category_data$SSHWS_Category[storm_index] <- as.character(2)
  }
  if (between(category_data$USA_WIND[storm_index], 96, 113)) {
    category_data$SSHWS_Category[storm_index] <- as.character(3)
  }
  if (between(category_data$USA_WIND[storm_index], 113, 137)) {
    category_data$SSHWS_Category[storm_index] <- as.character(4)
  }
  if (category_data$USA_WIND[storm_index] >= 137) {
    category_data$SSHWS_Category[storm_index] <- as.character(5)
  }
}



diffs <- as.numeric(category_data$SSHWS_Category) -
         as.numeric(category_data$Correct_Loss_Category)

#print(table(diffs))

#print(jj)



#### ---- PLOTTING ---- ####


### Loss Scale ###

#title <- expression(bold("c)") * " " * "Historical Normalised Economic Loss Per Hurricane and its Loss Scale Category (1979-2023)")
title <- "Historical Hurricane Averaged Normalised (to 2024) Economic Loss and its Predicted Loss Scale Category (1979-2024)"

ymax <- 270.0
x_intervals <- 20
y_title <- "Observed\nEconomic\nLoss\n(US$ bn)"

# Create a color palette for discrete values
ylorRd_colors <- c("yellow", "orange", "coral", "red2", "red4")  # Example colors

## order data frame by column value ##
category_data <- category_data[order(category_data$Observed_Loss), ]

category_data <- category_data[category_data$Observed_Loss > 5.0, ]

print(category_data)

# Create the plot
ggplot(data = category_data, 
       aes(x = reorder(Year_Name_Landfall_Marker, -Observed_Loss), y = Observed_Loss,
       fill = Loss_Category)) +
  geom_col(color = "black") +

  geom_text(aes(label = round(Observed_Loss)),  # Use Loss as the label
            position = position_stack(vjust = 0.5),  # Position the labels in the middle of the bars
            color = "black", size = 3) +  # Set label color
  
  geom_label(aes(label = Loss, y = 10), fill = alpha("white", 0.5), color = "black", size = 4, label.size = NA) +

  scale_fill_manual(values = ylorRd_colors) +

  theme_minimal(base_size = 14) +
  # labs(title = "Multi Linear Regression Model") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor.y = element_line(colour="grey90", size=0.1),
        panel.grid.major.y = element_line(colour="grey80", size=0.2),
        panel.grid.minor.x = element_line(colour="grey90", size=0.1),
        panel.grid.major.x = element_line(colour="grey80", size=0.2),
        legend.text = element_text(size = 16),
        legend.key = element_rect(fill = "white", colour = "black"),
        axis.line = element_line(color = "black", size = 0.8),  # Solid black lines on axis
        axis.ticks = element_line(color = "black", size = 0.8),
        axis.text.x = element_text(angle = 45, hjust = 1.0, vjust = 1.0, size = 10),
        axis.title.y = element_text(angle=0, vjust=0.5)) + # Black ticks on the axis)
  
  scale_y_continuous(name = y_title,
                     breaks = seq(0, ymax, x_intervals),
                     minor_breaks = seq(0, ymax, 5),
                     expand = c(0.0, 0.0),
                     limits=c(0, ymax)) +

  scale_x_discrete(name = "Hurricane Landfall (US$ 5+ bn)",
                     expand = c(0.02, 0.02)) +
  
  labs(title = title, fill = "Predicted\nLoss\nScale\nCategory")

ggsave(paste("PLOTS/Loss_Rank_Scale_Damage_Representation.png", sep = ""), width = 16, height = 6, dpi=150)

print(paste("PLOTS/Loss_Rank_Scale_Damage_Representation.png", sep = ""))

print(jj)




data_subset <- subset(all_data, select = c(SS_Category, USA_WIND_12hr, PRES_Category, USA_PRES_12hr, Loss_Category, Predicted_Loss, Combined_Loss_Rank, Loss, Storm_Names))
## order data frame by column value ##
data_subset <- data_subset[order(data_subset$USA_PRES_12hr), ]

print(head(data_subset, 10))

#print(jj)




print(mean(data_subset[data_subset$SS_Category == 1, ]$Loss))
print(mean(data_subset[data_subset$PRES_Category == 1, ]$Loss))
print(mean(data_subset[data_subset$Loss_Category == 1, ]$Loss))

print("---")
print(mean(data_subset[data_subset$SS_Category == 2, ]$Loss))
print(mean(data_subset[data_subset$PRES_Category == 2, ]$Loss))
print(mean(data_subset[data_subset$Loss_Category == 2, ]$Loss))

print("---")
print(mean(data_subset[data_subset$SS_Category == 3, ]$Loss))
print(mean(data_subset[data_subset$PRES_Category == 3, ]$Loss))
print(mean(data_subset[data_subset$Loss_Category == 3, ]$Loss))

print("---")
print(mean(data_subset[data_subset$SS_Category == 4, ]$Loss))
print(mean(data_subset[data_subset$PRES_Category == 4, ]$Loss))
print(mean(data_subset[data_subset$Loss_Category == 4, ]$Loss))

print("---")
print(mean(data_subset[data_subset$SS_Category == 5, ]$Loss))
print(mean(data_subset[data_subset$PRES_Category == 5, ]$Loss))
print(mean(data_subset[data_subset$Loss_Category == 5, ]$Loss))
