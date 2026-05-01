
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

all_data <- subset(all_data, select = c(-Year_Name_12hr, -Landfall_Marker_12hr, -Year_Name_Landfall_Marker, -Unadjusted_Loss))

loss_type <- "Adjusted_Loss"    # "Unadjusted_Loss" "Adjusted_Loss"

names(all_data)[names(all_data) == loss_type] <- "Loss"


loss_data <- all_data$Loss




#print(colnames(all_data))



#### ------- Rank Processing ------- ######
hurricane_landfall_attributes_data <- all_data

hurricane_landfall_attributes_data$USA_PRES_Fullhr <- 1020 - hurricane_landfall_attributes_data$USA_PRES_12hr
hurricane_landfall_attributes_data$USA_PRES_12hr   <- 1020 - hurricane_landfall_attributes_data$USA_PRES_12hr



if (rank_type == "rank") {
  ranked_hurricane_data <- hurricane_landfall_attributes_data %>% mutate(across(everything(), ~rank(-.)))
}

## Normalise numbers by max in each column ##
if (rank_type == "no_vars_yes_loss") {

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

  ranked_hurricane_data <- df_ranked

  ranked_hurricane_data$Loss <- rank(-loss_data, ties.method = "min")
}

if (rank_type == "no_rank") {
  ranked_hurricane_data <- hurricane_landfall_attributes_data
}


renamed_hurricane_data <- ranked_hurricane_data %>% rename_all(~paste0(., "_rank"))
rm(hurricane_landfall_attributes_data, ranked_hurricane_data); gc()





#### ------- Second-Order Feature Engineering ------- ######

#renamed_hurricane_data_second_order <- subset(renamed_hurricane_data, select = -c(Loss_rank))
## Get number of columns
#n <- ncol(renamed_hurricane_data_second_order)

# Loop over all pairs in original order
#for (i in 1:(n-1)) {
#  for (j in (i+1):n) {
#    col1 <- names(renamed_hurricane_data_second_order)[i]
#    col2 <- names(renamed_hurricane_data_second_order)[j]
#    new_col_name <- paste0(col1, "_", col2, "_combined_rank")
#    renamed_hurricane_data_second_order[[new_col_name]] <- 
#      rank(renamed_hurricane_data_second_order[[col1]] + renamed_hurricane_data_second_order[[col2]])
#  }
#}
#renamed_hurricane_data_second_order$Loss_rank <- renamed_hurricane_data$Loss_rank


#### --- Rename missing column name --- ###
#if ("R34_Sum_LitPOP_Exposure_Value_Fullhr_rank" %in% names(renamed_hurricane_data)) {
#  print("Yes")
#}
#if ("R34_Sum_PresentDay_Population_Density_Fullhr_rank" %in% names(renamed_hurricane_data)) {
#  print("Yes")
#}
#
#if (paste0("R34_Sum_PresentDay_Population_Density_Fullhr_rank", "_", "R34_Sum_LitPOP_Exposure_Value_Fullhr_rank", "_combined_rank") %in% names(renamed_hurricane_data_second_order)) {
#  print("Yes")
#}
#
#colnames(renamed_hurricane_data_second_order)[
#  colnames(renamed_hurricane_data_second_order) == "R34_Sum_PresentDay_Population_Density_Fullhr_rank_R34_Sum_LitPOP_Exposure_Value_Fullhr_rank_combined_rank"
#] <- "R34_Sum_LitPOP_Exposure_Value_Fullhr_rank_R34_Sum_PresentDay_Population_Density_Fullhr_rank_combined_rank"


  


#correlation_data_frame <- data.frame(Variables = colnames(renamed_hurricane_data_second_order)[-ncol(renamed_hurricane_data_second_order)])
#for (var_index in 1:nrow(correlation_data_frame)) {
#  correlation_data_frame$Correlation[var_index] <- abs(cor(renamed_hurricane_data_second_order[[var_index]], renamed_hurricane_data_second_order$Loss_rank))
#}
#correlation_data_frame_sorted_filter <- correlation_data_frame %>% filter(Correlation > 0.1) %>% arrange(desc(Correlation))
#var_names <- c(correlation_data_frame_sorted_filter$Variables, "Loss_rank")
#renamed_hurricane_data_second_order <- renamed_hurricane_data_second_order[, var_names]





print(nrow(all_data))



## Convert landfall marker to another string ##
all_data$Landfall_Marker_12hr <- gsub("Closest Bypasser", "CB", landfall_marker)
all_data$Landfall_Marker_12hr <- gsub("Pre - 1st Max Landfall", "LF Max", landfall_marker)
all_data$Landfall_Marker_12hr <- gsub("Pre - 2nd Max Landfall", "LF 2", landfall_marker)
all_data$Landfall_Marker_12hr <- gsub("Pre - 3rd Max Landfall", "LF 3", landfall_marker)

all_data$Year_Name_LandfallMarker <- paste(year_name, "_", landfall_marker, sep = "")
all_data$Year_Name_LandfallMarker <- gsub("_", "\n", all_data$Year_Name_LandfallMarker)


storm_Year_Name_Landfall_Marker <- all_data$Year_Name_LandfallMarker
storm_names    <- gsub("_", " ", paste(all_data$Year_Name_12hr, sep = ""))
storm_raw_loss <- all_data$Loss
storm_loss_rank <- rank(-all_data$Loss, ties.method = "min")







## -- Combined Rank -- ##

second_order_yes_no         <- "no"
billion_plus_loss_yes_or_no <- "no"


df <- if (second_order_yes_no == "yes") renamed_hurricane_data_second_order else renamed_hurricane_data


X_full <- X_full <- df[, !names(df) %in% "Loss_rank"]
y      <- storm_loss_rank


print(model_type)


if (model_type == "optimisation") {

  print(paste("Frac: ", frac, sep = ""))

  best_spearman <- -Inf
  best_rmse <- Inf
  best_combo <- NULL
  best_weights <- NULL

  # Filter top correlated variables (only once per frac)
  cor_vals <- sapply(X_full, function(x) cor(x, y, use = "complete.obs"))
  X_full_subset <- X_full[, abs(cor_vals) > 0.1, drop = FALSE]

  for (k in seq(1, 10, 1)) {

    print(paste(k, " out of ", 10, sep = ""))

    # Generate variable combinations
    if (second_order_yes_no == "yes") {
      if (k <= 2) {
        combos <- combn(names(X_full_subset), k, simplify = FALSE)
      } else {
        set.seed(123)
        var_names <- names(X_full_subset)
        num_samples <- 200000
        combos <- replicate(num_samples, sample(var_names, k), simplify = FALSE)
      }
    } else {
      combos <- combn(names(X_full_subset), k, simplify = FALSE)
    }

    for (vars in combos) {

      #print(vars)

      X <- as.matrix(X_full_subset[, vars, drop = FALSE])


      if (eval_Metric == "Correl") {

        result <- tryCatch(
          optim(
            par = rep(1, ncol(X)),       # starting values
            fn = function(w) {
              combined <- as.vector(X %*% w)

              idx <- order(y)[1:floor(frac * length(y))]
              y_sub <- y[idx]

              # Compute ranks for both predicted and actual
              combined_ranks <- rank(combined, ties.method = "min")
              y_ranks <- rank(y_sub, ties.method = "min")

              # Return negative Spearman correlation (because optim minimizes)
              -cor(combined_ranks[idx], y_ranks, method = "spearman")
            },
            method = "L-BFGS-B",
            lower = rep(-1, ncol(X)),     # lower bounds
            upper = rep(1, ncol(X))       # upper bounds
          ),
          error = function(e) NULL
        )
 
        current_correlation <- -result$value

        if (!is.null(result)) {

          if (current_correlation > best_spearman) {

            best_spearman <- -result$value   # convert back to positive correlation
            best_combo <- vars
            best_weights <- result$par

            print(paste("Best Spearman correlation: ", round(best_spearman, 4), sep = ""))

            # compute combined scores using optimized weights
            combined_ranks <- as.vector(X %*% result$par)
            combined_ranks <- rank(combined_ranks, ties.method = "min")

            summary_df <- data.frame(
              Model = c("Best Spearman", "Number of Variables", "Variables Used", "Optimal Weights"),
              Value = c(round(best_spearman, 4), length(best_combo),
                        paste(best_combo, collapse = ", "),
                        paste(round(best_weights, 4), collapse = ", "))
            )

            results_df <- data.frame(Variable = best_combo, Weight = best_weights)

            out_path <- paste0(
              "OUTPUT_Subsets/Best_Variable_Combo_Loss_Weighted_Rank_",
              round(frac * 100), "pct_",
              ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".rds"
            )

            saveRDS(list(
              best_spearman = best_spearman,
              best_combo = best_combo,
              best_weights = best_weights
            ), file = out_path)

            ranks_out_path <- paste0(
              "OUTPUT_Subsets/Best_Variable_Combo_Loss_Weighted_Rank_",
              round(frac * 100), "pct_",
              ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
            )

            output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks,
                                       Loss_rank = y)
            write_xlsx(output_ranks, ranks_out_path)
          }
        }
      }




      if (eval_Metric == "RMSE") {

        result <- tryCatch(
          optim(
            par = rep(1, ncol(X)),       # starting values
            fn = function(w) {
              combined <- as.vector(X %*% w)

              idx <- order(y)[1:floor(frac * length(y))]
              y_sub <- y[idx]

              ## Need to use normal rank, as combined is already reversed ranked, with high values given low rank ##
              combined <- rank(combined, ties.method = "min")
              sqrt(mean((combined[idx] - y_sub)^2, na.rm = TRUE))  # RMSE
            },
            method = "L-BFGS-B",
            lower = rep(-1, ncol(X)),     # lower bounds (e.g., all weights ? 0)
            upper = rep(1, ncol(X))      # upper bounds (e.g., all weights ? 1)
         ),
        error = function(e) NULL
        )

        if (!is.null(result)) {
          rmse_val <- result$value   # optim minimizes, so this is already the best RMSE

          if (rmse_val < best_rmse) {

            best_rmse <- rmse_val
            best_combo <- vars
            best_weights <- result$par

            print(paste("Best RMSE: ", round(best_rmse, 2), sep = ""))

            # compute combined scores using optimized weights
            combined_ranks <- as.vector(X %*% result$par)
   
            # print(combined_ranks)
            # compute ranks
            combined_ranks <- rank(combined_ranks, ties.method = "min")

            summary_df <- data.frame(
              Model = c("Best RMSE", "Number of Variables", "Variables Used", "Optimal Weights"),
              Value = c(round(best_rmse, 4), length(best_combo),
                        paste(best_combo, collapse = ", "),
                        paste(round(best_weights, 4), collapse = ", "))
            )
        
            results_df <- data.frame(Variable = best_combo, Weight = best_weights)

            out_path <- paste0(
              "OUTPUT_Subsets/Best_Variable_Combo_Loss_Weighted_Rank_",
              round(frac * 100), "pct_",
                ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".rds"
            )

            ## Save as RDS (keeps structure intact, recommended)
            saveRDS(list(
              best_rmse = best_rmse,
              best_combo = best_combo,
              best_weights = best_weights
            ), file = out_path)

            ranks_out_path <- paste0(
              "OUTPUT_Subsets/Best_Variable_Combo_Loss_Weighted_Rank_",
              round(frac * 100), "pct_",
              ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
            )
            #print(ranks_out_path)
  
            output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks,
                                       Loss_rank = y)
            write_xlsx(output_ranks, ranks_out_path)

          }
        }

        # Memory cleanup for each vars loop
        rm(X, result, vars)
        gc()
      }

      # Memory cleanup after combos loop
      rm(combos)
      gc()
    }

    idx <- order(y)[1:floor(1.0 * length(y))]
    y_sub <- y[idx]
    print(sqrt(mean((combined_ranks[idx] - y_sub)^2, na.rm = TRUE)))

    idx <- order(y)[1:floor(0.2 * length(y))]
    y_sub <- y[idx]
    print(sqrt(mean((combined_ranks[idx] - y_sub)^2, na.rm = TRUE)))

    #rm(X_full_subset, cor_vals)
    gc()
  }
}





#### ------- Random Forest ------- ######

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}



if (model_type == "random_forest") {

  set.seed(123)
  best_cor <- -Inf
  best_rmse <- Inf  
  best_combo <- NULL
  predictions <- NULL


  # Instead of all combinations, consider top N most correlated variables
  top_vars <- head(order(abs(cor(X_full, y, use = "complete.obs")), decreasing = TRUE), 15)

  cor_vals <- sapply(X_full, function(x) cor(x, y, use = "complete.obs"))
  X_full   <- X_full[, abs(cor_vals) > 0.1]

  idx <- order(y)[1:floor(frac * length(y))]

  #print(idx)
  #print(jj)

  # Tuning parameters for Random Forst #
  control <- trainControl(method = "cv", number = 5)
  grid <- expand.grid(mtry = c(2, 4, 6, 8))


  for (k in seq(1, 10, 1)) {

    # Generate combinations (all for k ? 2, or random samples for k > 2)
    if (second_order_yes_no == "yes") {
      if (k <= 2) {
        combos <- combn(names(X_full), k, simplify = FALSE)
      } else {
        set.seed(123)
        var_names <- names(X_full)
        num_samples <- 500000
        combos <- replicate(num_samples, sample(var_names, k), simplify = FALSE)
      }
    }

    # Generate combinations (all for k ? 2, or random samples for k > 2)
    if (second_order_yes_no == "no") {
      combos <- combn(names(X_full), k, simplify = FALSE)
    }

    print(paste(k, " out of ", ncol(X_full)))

    for (vars in combos) {
      X <- X_full[, vars, drop = FALSE]
      X <- as.data.frame(sapply(X, as.numeric))
      
      ## Find the best model for the top percentile of data ##
      preds <- numeric(n)
      append_index <- 1

      for (i in seq(1, nrow(X), 1)) {   # idx
        train_X <- X[-i, , drop = FALSE]
        train_y <- y[-i]
        test_X  <- X[i, , drop = FALSE]

        #model <- train(
        #  x = train_X, y = train_y,
        #  method = "rf",
        #  trControl = control,
        #  tuneGrid = grid,
        #  ntree = 500
        #)

        set.seed(123)
        model <- tryCatch(
          randomForest(x = train_X, y = train_y, ntree = 500),
          error = function(e) NULL
        )

        if (is.null(model)) {
          preds[append_index] <- NA
        } else {
          preds[append_index] <- predict(model, newdata = test_X)
        }
        append_index <- append_index + 1

        ## Need to rank predictions as they could come out as non-linear and egative ##
        ## Need to use normal rank, as combined is already reversed ranked, with high values given low rank ##
        all_preds <- rank(preds, ties.method = "min")

        #all_preds <- preds

      }

      ## Get the costliest percentile after all data has been ranked ##
      index_preds <- all_preds[idx]


      if (eval_Metric == "Correl"){
        current_cor <- cor(index_preds, y[idx], method = "spearman")

        if (current_cor > best_cor) {
          best_cor <- current_cor
          print(best_cor)
          best_combo <- vars
          predictions <- preds

          combined_ranks <- preds

          summary_df <- data.frame(
            Model = c("Best Correlation", "Number of Variables", "Variables Used"),
            Value = c(round(best_cor, 4), length(best_combo), paste(best_combo, collapse = ", "))
          )

          results_df <- data.frame(
            Actual = y,
            Predicted = combined_ranks
          )

          second_order    <- ifelse(second_order_yes_no == "yes", "_Second_Order", "_Not_Second_Order")
          billion_or_all  <- ifelse(billion_plus_loss_yes_or_no == "yes", "_Billion_Plus", "_All_Storms")
          normalisation   <- ifelse(normalised_yes_no == "yes", "_Normalised", "_UnNormalised")

          out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_RF_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".xlsx"
            )

          write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)

          ## save optimal ranks ##
          ranks_out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_RF_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
          )
   
          output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks,
                                     Loss_rank = y)
          write_xlsx(output_ranks, ranks_out_path)
        }
      }

      if (eval_Metric == "RMSE"){
        current_rmse <- rmse(y[idx], index_preds)

        if (current_rmse < best_rmse) {
          best_rmse <- current_rmse
          print(best_rmse)
          best_combo <- vars
          predictions <- preds

          combined_ranks <- all_preds

          #print(preds)


          summary_df <- data.frame(
            Model = c("Best Correlation", "Number of Variables", "Variables Used"),
            Value = c(round(best_cor, 4), length(best_combo), paste(best_combo, collapse = ", "))
          )

          results_df <- data.frame(
            Actual = y,
            Predicted = combined_ranks
          )

          second_order    <- ifelse(second_order_yes_no == "yes", "_Second_Order", "_Not_Second_Order")
          billion_or_all  <- ifelse(billion_plus_loss_yes_or_no == "yes", "_Billion_Plus", "_All_Storms")
          normalisation   <- ifelse(normalised_yes_no == "yes", "_Normalised", "_UnNormalised")

          out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_RF_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_1.xlsx"
            )

          write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)

          ## save optimal ranks ##
          ranks_out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_RF_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_",eval_Metric, "_", rank_type, "_combined_ranks_1.xlsx"
          )
   
          output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks,
                                     Loss_rank = y)
          write_xlsx(output_ranks, ranks_out_path)

        }
      }

    }
  }

  print("-----")
  idx <- order(y)[1:floor(1.0 * length(y))]
  print(sqrt(mean((combined_ranks[idx] - y[idx])^2, na.rm = TRUE)))

  idx <- order(y)[1:floor(0.2 * length(y))]
  print(sqrt(mean((combined_ranks[idx] - y[idx])^2, na.rm = TRUE)))

}




#### ------- Linear Regression ------- ######


if (model_type == "linear_regression") {

  set.seed(123)
  best_cor <- -Inf
  best_rmse <- Inf
  best_combo <- NULL
  predictions <- NULL


  # Instead of all combinations, consider top N most correlated variables
  top_vars <- head(order(abs(cor(X_full, y, use = "complete.obs")), decreasing = TRUE), 15)

  cor_vals <- sapply(X_full, function(x) cor(x, y, use = "complete.obs"))
  X_full   <- X_full[, abs(cor_vals) > 0.1]

  idx <- order(y)[1:floor(frac * length(y))]

  print(idx)

  for (k in seq(1, 10, 1)) {

    # Generate combinations (all for k ? 2, or random samples for k > 2)
    if (second_order_yes_no == "yes") {
      if (k <= 2) {
        combos <- combn(names(X_full), k, simplify = FALSE)
      } else {
        set.seed(123)
        var_names <- names(X_full)
        num_samples <- 500000
        combos <- replicate(num_samples, sample(var_names, k), simplify = FALSE)
      }
    }

    # Generate combinations (all for k ? 2, or random samples for k > 2)
    if (second_order_yes_no == "no") {
      combos <- combn(names(X_full), k, simplify = FALSE)
    }

    print(paste(k, " out of ", ncol(X_full)))

    for (vars in combos) {
      X <- X_full[, vars, drop = FALSE]
      X <- as.data.frame(sapply(X, as.numeric))
      
      ## Find the best model for the top percentile of data ##
      n <- length(idx)
      preds <- numeric(n)
      append_index <- 1

      for (i in seq(1, nrow(X), 1)) {   # idx
        train_X <- X[-i, , drop = FALSE]
        train_y <- y[-i]
        test_X  <- X[i, , drop = FALSE]

        model <- tryCatch(
          lm(train_y ~ ., data = data.frame(train_y = train_y, train_X)),
          error = function(e) NULL
        )

        if (is.null(model)) {
          preds[append_index] <- NA
        } else {
          preds[append_index] <- predict(model, newdata = test_X)
        }
        append_index <- append_index + 1

        ## Need to rank predictions as they could come out as non-linear and egative ##
        ## Need to use normal rank, as combined is already reversed ranked, with high values given low rank ##
        all_preds <- rank(preds, ties.method = "min")

      }

      ## Get the costliest percentile after all data has been ranked ##
      index_preds <- all_preds[idx]

      if (eval_Metric == "Correl"){

        current_cor <- cor(index_preds, y[idx], method = "spearman")

        if (!is.na(current_cor) && current_cor > best_cor) {
          best_cor <- current_cor
          print(best_cor)
          best_combo <- vars
          predictions <- preds

          combined_ranks <- all_preds

          summary_df <- data.frame(
            Model = c("Best Correlation", "Number of Variables", "Variables Used"),
            Value = c(round(best_cor, 4), length(best_combo), paste(best_combo, collapse = ", "))
          )

          results_df <- data.frame(
            Actual = y,
            Predicted = combined_ranks
          )

          second_order    <- ifelse(second_order_yes_no == "yes", "_Second_Order", "_Not_Second_Order")
          billion_or_all  <- ifelse(billion_plus_loss_yes_or_no == "yes", "_Billion_Plus", "_All_Storms")
          normalisation   <- ifelse(normalised_yes_no == "yes", "_Normalised", "_UnNormalised")

          out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_Linear_Regression_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".xlsx"
            )

          #print(out_path)

          write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)

          ## save optimal ranks ##
          ranks_out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_Linear_Regression_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
          )
   
          output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks,
                                     Loss_rank = y)
          write_xlsx(output_ranks, ranks_out_path)
        }
      }


      if (eval_Metric == "RMSE"){
        current_rmse <- rmse(y[idx], index_preds)

        if (current_rmse < best_rmse) {
          best_rmse <- current_rmse
          print(best_rmse)
          best_combo <- vars
          predictions <- all_preds

          combined_ranks <- all_preds

          summary_df <- data.frame(
            Model = c("Best Correlation", "Number of Variables", "Variables Used"),
            Value = c(round(best_cor, 4), length(best_combo), paste(best_combo, collapse = ", "))
          )

          results_df <- data.frame(
            Actual = y,
            Predicted = combined_ranks
          )

          second_order    <- ifelse(second_order_yes_no == "yes", "_Second_Order", "_Not_Second_Order")
          billion_or_all  <- ifelse(billion_plus_loss_yes_or_no == "yes", "_Billion_Plus", "_All_Storms")
          normalisation   <- ifelse(normalised_yes_no == "yes", "_Normalised", "_UnNormalised")

          out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_Linear_Regression_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".xlsx"
            )

          #print(out_path)

          write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)

          ## save optimal ranks ##
          ranks_out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_Linear_Regression_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_",eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
          )
   
          output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks,
                                     Loss_rank = y)
          write_xlsx(output_ranks, ranks_out_path)

        }
      }
    }
  }
}






#### ------- GAM ------- ######


if (model_type == "GAM") {

  library(mgcv)

  set.seed(123)
  best_cor <- -Inf
  best_rmse <- Inf
  best_combo <- NULL
  predictions <- NULL

  # Instead of all combinations, consider top N most correlated variables
  top_vars <- head(order(abs(cor(X_full, y, use = "complete.obs")), decreasing = TRUE), 15)

  cor_vals <- sapply(X_full, function(x) cor(x, y, use = "complete.obs"))
  X_full   <- X_full[, abs(cor_vals) > 0.1]

  idx <- order(y)[1:floor(frac * length(y))]
  print(idx)

  for (k in seq(1, 10, 1)) {

    # Generate combinations
    if (second_order_yes_no == "yes") {
      if (k <= 2) {
        combos <- combn(names(X_full), k, simplify = FALSE)
      } else {
        set.seed(123)
        var_names <- names(X_full)
        num_samples <- 500000
        combos <- replicate(num_samples, sample(var_names, k), simplify = FALSE)
      }
    } else {
      combos <- combn(names(X_full), k, simplify = FALSE)
    }

    print(paste(k, " out of ", ncol(X_full)))

    for (vars in combos) {
      X <- X_full[, vars, drop = FALSE]
      X <- as.data.frame(sapply(X, as.numeric))

      ## Find the best model for the top percentile of data ##
      n <- length(idx)
      preds <- numeric(n)
      append_index <- 1

      for (i in seq(1, nrow(X), 1)) {
        train_X <- X[-i, , drop = FALSE]
        train_y <- y[-i]
        test_X  <- X[i, , drop = FALSE]

        # Construct smooth GAM formula dynamically
        smooth_terms <- paste0("s(", names(train_X), ")", collapse = " + ")
        formula_gam <- as.formula(paste("train_y ~", smooth_terms))

        model <- tryCatch(
          gam(formula_gam, data = data.frame(train_y = train_y, train_X), method = "REML"),
          error = function(e) NULL
        )

        if (is.null(model)) {
          preds[append_index] <- NA
        } else {
          preds[append_index] <- predict(model, newdata = test_X)
        }
        append_index <- append_index + 1

        ## Rank predictions
        all_preds <- rank(preds, ties.method = "min")
      }

      ## Evaluate ##
      index_preds <- all_preds[idx]

      if (eval_Metric == "Correl") {
        current_cor <- cor(index_preds, y[idx], method = "spearman")

        if (!is.na(current_cor) && current_cor > best_cor) {
          best_cor <- current_cor
          print(best_cor)
          best_combo <- vars
          predictions <- preds
          combined_ranks <- all_preds

          summary_df <- data.frame(
            Model = c("Best Correlation", "Number of Variables", "Variables Used"),
            Value = c(round(best_cor, 4), length(best_combo), paste(best_combo, collapse = ", "))
          )

          results_df <- data.frame(Actual = y, Predicted = combined_ranks)

          out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_GAM_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".xlsx"
          )

          write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)

          ## save optimal ranks ##
          ranks_out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_GAM_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
          )

          output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks, Loss_rank = y)
          write_xlsx(output_ranks, ranks_out_path)
        }
      }

      if (eval_Metric == "RMSE") {
        current_rmse <- rmse(y[idx], index_preds)

        if (current_rmse < best_rmse) {
          best_rmse <- current_rmse
          print(best_rmse)
          best_combo <- vars
          predictions <- all_preds
          combined_ranks <- all_preds

          summary_df <- data.frame(
            Model = c("Best RMSE", "Number of Variables", "Variables Used"),
            Value = c(round(best_rmse, 4), length(best_combo), paste(best_combo, collapse = ", "))
          )

          results_df <- data.frame(Actual = y, Predicted = combined_ranks)

          out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_GAM_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, ".xlsx"
          )

          write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)

          ## save optimal ranks ##
          ranks_out_path <- paste0(
            "OUTPUT_Subsets/Best_Variable_Combo_Loss_GAM_",
            round(frac * 100), "pct_",
            ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"), "_", eval_Metric, "_", rank_type, "_combined_ranks.xlsx"
          )

          output_ranks <- data.frame(Optimal_Combined_Ranks = combined_ranks, Loss_rank = y)
          write_xlsx(output_ranks, ranks_out_path)
        }
      }
    }
  }
}

















if (model_type == "pairwise_optimisation") {

  # --- Pairwise RankNet loss ---
  pairwise_ranknet_loss <- function(v, X, y, lambda = 0.0) {
    # softmax transform -> w >= 0, sum(w)=1
    expv <- exp(v - max(v))
    w <- expv / sum(expv)

    s <- as.vector(X %*% w)  # scores
    # create all pairs where y[i] < y[j] (i should rank above j)
    pairs <- which(outer(y, y, FUN = "<"), arr.ind = TRUE)
    dif <- s[pairs[,1]] - s[pairs[,2]]   # s_i - s_j
    # logistic pairwise loss
    losses <- log1p(exp(-dif))
    loss <- mean(losses)
    # optional L2 regularization on weights
    if (lambda > 0) {
      loss <- loss + lambda * sum(w^2)
    }
    return(loss)
  }

  for (frac in fractions) {

    print(paste("Frac: ", frac, sep = ""))

    best_loss <- Inf
    best_combo <- NULL
    best_weights <- NULL

    # Filter top correlated variables (only once per frac)
    cor_vals <- sapply(X_full, function(x) cor(x, y, use = "complete.obs"))
    X_full_subset <- X_full[, abs(cor_vals) > 0.1, drop = FALSE]

    for (k in seq(1, 10, 1)) {

      print(paste(k, " out of ", 10, sep = ""))

      # Generate variable combinations
      if (second_order_yes_no == "yes") {
        if (k <= 2) {
          combos <- combn(names(X_full_subset), k, simplify = FALSE)
        } else {
          set.seed(123)
          var_names <- names(X_full_subset)
          num_samples <- 200000
          combos <- replicate(num_samples, sample(var_names, k), simplify = FALSE)
        }
      } else {
        combos <- combn(names(X_full_subset), k, simplify = FALSE)
      }

      for (vars in combos) {

        X <- as.matrix(X_full_subset[, vars, drop = FALSE])

        if (eval_Metric == "RankNet") {

          result <- tryCatch(
            optim(
              par = rep(0, ncol(X)),     # unconstrained v
              fn = function(v) pairwise_ranknet_loss(v, X, y),
              method = "BFGS",
              control = list(maxit = 500)
            ),
            error = function(e) NULL
          )

          if (!is.null(result)) {
            loss_val <- result$value
            if (is.null(best_loss) || loss_val < best_loss) {

              # decode weights from v
              expv <- exp(result$par - max(result$par))
              w_opt <- expv / sum(expv)

              best_loss <- loss_val
              best_combo <- vars
              best_weights <- w_opt

              print(paste("Best RankNet loss: ", round(best_loss, 4), sep = ""))

              # compute scores and ranks
              combined_scores <- as.vector(X %*% w_opt)
              combined_ranks <- rank(-combined_scores, ties.method = "min")

              summary_df <- data.frame(
                Model = c("Best RankNet Loss", "Number of Variables", "Variables Used", "Optimal Weights"),
                Value = c(round(best_loss, 4), length(best_combo),
                          paste(best_combo, collapse = ", "),
                          paste(round(best_weights, 4), collapse = ", "))
              )

              results_df <- data.frame(Variable = best_combo, Weight = best_weights)

              out_path <- paste0(
                "OUTPUT_Subsets/Best_Variable_Combo_Loss_Weighted_Rank_",
                round(frac * 100), "pct_",
                ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"),
                "_", eval_Metric, ".xlsx"
              )

              ranks_out_path <- paste0(
                "OUTPUT_Subsets/Best_Variable_Combo_Loss_Weighted_Rank_",
                round(frac * 100), "pct_",
                ifelse(second_order_yes_no == "yes", "Second_Order", "Not_Second_Order"),
                "_", eval_Metric, "_combined_ranks.xlsx"
              )

              output_ranks <- data.frame(
                Optimal_Combined_Ranks = combined_ranks,
                Loss_rank = y
              )
              #write_xlsx(output_ranks, ranks_out_path)

              #write_xlsx(list(Summary = summary_df, Weights = results_df), path = out_path)
            }
          }
        }

        rm(X, result, vars)
        gc()
      }

      rm(combos)
      gc()
    }

    rm(X_full_subset, cor_vals)
    gc()
  }
}
