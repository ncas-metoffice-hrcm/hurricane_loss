# hurricane_loss

Analysis code and data for:

Vessey, A. F., Baker, A. J., Marcellin-Honore, V., and Michelin, J. Combining hazard, exposure and vulnerability data to predict historical United States hurricane losses. *Natural Hazards and Earth System Sciences* . [https://doi.org/10.5194/egusphere-2025-5161](https://doi.org/10.5194/egusphere-2025-5161)


*Plot_Historical_Hurricane_Damage.R* - R code to plot historical hurricane damage, used for Figure 4.
*Plot_Saffir_Simpson_Scale_Against_Damage.R* - R code to plot historical hurricane damage per Saffir-Simpson cetegory, used for Figures 5 and 6.
*plot_Katrina_LitPOP.R* - R code to plot the track and footprint of Hurricane Katrina (2005) against LitPOP exposure value data, used for Figure 3.
*Extract_Hurricane_Loss_Attributes.R* - R code to determine risk variables for all historical hurricanes (i.e., the US_hurricane_landfall_data_Vessey_et_al_2026_NHESS.csv file. (N.B. multiple datasets need to be downloaded before running this script, and file paths will need to be changed.)
*Extract_Hurricane_Loss_Attributes_Test_R34_R50_R64_Models.R* - R code to test the R34, R50 and R64 estimation models, used for Figures 2, S1 and S2.
*Correlation_Testing.R* - R code to determine correlation coeficients for all risk variables, used for Figure 7.
*Find_Optimal_Prediction_Model.R* - R code to determine optimal model configurations using data from US_hurricane_landfall_data_Vessey_et_al_2026_NHESS.csv file, used for Figures 8, 9 and 10.
*Compare_Damage_Prediction_Vmax_Cp_Optimal_Model.R* - R code to compare the optimal model configurations (from 7) against damage prediction from using v<sub>max</sub> and c<sub>p</sub>.
