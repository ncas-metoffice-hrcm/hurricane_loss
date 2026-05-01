# hurricane_loss

Analysis code and data for:

Vessey, A. F., Baker, A. J., Marcellin-Honore, V., and Michelin, J. Combining hazard, exposure and vulnerability data to predict historical United States hurricane losses. *Natural Hazards and Earth System Sciences* . [https://doi.org/10.5194/egusphere-2025-5161](https://doi.org/10.5194/egusphere-2025-5161)
  
**Scripts**  
- *Plot_Historical_Hurricane_Damage.R* — R code to plot historical hurricane damage (loss), used for Figure 4.

- *Plot_Saffir_Simpson_Scale_Against_Damage.R* — R code to plot historical hurricane damage per Saffir-Simpson category, used for Figures 5 and 6.

- *plot_Katrina_LitPOP.R* — R code to plot the track and footprint of Hurricane Katrina (2005) against LitPOP exposure value data, used for Figure 3.

- *Extract_Hurricane_Loss_Attributes.R* — R code to determine risk variables for all historical hurricanes and output the *US_hurricane_landfall_data_Vessey_et_al_2026_NHESS.csv* file. (N.B. datasets need to be downloaded from cited sources before running this script, and file paths changed accordingly.)

- *Extract_Hurricane_Loss_Attributes_Test_R34_R50_R64_Models.R* — R code to test the R34, R50 and R64 estimation models, used for Figures 2, S1 and S2.

- *Correlation_Testing.R* — R code to determine correlation coeficients for all risk variables, used for Figure 7.

- *Find_Optimal_Prediction_Model.R* — R code to determine optimal model configurations with input data from *US_hurricane_landfall_data_Vessey_et_al_2026_NHESS.csv* file, used for Figures 8, 9 and 10.

- *Compare_Damage_Prediction_Vmax_Cp_Optimal_Model.R* — R code to compare the optimal model configurations (from 7) against loss prediction from using v<sub>max</sub> and c<sub>p</sub>.
  
  
**Note on data file**  
  
*US_hurricane_landfall_data_Vessey_et_al_2026_NHESS.csv*
  
We calculated risk variables using hurricane footprints along the 12hr post-landfall track ("_12hr") and along the full track ("_Fullhr"), and using R34, R50 and R64 radii ("R34", "R50" and "R64"). Therefore, for example, the total LitPOP impacted per hurricane is given in six columns (track x2 and size x3).
