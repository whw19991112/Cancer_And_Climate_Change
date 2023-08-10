library(plm)

#load data from the clean steps
load('E:/Haowen Wang/Cancer and climate change/SEER data/CI5_clean.Rdata')
load('E:/Haowen Wang/Cancer and climate change/SEER data/data_all_SEER.Rdata')

#Rename cancers & Filter gender
cancer_list_name <- c("All Sites", "Lip","Salivary Gland","Tonsil","Nasopharynx","Hypopharynx",
                      "Esophagus","Stomach","Colon and Rectum","Liver","Gallbladder","Pancreas","Larynx",
                      "Lung and Bronchus","Melanoma of the Skin",
                      "Mesothelioma","Kaposi Sarcoma","Penis","Prostate","Testis",
                      "Kidney and Renal Pelvis","Urinary Bladder","Brain and Other Nervous System",
                      "Thyroid","Hodgkin Lymphoma","Non-Hodgkin Lymphoma","Myeloma","Lymphocytic Leukemia")
data_SEER_male <- data_all_SEER%>%
  filter(CANCER %in% cancer_list_name)%>%
  filter(Sex == 1)
data_SEER_male <- data_SEER_male %>%
  mutate(cancer_code = case_when(
    CANCER == "All Sites" ~ 1,
    CANCER == "Lip" ~ 2,
    CANCER == "Salivary Gland" ~ 5,CANCER == "Tonsil" ~ 6,
    CANCER == "Nasopharynx" ~ 8,CANCER == "Hypopharynx" ~ 9,
    CANCER == "Esophagus" ~ 11,CANCER == "Stomach" ~ 19,
    CANCER == "Colon and Rectum" ~ 21,CANCER == "Liver" ~ 31,
    CANCER == "Gallbladder" ~ 41,CANCER == "Pancreas" ~ 42,
    CANCER == "Larynx" ~ 44,CANCER == "Lung and Bronchus" ~ 46,
    CANCER == "Melanoma of the Skin" ~ 63,CANCER == "Mesothelioma" ~ 64,
    CANCER == "Kaposi Sarcoma" ~ 65,CANCER == "Penis" ~ 100,
    CANCER == "Prostate" ~ 101,CANCER == "Testis" ~ 102,
    CANCER == "Kidney and Renal Pelvis" ~ 109,CANCER == "Urinary Bladder" ~ 112,
    CANCER == "Brain and Other Nervous System" ~ 131,CANCER == "Thyroid" ~ 142,
    CANCER == "Hodgkin Lymphoma" ~ 154,CANCER == "Non-Hodgkin Lymphoma" ~ 160,
    CANCER == "Myeloma" ~ 163,CANCER == "Lymphocytic Leukemia" ~ 164,
    TRUE ~ NA_integer_
  ))

selected_cancer_male <- c(1,2,5,6,8,9,11,19,21,31,41,42,44,46,63,64,65,100,101,102,109,
                          112,131,142,154,160,163,164)
data_CI5_male <- CI5_all %>%
  filter(cancer_code %in% selected_cancer_male)%>%
  filter(SEX == 1)

#Check common columns
common_columns <- intersect(names(data_SEER_male), names(data_CI5_male))
common_columns
data_SEER_male_common <- data_SEER_male[, common_columns]
data_CI5_male_common <- data_CI5_male[, common_columns]
data_male_all <- rbind(data_SEER_male_common,data_CI5_male_common)

cancer_list <- c(1,2,5,6,8,9,11,19,21,31,41,42,44,46,63,64,65,100,101,102,109,
                 112,131,142,154,160,163,164)

#######################################aged 6074########################################

# Fit the model for each cancer type and store the results in a list

plmmodel_results <- lapply(cancer_list, function(cancer_type) {
  # Filter the data for the current cancer type
  data_subset <- filter(data_male_all, cancer_code == cancer_type)
  pdata <- pdata.frame(data_subset,index = c("GEOID", "YEAR"))
  
  # Initialize an empty dataframe to store the model results
  results_df <- data.frame()
  
  # Run models and store each in the dataframe
  model0 <- plm(N60_74 ~ average0
                +taverage0+oaverage0+paverage0+saverage0
                +taverage1+oaverage1+paverage1+saverage1
                +taverage2+oaverage2+paverage2+saverage2
                +taverage3+oaverage3+paverage3+saverage3
                +taverage4+oaverage4+paverage4+saverage4
                +taverage5+oaverage5+paverage5+saverage5
                +taverage6+oaverage6+paverage6+saverage6
                +taverage7+oaverage7+paverage7+saverage7
                +taverage8+oaverage8+paverage8+saverage8
                +taverage9+oaverage9+paverage9+saverage9
                +taverage10+oaverage10+paverage10+saverage10
                +Smoking+GDP+PM25
                ,data = pdata, model = "within")
  
  # Get the summary of the model
  summary_model <- summary(model0)
  # FDR p value correction
  adjusted_p_values  <-  p.adjust(summary_model$coefficients[, "Pr(>|t|)"], method = "BH")
  # Extract coefficients, standard errors, and calculate p-values
  results_df <- rbind(results_df, data.frame(term = names(coef(model0)), 
                                             estimate = coef(model0), 
                                             std.error = summary_model$coefficients[, "Std. Error"], 
                                             p.value = summary_model$coefficients[, "Pr(>|t|)"], 
                                             adjusted_p_values = adjusted_p_values,
                                             model = paste0("model",0)))
  
  for(i in 1:10) {
    # Create a string of the formula
    formula_str <- paste0("N60_74 ~ average", i, 
                          "+taverage0+oaverage0+paverage0+saverage0",
                          "+taverage1+oaverage1+paverage1+saverage1",
                          "+taverage2+oaverage2+paverage2+saverage2",
                          "+taverage3+oaverage3+paverage3+saverage3",
                          "+taverage4+oaverage4+paverage4+saverage4",
                          "+taverage5+oaverage5+paverage5+saverage5",
                          "+taverage6+oaverage6+paverage6+saverage6",
                          "+taverage7+oaverage7+paverage7+saverage7",
                          "+taverage8+oaverage8+paverage8+saverage8",
                          "+taverage9+oaverage9+paverage9+saverage9",
                          "+taverage10+oaverage10+paverage10+saverage10+Smoking+GDP+PM25")
    
    # Convert the string to a formula object
    formula_obj <- as.formula(formula_str)
    
    model <- plm(formula_obj, data = pdata, model = "within")
    
    # Get the summary of the model
    summary_model <- summary(model)
    adjusted_p_values  <-  p.adjust(summary_model$coefficients[, "Pr(>|t|)"], method = "BH")
    # Extract coefficients, standard errors, and calculate p-values
    results_df <- rbind(results_df, data.frame(term = names(coef(model)), 
                                               estimate = coef(model), 
                                               std.error = summary_model$coefficients[, "Std. Error"], 
                                               p.value = summary_model$coefficients[, "Pr(>|t|)"], 
                                               adjusted_p_values = adjusted_p_values,
                                               model = paste0("model",i)))
  }
  
  # Return the dataframe of model results
  return(results_df)
})
plmmodel_results[1]

# Initialize an empty dataframe to store the merged results
fixedmerged_results <- data.frame()

# Loop over each cancer type and its corresponding model results
for(i in 1:length(cancer_list)) {
  # Add a new column for the cancer type
  plmmodel_results[[i]]$cancer_type <- cancer_list[i]
  
  # Merge the results
  fixedmerged_results <- bind_rows(fixedmerged_results, plmmodel_results[[i]])
}


# Check the merged results
head(fixedmerged_results)
fixedmerged_results$age <- 1
setwd("E:/Haowen Wang/Cancer and climate change/export/Forestplot/NDVI_Moving_Average/6074")
write.csv(fixedmerged_results, "plm_model_results_6074_male_navilag.csv", row.names = FALSE)

#reshape the results to fit the plots
cancer_type_names <- c("All", "Lip","Salivary glands","Tonsil","Nasopharynx","Hypopharynx",
                       "Oesophagus","Stomach","Colon","Liver","Gallbladder","Pancreas","Larynx","Lung",
                       "Melanoma of skin","Mesothelioma","Kaposi sarcoma","Penis","Prostate","Testis","Kidney","Bladder"
                       ,"Brain and CNS","Thyroid","Hodgkin lymphoma","Non-Hodgkin lymphoma","Multiple myeloma","Lymphoid leukaemia")  # Add all cancer type names
names(cancer_type_names) <- c(1,2,5,6,8,9,11,19,21,31,41,42,44,46,63,64,65,100,101,102,109,
                              112,131,142,154,160,163,164)  # Add the corresponding cancer type numbers as names
results <- fixedmerged_results[fixedmerged_results$term %in% c("average0","average1","average2","average3","average4","average5","average6","average7","average8","average9","average10"), ]%>%
  mutate(HR = exp(estimate),  # Calculate hazard ratios
         LowerCI = exp(estimate - 1.96 * (std.error)),  # Calculate lower confidence interval
         UpperCI = exp(estimate + 1.96 * (std.error)))%>%# Calculate upper confidence interval
  mutate(cancer_type_name = cancer_type_names[as.character(cancer_type)]) 

unique_terms <- unique(results$cancer_type_name)
generate_forest_plot <- function(variable) {
  # Filter the data to include only the specified variable
  results_filtered <- results %>%
    filter(cancer_type_name == variable)
  lag_coefficients <- results_filtered$estimate[1:11] 
  lag_std_errors <- results_filtered$std.error[1:11]
  lag_data <- data.frame(
    lag = 0:10,
    coefficient = lag_coefficients,
    std.error = lag_std_errors
  )
  lag_data$LowerCI <- results_filtered$LowerCI
  lag_data$UpperCI <- results_filtered$UpperCI
  lag_data$HR <- results_filtered$HR
  lag_data$lag <- factor(lag_data$lag, levels = 0:10)  # change the factor levels to ascending order
  
  # Calculate the range of x-axis
  x_min <- min(lag_data$LowerCI, na.rm = TRUE) *0.95 # leave 5% blank at the lower end
  x_max <- max(lag_data$UpperCI, na.rm = TRUE) *1.05 # leave 5% blank at the upper end
  
  # Create the forest plot for the specified variable
  forest_plot <- ggplot(lag_data, aes(y = HR, x = lag)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "steelblue") +
    
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.3, color = "black") +
    geom_point(size = 3, color = "steelblue") +
    labs(title = paste("Per 0.1 increase of NDVI on", variable,"cancer incidence (/10000), aged 60-74, male"), y = "HR", x = "Lag") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    coord_cartesian(ylim = c(x_min, x_max)) # Set y-axis limits
  
  return(forest_plot)
}

forest_plots <- lapply(unique_terms, generate_forest_plot)
print(forest_plots)
setwd("E:/Haowen Wang/Cancer and climate change/export/Forestplot/NDVI_Moving_Average/6074")

# Export all the forest plots as separate image files
for (i in seq_along(forest_plots)) {
  plot_name <- unique_terms[i]
  file_name <- paste0("forest_plots_6074_plm_lag", plot_name, ".jpg")
  ggsave(file_name, forest_plots[[i]], width = 18, height = 8, units = "in")
}

#######################################aged 4559########################################
# Fit the model for each cancer type and store the results in a list

plmmodel_results <- lapply(cancer_list, function(cancer_type) {
  # Filter the data for the current cancer type
  data_subset <- filter(data_SEER_male_common, cancer_code == cancer_type)
  pdata <- pdata.frame(data_subset,index = c("GEOID", "YEAR"))
  
  # Initialize an empty dataframe to store the model results
  results_df <- data.frame()
  
  # Run models and store each in the dataframe
  model0 <- plm(N45_59 ~ average0
                +taverage0+oaverage0+paverage0+saverage0
                +taverage1+oaverage1+paverage1+saverage1
                +taverage2+oaverage2+paverage2+saverage2
                +taverage3+oaverage3+paverage3+saverage3
                +taverage4+oaverage4+paverage4+saverage4
                +taverage5+oaverage5+paverage5+saverage5
                +taverage6+oaverage6+paverage6+saverage6
                +taverage7+oaverage7+paverage7+saverage7
                +taverage8+oaverage8+paverage8+saverage8
                +taverage9+oaverage9+paverage9+saverage9
                +taverage10+oaverage10+paverage10+saverage10
                +Smoking+GDP+PM25
                ,data = pdata, model = "within")
  
  # Get the summary of the model
  summary_model <- summary(model0)
  # Extract coefficients, standard errors, and calculate p-values
  adjusted_p_values  <-  p.adjust(summary_model$coefficients[, "Pr(>|t|)"], method = "BH")
  # Extract coefficients, standard errors, and calculate p-values
  results_df <- rbind(results_df, data.frame(term = names(coef(model0)), 
                                             estimate = coef(model0), 
                                             std.error = summary_model$coefficients[, "Std. Error"], 
                                             p.value = summary_model$coefficients[, "Pr(>|t|)"], 
                                             adjusted_p_values = adjusted_p_values,
                                             model = paste0("model",i)))
  
  for(i in 1:10) {
    # Create a string of the formula
    formula_str <- paste0("N45_59 ~ average", i, 
                          "+taverage0+oaverage0+paverage0+saverage0",
                          "+taverage1+oaverage1+paverage1+saverage1",
                          "+taverage2+oaverage2+paverage2+saverage2",
                          "+taverage3+oaverage3+paverage3+saverage3",
                          "+taverage4+oaverage4+paverage4+saverage4",
                          "+taverage5+oaverage5+paverage5+saverage5",
                          "+taverage6+oaverage6+paverage6+saverage6",
                          "+taverage7+oaverage7+paverage7+saverage7",
                          "+taverage8+oaverage8+paverage8+saverage8",
                          "+taverage9+oaverage9+paverage9+saverage9",
                          "+taverage10+oaverage10+paverage10+saverage10+Smoking+GDP+PM25")
    
    # Convert the string to a formula object
    formula_obj <- as.formula(formula_str)
    
    model <- plm(formula_obj, data = pdata, model = "within")
    
    # Get the summary of the model
    summary_model <- summary(model)
    adjusted_p_values  <-  p.adjust(summary_model$coefficients[, "Pr(>|t|)"], method = "BH")
    # Extract coefficients, standard errors, and calculate p-values
    results_df <- rbind(results_df, data.frame(term = names(coef(model)), 
                                               estimate = coef(model), 
                                               std.error = summary_model$coefficients[, "Std. Error"], 
                                               p.value = summary_model$coefficients[, "Pr(>|t|)"], 
                                               adjusted_p_values = adjusted_p_values,
                                               model = paste0("model",i)))
  }
  
  # Return the dataframe of model results
  return(results_df)
})
plmmodel_results[1]
# Initialize an empty dataframe to store the merged results
fixedmerged_results <- data.frame()

# Loop over each cancer type and its corresponding model results
for(i in 1:length(cancer_list)) {
  # Add a new column for the cancer type
  plmmodel_results[[i]]$cancer_type <- cancer_list[i]
  
  # Merge the results
  fixedmerged_results <- bind_rows(fixedmerged_results, plmmodel_results[[i]])
}

# Check the merged results
head(fixedmerged_results)
fixedmerged_results$age <- 2
setwd("E:/Haowen Wang/Cancer and climate change/export/Forestplot/NDVI_Moving_Average/4559")
write.csv(fixedmerged_results, "plm_model_results_4559_male_navilag.csv", row.names = FALSE)

cancer_type_names <- c("All", "Lip","Salivary glands","Tonsil","Nasopharynx","Hypopharynx",
                       "Oesophagus","Stomach","Colon","Liver","Gallbladder","Pancreas","Larynx","Lung",
                       "Melanoma of skin","Mesothelioma","Kaposi sarcoma","Penis","Prostate","Testis","Kidney","Bladder"
                       ,"Brain and CNS","Thyroid","Hodgkin lymphoma","Non-Hodgkin lymphoma","Multiple myeloma","Lymphoid leukaemia")  # Add all cancer type names
names(cancer_type_names) <- c(1,2,5,6,8,9,11,19,21,31,41,42,44,46,63,64,65,100,101,102,109,
                              112,131,142,154,160,163,164)  # Add the corresponding cancer type numbers as names
results <- fixedmerged_results[fixedmerged_results$term %in% c("average0","average1","average2","average3","average4","average5","average6","average7","average8","average9","average10"), ]%>%
  mutate(HR = exp(estimate/100),  # Calculate hazard ratios
         LowerCI = exp(estimate/100 - 1.96 * (std.error/100)),  # Calculate lower confidence interval
         UpperCI = exp(estimate/100 + 1.96 * (std.error/100)))%>%# Calculate upper confidence interval
  mutate(cancer_type_name = cancer_type_names[as.character(cancer_type)]) 

unique_terms <- unique(results$cancer_type_name)
generate_forest_plot <- function(variable) {
  # Filter the data to include only the specified variable
  results_filtered <- results %>%
    filter(cancer_type_name == variable)
  lag_coefficients <- results_filtered$estimate[1:11] 
  lag_std_errors <- results_filtered$std.error[1:11]
  lag_data <- data.frame(
    lag = 0:10,
    coefficient = lag_coefficients,
    std.error = lag_std_errors
  )
  lag_data$LowerCI <- results_filtered$LowerCI
  lag_data$UpperCI <- results_filtered$UpperCI
  lag_data$HR <- results_filtered$HR
  lag_data$lag <- factor(lag_data$lag, levels = 0:10)  # change the factor levels to ascending order
  
  # Calculate the range of x-axis
  x_min <- min(lag_data$LowerCI, na.rm = TRUE) *0.95 # leave 5% blank at the lower end
  x_max <- max(lag_data$UpperCI, na.rm = TRUE) *1.05 # leave 5% blank at the upper end
  
  # Create the forest plot for the specified variable
  forest_plot <- ggplot(lag_data, aes(y = HR, x = lag)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "steelblue") +
    
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.3, color = "black") +
    geom_point(size = 3, color = "steelblue") +
    labs(title = paste("Per 0.1 increase of NDVI on", variable,"cancer incidence (/10000), aged 45-59, male"), y = "HR", x = "Lag") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    coord_cartesian(ylim = c(x_min, x_max)) # Set y-axis limits
  
  return(forest_plot)
}

forest_plots <- lapply(unique_terms, generate_forest_plot)
# print(forest_plots)
setwd("E:/Haowen Wang/Cancer and climate change/export/Forestplot/NDVI_Moving_Average/4559")
# Export all the forest plots as separate image files
for (i in seq_along(forest_plots)) {
  plot_name <- unique_terms[i]
  file_name <- paste0("forest_plots_4559_plm_lag", plot_name, ".jpg")
  ggsave(file_name, forest_plots[[i]], width = 18, height = 8, units = "in")
}

#######################################all########################################
# Fit the model for each cancer type and store the results in a list

plmmodel_results <- lapply(cancer_list, function(cancer_type) {
  # Filter the data for the current cancer type
  data_subset <- filter(data_male_all, cancer_code == cancer_type)
  pdata <- pdata.frame(data_subset,index = c("GEOID", "YEAR"))
  
  # Initialize an empty dataframe to store the model results
  results_df <- data.frame()
  
  # Run models and store each in the dataframe
  model0 <- plm(N_all ~ average0
                +taverage0+oaverage0+paverage0+saverage0
                +taverage1+oaverage1+paverage1+saverage1
                +taverage2+oaverage2+paverage2+saverage2
                +taverage3+oaverage3+paverage3+saverage3
                +taverage4+oaverage4+paverage4+saverage4
                +taverage5+oaverage5+paverage5+saverage5
                +taverage6+oaverage6+paverage6+saverage6
                +taverage7+oaverage7+paverage7+saverage7
                +taverage8+oaverage8+paverage8+saverage8
                +taverage9+oaverage9+paverage9+saverage9
                +taverage10+oaverage10+paverage10+saverage10
                +Smoking+GDP+PM25
                ,data = pdata, model = "within")
  
  # Get the summary of the model
  summary_model <- summary(model0)
  # Extract coefficients, standard errors, and calculate p-values
  adjusted_p_values  <-  p.adjust(summary_model$coefficients[, "Pr(>|t|)"], method = "BH")
  # Extract coefficients, standard errors, and calculate p-values
  results_df <- rbind(results_df, data.frame(term = names(coef(model0)), 
                                             estimate = coef(model0), 
                                             std.error = summary_model$coefficients[, "Std. Error"], 
                                             p.value = summary_model$coefficients[, "Pr(>|t|)"], 
                                             adjusted_p_values = adjusted_p_values,
                                             model = paste0("model",i)))
  
  for(i in 1:10) {
    # Create a string of the formula
    formula_str <- paste0("N_all ~ average", i, 
                          "+taverage0+oaverage0+paverage0+saverage0",
                          "+taverage1+oaverage1+paverage1+saverage1",
                          "+taverage2+oaverage2+paverage2+saverage2",
                          "+taverage3+oaverage3+paverage3+saverage3",
                          "+taverage4+oaverage4+paverage4+saverage4",
                          "+taverage5+oaverage5+paverage5+saverage5",
                          "+taverage6+oaverage6+paverage6+saverage6",
                          "+taverage7+oaverage7+paverage7+saverage7",
                          "+taverage8+oaverage8+paverage8+saverage8",
                          "+taverage9+oaverage9+paverage9+saverage9",
                          "+taverage10+oaverage10+paverage10+saverage10+Smoking+GDP+PM25")
    
    # Convert the string to a formula object
    formula_obj <- as.formula(formula_str)
    
    model <- plm(formula_obj, data = pdata, model = "within")
    
    # Get the summary of the model
    summary_model <- summary(model)
    adjusted_p_values  <-  p.adjust(summary_model$coefficients[, "Pr(>|t|)"], method = "BH")
    # Extract coefficients, standard errors, and calculate p-values
    results_df <- rbind(results_df, data.frame(term = names(coef(model)), 
                                               estimate = coef(model), 
                                               std.error = summary_model$coefficients[, "Std. Error"], 
                                               p.value = summary_model$coefficients[, "Pr(>|t|)"], 
                                               adjusted_p_values = adjusted_p_values,
                                               model = paste0("model",i)))
  }
  
  # Return the dataframe of model results
  return(results_df)
})
plmmodel_results[1]
# Initialize an empty dataframe to store the merged results
fixedmerged_results <- data.frame()

# Loop over each cancer type and its corresponding model results
for(i in 1:length(cancer_list)) {
  # Add a new column for the cancer type
  plmmodel_results[[i]]$cancer_type <- cancer_list[i]
  
  # Merge the results
  fixedmerged_results <- bind_rows(fixedmerged_results, plmmodel_results[[i]])
}

# Check the merged results
head(fixedmerged_results)
fixedmerged_results$age <- 0
setwd("E:/Haowen Wang/Cancer and climate change/export/Forestplot/NDVI_Moving_Average/all age")
write.csv(fixedmerged_results, "plm_model_results_all_male_navilag.csv", row.names = FALSE)

cancer_type_names <- c("All", "Lip","Salivary glands","Tonsil","Nasopharynx","Hypopharynx",
                       "Oesophagus","Stomach","Colon","Liver","Gallbladder","Pancreas","Larynx","Lung",
                       "Melanoma of skin","Mesothelioma","Kaposi sarcoma","Penis","Prostate","Testis","Kidney","Bladder"
                       ,"Brain and CNS","Thyroid","Hodgkin lymphoma","Non-Hodgkin lymphoma","Multiple myeloma","Lymphoid leukaemia")  # Add all cancer type names
names(cancer_type_names) <- c(1,2,5,6,8,9,11,19,21,31,41,42,44,46,63,64,65,100,101,102,109,
                              112,131,142,154,160,163,164)  # Add the corresponding cancer type numbers as names
results <- fixedmerged_results[fixedmerged_results$term %in% c("average0","average1","average2","average3","average4","average5","average6","average7","average8","average9","average10"), ]%>%
  mutate(HR = exp(estimate/0.1),  # Calculate hazard ratios
         LowerCI = exp(estimate/0.1 - 1.96 * (std.error/0.1)),  # Calculate lower confidence interval
         UpperCI = exp(estimate/0.1 + 1.96 * (std.error/0.1)))%>%# Calculate upper confidence interval
  mutate(cancer_type_name = cancer_type_names[as.character(cancer_type)]) 

unique_terms <- unique(results$cancer_type_name)
generate_forest_plot <- function(variable) {
  # Filter the data to include only the specified variable
  results_filtered <- results %>%
    filter(cancer_type_name == variable)
  lag_coefficients <- results_filtered$estimate[1:11] 
  lag_std_errors <- results_filtered$std.error[1:11]
  lag_data <- data.frame(
    lag = 0:10,
    coefficient = lag_coefficients,
    std.error = lag_std_errors
  )
  lag_data$LowerCI <- results_filtered$LowerCI
  lag_data$UpperCI <- results_filtered$UpperCI
  lag_data$HR <- results_filtered$HR
  lag_data$lag <- factor(lag_data$lag, levels = 0:10)  # change the factor levels to ascending order
  
  # Calculate the range of x-axis
  x_min <- min(lag_data$LowerCI, na.rm = TRUE) *0.9 # leave 5% blank at the lower end
  x_max <- max(lag_data$UpperCI, na.rm = TRUE) *1.1 # leave 5% blank at the upper end
  
  # Create the forest plot for the specified variable
  forest_plot <- ggplot(lag_data, aes(y = HR, x = lag)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "steelblue") +
    
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.3, color = "black") +
    geom_point(size = 3, color = "steelblue") +
    labs(title = paste("Per 0.1 increase of NDVI on", variable,"cancer incidence (/10000), all age, male"), y = "HR", x = "Lag") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    coord_cartesian(ylim = c(x_min, x_max)) # Set y-axis limits
  
  return(forest_plot)
}

forest_plots <- lapply(unique_terms, generate_forest_plot)
print(forest_plots)
setwd("E:/Haowen Wang/Cancer and climate change/export/Forestplot/NDVI_Moving_Average/all age")
# Export all the forest plots as separate image files
for (i in seq_along(forest_plots)) {
  plot_name <- unique_terms[i]
  file_name <- paste0("forest_plots_all_plm_lag ", plot_name, ".jpg")
  ggsave(file_name, forest_plots[[i]], width = 18, height = 8, units = "in")
}