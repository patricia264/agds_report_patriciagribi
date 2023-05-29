#stepwise forward regression

step_regression <- function(dep_var, indep_var){
  
  # the dependent variable
  y <- dep_var
  
  # vector of the independent variables
  all_vars_selected <- indep_var
  
  predictors <- NULL
  
  selected_candidates <- NULL
  
  continue <- TRUE
  
  AIC_last <- 99999
  AIC_new <- 99998
  
  df_metrics <- tibble()
  
  while(continue){
    
    # AIC changes with each round, the model improves as long as the AIC gets bigger
    AIC_last <- AIC_new
    
    # the r_squared value is set to zero. it gets resetted in each round of the for-loop
    curr_r_squared <- 0
    
    # implementation of the stepwise forward regression
    for (i in all_vars_selected) {
      
      # in each round, a new formula for the linear regression model is created
      current_model <- paste0("GPP_NT_VUT_REF~", paste(c(predictors, i), collapse="+"))
      current_formula <- as.formula(current_model)
      
      # specific model
      linear_model <- lm(current_formula, data = daily_fluxes)
      
      # r-squared gets extracted
      new_r_squared <- summary(linear_model)$r.squared 
      
      # AIC gets extracted
      AIC_new <- extractAIC(linear_model)[2]
      
      # if the new calculated r-squared of this model is higher, it gets selected 
      if(new_r_squared > curr_r_squared){
        
        curr_r_squared <- new_r_squared
        
        # the new best variable for the model is saved 
        selected_candidates <- as.character(i)
        
        # the new best model is saved 
        curr_best_model <- current_model
        
      } else{
        
      }
    } 
    
    # the variable with the best fit of one full round is deleted
    all_vars_selected <- all_vars_selected[!all_vars_selected == selected_candidates]
    
    # the variable with the best fit is added to the predictors
    predictors <- c(predictors, selected_candidates)
    
    # best model and metrics are chosen
    linear_model <- lm(as.formula(curr_best_model), 
                       data = daily_fluxes)
    
    r_squared <- summary(linear_model)$r.squared 
    
    AIC <- extractAIC(linear_model)[2]
    
    # models and metrics are saved
    df_metrics <-
      bind_rows(df_metrics,
                tibble(
                  model = curr_best_model,
                  rsq = r_squared,
                  AIC = AIC
                ))
    
    # the algorithm terminates when all variables are selected for the model
    if(length(all_vars_selected) == 0) 
      continue <- FALSE
    
  }
  
  return (df_metrics)
}