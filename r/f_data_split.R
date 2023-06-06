# data splitting and model pre-processing formulation
#library(recipes)

data_split <- function (data, prop){
  
  # Data splitting
  set.seed(1982)  # for reproducibility
  split <- rsample::initial_split(data, prop, strata = "VPD_F")
  daily_fluxes_train <- rsample::training(split)
  daily_fluxes_test <- rsample::testing(split)
  
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = daily_fluxes_train |> drop_na()) |> 
    recipes::step_BoxCox(recipes::all_predictors(), -VPD_F, -TA_F) |> 
    recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
    recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())
 
  
  return(list(pp = pp, daily_fluxes_train = daily_fluxes_train, 
              daily_fluxes_test = daily_fluxes_test))
  
}
