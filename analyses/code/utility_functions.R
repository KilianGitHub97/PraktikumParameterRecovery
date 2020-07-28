# Libraries ---------------------------------------------------------------
library(data.table)
library(cognitivemodels)

#library(devtools)
#install_github("janajarecki/cognitivemodels@development", force = TRUE)

# Parameter recovery simulation -------------------------------------------
# Parameter Recovery
recover <- function(discounts, nblocks, types, true_pars, runs, d = data_shep){
  foreach(discount = discounts, .combine = "rbind", .packages = c("cognitivemodels", "data.table")) %:% 
    foreach (nblock = nblocks, .combine = "rbind"  ) %:% 
      foreach (type = types, .combine = "rbind"  ) %:% 
        foreach (row = 1:nrow(true_pars), .combine = "rbind") %:%
          foreach(run = runs, .combine = "rbind") %dopar% 
  {
    {
      {
        {
          # make true_par a single row of the expand.grid()
          true_par <- true_pars[row, ]
          
          # replicate all rows of data_shep by the number of nblock
          data <- d[rep(1:nrow(d), nblock),]
          
          # GCM with fixed parameters
          model <- gcm(data = data,
                       formula = ~ size + shape + color, 
                       class = paste("cat", type, sep = "_"), 
                       choicerule = "softmax", 
                       fix = true_par,
                       discount = 0)
          
          # Simulation of repeated measurements
          predictions <- predict(model)
          
          {
          
            # Add the predicted binominal value ({0,1}) to the data
            data$simulations <- rbinom(length(predictions) , 1, predictions)
            
            # Estimate the parameters given the response (simulations) and the class (cat_)
            fitted_model <- gcm(data = data,
                                formula = simulations ~ size + shape + color, 
                                class = paste("cat", type, sep = "_"), 
                                choicerule = "softmax", 
                                discount = discount,
                                fix = list(r = true_par$r,
                                           q = true_par$q)
                                )
            
            # Save the necessary components to the results
            data.table(
              run = run,
              discount = discount,
              nblock = nblock,
              type = type,
              row = row,
              names = names(coef(fitted_model)),
              par = coef(fitted_model),
              true_par = model$get_par("all")[names(coef(fitted_model))],
              convergence = fitted_model$fitobj$convergence
            )
          }
        }
      }
    }
  }
}