# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(cognitivemodels)
library(doParallel)
library(foreach)
library(plotly)

# Setup -------------------------------------------------------------------
discounts <- 0:2 #0:8
nblocks <- 1:2 #später 1:6
types <- 1 #1:6
true_pars <- expand.grid(lambda = 1:2, 
                         size = c(0.2, 0.5, 0.8), 
                         shape = c(0.2, 0.5, 0.8), 
                         r = 1, 
                         q = 1, 
                         b0 = 0.5, 
                         tau = c(0.1, 1, 2)) #more sensitive
true_pars <- true_pars[1:2,] #delete
runs <- 1:3 #1:50
#wie probab. die Handlung ungesetzt wird = tau


# Shepard dataframe -------------------------------------------------------


data_raw_shepard <- data.frame(
  size = as.factor( c("small", "small", "small", "small", "large", "large", "large", "large" )), # c(small, large)
  shape = as.factor( c("triangle", "triangle", "square", "square", "triangle", "triangle", "square", "square" )), # c(triangle, square)
  color = as.factor( c("black", "white", "black", "white", "black", "white", "black", "white")), # c(black, white)
  cat_1 = c(0, 1, 0, 1, 0, 1, 0, 1), # c(0, 1)
  cat_2 = c(0, 1, 1, 0, 0, 1, 1, 0), # c(0, 1)
  cat_3 = c(0, 0, 1, 1, 0, 1, 0, 1), # c(0, 1)
  cat_4 = c(0, 1, 1, 1, 0, 0, 0, 1), # c(0, 1)
  cat_5 = c(0, 1, 1, 0, 0, 1, 0, 1), # c(0, 1)
  cat_6 = c(1, 0, 0, 1, 0, 1, 1, 0) # c(0, 1)
)

data_shep <- data_raw_shepard %>% 
  mutate( size = recode(size, "small" = 0, "large" = 1),
          shape = recode(shape, "triangle" = 0, "square" = 1),
          color = recode(color, "black" = 0, "white" = 1))

# data_shep1 <- data_raw_shepard[, c("size", "shape", "color") := sapply(size, function(x) {if(x=="small") 0 
#                                                                                         else if (x=="large") 1}),
#                                                                sapply(shape, function(y) {if(y=="triangle") 0
#                                                                                          else if (y=="square") 1}),
#                                                                sapply(color, function(z) {if(z == "black") 0
#                                                                                          else if (z == "white") 1})]


# Simulate ----------------------------------------------------------------
registerDoParallel(4)


# Parameter Recovery
results <- 
  foreach(discount = discounts, .combine = "rbind", .packages = c("cognitivemodels", "data.table")) %:% 
    foreach (nblock = nblocks, .combine = "rbind") %:% 
      foreach (type = types, .combine = "rbind") %:% 
        foreach (row = 1:nrow(true_pars), .combine = "rbind") %:%
          foreach(run = runs, .combine = "rbind") %dopar% 
  {
    {
      {
        {
          # make true_par a single row of the expand.grid()
          true_par <- true_pars[row, ]
          
          # replicate all rows of data_shep by the number of nblock
          data <- data_shep[rep(1:nrow(data_shep), nblock),]
          
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
                                discount = discount)
            
            # Save the necessary components to the results
            data.table(
              run = run,
              discount = discount,
              nblock = nblock,
              type = type,
              row = row,
              names = names(coef(fitted_model)),
              par = coef(fitted_model),
              true_par = model$get_par("all"),
              convergence = fitted_model$fitobj$convergence
            )
          }
        }
      }
    }
  }


# Calcultation of the means of the runs -----------------------------------

#spread data.table by par and true_par
results_wide <- dcast(results, run + discount + nblock + type + row + convergence ~ names, value.var = c("par", "true_par"))

#calculate Means from the different runs
results_mean <-
foreach (discount_m = discounts, .combine = "rbind") %:%
  foreach (nblock_m = nblocks, .combine = "rbind") %:%
    foreach (type_m = types, .combine = "rbind") %:%
      foreach (true_par_m = 1:nrow(true_pars), .combine = "rbind") %dopar%
{
  {
    {
      {
        data.table(
          discount = discount_m,
          nblock = nblock_m,
          type = type_m,
          row = true_par_m,
          
          #filter all possible combinations of the parameters and calculate the mean
          results_wide[discount == discount_m & 
                       nblock == nblock_m & 
                       type == type_m & 
                       row == true_par_m, 
                       list(Mean_b0 = mean(par_b0), 
                            Mean_b1 = mean(par_b1),
                            Mean_color = mean(par_color),
                            Mean_lambda = mean(par_lambda),
                            Mean_q = mean(par_q),
                            Mean_r = mean(par_r),
                            Mean_shape = mean(par_shape),
                            Mean_size = mean(par_size),
                            Mean_tau = mean(par_tau),
                            true_b0 = true_par_b0,
                            true_b1 = true_par_b1,
                            true_color = true_par_color,
                            true_lambda = true_par_lambda,
                            true_q = true_par_q,
                            true_r = true_par_r,
                            true_shape = true_par_shape,
                            true_size = true_par_size,
                            true_tau = true_par_tau
                       )]
        )
      }
    }
  }
}        

#gather the mean results back to its original form
results_mean_long <- 
  unique(
    melt(results_mean, id.vars = c("discount", 
                                   "nblock", 
                                   "type", 
                                   "row"),
         measure.vars = list( c("Mean_b0", 
                                "Mean_b1", 
                                "Mean_color", 
                                "Mean_lambda", 
                                "Mean_q", 
                                "Mean_r", 
                                "Mean_shape", 
                                "Mean_size", 
                                "Mean_tau"),
                              c("true_b0", 
                                "true_b1", 
                                "true_color", 
                                "true_lambda", 
                                "true_q", 
                                "true_r", 
                                "true_shape", 
                                "true_size", 
                                "true_tau")),
         variable.name = "names",
         value.name = c( "par", 
                         "true_par")
         )
  )

#recoding the names column
results_mean_long[, "names" := sapply(names, function(x) {if(x==1) "b0" 
                                                          else if (x==2) "b1" 
                                                          else if (x==3) "color"
                                                          else if (x==4) "lambda"
                                                          else if (x==5) "q"
                                                          else if (x==6) "r"
                                                          else if (x==7) "shape"
                                                          else if (x==8) "size"
                                                          else if (x==9) "tau"
                                                          })]

# Violin Plots ------------------------------------------------------------

ggplot()

