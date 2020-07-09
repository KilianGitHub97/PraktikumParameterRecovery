# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(cognitivemodels)
library(doParallel)
library(plotly)
library(jtools)
library(pgirmess)

# Setup -------------------------------------------------------------------
discounts <- 8
nblocks <- 6
types <- 1
true_pars <- expand.grid(lambda = 1, 
                         size = 0.333, 
                         shape = 0.333, 
                         r = 1, 
                         q = 1, 
                         b0 = 0.5, 
                         tau = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 2))
runs <- 1:50 
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


# Parallel Setup ----------------------------------------------------------
cluster <- makeCluster(8)
registerDoParallel(cluster)
foreach::getDoParWorkers()

# Parameter recovery simulation -------------------------------------------

# Parameter Recovery
results <- 
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

write.csv(results,"D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\LambdaTau.csv", row.names = FALSE)


# reading in parts of the data --------------------------------------------

results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat1.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat6.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\BlockTau.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\CatTau.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\LambdaTau.csv")

# deleting the convergence (if necessary)
results <- results[convergence != 1]


# Calcultation of the means and medians of the runs -----------------------

#spread data.table by par and true_par
results_wide <- dcast(results, run + discount + nblock + type + row + convergence ~ names, value.var = c("par", "true_par"))

#calculate Means from the different runs
results_mean <-
foreach (discount_m = discounts, .combine = "rbind", .packages = "data.table") %:%
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
          
          #filter all possible combinations of the parameters and calculate the mean & median
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
                            Median_b0 = median(par_b0), 
                            Median_b1 = median(par_b1),
                            Median_color = median(par_color),
                            Median_lambda = median(par_lambda),
                            Median_q = median(par_q),
                            Median_r = median(par_r),
                            Median_shape = median(par_shape),
                            Median_size = median(par_size),
                            Median_tau = median(par_tau),
                            true_b0 = true_par_b0,
                            true_b1 = true_par_b1,
                            true_color = true_par_color,
                            true_lambda = true_par_lambda,
                            true_q = true_par_q,
                            true_r = true_par_r,
                            true_shape = true_par_shape,
                            true_size = true_par_size,
                            true_tau = true_par_tau)]
        )
      }
    }
  }
}        

#gather the mean & median results back to its original form
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
                              c("Median_b0", 
                                "Median_b1", 
                                "Median_color", 
                                "Median_lambda", 
                                "Median_q", 
                                "Median_r", 
                                "Median_shape", 
                                "Median_size", 
                                "Median_tau"),
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
         value.name = c( "Mean_par",
                         "Median_par",
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


# Calculation of the absolute values of estimation errors -----------------
head(results_wide)

# Add absolute values as new columns
results_wide$abs_par_b0 <- abs(results_wide$par_b0 - results_wide$true_par_b0)
results_wide$abs_par_b1 <- abs(results_wide$par_b1 - results_wide$true_par_b1)
results_wide$abs_par_r <- abs(results_wide$par_r - results_wide$true_par_r)
results_wide$abs_par_q <- abs(results_wide$par_q - results_wide$true_par_q)
results_wide$abs_par_lambda <- abs(results_wide$par_lambda - results_wide$true_par_lambda)
results_wide$abs_par_size <- abs(results_wide$par_size - results_wide$true_par_size)
results_wide$abs_par_shape <- abs(results_wide$par_shape - results_wide$true_par_shape)
results_wide$abs_par_color <- abs(results_wide$par_color - results_wide$true_par_color)
results_wide$abs_par_tau <- abs(results_wide$par_tau - results_wide$true_par_tau)


#graphically checking the distribution
ggplot(data = results_wide,
       mapping = aes(x = abs_par_tau )) +
  geom_histogram() +
  facet_wrap(~discount) +
  theme_apa()

# formally checking distribution
shapiro.test(results_wide$abs_par_b0) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_b1) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_r) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_q) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_lambda) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_size) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_color) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_shape) #significant: no normal distibution can be assumed
shapiro.test(results_wide$abs_par_tau) #significant: no normal distibution can be assumed

#H1: There is a significant difference between the different discounts, when tau is kept constant
kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 0.1,]) #H1 is rejected: p: 0.5147
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 0.1,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 0.3,]) #H1 is rejected: 0.9944
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 0.3,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 0.5,]) #H1 is rejected: p: 0.9437
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 0.5,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 0.7,]) #H1 is rejected: p: 0.3213
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 0.7,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 0.9,]) #H1 is rejected: p: 0.6356
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 0.9,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 1.1,]) #H1 is rejected: p: 0.6804
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 1.1,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 1.3,]) #H1 is accepted: p: 0.04147 // handle with care, bc. the estimates got worse with increasing tau
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 1.3,]) #but all comparisons are negative though?

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 1.5,]) #H1 is rejected: p: 0.8126
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 1.5,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 1.7,]) #H1 is rejected: p: 0.3262
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 1.7,])

kruskal.test(abs_par_tau ~ discount, results_wide[true_par_tau == 2.0,]) #H1 is rejected: p: 0.7071
kruskalmc(abs_par_tau ~ discount, results_wide[true_par_tau == 2.0,])


# H1: There is a significant difference between the absolute values of tau when the discounts are kept constant
kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 1,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 1,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 2,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 2,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 3,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 3,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 4,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 4,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 5,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 5,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 6,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 6,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 7,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 7,] )

kruskal.test(abs_par_tau ~ true_par_tau, results_wide[discount == 8,]) #significant
kruskalmc(abs_par_tau ~ true_par_tau, results_wide[discount == 8,] )


# Impact of omitting the first 0-8 rows on recovering tau -----------------

# true_tau ~ estimated tau (dependent on discounts)
ggplot(data = results_wide,
       mapping = aes(x = true_par_tau,
                     y = par_tau)) +
  stat_summary(fun = "median", geom = "point")+
  geom_line(aes(x = true_par_tau, y = true_par_tau), color = "red") +
  facet_wrap(~discount) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_apa()

# estimated tau ~ discount
ggplot(data = results_wide,
       mapping = aes(x = as.factor(discount),
                     y = par_tau)) +
  geom_violin(width = 0.7)+
  stat_summary(fun = "median", geom = "point")+
  geom_line(aes(x = discount + 1, y = true_par_tau), color = "red", size = 0.5)+
  facet_wrap(~true_par_tau, nrow = 2)+
  ylim(0, 3)+
  xlab("Discount")+
  ylab("Tau")+
  theme_apa()


# Impact of learning with more Blocks on recovery of tau ------------------

# estimated tau ~ nblock
ggplot(data = results_wide,
       mapping = aes(x = as.factor(nblock),
                     y = par_tau)) +
  geom_violin() +
  stat_summary(fun = "median", geom = "point")+
  geom_line(aes(x = nblock, y = true_par_tau), color = "red")+
  facet_wrap(~true_par_tau) + 
  xlab("Number of repetitive Blocks seen during learning phase") +
  ylab("Estimated Tau") +
  theme_minimal()

# true_tau ~ estimated tau (depending on nblocks)
ggplot(data = results_wide,
       mapping = aes(x = true_par_tau,
                     y = par_tau)) +
  stat_summary(fun = "median", geom = "point")+
  geom_line( aes(x = true_par_tau, y = true_par_tau), color = "red") +
  facet_wrap(~nblock) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_apa()


# ability of the GCM to grasp the difficulty of the categories on the tau parameter ------------

# When classifications of these six types are encountered for the first time, they consistently differ in difficulty according to the
# ranking I < II < ( I I I , IV, V) < VI (with I I I , IV, and V about equal in difficulty) . The same ranking is found for
# learning and memory tasks, inspection time and error scores, and a variety of different kinds of stimuli. (From Shepard)

# estimated tau ~ category
ggplot(data = results_wide,
       mapping = aes(x = as.factor(type),
                     y = par_tau)) +
  geom_violin() +
  stat_summary(fun = "median", geom = "point")+
  geom_line(aes(x = type, y = true_par_tau), color = "red")+
  facet_wrap(~true_par_tau) + 
  xlab("Category") +
  ylab("Estimated Tau") +
  theme_minimal()
  
# true_tau ~ estimated tau (depending on nblocks)
ggplot(data = results_wide,
       mapping = aes(x = true_par_tau,
                     y = par_tau)) +
  stat_summary(fun = "median", geom = "point")+
  geom_line( aes(x = true_par_tau, y = true_par_tau), color = "red") +
  facet_wrap(~type) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_apa()  

# Influence of Lambda on the recovery of Tau ------------------------------

# estimated tau ~ lambda
ggplot(data = results_wide,
       mapping = aes(x = as.factor(true_par_lambda),
                     y = par_tau)) +
  geom_violin() +
  stat_summary(fun = "median", geom = "point")+
  geom_line(aes(x = true_par_lambda, y = true_par_tau), color = "red")+
  facet_wrap(~true_par_tau) + 
  xlab("Lambda") +
  ylab("Estimated Tau") +
  theme_minimal()

# true_tau ~ estimated tau (depending on nblocks)
ggplot(data = results_wide,
       mapping = aes(x = true_par_tau,
                     y = par_tau)) +
  stat_summary(fun = "median", geom = "point")+
  geom_line( aes(x = true_par_tau, y = true_par_tau), color = "red") +
  facet_wrap(~true_par_lambda) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_apa()  
# Recovery of all the parameters other than tau. --------------------------
results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat6.csv")
results <- results[convergence != 1]
results_wide <- dcast(results, run + discount + nblock + type + row + convergence ~ names, value.var = c("par", "true_par"))

#recovery of b0, when b0 is 0.5
ggplot(data = results_wide,
       mapping = aes(y = par_b0)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_b0), color = "red") +
  geom_hline(aes(yintercept = median(par_b0)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of b1, , when b1 is 0.5
ggplot(data = results_wide,
       mapping = aes(y = par_b1)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_b1), color = "red") +
  geom_hline(aes(yintercept = median(par_b1)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of size, when size is ~0.33
ggplot(data = results_wide,
       mapping = aes(y = par_size)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_size), color = "red") +
  geom_hline(aes(yintercept = median(par_size)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of color, when color is ~0.33
ggplot(data = results_wide,
       mapping = aes(y = par_color)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_color), color = "red") +
  geom_hline(aes(yintercept = median(par_color)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of shape, when shape is ~0.33
ggplot(data = results_wide,
       mapping = aes(y = par_shape)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_shape), color = "red") +
  geom_hline(aes(yintercept = median(par_shape)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of r, when r is 1
ggplot(data = results_wide,
       mapping = aes(y = par_r)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_r), color = "red") +
  geom_hline(aes(yintercept = median(par_r)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of q, when q is 1
ggplot(data = results_wide,
       mapping = aes(y = par_q)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_q), color = "red") +
  geom_hline(aes(yintercept = median(par_q)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of lambda, when lambda is 1
ggplot(data = results_wide,
       mapping = aes(y = par_lambda)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_lambda), color = "red") +
  geom_hline(aes(yintercept = median(par_lambda)), color="blue", linetype="dashed")+
  facet_wrap(~discount)+
  theme_apa()

#recovery of tau when discount = 8
ggplot(data = results_wide[discount == 8,],
       mapping = aes(y = par_tau)) +
  geom_histogram()+
  geom_hline(aes(yintercept = true_par_tau), color = "red") +
  geom_hline(aes(yintercept = median(par_tau)), color="blue", linetype="dashed")+
  facet_wrap(~true_par_tau)+
  theme_apa()

