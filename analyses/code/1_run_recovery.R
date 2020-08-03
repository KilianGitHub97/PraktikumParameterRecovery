install.packages("psych")
# Libraries ---------------------------------------------------------------
pkgs <- c("doParallel",
          "jtools",
          "pgirmess",
          "emmeans",
          "tidyverse")

lapply(pkgs, library, character.only = TRUE)

# Setwd -------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("utility_functions.R")

# Shepard dataframe -------------------------------------------------------

data_raw_shepard <- data.frame(
  size = as.factor( c("small", "small", "small", "small", "large", "large", "large", "large" )), 
  shape = as.factor( c("triangle", "triangle", "square", "square", "triangle", "triangle", "square", "square" )), 
  color = as.factor( c("black", "white", "black", "white", "black", "white", "black", "white")), 
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


# Parallel Setup ----------------------------------------------------------
cluster <- makeCluster(8)
registerDoParallel(cluster)
foreach::getDoParWorkers()

# Setup -------------------------------------------------------------------
discounts <- c(0, 8)
nblocks <- c(30, 100)
types <- 1:6
true_pars <- expand.grid(lambda = c(1,5),
                         size = 0.333, 
                         shape = 0.333, 
                         r = 1, 
                         q = 1, 
                         b0 = 0.5, 
                         tau = c(0.1, 0.3, 0.5, 1, 1.5, 2))
runs <- 1:50 


# Parameter recovery simulation -------------------------------------------
# Parameter Recovery
results <- recover( discounts = discounts,
                    nblocks = nblocks,
                    types = types,
                    true_pars = true_pars,
                    runs = runs,
                    d = data_shep)

write.csv(results, "../../data/raw/recovery_results.csv")
saveRDS(results, file = "../../data/raw/recovery_results.RDS")

# reading in parts of the data --------------------------------------------

#results <- fread("D:\\Bibliotheken\\Dokumente\\GitHub\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat1.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\GitHub\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat6.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\GitHub\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\BlockTau.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\GitHub\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\CatTau.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\GitHub\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\LambdaTau.csv")

# deleting the convergence (if necessary)
results <- results[convergence != 1]

#spread data.table by par and true_par
results_wide <- dcast(results, run + discount + nblock + type + row + convergence ~ names, value.var = c("par", "true_par"))

# Calcultation of the means and medians of the runs -----------------------




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




# Impact of omitting the first 0-8 rows on recovering tau -----------------

#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat1.csv")
#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat6.csv")

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

#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\BlockTau.csv")

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

#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\CatTau.csv")

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

#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\LambdaTau.csv")

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


# Recovery of all the parameters ------------------------------------------

#results <- fread("D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\Data\\DiscountTauCat6.csv")
#results <- results[convergence != 1]
#results_wide <- dcast(results, run + discount + nblock + type + row + convergence ~ names, value.var = c("par", "true_par"))

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

