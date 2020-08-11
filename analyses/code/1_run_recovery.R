install.packages("qgraph")
# Libraries ---------------------------------------------------------------
pkgs <- c("doParallel",
          "jtools",
          "pgirmess",
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
types <- 1
true_pars <- expand.grid(
  lambda = 1,
  color = 0.98, 
  shape = 0.01,
  size = 0.01,
  r = 1, 
  q = 1, 
  b0 = 0.5, 
  tau = c(0.1, 0.3, 0.5, 1, 1.5, 2))
runs <- 1:50 

# Parameter recovery simulation -------------------------------------------
# Parameter Recovery
results <- recover(
  discounts = discounts,
  nblocks = nblocks,
  types = types,
  true_pars = true_pars,
  runs = runs,
  d = data_shep)

write.csv(results, "../../data/raw/results_type1.csv")
saveRDS(results, file = "../../data/raw/results_type1.RDS")