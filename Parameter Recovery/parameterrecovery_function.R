
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(cognitivemodels)
library(broom)
library(seewave)
library(doParallel)
library(foreach)
library(plotly)

# Datensatz ---------------------------------------------------------------

data_raw_shepard <- data.frame(
  size = as.factor( c("small", "small", "small", "small", "large", "large", "large", "large" )), # c(small, large)
  shape = as.factor( c("triangle", "triangle", "square", "square", "triangle", "triangle", "square", "square" )), # c(triangle, square)
  color = as.factor( c("black", "white", "black", "white", "black", "white", "black", "white")), # c(black, white)
  Kategorie_1 = c(0, 1, 0, 1, 0, 1, 0, 1), # c(0, 1)
  Kategorie_2 = c(0, 1, 1, 0, 0, 1, 1, 0), # c(0, 1)
  Kategorie_3 = c(0, 0, 1, 1, 0, 1, 0, 1), # c(0, 1)
  Kategorie_4 = c(0, 1, 1, 1, 0, 0, 0, 1), # c(0, 1)
  Kategorie_5 = c(0, 1, 1, 0, 0, 1, 0, 1), # c(0, 1)
  Kategorie_6 = c(1, 0, 0, 1, 0, 1, 1, 0) # c(0, 1)
)

data_shep <- data_raw_shepard %>% 
  mutate( size = recode(size, "small" = 0, "large" = 1),
          shape = recode(shape, "triangle" = 0, "square" = 1),
          color = recode(color, "black" = 0, "white" = 1)) %>%
  mutate(y = NA)

registerDoParallel(4)
Kategorie = data_shep$Kategorie_1
ruleofchoice = "none"
chooselambda = 1
choosesize = 0.34
chooseshape = 0.33
choosecolor = 0.33
chooser = 1.5
chooseq = 1.5

parameterrecovery <- function(Kategorie, ruleofchoice, chooselambda, choosesize, chooseshape, choosecolor, chooser, chooseq){
  fixpar <- list(lambda = chooselambda, size = choosesize, shape = chooseshape, color = choosecolor, r = chooser, q = chooseq)
  allFrames <- list()
  models = list()
  preddata = list()
  rbincol <- list()
  data_shep_rbin <- list()
  kldiv <- list()
  for (i in 1:6) {
    allFrames[[i]] <- data_shep[rep(1:8, i), ]
    models[[i]] <- gcm(data = allFrames[[i]], formula = y ~ size + shape + color, class = ~ Kategorie, choicerule = ruleofchoice, fix = fixpar) 
    preddata[[i]] <- cbind(allFrames[[i]], predict(models[[i]])) 
    rbincol[[i]] <- data.frame(binval = rbinom(nrow(preddata[[i]]) , 1, predict(models[[i]])))
    data_shep_rbin[[i]] <- cbind(data_shep, rbincol[[i]])
    kldiv[[i]] <- kl.dist(data_shep_rbin[[i]]$Kategorie_1, data_shep_rbin[[i]]$binval)}
  newpar <- foreach(i = 1:6, .combine = "rbind") %dopar% {
    a <- summary(gcm(data = data_shep_rbin[[i]], formula = binval ~ size + shape + color, class = ~ Kategorie, choicerule = ruleofchoice))
    b <- tidy(a$coefficients)
    b <- b %>% mutate(ID = i)
  }
  newpar <- newpar %>% spread(key = .rownames, value = Estimate)
  newpar$b0 <- unlist(newpar$b0)
  newpar$b1 <- unlist(newpar$b1)
  newpar$color <- unlist(newpar$color)
  newpar$lambda <- unlist(newpar$lambda)
  newpar$q <- unlist(newpar$q)
  newpar$r <- unlist(newpar$r)
  newpar$shape <- unlist(newpar$shape)
  newpar$size <- unlist(newpar$size)
  cutnewpar <- list()
  l = 1
  for (j in 1:7) {
    cutnewpar[[l]] <- foreach(i = 1:6, .combine = "rbind") %do% {
      a <- summary(gcm(data = tail(data_shep_rbin[[i]], -j), formula = binval ~ size + shape + color, class = ~ Kategorie, choicerule = ruleofchoice))
      b <- tidy(a$coefficients)
      b <- b %>% mutate(ID = i)}
    l <- l + 1
    }
  names <- c("ID", "b0", "b1", "color", "lambda", "q", "r", "shape", "size")
  for (i in 1:7) {
    unlist(cutnewpar[[i]])
    cutnewpar[[i]] <- cutnewpar[[i]] %>% spread(key = .rownames, value = Estimate)
    cutnewpar[[i]] <- cutnewpar[[i]][,names]
    cutnewpar[[i]] <- cutnewpar[[i]] %>% mutate(number_of_deleted_values = i)}
  allcutpar <- as.data.frame(
    rbind(cutnewpar[[1]], 
          cutnewpar[[2]], 
          cutnewpar[[3]], 
          cutnewpar[[4]], 
          cutnewpar[[5]], 
          cutnewpar[[6]], 
          cutnewpar[[7]]))
  allcutpar$b0[lengths(allcutpar$b0) == 0] <- NA
  allcutpar$b1[lengths(allcutpar$b1) == 0] <- NA
  allcutpar$color[lengths(allcutpar$color) == 0] <- NA
  allcutpar$lambda[lengths(allcutpar$lambda) == 0] <- NA
  allcutpar$q[lengths(allcutpar$q) == 0] <- NA
  allcutpar$r[lengths(allcutpar$r) == 0] <- NA
  allcutpar$shape[lengths(allcutpar$shape) == 0] <- NA
  allcutpar$size[lengths(allcutpar$size) == 0] <- NA
  allcutpar$b0 <- unlist(allcutpar$b0)
  allcutpar$b1 <- unlist(allcutpar$b1)
  allcutpar$color <- unlist(allcutpar$color)
  allcutpar$lambda <- unlist(allcutpar$lambda)
  allcutpar$q <- unlist(allcutpar$q)
  allcutpar$r <- unlist(allcutpar$r)
  allcutpar$shape <- unlist(allcutpar$shape)
  allcutpar$size <- unlist(allcutpar$size)
  allcutpar <- allcutpar %>%
    filter(b0 != is.na(NA)) %>%
    filter(r != 1.5)
  dist_allcutpar <- allcutpar %>%
    mutate(diff_b0 = abs(0.5 - allcutpar$b0)) %>%
    mutate(diff_b1 = abs(0.5 - allcutpar$b1)) %>%
    mutate(diff_color = abs(choosecolor - allcutpar$color)) %>%
    mutate(diff_lambda = abs(chooselambda - allcutpar$lambda)) %>%
    mutate(diff_q = abs(chooseq - allcutpar$r)) %>%
    mutate(diff_r = abs(chooser - allcutpar$r)) %>%
    mutate(diff_shape = abs(chooseshape - allcutpar$shape)) %>%
    mutate(diff_size = abs(choosesize - allcutpar$size))
  print(dist_allcutpar %>%
          group_by(ID, number_of_deleted_values) %>%
          summarise(
            b0 = diff_b0, 
            b1 = diff_b1, 
            color =diff_color, 
            lambda = diff_lambda, 
            q = diff_q, 
            r = diff_r, 
            shape = diff_shape, 
            size = diff_size 
          ) %>%
          print(n = 40))
  dist_newpar <- newpar %>%
    mutate(diff_b0 = abs(0.5 - newpar$b0)) %>%
    mutate(diff_b1 = abs(0.5 - newpar$b1)) %>%
    mutate(diff_color = abs(0.33334 - newpar$color)) %>%
    mutate(diff_lambda = abs(1 - newpar$lambda)) %>%
    mutate(diff_q = abs(1.5 - newpar$r)) %>%
    mutate(diff_r = abs(1.5 - newpar$r)) %>%
    mutate(diff_shape = abs(0.33333 - newpar$shape)) %>%
    mutate(diff_size = abs(0.33333 - newpar$size))
  print(dist_newpar %>%
          group_by(ID)%>%
          summarise(
            b0 = diff_b0, 
            b1 = diff_b1, 
            color =diff_color, 
            lambda = diff_lambda, 
            q = diff_q,
            r = diff_r, 
            shape = diff_shape, 
            size = diff_size 
          ))
}

parameterrecovery(Kategorie = data_shep$Kategorie_1, ruleofchoice = "none", chooselambda = 1, choosesize = 0.34, 
                  chooseshape = 0.33, choosecolor = 0.33, chooser = 1.5, chooseq = 1.5)


