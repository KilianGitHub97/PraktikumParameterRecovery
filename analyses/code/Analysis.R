# Libraries ---------------------------------------------------------------

pkgs <- c(
  "data.table",
  "tidyverse",
  "doParallel",
  "Hmisc",
  "psych",
  "qgraph"
)

lapply(pkgs, library, character.only = TRUE)

# Setwd -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data --------------------------------------------------------------------

data.l <- readRDS("../../data/raw/recovery_results.RDS")
data.l <- data.l[convergence != 1][, convergence := NULL]
data.w <- dcast(
  data = data.l,
  formula = run + discount + nblock + type + row ~ names,
  value.var = c("par", "true_par"))
data.w <- data.w[true_par_lambda == 1]


# Correlation matrix ------------------------------------------------------

#Prepare data for correlation matrix
data.w.cor <- data.w[, -c("row", "run")]

#correlation matrix with p-values
cormat <- rcorr(
  as.matrix(data.w.cor),
  type = "spearman")
cormat.r <- as.data.frame(
  t(cormat$r))
cormat.p <- as.data.frame(
  cormat$P)
data.cor <-data.table(
  Var1 = rownames(cormat.r)[row(cormat.r)[upper.tri(cormat.r)]], 
  Var2 = colnames(cormat.r)[col(cormat.r)[upper.tri(cormat.r)]], 
  corr=round(cormat.r[upper.tri(cormat.r)],4),
  p = round(cormat.p[upper.tri(cormat.p)],4))

#filter relevant values
data.cor.sig <- data.cor[corr != 0 & corr != 1 & corr != -1 & p < 0.05]

# correlation networks ----------------------------------------------------

#subset data
data.qgraph.post <- data.w[,c(
  "par_b0",
  "par_b1",
  "par_color",
  "par_lambda",
  "par_shape",
  "par_size",
  "par_tau")]
data.qgraph.pre <- data.w[,c(
  "discount",
  "type",
  "nblock",
  "true_par_tau")]

#transform into desired format
data.qgraph.post <- cor(
  data.qgraph.post,
  use = "pairwise.complete.obs",
  method = "spearman")
data.qgraph.pre <- cor(
  data.qgraph.pre,
  use = "pairwise.complete.obs",
  method = "spearman")

#network graphs
data.qgraph.post.qgraph <- qgraph(
  data.qgraph.post,
  graph = "pcor",
  layout = "spring")
data.qgraph.pre.qgraph <- qgraph(
  data.qgraph.pre,
  graph = "pcor",
  layout = "spring")

# Statistical validation of the impact of the input parameters ------------

#dataframe with all possible combinations
combinations <- expand.grid(
  disc1 = c(0, 8),
  disc2 = c(0, 8),
  nblo1 = c(30, 100),
  nblo2 = c(30, 100),
  type1 = 1:6,
  type2 = 1:6)

#dataframe with all combinations of interest
combinations <- subset(
  combinations, 
  disc1 != disc2 &
    nblo1 == nblo2 &
    type1 == type2 | 
    disc1 == disc2 &
    nblo1 != nblo2 &
    type1 == type2 |
    disc1 == disc2 &
    nblo1 == nblo2 &
    type1 != type2,
  drop = TRUE)

results <- list()
l = 1

for (i in 1:nrow(combinations)) {
  #filter every row separatly and subset values
  row <- combinations[i,]
  disc1 <- as.double(row["disc1"])
  disc2 <- as.double(row["disc2"])
  nblo1 <- as.double(row["nblo1"])
  nblo2 <- as.double(row["nblo2"])
  type1 <- as.double(row["type1"])
  type2 <- as.double(row["type2"])
  
  #make components for rtest
  component_1 <- data.w[
    discount == disc1 &
      nblock == nblo1 &
      type == type1]
  component_2 <- data.w[
    discount == disc2 &
      nblock == nblo2 &
      type == type2]
  
  #spearman and pearson correlations
  cor.c1.spearman <- cor(
    component_1$par_tau,
    component_1$true_par_tau,
    method = "spearman")
  cor.c2.spearman <- cor(
    component_2$par_tau,
    component_2$true_par_tau,
    method = "spearman")
  cor.c1.pearson <- cor(
    component_1$par_tau,
    component_1$true_par_tau,
    method = "pearson")
  cor.c2.pearson <- cor(
    component_2$par_tau,
    component_2$true_par_tau,
    method = "pearson")
  mse1 <- mean(
    component_1$par_tau - component_1$true_par_tau
  )^2
  mse2 <- mean(
    component_2$par_tau - component_2$true_par_tau
  )^2
  
  #correlation tests for spearman and pearson
  rtest.spearman <- r.test(
    n = nrow(component_1),
    n2 = nrow(component_2),
    r12 = cor.c1.spearman,
    r34 = cor.c2.spearman)
  rtest.pearson <- r.test(
    n = nrow(component_1),
    n2 = nrow(component_2),
    r12 = cor.c1.pearson,
    r34 = cor.c2.pearson)
  
  #list with all information of interest
  results[[l]] <- data.table(
    discount_1 = disc1,
    discount_2 = disc2,
    nblock_1 = nblo1,
    nblock_2 = nblo2,
    type_1 = type1,
    type_2 = type2,
    spearman_1 = cor.c1.spearman,
    spearman_2 = cor.c2.spearman,
    pearson_1 = cor.c1.pearson,
    pearson_2 = cor.c2.pearson,
    mse_1 = mse1,
    mse_2 = mse2,
    p.spearman = round(rtest.spearman$p, 4),
    z.spearman = round(rtest.spearman$z, 4),
    p.pearson = round(rtest.pearson$p, 4),
    z.pearson = round(rtest.pearson$z, 4)
  )
  l = l + 1
}

#convert results into data.table
results <- rbindlist(results)
results <- results[p.spearman < 0.05 | p.pearson < 0.05][order(p.pearson)][-seq(2, 68, by = 2)]

# adjust p-values
results$adj_spearman <- p.adjust(
  results$p.spearman,
  method = "bonferroni")
results$adj_pearson <- p.adjust(
  results$p.pearson,
  method = "bonferroni")

#possibility for alpha inflation error
1 - (1 - 0.05)^34

#pvalue after bonferroni
0.05/34

results.spearman <- results[adj_spearman < 0.05]
results.pearson <- results[adj_pearson < 0.05]

# graphs ------------------------------------------------------------------

#recovery of b0
data.w.b0 <- data.w %>% 
  group_by(type, true_par_tau) %>% 
  mutate(med_b0 = median(par_b0))

recovery.b0 <- ggplot(data = data.w.b0,
       mapping = aes(y = par_b0)) +
  geom_histogram(bins = 100) +
  geom_hline(aes(yintercept = true_par_b0), color = "red") +
  geom_hline(aes(yintercept = med_b0), color="blue", linetype="dashed")+
  facet_grid(true_par_tau ~ type)+
  theme_minimal()

#recovery of b1
data.w.b1 <- data.w %>% 
  group_by(type, true_par_tau) %>% 
  mutate(med_b1 = median(par_b1))

recovery.b1 <- ggplot(data = data.w.b1,
       mapping = aes(y = par_b1)) +
  geom_histogram(bins = 100) +
  geom_hline(aes(yintercept = true_par_b1), color = "red") +
  geom_hline(aes(yintercept = med_b1), color="blue", linetype="dashed")+
  facet_grid(true_par_tau ~ type) +
  theme_minimal()

#recovery of size
data.w.size <- data.w %>% 
  group_by(type, true_par_tau) %>% 
  mutate(med_size = median(par_size))

recovery.size <- ggplot(data = data.w.size,
       mapping = aes(y = par_size)) +
  geom_histogram(bins = 100) +
  geom_hline(aes(yintercept = true_par_size), color = "red") +
  geom_hline(aes(yintercept = med_size, group = type), color="blue", linetype="dashed")+
  facet_grid(true_par_tau ~ type)+
  theme_minimal()

#recovery of color
data.w.color <- data.w %>% 
  group_by(type, true_par_tau) %>% 
  mutate(med_color = median(par_color))

recovery.color <- ggplot(data = data.w.color,
                         mapping = aes(y = par_color)) +
  geom_histogram(bins = 100) +
  geom_hline(aes(yintercept = true_par_color), color = "red") +
  geom_hline(aes(yintercept = med_color), color="blue", linetype="dashed") +
  facet_grid(true_par_tau ~ type) +
  theme_minimal()

#recovery of shape
data.w.shape <- data.w %>%
  group_by(true_par_tau, type) %>%
  mutate(med_shape = median(par_shape))

recovery.shape <- ggplot(data = data.w.shape,
                         mapping = aes(y = par_shape)) +
  geom_histogram(bins = 100) +
  geom_hline(aes(yintercept = true_par_shape), color = "red") +
  geom_hline(aes(yintercept = med_shape), color="blue", linetype="dashed") +
  facet_grid(true_par_tau ~ type) +
  theme_minimal()

#recovery of lambda
data.w.lambda <- data.w %>%
  group_by(true_par_tau, type) %>%
  mutate(med_lambda = median(par_lambda))

recovery.lambda <- ggplot(data = data.w.lambda,
                          mapping = aes(y = par_lambda)) +
  geom_histogram(bins = 100) +
  geom_hline(aes(yintercept = true_par_lambda), color = "red") +
  geom_hline(aes(yintercept = med_lambda), color="blue", linetype="dashed")+
  facet_grid(true_par_tau ~ type)+
  theme_minimal()

#violin plots for discount per par_tau
violin.discount<- ggplot(
  data = data.w,
  mapping = aes(x = as.factor(discount),
                y = par_tau)) +
  geom_violin(width = 0.9) +
  stat_summary(fun = "median", 
               geom = "point") +
  geom_line(aes(x = discount, 
                y = true_par_tau), 
            color = "red", 
            size = 0.5) +
  facet_grid(true_par_tau ~ type) +
  ylim(0, 10) +
  xlab("Discount") +
  ylab("Tau") +
  theme_minimal()

# line plots for discount per par_tau
line.discount <- ggplot(
  data = data.w,
  mapping = aes(x = true_par_tau,
                y = par_tau)) +
  geom_point(aes(x = true_par_tau,
                 y = par_tau),
             alpha = 1 / 20) +
  geom_line(aes(x = true_par_tau,
                y = true_par_tau),
            color = "red") +
  stat_summary(aes(group = discount),
               fun = median,
               geom = "line",
               color =
                 rep(
                   c(
                     rep("green", 6),
                     rep("blue", 6)
                   ),6)) +
  facet_wrap(~type) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_minimal()

#violin plots for nblock per par_tau 
violin.nblock <- ggplot(
  data = data.w,
  mapping = aes(x = as.factor(nblock),
                y = par_tau)) +
  geom_violin(width = 0.9) +
  stat_summary(fun = "median", 
               geom = "point") +
  geom_line(aes(x = discount,
                y = true_par_tau), 
            color = "red", 
            size = 0.5)+
  facet_grid(type ~ true_par_tau) +
  ylim(0, 10) +
  xlab("nblock") +
  ylab("Tau") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 7)) 

# line plot for nblock per par_tau 
line.nblock <- ggplot(
  data = data.w,
  mapping = aes(x = true_par_tau,
                y = par_tau)) +
  geom_point(aes(x = true_par_tau,
                 y = par_tau),
             alpha = 1 / 20) +
  geom_line(aes(x = true_par_tau,
                y = true_par_tau),
            color = "red") +
  stat_summary(aes(group = nblock),
               fun = median,
               geom = "line",
               color = 
                 rep(
                   c(
                    rep("green", 6),
                    rep("blue", 6)
                   ),6)) +
  facet_wrap(~type) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_minimal()

#violin plots for par_tau per type
violin.type <- ggplot(
  data = data.w,
  mapping = aes(x = as.factor(type),
                y = par_tau)) +
  geom_violin(width = 0.9) +
  stat_summary(fun = "median", 
               geom = "point") +
  geom_line(aes(
    x = discount,
    y = true_par_tau), 
            color = "red", 
            size = 0.5) +
  facet_wrap(~true_par_tau)+
  ylim(0, 10) +
  xlab("type") +
  ylab("Tau") +
  theme_minimal()

# line plot for par_tau per type
line.type <- ggplot(data = data.w,
       mapping = aes(x = true_par_tau,
                     y = par_tau)) +
  geom_point(aes(x = true_par_tau,
                 y = par_tau),
             alpha = 1 / 20) +
  geom_line(aes(x = true_par_tau,
                y = true_par_tau),
            color = "red") +
  stat_summary(aes(group = type),
               fun = median,
               geom = "line",
               color = c(rep("blue", 144)
               ))+
  facet_grid(nblock + discount ~ type)+
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 9))

# Evaluation of lambda ----------------------------------------------------

#re-load data
data.l <- readRDS("../../data/raw/recovery_results.RDS")
data.l <- data.l[convergence != 1][, convergence := NULL]
data.w <- dcast(
  data = data.l,
  formula = run + discount + nblock + type + row ~ names,
  value.var = c("par", "true_par"))

#violin plots for true lambda per par_tau
violin.lambda <- ggplot(
  data = data.w,
  mapping = aes(x = as.factor(true_par_lambda),
                y = par_tau)) +
  geom_violin(width = 0.9) +
  stat_summary(fun = "median", 
               geom = "point") +
  geom_line(aes(x = discount, 
                y = true_par_tau), 
            color = "red", 
            size = 0.5) +
  facet_grid(true_par_tau ~ type)+
  ylim(0, 10) +
  xlab("Lambda") +
  ylab("Tau") +
  theme_minimal()

#line plots for true lambda per par_tau
line.lambda <- ggplot(
  data = data.w,
  mapping = aes(x = true_par_tau,
                y = par_tau)) +
  geom_point(aes(x = true_par_tau,
                 y = par_tau),
             alpha = 1 / 20) +
  geom_line(aes(x = true_par_tau,
                y = true_par_tau),
            color = "red") +
  stat_summary(aes(group = true_par_lambda),
               fun = median,
               geom = "line",
               color = rep(
                 c(
                   rep("green", 6),  #lambda = 1
                   rep("blue", 6)   #lambda = 5
                   ), 6))+
  facet_wrap(~type) +
  ylim(0,10) +
  xlab("True tau") +
  ylab("Median of estimated tau") +
  theme_minimal()


