install.packages("")
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(cognitivemodels)
library(broom)
library(seewave)
library(doParallel)
library(foreach)
library(plotly)
library(ggpubr)

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


# 6 data.frames with repetitive Blocks ------------------------------------

#shepard datensatz mit rep auf 5 skalieren

#List für die Datensätze
allFrames <- list()

#For-Loop der den Datensatz mit bis zu sechs Blöcken erweitert. 
for (i in 1:6) {
  allFrames[[i]] <- data_shep[rep(1:8, i), ]
}


# Calculating binominal coefficients and adding them to the shepar --------

#liste mit gefixten Parametern erstellen mit neutralen Werten (lambda = 1)
fixpar <- list(lambda = 1, size = 0.33333, shape = 0.33333, color = 0.33334, r = 1.5, q = 1.5)

##Erstelle ein Modell, dass die Daten von Shepard mit den Parametern simuliert
models = list()

preddata = list()

rbincol <- list()

data_shep_rbin <- list()

kldiv <- list()

for (i in 1:6) {
  #Modell mit fixen Parametern und für Kategorie_1
  models[[i]] <- gcm(data = allFrames[[i]], formula = y ~ size + shape + color, class = ~ Kategorie_1, choicerule = "none", 
                     fix = fixpar) 
  
  #Datensatz mit den gelernten Prädiktionen  
  preddata[[i]] <- cbind(allFrames[[i]], predict(models[[i]])) 
  
  # Binominalisierung der Wahrscheinlichkeiten
  # rbinom(): n = number of rows // size = max possible value // prob = Probability of either 1 or 2
  rbincol[[i]] <- data.frame(
    binval = rbinom(nrow(preddata[[i]]) , 1, predict(models[[i]]))
  )
  
  #Neuen Datensatz erstellen, bei dem die Prädiktionen auch angehängt sind.   
  data_shep_rbin[[i]] <- cbind(data_shep, rbincol[[i]])
  
  #Ähnlichkeit der beiden Verteilungen mit Kullback-Leibler Divergenz, ich bin mir aber nicht sicher, ob die in unserem Fall angewendet werden sollte. 
  #Je höher der Wert, desto unähnlicher die beiden Spalten.
  kldiv[[i]] <- kl.dist(data_shep_rbin[[i]]$Kategorie_1, data_shep_rbin[[i]]$binval)
  
}

# Function die alles ausgibt
printdet <- function(Block) {
  print(models[Block]);
  print(preddata[Block]);
  print(data_shep_rbin[Block]);
  print(kldiv[Block])
}

#alle wichtigen Daten zu Block 4 ausgeben:
printdet(Block = 4)

# Comparing fixed and fitted parameters -----------------------------------

# GCM mit freien Parametern

# Schätzung der Parameter mit binval als Response Variable 
newpar <- foreach(i = 1:6, .combine = "rbind", .packages = c("cognitivemodels", "broom", "dplyr")) %dopar% {
  a <- summary(gcm(data = data_shep_rbin[[i]], formula = binval ~ size + shape + color, class = ~ Kategorie_1, choicerule = "none"))
  
  b <- tidy(a$coefficients)
  
  b <- b %>% mutate(ID = i)
}
#long to wide
newpar <- newpar %>% spread(key = .rownames, value = Estimate)

#in sauberen Datensatz verwandeln
newpar$b0 <- unlist(newpar$b0)
newpar$b1 <- unlist(newpar$b1)
newpar$color <- unlist(newpar$color)
newpar$lambda <- unlist(newpar$lambda)
newpar$q <- unlist(newpar$q)
newpar$r <- unlist(newpar$r)
newpar$shape <- unlist(newpar$shape)
newpar$size <- unlist(newpar$size)

#Die gefixten und die geschätzten Parameter zusammenfügen: par
a <- summary(gcm(data = allFrames[[1]], formula = y ~ size + shape + color, class = ~ Kategorie_1, choicerule = "none", fix = fixpar))
oldpar <- tidy(a$coefficients)
oldpar <- oldpar %>% mutate(ID = "original")
oldpar <- oldpar %>% spread(key = .rownames, value = Estimate)
names <- c("ID", "b0", "b1", "(color)", "(lambda)", "(q)", "(r)", "(shape)", "(size)")
oldpar <- oldpar[,names]
names(oldpar) <- names(newpar)

#Datensatz mit neuen und alten Parametern
par <- rbind(newpar, oldpar)

#Alle gefitteten Estimates. Die ID zeigt, wie oft ein Block repliziert wurde. ID == original zeigt die ehemals festgelegten Werte
# view(par)

# Estimates with the first n = 1:7 rows removed -------------------------------

registerDoParallel(4)

cutnewpar <- list()

l = 1

#For-loop der bei jedem Datensatz jeweils die erste (bis siebte) Zeile löscht und die Parameter in der List cutnewpar speichert
for (j in 1:7) {
  cutnewpar[[l]] <- foreach(i = 1:6, .combine = "rbind", .packages = c("cognitivemodels", "broom", "dplyr")) %dopar% {
    a <- summary(gcm(data = tail(data_shep_rbin[[i]], -j), formula = binval ~ size + shape + color, class = ~ Kategorie_1, choicerule = "none"))
    
    b <- tidy(a$coefficients)
    
    b <- b %>% mutate(ID = i)
  }
  l = l + 1
}

#for loop, der jedes Element der Liste cutnewpar von long zu wide transformiert und die gefixten Parameter anheftet
names <- c("ID", "b0", "b1", "color", "lambda", "q", "r", "shape", "size")

for (i in 1:7) {
  unlist(cutnewpar[[i]])
  cutnewpar[[i]] <- cutnewpar[[i]] %>% spread(key = .rownames, value = Estimate)
  cutnewpar[[i]] <- cutnewpar[[i]][,names]
  cutnewpar[[i]] <- cutnewpar[[i]] %>% mutate(number_of_deleted_values = i)
}

# Die Nummer in der eckigen Klammer ist gleich der Anzahl der gelöschten Zeilen
# view(cutnewpar[[2]])


##Graphische Darstellung der Beschneidung

# Datensatz erstellen mit allen geschätzten Fit Indizes pro Block (ID), pro geläschtem Wert (number_of_deleted_values)

allcutpar <- as.data.frame(
  rbind(cutnewpar[[1]], 
        cutnewpar[[2]], 
        cutnewpar[[3]], 
        cutnewpar[[4]], 
        cutnewpar[[5]], 
        cutnewpar[[6]], 
        cutnewpar[[7]]))

#Wandle alle NULL Objekte der List in NA um, ansonsten würden sie gelöscht werden, wenn die Liste konvertiert wird
allcutpar$b0[lengths(allcutpar$b0) == 0] <- NA
allcutpar$b1[lengths(allcutpar$b1) == 0] <- NA
allcutpar$color[lengths(allcutpar$color) == 0] <- NA
allcutpar$lambda[lengths(allcutpar$lambda) == 0] <- NA
allcutpar$q[lengths(allcutpar$q) == 0] <- NA
allcutpar$r[lengths(allcutpar$r) == 0] <- NA
allcutpar$shape[lengths(allcutpar$shape) == 0] <- NA
allcutpar$size[lengths(allcutpar$size) == 0] <- NA

#Beim allcutpar Datensatz liegen alle Spalten in Lists vor. Hier wird der Datensatz "normalisiert"
allcutpar$b0 <- unlist(allcutpar$b0)
allcutpar$b1 <- unlist(allcutpar$b1)
allcutpar$color <- unlist(allcutpar$color)
allcutpar$lambda <- unlist(allcutpar$lambda)
allcutpar$q <- unlist(allcutpar$q)
allcutpar$r <- unlist(allcutpar$r)
allcutpar$shape <- unlist(allcutpar$shape)
allcutpar$size <- unlist(allcutpar$size)


#Filtere alle Zeilen raus, die die originalen Parameter darstellen und die durch das beschneiden nicht mehr akkurat sind
allcutpar <- allcutpar %>%
  filter(b0 != is.na(NA)) %>%
  filter(r != 1.5)


# Graphische Darstellung der Parameter Recovery ---------------------------

#Evaluation der Recovery des b0 Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = b0)) + 
    geom_line(aes(x = 0.5, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des b1 Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = b1)) + 
    geom_line(aes(x = 0.5, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des color Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = color)) + 
    geom_line(aes(x = 0.33334, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des size Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = size)) + 
    geom_line(aes(x = 0.33333, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des shape Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = shape)) + 
    geom_line(aes(x = 0.33333, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des lambda Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = lambda)) + 
    geom_line(aes(x = 1, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des r Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = r)) + 
    geom_line(aes(x = 1.5, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)

#Evaluation der Recovery des q Parameters
ggplotly(
  ggplot(data = allcutpar,
         aes(y = number_of_deleted_values)) +
    geom_line(aes(x = q)) + 
    geom_line(aes(x = 1.5, color = "red"))+ #original Parameter
    facet_wrap(~ID)+  #ID = Size of the data frame (Number of Blocks)
    theme_classic()
)


# Calculate distances to original values ----------------------------------
#Frage: hat die Parameter Recovery funktioniert?

#Datensatz, bei dem die Distanzen zwischen den gefixten Werten und den freien Werten als Betrag abgespeichert werden
dist_allcutpar <- allcutpar %>%
  mutate(diff_b0 = abs(0.5 - allcutpar$b0)) %>%
  mutate(diff_b1 = abs(0.5 - allcutpar$b1)) %>%
  mutate(diff_color = abs(0.33334 - allcutpar$color)) %>%
  mutate(diff_lambda = abs(1 - allcutpar$lambda)) %>%
  mutate(diff_q = abs(1.5 - allcutpar$r)) %>%
  mutate(diff_r = abs(1.5 - allcutpar$r)) %>%
  mutate(diff_shape = abs(0.33333 - allcutpar$shape)) %>%
  mutate(diff_size = abs(0.33333 - allcutpar$size))

dist_allcutpar %>%
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
  print(n = 40)



# Bei newpar wurde keiner der Werte weggeschnitten
dist_newpar <- newpar %>%
  mutate(diff_b0 = abs(0.5 - newpar$b0)) %>%
  mutate(diff_b1 = abs(0.5 - newpar$b1)) %>%
  mutate(diff_color = abs(0.33334 - newpar$color)) %>%
  mutate(diff_lambda = abs(1 - newpar$lambda)) %>%
  mutate(diff_q = abs(1.5 - newpar$r)) %>%
  mutate(diff_r = abs(1.5 - newpar$r)) %>%
  mutate(diff_shape = abs(0.33333 - newpar$shape)) %>%
  mutate(diff_size = abs(0.33333 - newpar$size))

#Hier sind die Beträge der Abweichungen des geschätzten Parameters vom ursprünglichen Parameter pro Block zu sehen
dist_newpar %>%
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
  )

#Grafische Darstellung der Schätzfehler

gg1 <- ggplot(data = dist_newpar,
                 aes(x = diff_b0,
                     y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

gg2 <- ggplot(data = dist_newpar,
              aes(x = diff_b1,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

gg3 <- ggplot(data = dist_newpar,
              aes(x = diff_color,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

gg4 <- ggplot(data = dist_newpar,
              aes(x = diff_lambda,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

gg5 <- ggplot(data = dist_newpar,
              aes(x = diff_q,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

gg6 <- ggplot(data = dist_newpar,
              aes(x = diff_r,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

gg7 <- ggplot(data = dist_newpar,
              aes(x = diff_shape,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

gg8 <- ggplot(data = dist_newpar,
              aes(x = diff_size,
                  y = ID)) + 
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() 

#jeder Plot zeigt an, wie sich die Schätzungen abhängig von der Anzahl Blöcke verändern [Wichtig ist aber, dass die Punkte beachtet werden!]
plot1 <- ggarrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8)




