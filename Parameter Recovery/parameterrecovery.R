

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(cognitivemodels)


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


#shepard datensatz mit rep auf 5 skalieren

allFrames <- list()

l = 1

for (i in 1:6) {
  allFrames[[l]] <- data_shep[rep(1:8, i), ]
  l <- l + 1
}

allFrames[[6]]

#liste mit gefixten Parametern erstellen mit neutralen Werten (lambda = 1)
fixpar <- list(lambda = 1, size = 0.33333, shape = 0.33333, r = 1.5, q = 1.5)

#Erstelle ein Modell, dass die Daten von Shepard mit den Parametern simuliert
m <- gcm(data = allFrames[[6]], formula = ~ size + shape + color, class = ~Kategorie_1, choicerule = "none", 
         fix = fixpar)

cbind(allFrames[[6]], predict(m))
?gcm
predict(m)
#Mache Prediktionen und verwandle diese mit rbinom in diskrete daten
as.data.frame(rbinom(48, 1, predict(m)))

#Hänge diese Daten an den Shepard simulierten datensatz (rbinom) dran

#Erstelle ein Weiteres Modell, mit dem neuen Datensatz, bei dem keine Parameter gefixt sind, das also die simulierten Daten als Response (y) nimmt

#Vergleiche die Geschätzten Daten mit

#Mit vershcieden Längen und anderen Parameter 

#Wollen wir den ersten Block ignorierr ? (Da die Präd sehr uninformativ sind)