install.packages("", dependencies = TRUE)

# Libraries ---------------------------------------------------------------
library(cognitivemodels)
library(tidyverse)
library(foreach)
library(doParallel)
library(data.table)
library(broom)
library(plotly)

# data nosofsky -----------------------------------------------------------

#convert factors to doubles


## Infos zu Nosofsky1989
      # stim_id =  ID des Stimulus, jeder der 16 Stimulus (siehe Nosofsky) hat eine ID 
      # condition = Bedingung, Also Gr?sse, Winkel, Criss-Cross und Diagnonal
      # angle = Grad, in dem das Ausgangsbild gedreht wurde
      # size = Gr?sse des Stimulus. 
      # Anzahl, mit der die Versuchspersonen den Stimulus gesehen haben.
      # obs_cat = Anzahl, mit der die Versuchspersonen den Stimulus der Kategorie 1 zugeordnet haben.
      # pobs = Wahrscheinlichkeit, mit der der Stimulus zu Kategorie 1 zugeordnet wurde
      # true_cat: Wahre Kategorie; Dies bezieht sich auf die Trainingscondition, bei der dem Probanden die wahre Kategorie mitgeteilt wurde.
      # NA bedeutet, dass das Feld der (m, n) - Matrix keiner Kategorie zugeordnet wurde. 

#nosofsky1989 Datensatz lokal speichern und zu einem Data Table umformen
nosofsky1989 <- copy(nosofsky1989)
nosofsky1989 <- setDT(nosofsky1989)

head(nosofsky1989)
glimpse(nosofsky1989)

#Mittelwerte
nosofsky1989 %>%
  select(-condition) %>%
  colMeans()

#Anzahl, mit der jeder Stimulus präsentiert wurde
nosofsky1989[, list(Total_Prä = sum(N)), keyby = stim_id]

# Task 1 ------------------------------------------------------------------

## Liste erstellen mit Datensätzen, die alle möglichen Kombinationen von 0 und 1 beinhalten

#Liste für Endprodukt erstellen
allFrames <- list()

#Grundstruktur für mein for-loop gebastel
fr <- expand.grid(0:1, 0:1, 0:1, 0:1)

#counter für for-loop
l <- 1

#Erstelle alle möglichen Kombinationen von 0 und 1 in einem datensatz mit 4 Variablen [LIMITATIONEN: Die Dimensionen sind binär 
#(0 und 1) und es gibt keine NA Werte im Kriterium. GRUND: Ich wollte meinen Rechner über die Nacht nicht laufen lassen]
for(i in 1:16) {
  for (j in 1:16) {
    for (k in 1:16) {
      for (m in 1:16) {
        xa <- fr[i,]
        xb <- fr[j,]
        xc <- fr[k,]
        xd <- fr[m,]
        allFrames[[l]] <- as.data.frame(rbind(xa, xb, xc, xd))
        l <- l + 1
      }
    }
  }
}

#Jeder beliebige Datensatz n kann wie folgt abgerufen werden: allFrames[n]



## alle Elemente der List durch das GCM rendern und in Datensatz data.1 speichern

#alle 4 Kerne registrieren, für parallelisierten For loop
registerDoParallel(4)

# Parallelisierter For loop der alle Koeffizienten, AIC, BIC und LOGLIK für jeden Datensatz erfasst
data.1 <- foreach(a = 1:65536, .combine = rbind, .packages = c("cognitivemodels", "tidyverse", "broom")) %dopar% {
  
  b <- summary(gcm(data = allFrames[a], formula = Var3 ~ Var1 + Var2, class = ~Var4, choicerule = "none"))
 
  c <- tidy(b$coefficients) %>% mutate(ID = a)
 
  d <- data.frame(AIC = b$aic,
                  BIC = b$bic,
                  LOGLIK = b$logLik)
  
  g <- cbind(c, d)
}

# Die Variable ist noch als List abgespeichert
class(data.1$Estimate)
glimpse(data.1)

#deshalb muss sie umcodiert werden
data.1$Estimate <- as.numeric(data.1$Estimate)

#Den Datensatz exportieren, da es unsinnig ist, ihn jedes Mal neu zu rendern
write.csv(data.1,"D:\\Bibliotheken\\Dokumente\\R\\allpos.csv", row.names = FALSE)





## Datensatz neu laden
dat1_raw <- fread(input = "D:\\Bibliotheken\\Dokumente\\R\\PraktikumParameterRecovery\\Parameter Recovery\\allpos.csv")
head(dat1, 15)

#long in wide transformieren
dat1 <- dat1_raw %>% spread(key = .rownames, value = Estimate)

# Schönheitsoperation
colvec <- c("ID","lambda", "q", "r", "Var1", "Var2", "(b0)", "(b1)", "b0", "b1", "AIC", "BIC", "LOGLIK")
dat1 <- dat1[, ..colvec]
dat2 <- dat1

## Den Datensatz verstehen

# Die Fit-Indizes betrachten. 

#min und max von AIC, BIC und LOGLIK
dat1[65536] %>%
  summarise(
    MIN_AIC = min(AIC), 
    MAX_AIC = max(AIC),
    MIN_BIC = min(BIC),
    MAX_BIC = max(BIC),
    MIN_LOGLIK = min(LOGLIK),
    MAX_LOGLIK = max(LOGLIK)
  )

#ID Nummern dieser Werte: Es fällt auf, dass Min und Max jeweils in den selben Datensätzen vorliegen
dat1 %>%
  summarise(
    MIN_AIC = which.min(AIC), 
    MAX_AIC = which.max(AIC),
    MIN_BIC = which.min(BIC),
    MAX_BIC = which.max(BIC),
    MIN_LOGLIK = which.min(LOGLIK),
    MAX_LOGLIK = which.max(LOGLIK)
  )


#Untersuche diese Datensätze: Es sei angemerkt, dass es nicht nur ein min/max gibt, dies wird an den Graphen weiter unten klar.
#Z.B. Haben allFrames[1093], allFrames[1094], allFrames[1095], allFrames[1096] alle identische Fitindizes. 
allFrames[51337]
allFrames[1093]



#Verhalten des AIC
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID, y = AIC)) +
    geom_line() +
    theme_minimal()
)

#Verhalten des BIC
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID, y = BIC)) +
    geom_line() +
    #xlim(2000,3000)+
    theme_minimal()
)

# Korrelation des Verhalten (identisch)
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = BIC, color = "red")) +
    geom_line(aes(y = AIC, color = "blue")) + 
    theme_minimal()
)

# Verhalten der Loglik
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID, y = LOGLIK)) +
    geom_line() +
    theme_minimal()
)

# Korrelation der drei Parameter
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = BIC, color = "red")) +
    geom_line(aes(y = AIC, color = "blue")) +
    geom_line(aes(y = LOGLIK, color = "green")) + 
    theme_minimal()
)


# Untersuchung von Lambda, r, q, Var1 und Var2

#min und max
dat1 %>%
  summarise(
    MIN_LAMBDA = min(lambda), 
    MAX_LAMBDA = max(lambda),
    MIN_R = min(r),
    MAX_R = max(r),
    MIN_Q = min(q),
    MAX_Q = max(q),
    MIN_VAR1 = min(Var1),
    MAX_VAR1 = max(Var1),
    MIN_VAR2 = min(Var2),
    MAX_VAR2 = max(Var2)
  )

#ID Nummern dieser Werte: Es gibt hier keine Systematik ausser bei Var1 & 2
dat1 %>%
  summarise(
    MIN_LAMBDA = which.min(lambda), 
    MAX_LAMBDA = which.max(lambda),
    MIN_R = which.min(r),
    MAX_R = which.max(r),
    MIN_Q = which.min(q),
    MAX_Q = which.max(q),
    MIN_VAR1 = which.min(Var1),
    MAX_VAR1 = which.max(Var1),
    MIN_VAR2 = which.min(Var2),
    MAX_VAR2 = which.max(Var2)
  )

#Die Datensätze
allFrames[2772]
allFrames[2388]
allFrames[3395]
allFrames[2401]
allFrames[2358]
allFrames[2499]
allFrames[2404]
allFrames[3795]
allFrames[3795]
allFrames[2404]

# Lambda ist meistens nahe 1 oder 10
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = lambda, color = "red")) +
    theme_minimal()
)


# r ist meist nahe 1 und 2 (Manhattan, Euklidisch)
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = r, color = "red")) +
    theme_minimal()
)

# q hat dasselbe Muster wie r
ggplotly(
  ggplot( data = dat1,
  mapping = aes(x = ID)) +
    geom_line(aes(y = q, color = "red")) +
    theme_minimal()
)

# r und q: Ähnlich, aber nicht identisch
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = r, color = "red")) +
    geom_line(aes(y = q, color = "green")) +
    theme_minimal()
)

#Var1
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = Var1, color = "red")) +
    theme_minimal()
)

#Var2
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = Var1, color = "red")) +
    theme_minimal()
)

#Var1 und Var2: Ähnlich, aber nicht identisch
ggplotly(
  ggplot( data = dat1,
          mapping = aes(x = ID)) +
    geom_line(aes(y = Var1, color = "red")) +
    geom_line(aes(y = Var2, color = "green")) +
    theme_minimal()
)

# Task 2 ------------------------------------------------------------------
#Datensatz mit Kategorien nach Shepard
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

#Die Daten in numerische umwandeln
data_shep <- data_raw_shepard %>% 
  mutate( size = recode(size, "small" = 0, "large" = 1),
          shape = recode(shape, "triangle" = 0, "square" = 1),
          color = recode(color, "black" = 0, "white" = 1)) %>%
  mutate(y = NA)


#fitindizes sind bei allen Kategorien gleich
for (i in 1:6) {
  m <- gcm(formula = y ~ size + shape + color, choicerule = "none", class = ~ paste0("Kategorie_", i), data = data_shep)
  print(summary(m))
}
