## ----setup, include=FALSE------------------------------------------------
source("knitr_setup.R")

## ------------------------------------------------------------------------
## Importando os dados para o R
milsa <- read.csv("dados/milsa.csv")

## ---- echo=FALSE, results='asis'-----------------------------------------
kable(milsa)

## ------------------------------------------------------------------------
civil.tb <- table(milsa$Est.civil)
cbind("f" = civil.tb)

## ------------------------------------------------------------------------
cbind("f" = addmargins(civil.tb))

## ------------------------------------------------------------------------
cbind("fr" = prop.table(civil.tb))

## ------------------------------------------------------------------------
cbind("fr" = addmargins(prop.table(civil.tb)))

## ----bar-quali-nom, fig.show='hold'--------------------------------------
par(mfrow = c(1, 2))
barplot(civil.tb, ylab = "Frequência absoluta")
barplot(prop.table(civil.tb), ylab = "Frequência relativa",
        ylim = c(0, .6))
par(mfrow = c(1,1))

## ----pie-quali-nom-------------------------------------------------------
pie(civil.tb)

## ------------------------------------------------------------------------
names(civil.tb)[which.max(civil.tb)]

## ------------------------------------------------------------------------
inst.tb <- table(milsa$Inst)
cbind("f" = addmargins(inst.tb))

## ------------------------------------------------------------------------
cbind("f" = addmargins(inst.tb),
      "fr" = addmargins(prop.table(inst.tb)))

## ----bar-quali-ord1------------------------------------------------------
barplot(inst.tb)

## ----bar-quali-ord2, fig.show='hold'-------------------------------------
par(mfrow = c(1,2))
## Menor para maior
barplot(sort(inst.tb))
## Maior para menor
barplot(sort(inst.tb, decreasing = TRUE))
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
names(inst.tb)[which.max(inst.tb)]

## ------------------------------------------------------------------------
filhos.tb <- table(milsa$Filhos)
cbind("f" = addmargins(filhos.tb))

## ------------------------------------------------------------------------
cbind("f" = addmargins(table(milsa$Filhos, useNA = "always")))

## ------------------------------------------------------------------------
cbind("f" = addmargins(filhos.tb),
      "fr" = addmargins(prop.table(filhos.tb)))

## ------------------------------------------------------------------------
filhos.tba <- as.table(cumsum(filhos.tb))
cbind("f" = addmargins(filhos.tb),
      "fr" = addmargins(prop.table(filhos.tb)),
      "F" = c(filhos.tba, NA))

## ------------------------------------------------------------------------
filhos.tba <- as.table(cumsum(filhos.tb))
cbind("f" = addmargins(filhos.tb),
      "fr" = addmargins(prop.table(filhos.tb)),
      "F" = c(filhos.tba, NA),
      "Fr" = c(filhos.tba/20, NA))

## ---- bar-quant-discr1---------------------------------------------------
plot(filhos.tb, xlab = "Número de filhos",
     ylab = "Frequência absoluta")

## ----bar-quant-discr2, fig.show='hold'-----------------------------------
par(mfrow = c(1,2))
## Frequência relativa
plot(prop.table(filhos.tb), xlab = "Número de filhos",
     ylab = "Frequência relativa")
## Frequência relativa acumulada
plot(filhos.tba/20, type = "S", # tipo step (escada)
     xlab = "Número de filhos",
     ylab = "Frequência acumulada relativa")
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
sort(milsa$Salario)

## ------------------------------------------------------------------------
(quebra <- seq(4, 24, 4))

## ------------------------------------------------------------------------
classes <- cut(milsa$Salario, breaks = quebra, right = FALSE)
classes.tab <- table(classes)
cbind("f" = addmargins(classes.tab))

## ------------------------------------------------------------------------
cbind("f" = addmargins(classes.tab),
      "fr" = addmargins(prop.table(classes.tab)),
      "F" = c(cumsum(classes.tab), NA),
      "Fr" = c(cumsum(classes.tab), NA)/36)

## ------------------------------------------------------------------------
plot(milsa$Salario, xlab = "Índice", ylab = "Salário")

## ----hist, fig.show='hold'-----------------------------------------------
par(mfrow = c(1,2))
hist(milsa$Salario, breaks = quebra, right = FALSE, main = "",
     xlab = "Classes", ylab = "Frequência absoluta",
     xlim = c(4, 24), labels = TRUE, axes = FALSE)
axis(1, at = quebra, labels = quebra); axis(2)
h <- hist(milsa$Salario, breaks = quebra, right = FALSE, main = "",
          xlab = "Classes", ylab = "Densidade",
          xlim = c(4, 24), ylim = c(0, .09), freq = FALSE, axes = FALSE)
axis(1, at = quebra, labels = quebra); axis(2)
text(h$mids, h$density, labels = round(h$density * 4, 2), pos = 3)
par(mfrow = c(1,1))

## ------------------------------------------------------------------------
cbind("f" = addmargins(classes.tab),
      "fr" = addmargins(prop.table(classes.tab)),
      "F" = c(cumsum(classes.tab), NA),
      "Fr" = c(cumsum(classes.tab), NA)/36,
      "Dens" = c(prop.table(classes.tab)/4, NA))

## ------------------------------------------------------------------------
stem(milsa$Salario, scale = 2)

## ------------------------------------------------------------------------
ex <- c("ótimo", "bom", "bom", "péssimo", "bom", "bom", "ótimo",
   "ótimo", "bom", "ótimo", "bom", "ótimo", "bom", "bom", "ótimo",
   "bom", "péssimo", "bom", "péssimo", "bom", "péssimo", "bom", "bom",
   "bom", "bom", "ótimo", "bom", "péssimo", "ótimo", "ótimo", "bom",
   "péssimo")
ex

## ---- echo=FALSE, include=FALSE------------------------------------------
## Tabela
ex <- factor(ex, levels = c("péssimo", "bom", "ótimo"), ordered = TRUE)
ex.tab <- table(ex)
cbind("f" = addmargins(ex.tab),
      "fr" = addmargins(prop.table(ex.tab)),
      "F" = c(cumsum(ex.tab), NA),
      "Fr" = c(cumsum(ex.tab), NA)/length(ex))
## Grafico
barplot(ex.tab, ylab = "Frequência absoluta")

## ------------------------------------------------------------------------
ex1 <- c(11,13,10,9,0,16,28,15,14,1,2,17,
         18,12,18,8,9,22,16,20,34,15,21,13,
         12,14,13,19,0,2,17,11,18,16,13,6,
         19,8,8,12,13,21,11,19,1,14,4,16)
sort(ex1)

## ---- echo=FALSE, include=FALSE------------------------------------------
(amp <- max(ex1) - min(ex1))
(k <- sqrt(length(ex1)))
(h <- amp/k)
quebra <- seq(0, 35, 5)
ex1.tab <- table(cut(ex1, breaks = quebra, right = FALSE))
cbind("f" = addmargins(ex1.tab),
      "fr" = addmargins(prop.table(ex1.tab)),
      "F" = c(cumsum(ex1.tab), NA),
      "Fr" = c(cumsum(ex1.tab), NA)/length(ex1),
      "Dens" = c(prop.table(ex1.tab)/5, NA))
hist(ex1, freq = FALSE, breaks = quebra, right = FALSE)
stem(ex1, scale = 2)

