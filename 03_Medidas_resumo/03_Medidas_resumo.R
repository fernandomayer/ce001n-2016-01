## ----setup, include=FALSE, cache=FALSE-----------------------------------
## Opcoes globais
opts_chunk$set(size = "footnotesize",
               prompt = FALSE,
               comment = NA,
               tidy = FALSE,
               cache = TRUE,
               fig.align = "center",
               fig.width = 8,
               fig.height = 6,
               out.width=".8\\textwidth")
## thm <- knit_theme$get("beamer3")
## knit_theme$set(thm)
options(width = 65, digits = 5, continue = "  ")
## Use pdfcrop=TRUE nos chunks para reduzir a area do PDF
knit_hooks$set(pdfcrop = hook_pdfcrop)

## ----pacotes, echo=FALSE-------------------------------------------------
require(xtable, quietly = TRUE)

## ----echo=FALSE, include=FALSE-------------------------------------------
aa <- c(43, 47, 48, 51, 51, 55, 55, 57, 59)
ab <- c(20, 22, 45, 46, 53, 54, 56, 57)
median(aa)
median(ab)
(maa <- mean(aa))
(mab <- mean(ab))
(aa - maa)^2
(ab - mab)^2
sum((aa - maa)^2)
sum((ab - mab)^2)
(vaa <- sum((aa - maa)^2)/length(aa))
(vab <- sum((ab - mab)^2)/length(ab))
## Form alternativa
(saa <- sum(aa))
(sab <- sum(ab))
(saa2 <- sum(aa^2))
(sab2 <- sum(ab^2))
(vaa <- (saa2 - (saa^2/length(aa)))/length(aa))
(vab <- (sab2 - (sab^2/length(ab)))/length(ab))
(daa <- sqrt(vaa))
(dab <- sqrt(vab))
daa/maa * 100
dab/mab * 100

## ----echo=FALSE, out.width=".95\\textwidth"------------------------------
par(mfrow=c(2,1))
set.seed(2)
hist(rnorm(1000, 100, 2), main = "N(100, 4)",
     ylab = "Frequência absoluta", xlab = "x",
     xlim = c(40, 160)); box()
abline(v=100, col="red", lwd = 3)
hist(rnorm(1000, 100, 10), main = "N(100, 100)",
     ylab = "Frequência absoluta", xlab = "x",
     xlim = c(40, 160)); box()
abline(v=100, col="red", lwd = 3)

## ----echo=FALSE, include=FALSE-------------------------------------------
x <- c(0,1,2,3,5)
f <- c(4, 5, 7, 3, 1)
n <- sum(f)
(mx <- sum(x*f)/n)
x - mx
(x - mx)^2
f*(x - mx)^2
sum(f*(x - mx)^2)
sum(f*(x - mx)^2)/n

## ----echo=FALSE, include=FALSE-------------------------------------------
x <- c(6, 10, 14, 18, 22)
f <- c(10, 12, 8, 5, 1)
n <- sum(f)
(mx <- sum(x*f)/n)
x - mx
(x - mx)^2
f*(x - mx)^2
sum(f*(x - mx)^2)
sum(f*(x - mx)^2)/n

## ----echo=FALSE,include=FALSE--------------------------------------------
ta <- c(0, 2, 4, 5, 5, 6, 8, 10)
tb <- c(4, 4.5, 5, 5, 5, 5, 5.5, 6)
(mta <- mean(ta))
(mtb <- mean(tb))
(ta - mta)^2
(tb - mtb)^2
sum((ta - mta)^2)
sum((tb - mtb)^2)
(vta <- sum((ta - mta)^2)/length(ta))
(vtb <- sum((tb - mtb)^2)/length(tb))
sqrt(vta)
sqrt(vtb)
var(ta) # usa n-1
var(tb)
sd(ta)
sd(tb)

## ----echo=FALSE, include=FALSE-------------------------------------------
aa <- c(43, 47, 48, 51, 51, 55, 55, 57, 59)
ab <- c(20, 22, 45, 46, 53, 54, 56, 57)
median(aa)
median(ab)
(maa <- mean(aa))
(mab <- mean(ab))
(aa - maa)^2
(ab - mab)^2
sum((aa - maa)^2)
sum((ab - mab)^2)
(vaa <- sum((aa - maa)^2)/length(aa))
(vab <- sum((ab - mab)^2)/length(ab))
## Form alternativa
(saa <- sum(aa))
(sab <- sum(ab))
(saa2 <- sum(aa^2))
(sab2 <- sum(ab^2))
(vaa <- (saa2 - (saa^2/length(aa)))/length(aa))
(vab <- (sab2 - (sab^2/length(ab)))/length(ab))
(daa <- sqrt(vaa))
(dab <- sqrt(vab))
daa/maa * 100
dab/mab * 100

## ----eval=FALSE,include=FALSE--------------------------------------------
## x <- c(1.75, 3.25,4.75,6.25,7.75,9.25)
## f <- c(3,5,3,7,9,13)
## n <- sum(f)
## fr <- f/n
## xf <- x*f
## x2 <- x^2
## x2f <- x2*f
## (sum(x2f) - (sum(xf)^2/n))/n
## x2fr <- x2*fr
## xfr <- x*fr
## sum(x2fr) - sum(xfr)^2

## ----echo=FALSE----------------------------------------------------------
quant <- c(15,21,28,25,30,11,17,12,25,20,16,23,12,10)
sort(quant)

## ----echo=FALSE----------------------------------------------------------
quant2 <- c(15,21,28,25,30,11,17,12,25,20,16,23,12,10, 59)
sort(quant2)

## ----echo=FALSE, eval=FALSE----------------------------------------------
## quantile(quant, probs = c(.3, .65))
## quantile(quant2, probs = c(.3, .65))

## ----echo=FALSE----------------------------------------------------------
sort(quant)

## ----echo=FALSE----------------------------------------------------------
sort(quant2)

## ----echo=FALSE,eval=FALSE-----------------------------------------------
## fivenum(quant)
## fivenum(quant2)

## ----echo=FALSE----------------------------------------------------------
sort(quant)

## ----echo=FALSE----------------------------------------------------------
dquant <- fivenum(quant)
names(dquant) <- c("x(1)", "Q1", "Q2", "Q3", "x(n)")
dquant

## ----echo=FALSE----------------------------------------------------------
sort(quant2)

## ----echo=FALSE----------------------------------------------------------
dquant2 <- fivenum(quant2)
names(dquant2) <- c("x(1)", "Q1", "Q2", "Q3", "x(n)")
dquant2

## ----echo=FALSE----------------------------------------------------------
sort(quant)

## ----echo=FALSE----------------------------------------------------------
dquant

## ----echo=FALSE, pdfcrop=TRUE, fig.width=6, fig.height=4, out.width=".6\\textwidth"----
boxplot(quant, horizontal = TRUE)

## ----echo=FALSE----------------------------------------------------------
sort(quant2)

## ----echo=FALSE----------------------------------------------------------
dquant2

## ----echo=FALSE, pdfcrop=TRUE, fig.width=6, fig.height=4, out.width=".6\\textwidth"----
boxplot(quant2, horizontal = TRUE)

## ----echo=FALSE----------------------------------------------------------
hom <- c(5,2,7,9,3,4,3,1,3,8)
mul <- c(3,5,7,4,5,6,7,6,5,4)
djunto <- fivenum(c(hom,mul))
names(djunto) <- c("x(1)", "Q1", "Q2", "Q3", "x(n)")
djunto

## ----echo=FALSE, pdfcrop=TRUE, fig.width=6, fig.height=4, out.width=".6\\textwidth"----
boxplot(c(hom,mul), horizontal = TRUE,
        xlab = "Tempo de espera (minutos)", ylim = c(0, 10))

## ----echo=c(1,5)---------------------------------------------------------
## Homens
dhom <- fivenum(hom)
names(dhom) <- c("x(1)", "Q1", "Q2", "Q3", "x(n)")
dhom
## Mulheres
dmul <- fivenum(mul)
names(dmul) <- c("x(1)", "Q1", "Q2", "Q3", "x(n)")
dmul

## ----echo=FALSE, pdfcrop=TRUE, fig.width=6, fig.height=4, out.width=".5\\textwidth"----
boxplot(data.frame(hom,mul),
        names = c("Homens", "Mulheres"), ylim = c(0, 10),
        ylab = "Tempo de espera (minutos)",
        col = c("blue", "pink"))

## ----teste,eval=FALSE,echo=FALSE-----------------------------------------
## x <- rnorm(100, 5, 1)
## mean(x)
## var(x)
## x <- c(1,3,5,5,7)
## require(MASS)
## margin.table(x)
## fractions(addmargins(prop.table(table(x))))
## fractions(addmargins(prop.table(table(x,x))))

