##======================================================================
## Dados
##======================================================================

## Idades dos alunos
x <- c(22, 21, 24, 23, 20, 22, 21, 25, 24, 24, 23, 19, 25, 24, 23, 23,
       20, 21, 23, 20, 23, 22, 23, 23, 25, 25, 20, 23, 24, 20)

##======================================================================
## Amostras aleatórias
##======================================================================

# numeros de 1 a 10
sample(1:10)
# sem reposicao
sample(1:10, size = 5)
# com reposicao
sample(1:10, size = 5, replace = TRUE)

# fixando uma semente, o resultado é sempre o mesmo
set.seed(1)
sample(1:10, size = 5)
set.seed(09032016)
sample(1:10, size = 5)

# Idades sem reposição
sample(x, size = 5, replace = FALSE)
# Idades com reposição
sample(x, size = 5, replace = TRUE)

##======================================================================
## Erros amostrais
##======================================================================

# Média da população de idades
(mi <- mean(x))

# AAS (com reposição) das idades
(amostra <- sample(x, size = 5, replace = TRUE))
# Média da amostra
(med.amostra <- mean(amostra))
# Diferença entre a média da amostral e a média populacional
(difer <- med.amostra - mi)

stop
##======================================================================
## Distribuição amostral
##======================================================================

# media e variancia amostrais
(med <- mean(x))
(vari <- var(x))
# histograma
pdf("dist_amostral_idades.pdf", width = 8, height = 4)
par(mfrow = c(1, 4))
hist(x, freq = TRUE, main = "População - N = 20",
     include.lowest = TRUE, right = FALSE,
     col = "blue", xlab = "Idades", xlim = c(19, 25),
     ylab = "Frequência")
abline(v = med, col = 2, lwd = 2)

## Amostragem
# n amostral
n <- c(5, 10, 15)
# m = número de amostras aleatórias de tamanho n
m <- 1000
# vetor temporario para receber os valores de média
temp <- numeric(m)
for(i in 1:3){
    for(j in 1:m){
        temp[j] <- mean(sample(x, size = n[i], replace = TRUE))
    }
    hist(temp, freq = TRUE, main = paste("n = ", n[i]),
         include.lowest = TRUE, right = FALSE,
         xlab = "Médias amostrais", ylab = "Frequência",
         xlim = c(19, 25), col = "gray")
    abline(v = med, col = 2, lwd = 2)
}
dev.off()

## O ultimo objeto temp, refere-se as 1000 medias de tamanho n = 15,
## portanto,
mean(x); mean(temp)
var(x); var(temp); var(x)/15

##======================================================================
## Script Teorema do Limite Central - TLC
##======================================================================

set.seed(2014)
## Grafico de convergência de 4 distribuições de acordo com o TLC
# Normal(500, 1000)
norm <- rnorm(500, mean = 500, sd = 100)
# Uniforme[200,800]
unif <- runif(500, min = 200, max = 800)
# Exponencial(1)
expo <- rexp(500, rate = 1)
# Poisson(2)
pois <- rpois(500, lambda = 2)
# n amostral
n <- c(5, 25, 100)
# m = número de amostras aleatórias de tamanho n
m <- 500
# vetor temporario para receber os valores de média
temp <- numeric(m)

## Limites para cada distribuicao
xlim.norm <- c(150, 800)
xlim.unif <- c(200, 800)
xlim.expo <- c(0, 6)
xlim.pois <- c(0, 7)

pdf("dist_amostrais.pdf", width = 8, height = 8)
par(mfrow = c(4, 4))
# Distribuição Normal
hist(norm, freq = TRUE, main = "População - N = 500",
     include.lowest = TRUE, right = FALSE, ylab = "Frequência",
     col = "lightgray", xlab = "Normal", xlim = xlim.norm)
for(i in 1:3){
    for(j in 1:m){
        temp[j] <- mean(sample(norm, size = n[i], replace = TRUE))
    }
    hist(temp, freq = TRUE, main = paste("n = ", n[i]),
         include.lowest = TRUE, right = FALSE, ylab = "Frequência",
         xlab = "Médias amostrais", xlim = xlim.norm)
}
# Distribuição Uniforme
hist(unif, freq = TRUE, main = "População - N = 500",
     include.lowest = TRUE, right = FALSE, xlim = xlim.unif,
     col = "lightgray", xlab = "Uniforme", ylab = "Frequência")
for(i in 1:3){
    for(j in 1:m){
        temp[j] <- mean(sample(unif, size = n[i], replace = TRUE))
    }
    hist(temp, freq = TRUE, main = paste("n = ", n[i]),
         include.lowest = TRUE, right = FALSE, ylab = "Frequência",
         xlab = "Médias amostrais", xlim = xlim.unif)
}
# Distribuição Exponencial
hist(expo, freq = TRUE, main = "População - N = 500",
     include.lowest = TRUE, right = FALSE, xlim = xlim.expo,
     col = "lightgray", xlab = "Exponencial", ylab = "Frequência")
for(i in 1:3){
    for(j in 1:m){
        temp[j] <- mean(sample(expo, size = n[i], replace = TRUE))
    }
    hist(temp, freq = TRUE, main = paste("n = ", n[i]),
         include.lowest = TRUE, right = FALSE, ylab = "Frequência",
         xlab = "Médias amostrais", xlim = xlim.expo)
}
# Distribuição Poisson
hist(pois, freq = TRUE, main = "População - N = 500",
     include.lowest = TRUE, right = FALSE, xlim = xlim.pois,
     col = "lightgray", xlab = "Poisson", ylab = "Frequência")
for(i in 1:3){
    for(j in 1:m){
        temp[j] <- mean(sample(pois, size = n[i], replace = TRUE))
    }
    hist(temp, freq = TRUE, main = paste("n = ", n[i]),
         include.lowest = TRUE, right = FALSE, ylab = "Frequência",
         xlab = "Médias amostrais", xlim = xlim.pois)
}
par(mfrow = c(1, 1))
dev.off()

##======================================================================

##======================================================================
## Exemplo do slide
##======================================================================

medias <- c(23.2, 21.2, 21.4, 22.4, 21.4)

# histograma das medias
pdf("hist.pdf")
hist(medias, col = "blue",
     main = "", xlab = "Médias", ylab = "Frequência")
dev.off()

## media das medias
mean(medias)
## variancia das medias
var(medias)


##======================================================================
## Script Teorema do Limite Central - TLC
## para uma proporção
##======================================================================

## numero de lancamentos
## "provas de Bernoulli"
n <- 10
## numero de repeticoes do experimento
m <- 1000
## resultados possiveis
moeda <- c("Cara", "Coroa")

n <- 10
## vetor para armazenar os n ensaios
res <- numeric(n)

pdf("dist_amostral_proporcoes.pdf", width = 10, height = 6)
par(mfrow = c(1, 5))
## m = 10
m <- 10
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 10",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
## m = 30
m <- 30
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 30",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
## m = 100
m <- 100
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 100",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
## m = 1000
m <- 1000
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 1000",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
## m = 10000
m <- 10000
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 10000",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
dev.off()


moeda <- c("Cara", "Coroa")
res <- numeric(n)
m <- 1000

pdf("dist_amostral_proporcoes.pdf", width = 10, height = 6)
par(mfrow = c(1, 5))
## m = 10
n <- 10
m <- 1000
res <- numeric(n)
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 10",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
## m = 10
n <- 100
m <- 1000
res <- numeric(n)
prop <- numeric(m)
for(i in 1:m){
    for(j in 1:n){
        lanca <- sample(moeda, size = 1)
        if(lanca == "Cara"){
            res[j] <- 1
        } else{
            res[j] <- 0
        }
    }
    prop[i] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "n = 10, m = 10",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
dev.off()


##======================================================================
## Dados - proporcao de sucesso = impar
##======================================================================
par(mfrow=c(1,4))
# m = 10
m <- 10
prop <- numeric(m)
n <- 5
res <- numeric(n)
for(j in 1:m){
    for(i in 1:n){
        dados <- sample(1:6, size = 1)
        impar <- c(1, 3, 5)
        res[i] <- ifelse(dados %in% impar, 1, 0)
    }
    prop[j] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
# m = 30
m <- 30
prop <- numeric(m)
n <- 5
res <- numeric(n)
for(j in 1:m){
    for(i in 1:n){
        dados <- sample(1:6, size = 1)
        impar <- c(1, 3, 5)
        res[i] <- ifelse(dados %in% impar, 1, 0)
    }
    prop[j] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
# m = 100
m <- 100
prop <- numeric(m)
n <- 5
res <- numeric(n)
for(j in 1:m){
    for(i in 1:n){
        dados <- sample(1:6, size = 1)
        impar <- c(1, 3, 5)
        res[i] <- ifelse(dados %in% impar, 1, 0)
    }
    prop[j] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
# m = 1000
m <- 1000
prop <- numeric(m)
n <- 5
res <- numeric(n)
for(j in 1:m){
    for(i in 1:n){
        dados <- sample(1:6, size = 1)
        impar <- c(1, 3, 5)
        res[i] <- ifelse(dados %in% impar, 1, 0)
    }
    prop[j] <- sum(res)/n
}
hist(prop, xlim = c(0,1), main = "",
     xlab = "Proporções amostrais",
     ylab = "Frequência")
abline(v = 0.5, col = 2, lwd = 2)
