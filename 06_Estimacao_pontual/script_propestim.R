
require(plyr)
require(lattice)
require(latticeExtra)

##======================================================================
## População: v.a. com densidade Gaussiana.
## Parâmetro: Média populacional (\mu).

## Estimador 1: média amostral.
## Estimador 2: ponto médio entre extremos.

##-----------------------------------------------------------------------------
## Viés, variância e erro quadrático médio.

## Definições:
## * Amostra aleatória de tamanho n=10.
## * X ~ Gaussiana(\mu=3, \sigma^2=1)
## * Gerar N=1000 processos de estimação.

N <- 1000
n <- 10

th1 <- replicate(N, mean(rnorm(n, mean=3, sd=1)))
th2 <- replicate(N, mean(range(rnorm(n, mean=3, sd=1))))

L <- list(th1=data.frame(est=th1), th2=data.frame(est=th2))
L <- ldply(L)
str(L)

pdf("vies_normal.pdf",w=8,h=6)
densityplot(~est|.id, data = L,
            panel = function(x, ...){
    panel.densityplot(x, ...)
    panel.abline(v = mean(x))
},
xlab = "Estimativa", ylab = "Densidade",
strip = strip.custom(factor.levels =
                         c(expression(hat(theta[1])), expression(hat(theta[2])))))+
    layer(panel.abline(v=3))
dev.off()

##-----------------------------------------------------------------------------
## Consistência.

N <- 100
nval <- c(2,3,5,10,20,50,100,500,1000)

th1 <- sapply(nval,
              function(n){
                  replicate(N, mean(rnorm(n, mean=3, sd=1)))
              })
str(th1)
th1 <- stack(as.data.frame(th1))
levels(th1$ind) <- as.character(nval)
th1$ind <- as.numeric(as.character(th1$ind))

xyplot(values~ind, th1, scales=list(x=list(log=10)))

th2 <- replicate(N, mean(range(rnorm(N, mean=3, sd=1))))

th2 <- sapply(nval,
              function(n){
                  replicate(N, mean(range(rnorm(n, mean=3, sd=1))))
              })
str(th2)
th2 <- stack(as.data.frame(th2))
levels(th2$ind) <- as.character(nval)
th2$ind <- as.numeric(as.character(th2$ind))

xyplot(values~ind, th2, scales=list(x=list(log=10)))

L <- list(th1=th1, th2=th2)
L <- ldply(L)
L$.id <- factor(L$.id)

xyplot(values~ind|factor(.id), L, scales=list(x=list(log=10)))+
    layer(panel.abline(h=3))

xyplot(values~.id|factor(ind), L, jitter.x=TRUE, as.table = TRUE)+
    layer(panel.abline(h=3))


##-----------------------------------------------------------------------------
## Eficiência.

stop
##=============================================================================
##=============================================================================
##=============================================================================

##-----------------------------------------------------------------------------
## Viés, variância e erro quadrático médio.

## Definições:
## * Amostra aleatória de tamanho n=10.
## * X ~ Gaussiana(\mu=3, \sigma^2=1)
## * Gerar N=1000 processos de estimação.

N <- 1000
n <- 10

th1 <- replicate(N, mean(runif(n, min=2, max=4)))
th2 <- replicate(N, mean(range(runif(n, min=2, max=4))))

L <- list(th1=data.frame(est=th1), th2=data.frame(est=th2))
L <- ldply(L)
str(L)

pdf("vies_uniforme.pdf",w=8,h=6)
densityplot(~est|.id, data=L,
            panel=function(x, ...){
                panel.densityplot(x, ...)
                panel.abline(v=mean(x))
            },
            xlab = "Estimativa", ylab = "Densidade",
            strip = strip.custom(factor.levels =
                                     c(expression(hat(theta[1])), expression(hat(theta[2])))))+
                layer(panel.abline(v=3))
dev.off()


##-----------------------------------------------------------------------------
## Consistência.

N <- 100
nval <- c(2,3,5,10,20,50,100,500,1000)

th1 <- sapply(nval,
              function(n){
                  replicate(N, mean(runif(n, min=2, max=4)))
              })
str(th1)
th1 <- stack(as.data.frame(th1))
levels(th1$ind) <- as.character(nval)
th1$ind <- as.numeric(as.character(th1$ind))

xyplot(values~ind, th1, scales=list(x=list(log=10)))

th2 <- replicate(N, mean(range(runif(N, min=2, max=4))))

th2 <- sapply(nval,
              function(n){
                  replicate(N, mean(range(runif(n, min=2, max=4))))
              })
str(th2)
th2 <- stack(as.data.frame(th2))
levels(th2$ind) <- as.character(nval)
th2$ind <- as.numeric(as.character(th2$ind))

xyplot(values~ind, th2, scales=list(x=list(log=10)))

L <- list(th1=th1, th2=th2)
L <- ldply(L)
L$.id <- factor(L$.id)

pdf("consistencia_uniforme.pdf",w=8,h=6)
xyplot(values~ind|.id, L,
       xlab = "Tamanho da amostra (escala log)", ylab = "Estimativas",
       strip = strip.custom(factor.levels =
                                c(expression(hat(theta[1])), expression(hat(theta[2])))),
       scales=list(x=list(log=10)))+
           layer(panel.abline(h=3))
dev.off()

xyplot(values~.id|factor(ind), L, jitter.x=TRUE, as.table=TRUE)+
    layer(panel.abline(h=3))


##-----------------------------------------------------------------------------
## Eficiência.



## Exemplo 3

x <- c(11.96, 5.03, 67.40, 16.07, 31.5, 7.73, 11.1, 22.38)
n <- length(x)
xbarra <- mean(x)
estm <- 1/xbarra

pdf("vero_exp.pdf",w=8,h=6)
theta <- seq(0.03, 0.06, length = 100)
vero <- n * log(theta) - theta * sum(x)
plot(theta, vero, type = "l",
     xlab = expression(theta), ylab = expression(l(theta,x)))
abline(v = estm)
dev.off()

##======================================================================
## População: v.a. com densidade Gaussiana.
## Parâmetro: Variância populacional (\sigma^2).

## Estimador 1: variancia/n
## Estimador 2: variancia/(n-1)

## Definições:
## * Amostra aleatória de tamanho n=10.
## * X ~ Gaussiana(\mu=3, \sigma^2=1)
## * Gerar N=1000 processos de estimação.

N <- 1000
n <- 10

##----------------------------------------------------------------------
## Teste
a <- rnorm(n, 3, 1)
var(a)
var(a) * ((n-1)/n)

var2 <- function(x){
    v2 <- (1/n) * (sum(x^2) - n * mean(x)^2)
    return(v2)
}
var2(a)
## Exatamente igual
##----------------------------------------------------------------------

th1 <- replicate(N, var(rnorm(n, mean=3, sd=1)))
th2 <- replicate(N, var(rnorm(n, mean=3, sd=1) * ((n-1)/n)))

L <- list(th1=data.frame(est=th1), th2=data.frame(est=th2))
L <- ldply(L)
str(L)

densityplot(~est|.id, data = L,
            panel = function(x, ...){
    panel.densityplot(x, ...)
    ## panel.abline(v = var(x))
},
xlab = "Estimativa", ylab = "Densidade",
strip = strip.custom(factor.levels =
                         c(expression(hat(theta[1])), expression(hat(theta[2])))))+
    layer(panel.abline(v=1))


##-----------------------------------------------------------------------------
## Consistência.

N <- 100
nval <- c(2,3,5,10,20,50,100,500,1000)

th1 <- sapply(nval,
              function(n){
                  replicate(N, var(rnorm(n, mean=3, sd=1)))
              })
str(th1)
th1 <- stack(as.data.frame(th1))
levels(th1$ind) <- as.character(nval)
th1$ind <- as.numeric(as.character(th1$ind))

xyplot(values~ind, th1, scales=list(x=list(log=10)))

th2 <- replicate(N, mean(range(rnorm(N, mean=3, sd=1))))

th2 <- sapply(nval,
              function(n){
                  replicate(N, var(rnorm(n, mean=3,sd=1)) * ((n-1)/n))
              })
str(th2)
th2 <- stack(as.data.frame(th2))
levels(th2$ind) <- as.character(nval)
th2$ind <- as.numeric(as.character(th2$ind))

xyplot(values~ind, th2, scales=list(x=list(log=10)))

L <- list(th1=th1, th2=th2)
L <- ldply(L)
L$.id <- factor(L$.id)

xyplot(values~ind|factor(.id), L, scales=list(x=list(log=10)))+
    layer(panel.abline(h=1))

xyplot(values~.id|factor(ind), L, jitter.x=TRUE, as.table = TRUE)+
    layer(panel.abline(h=3))
