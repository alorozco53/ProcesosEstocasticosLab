source('funciones_generales.R')

BodyParticlesProbab <- function(x, y, lambda, p) {
    probab <- 0
    for (z in 0:min(x,y)) {
        particles.added <- ((lambda**(y-z)) * exp(-lambda)) /
            factorial(y - z)
        rem.part.1 <- comb(x, z)
        rem.part.2 <- (p**z) * ((1 - p)**(x - z))
        probab <- probab + (particles.added * rem.part.1 * rem.part.2)
    }
    return(probab)
}

BodyParticlesChain <- function(x0, lambda, p, n) {
    H <- function(rem.part, added.part) {
        return(rem.part + added.part)
    }
    
    times <- c(0)
    
    UpdateParticlesTime <- function(number.particles) {
        counter <- length(times) + 1
        if (number.particles > 0) {
            for (i in 1:number.particles) {
                times[counter] <- rgeom(1, p)
                counter <- counter + 1
            }
        }
    }

    getSurvivingParticles <- function() {
        parts <- 0
        if (length(times) > 0) {
            for (i in 1:length(times)) {
                times[i] <- times[i] - 1
                if (times[i] > 0)
                    parts <- parts + 1
            }
        }
        times <- times[which(times > 0)]
        return(parts)
    }
    
    markov.chain <- c(x0)
    UpdateParticlesTime(markov.chain[1])
    if (n == 1)
        markov.chain[2] <- H(getSurvivingParticles(), rpois(1, lambda))
    else {
        for (i in 2:n) {
            markov.chain[i] <- H(getSurvivingParticles(), rpois(1, lambda))
            UpdateParticlesTime(markov.chain[i])
        }
    }
    return(markov.chain)
}

BodyParticlesStatDistrib <- function(lambda, p, n) {
    distrib <- c()
    for (i in 0:(n-1)) {
        numer <- ((lambda / (1 -p))**i) * (exp(-lambda / (1 - p)))
        denom <- factorial(i)
        distrib[i+1] <- numer / denom
    }
    return(distrib)
}

ex.lambda <- floor(10 * (runif(1) + 0.01))
ex.x0 <- rpois(1, ex.lambda)
ex.p <- runif(1)
ex.n <- floor(1000 * (runif(1) + 0.01)) + 200
example <- BodyParticlesChain(ex.x0, ex.p, ex.lambda, ex.n)
print('Parámetros:')
print(paste('x0:', ex.x0, ' '))
print(paste('p:', ex.p, ' '))
print(paste('n:', ex.n, ' '))
print(paste('lambda:', ex.lambda, ' '))
plot(example, type="o", col="red",
     main='Simulación de cadena de partículas en un cuerpo',
     xlab="tiempos", ylab="número de partículas en el cuerpo")

ex.stat.n <- floor(100 * (1 + 0.01)) + 69
ex.stat.distrib <- BodyParticlesStatDistrib(ex.lambda,
                                            ex.p,
                                            ex.stat.n)
approx.pi <- c(0)
for (j in 0:(ex.stat.n-1))
    approx.pi[j] <- AvgVisits(example, j, ex.stat.n)
x11()
old.par <- par(mfrow=c(2,1))
plot(ex.stat.distrib, type="p", col="green",
     main='Simulación de la distribución estacionaria',
     xlab="estados", ylab="valores para la distribución")
plot(approx.pi, type="p", col="blue",
     main='Aproximación de la distribución estacionaria',
     xlab="estados", ylab="valores para la distribución")
par(old.par)

x11()
plot(VectorComparison(approx.pi, ex.stat.distrib), type="p", col="black",
     main='Comparación entre distribuciones estacionarias',
     xlab="subconjunto de estados", ylab="diferencia entre ambas distribuciones")
