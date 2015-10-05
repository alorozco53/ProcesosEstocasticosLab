## Ejercicio 3
## Author: AlOrozco53

source('funciones_generales.R')

print('Ejercicio 3')

## Simula una cadena de Márkov que modela el número de partículas
## en un cuerpo de acuerdo a la explicación del ejercicio.
## x0: número de partículas inicial
## lambda: parámetro dado para la distribución Poisson
## p: parámetro dado para la distribución geométrica
## n: tamaño de la simulación
BodyParticlesChain <- function(x0, lambda, p, n) {
    ## Función auxiliar que define el valor de la cadena
    ## de Márkov en cada paso; X[n] = H(X[n-1], y)
    H <- function(rem.part, added.part) {
        return(rem.part + added.part)
    }
    
    times <- c(0)

    ## Actualiza el vector de tiempos 'times' que corresponde
    ## a las partículas 'vivas' dentro del cuerpo.
    ## number.particles: número de partículas a agregar
    UpdateParticlesTime <- function(number.particles) {
        counter <- length(times) + 1
        if (number.particles > 0) {
            for (i in 1:number.particles) {
                times[counter] <- rgeom(1, p)
                counter <- counter + 1
            }
        }
    }

    ## Devuelve el número de partículas sobrevivientes en cada paso
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

## Calcula un vector con n entradas (de 0 hasta n) de la distribución
## estacionaria π, usando la fórmula que se derivó.
## lambda: parámetro dado
## p: parámetro dado
## n: tamaño de la simulación
BodyParticlesStatDistrib <- function(lambda, p, n) {
    distrib <- c()
    for (i in 0:(n-1)) {
        numer <- ((lambda / (1 -p))**i) * (exp(-lambda / (1 - p)))
        denom <- factorial(i)
        distrib[i+1] <- numer / denom
    }
    return(distrib)
}

## Simulación de la cadena de Márkov
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

## Simulación y estimación de la distribución estacionaria
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

## Comparación entre la aproximación y la distribución estacionaria
x11()
plot(VectorComparison(approx.pi, ex.stat.distrib), type="p", col="black",
     main='Comparación entre distribuciones estacionarias',
     xlab="subconjunto de estados", ylab="diferencia entre ambas distribuciones")
