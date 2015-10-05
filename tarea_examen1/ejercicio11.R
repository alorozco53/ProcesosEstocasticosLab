## Ejercicio 11
## Author: AlOrozco53

source('funciones_generales.R')

print('Ejercicio 11')

## Construye la matriz de probabilidades de transición
## demostrada en el inciso anterior
## p: parámetro dado
InitMatrix <- function(p) {
    P <- matrix(c(0), 4, 4)
    P[1,1] <- P[2,3] <- P[3,1] <- P[4,1] <- p
    P[1,2] <- P[2,4] <- P[3,2] <- P[4,2] <- 1-p
    return(P)
}

### Parámetro p de la matriz
p <- runif(1)
print(paste('parámetro p:', toString(p), ' '))
print('matriz de probabilidades de transición:')
init.matrix.0 <- InitMatrix(p)
print(init.matrix.0)

## b)
print('b)')
### Distribución inicial de ejemplo
print('distribución inicial de ejemplo:')
init <- (1/4) * c(1, 1, 1, 1)
print(init)
### Tiempo de simulación
n <- floor(1000 * (runif(1) + 0.01)) + 200
print('tiempo de simulación de ejemplo:')
print(n)
simulation <- MarkovSimulation(init.matrix.0, init, n)
plot(simulation, type="o", col="blue",
     main="Cadena del viaje con o sin lluvia, con o sin coche",
     xlab="tiempo", ylab="")

## c)
print('c)')
### Distribución inicial, iniciando en algún estado i
i <- floor(4*runif(1) + 1)
print(paste('Se iniciarán las simulaciones en i =', toString(i-1), ' '))
init.distrib.2 <- c(0, 0, 0, 0)
init.distrib.2[i] = 1
print(init.distrib.2)
avg.vis.1000 <- c(0)
avg.vis.10000 <- c(0)
sim.1000 <- MarkovSimulation(init.matrix.0, init.distrib.2, 1000)
sim.10000 <- MarkovSimulation(init.matrix.0, init.distrib.2, 10000)
for (j in 0:3) {
    avg.vis.1000[j+1] <- AvgVisits(sim.1000, j, 1000)
    avg.vis.10000[j+1] <- AvgVisits(sim.10000, j, 10000)
}
### Resultados
print('Visitas promedio a cada estado en 1000 pasos')
print(avg.vis.1000)
print('Visitas promedio a cada estado en 10000 pasos')
print(avg.vis.10000)
### Graficación de las simulaciones
x11()
old.par <- par(mfrow=c(2,1))
plot(avg.vis.1000, x=0:3,
     main="Visitas promedio a cada estado en 1000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
plot(avg.vis.10000, x=0:3,
     main="Visitas promedio a cada estado en 10000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
par(old.par)
