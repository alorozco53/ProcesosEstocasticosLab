## Ejercicio 8
## Author: AlOrozco53

source('funciones_generales.R')

mat <- matrix(0, 7, 7)
mat[1,1] <- mat[5,5] <- mat[6,6] <- mat[7,7] <-
    mat[6,5] <- mat[7,6] <- mat[5,7] <- 1/2
mat[1,3] <- mat[1,5] <- 1/8
mat[1,4] <- 1/4
mat[2,3] <- mat[3,4] <- mat[4,2] <- 1

## c)
print('c)')
print('matriz de probabilidades de transición:')
print(mat)
### Distribución inicial de ejemplo
print('distribución inicial de ejemplo:')
init <- (1/7) * c(1, 1, 1, 1, 1, 1, 1)
print(init)
### Tiempo de simulación
n <- floor(1000 * (runif(1) + 0.01)) + 200
print('tiempo de simulación de ejemplo:')
print(n)
simulation <- MarkovSimulation(mat, init, n)
plot(simulation, type="o", col="black",
     main="Cadena de Márkov",
     xlab="tiempo", ylab="")

## d)
print('d)')
### Distribución inicial, iniciando en algún estado i
i <- floor(7*runif(1) + 1)
print(paste('Se iniciarán las simulaciones en i =', toString(i-1), ' '))
init.distrib.2 <- c(0, 0, 0, 0, 0, 0, 0)
init.distrib.2[i] = 1
print(init.distrib.2)
avg.vis.1000 <- c(0)
avg.vis.10000 <- c(0)
sim.1000 <- MarkovSimulation(mat, init.distrib.2, 1000)
sim.10000 <- MarkovSimulation(mat, init.distrib.2, 10000)
for (j in 0:6) {
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
plot(avg.vis.1000, x=0:6,
     main="Visitas promedio a cada estado en 1000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
plot(avg.vis.10000, x=0:6,
     main="Visitas promedio a cada estado en 10000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
par(old.par)
