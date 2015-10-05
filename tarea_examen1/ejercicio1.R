source('funciones_generales.R')

## Ejercicio 1
print('Ejercicio 1')

## Construye la matriz de probabilidades de transición
## p: parámetro dado
InitMatrix <- function(p) {
    P <- matrix(c(0), 4, 4)
    P[1,2] <- P[3,2] <- P[2,4] <- p
    P[1,1] <- P[3,1] <- P[2,3] <- 1-p
    P[4,2] <- 1
    return(P)
}

## b)
print('b)')
### Parámetro p de la matriz
p <- 1/4
print('matriz de probabilidades de transición P:')
init.matrix.0 <- InitMatrix(p)
print(init.matrix.0)
### Distribución inicial de ejemplo
print('distribución inicial de ejemplo:')
init <- (1/4) * c(1, 1, 1, 1)
print(init)
### Tiempo de simulación
n <- 100
print('tiempo de simulación de ejemplo:')
print(n)
simulation <- MarkovSimulation(init.matrix.0, init, n)
print(simulation)
plot(simulation, type="o", col="green",
     main="Cadena de Márkov",
     xlab="tiempo", ylab="")

## c)
print('c)')
### Valores de p de ejemplo
print('Dos valores, p1 y p2:')
p1 <- 53/234
p2 <- 8/9
print(c(p1, p2))
### Matrices de transición
print('Matrices de transición:')
init.matrix.1 <- InitMatrix(p1)
init.matrix.2 <- InitMatrix(p2)
print(init.matrix.1)
print(init.matrix.2)
### Distribución inicial, iniciando en i = 2
print('Se iniciarán las simulaciones en i = 2')
init.distrib.2 <- c(0,0,1,0)
print(init.distrib.2)
avg.vis.p1.1000 <- c(0)
avg.vis.p1.10000 <- c(0)
avg.vis.p2.1000 <- c(0)
avg.vis.p2.10000 <- c(0)
sim.p1.1000 <- MarkovSimulation(init.matrix.1,
                                init.distrib.2, 1000)
sim.p1.10000 <- MarkovSimulation(init.matrix.1,
                                 init.distrib.2, 10000)
sim.p2.1000 <- MarkovSimulation(init.matrix.2,
                                init.distrib.2, 1000)
sim.p2.10000 <- MarkovSimulation(init.matrix.2,
                                 init.distrib.2, 10000)
for (j in 0:3) {
    avg.vis.p1.1000[j+1] <- AvgVisits(sim.p1.1000, j, 1000)
    avg.vis.p1.10000[j+1] <- AvgVisits(sim.p1.10000, j, 10000)
    avg.vis.p2.1000[j+1] <- AvgVisits(sim.p2.1000, j, 1000)
    avg.vis.p2.10000[j+1] <- AvgVisits(sim.p2.10000, j, 10000)
}
### Resultados
print('Visitas promedio con p1, i = 2, 1000 pasos, para toda j:')
print(avg.vis.p1.1000)
print('Visitas promedio con p1, i = 2, 10000 pasos, para toda j:')
print(avg.vis.p1.10000)
print('Visitas promedio con p2, i = 2, 1000 pasos, para toda j:')
print(avg.vis.p2.1000)
print('Visitas promedio con p2, i = 2, 10000 pasos, para toda j:')
print(avg.vis.p2.10000)
### Graficación de las simulaciones
x11()
old.par <- par(mfrow=c(2,2))
plot(avg.vis.p1.1000, x=0:3,main="Visitas promedio con p1, empezando en 2, 1000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
plot(avg.vis.p1.10000, x=0:3, main="Visitas promedio con p1, empezando en 2, 10000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
plot(avg.vis.p2.1000, x=0:3, main="Visitas promedio con p2, empezando en 2, 10000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
plot(avg.vis.p2.10000, x=0:3, main="Visitas promedio con p2, empezando en 2, 1000 pasos",
     type='p', xlab='estados', ylab='distribución estacionaria')
par(old.par)
