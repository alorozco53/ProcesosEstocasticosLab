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
     main="Cadena de Márkov con probabilidad de transición P",
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
print('Se iniciará la cadena en i = 2')
init.distrib.2 <- c(0,0,1,0)
print(init.distrib.2)
sim.p1.1000 <- c(0)
sim.p1.10000 <- c(0)
sim.p2.1000 <- c(0)
sim.p2.10000 <- c(0)
for (j in 1:4) {
    sim.p1.1000[j] <- AvgVisits(init.matrix.1,
                                init.distrib.2, j, 1000)
    sim.p1.10000[j] <- AvgVisits(init.matrix.1,
                                 init.distrib.2, j, 10000)
    sim.p2.1000[j] <- AvgVisits(init.matrix.2,
                                init.distrib.2, j, 1000)
    sim.p2.10000[j] <- AvgVisits(init.matrix.1,
                                 init.distrib.2, j, 10000)
}
### Resultados
print('Simulación con p1, i = 2, 1000 pasos, para toda j:')
print(sim.p1.1000)
print('Simulación con p1, i = 2, 10000 pasos, para toda j:')
print(sim.p1.10000)
print('Simulación con p2, i = 2, 1000 pasos, para toda j:')
print(sim.p2.1000)
print('Simulación con p2, i = 2, 10000 pasos, para toda j:')
print(sim.p2.10000)
