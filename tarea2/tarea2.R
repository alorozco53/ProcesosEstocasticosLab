## Simulaciones para la Tarea 2 - Procesos Estocásticos
## Author: AlOrozco53

HomPPUniformTimes <- function(lambda, t) {
    ## This function simulates a homogeneous Poisson process Nt with rate λ.
    ## It assumes that given n = Nt, the arriving times' distribution is the
    ## uniform(0,t) order statistics.
    events <- c()
    n <- rpois(1, lambda * t)
    len.process <- t
    if (n > 0 && t > 1) {
        arrival.times <- sort(t * runif(n))
        for (time in arrival.times[1:t])
            events <- c(events, rpois(1, lambda * time))
        events <- sort(events)
        if (events[1] != 0) {
            events <- c(0, events)
            arrival.times <- c(0, arrival.times)
        }
        len.process <- length(arrival.times)
    } else if (t == 0)
        return(matrix(c(0),1,1))
      else if (t == 1)
          return(matrix(c(0, 0, 1, rpois(1, lambda)), 2, 2))
    p.process <- matrix(c(0), 2, len.process)
    p.process[1,] <- events
    p.process[2,] <- arrival.times
    return(p.process)
}

Inciso1 <- function(lambda, t) {
    ## Inciso 1
    simulation <- HomPPUniformTimes(lambda, t)
    print(paste('lambda:', toString(lambda), ' '))
    print(paste('t:', toString(t), ' '))
    print(simulation)
    plot(x=simulation[2,], y=simulation[1,], type="s", col="red",
         main="Proceso de Poisson homogéneo",
         xlab="tiempo", ylab="eventos");
}
