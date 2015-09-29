H <- function(x, y) {
    if (y == 0)
        return(0)
    else
        return(x + 1)
}

StreakMarkovChain <- function(x0, p, n) {
    markov.chain <- c(x0)
    for (i in 2:n)
        markov.chain[i] <- H(markov.chain[i-1], rbinom(1, 1, p))
    return(markov.chain)
}

example <- StreakMarkovChain(0, 53/97, 100)

plot(example, type="o", col="red", main="Cadena de Rachas",
     xlab="partidas", ylab="racha de victorias")
