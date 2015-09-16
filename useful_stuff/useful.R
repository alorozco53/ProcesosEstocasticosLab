## Computes the marginal probability P[Xn = j] of a Markov chain.
## Given a transition matrix, an initial distribution vector,
## a time n, and a state j, the following recursive function
## computes the probability that the chain is in state j at time n.
## NOTE: dim(trans.matrix) == dim(initial.distrib),
## 1 <= j <= dim(trans.matrix).
MarginalProbability <- function(trans.matrix, initial.distrib, n, j) {
    if (n == 0)
        return(initial.distrib[j])
    probab <- 0
    if (n == 1) {
        for(i in 1:dim(trans.matrix)[1]) {
            probab <- probab + (trans.matrix[i,j] * initial.distrib[i])
        }
        return(probab)
    }
    for (i in 1:dim(trans.matrix)[1]) {
        probab <- probab + (trans.matrix[i,j] * MarginalProbability(trans.matrix,
                                                                    initial.distrib,
                                                                    n-1,
                                                                    i))
    }
    return(probab)
}

## Computes the m-step transition probability (P^m)i,j
## via the Chapman-Kolmogorov equations, beginning at time n.
MStep <- function(trans.matrix, n, m, i, j) {
    if (n+m == 0) {
        if (i == j)
            return(1)
        else
            return(0)
    }
    if (n+m == 1)
        return(trans.matrix[i,j])
    probab <- 0
    for (k in 1:dim(trans.matrix)[1]) {
        n.step <- MStep(trans.matrix, n-1, 1, i, k)
        m.step <- MStep(trans.matrix, m-1, 1, k, j)
        probab <- probab + (n.step * m.step)
    }
    return(probab)
}
