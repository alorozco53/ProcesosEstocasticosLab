## Computes the marginal probability P[Xn = j] of a Markov chain.
## Given a transition matrix, an initial distribution vector,
## a time n, and a state j, the following recursive function
## computes the probability that the chain is in state j at time n.
## NOTE: dim(trans.matrix) == dim(initial.distrib),
## 1 <= j <= dim(trans.matrix).
MarginalProbability <- function(trans.matrix, initial.distrib, n, j) {
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
