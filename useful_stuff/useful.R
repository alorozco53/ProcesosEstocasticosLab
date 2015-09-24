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

## Checks if i ⮩ j.
Leads <- function(trans.matrix, i, j) {
    
    ## Auxiliar function
    LeadsAux <- function(t.matrix, x, y, prev.states) {
        if (t.matrix[x,y] > 0)
            return(TRUE)
        else {
            for (k in 1:dim(t.matrix)[1]) {
                if (!(k %in% prev.states) && t.matrix[x,k] > 0) {
                    if (LeadsAux(t.matrix, k, y, c(prev.states, x)))
                        return(TRUE)
                }
            }
            return(FALSE)
        }
    }
    
    if (trans.matrix[i,j] > 0)
        return(TRUE)
    else
        return(LeadsAux(trans.matrix, i, j, c(i)))
}

## Checks if two states communicate each other
Communicate <- function(trans.matrix, i, j) {
    return(Leads(trans.matrix, i, j) && Leads(trans.matrix, j, i))
}

## Computes the state classification matrix.
## trans.matrix[i,j] == '+' iff i ⮩ j
StateClassifMatrix <- function(trans.matrix) {
    classif.matrix <- array(c('0'), dim=dim(trans.matrix))
    for (i in 1:dim(trans.matrix)[1]) {
        for (j in 1:dim(trans.matrix)[2]) {
            if (Leads(trans.matrix, i, j))
                classif.matrix[i,j] <- '+'
        }
    }
    return(classif.matrix)
}

## Computes the probabilities of, starting at a given state i,
## ever arrive to any of the states k (ρik).
## For now, it is required to give Ct, the set of transient
## states.
## Rho <- function(trans.matrix, i) {
##     A <- array(c(0), dim=dim(trans.matrix))
##     b <- matrix(c(0), dim(trans.matrix)[1], 1)
##     for (k in 1:dim(b)[1]) {
##         b[k] <- -trans.matrix[i,k]
##         for (j in 1:dim(A)[1]) {
##             if (j != k)
##                 A[i,j] <- trans.matrix[i,j]
##        }
##     }
##     print(A)
##     print(b)
## }
