f4 <- function(x, y, a) {
    if(x==a)
        return(a)
    else
        return(x+y)
}

CA.1babs <- function(x0, p, n, a) {
    y <- c(0)
    y[1] <- x0
    for(i in 2:n)
        y[i] <- f4(y[i-1],2*rbinom(1,1,p)-1,a)
    plot(y, type="o",main="CA1BAbs")
}

f5 <- function(x, y, a, b) {
    if(x==a) 
        return(a)
    else {
        if(x==b)
            return(b)
        else
            return(x+y)
    }
}


CA.2babs <- function(x0, p, n, a, b) {
    y <- c(0)
    y[1] <- x0
    for(i in 2:n)
        y[i] <- f5(y[i-1], 2*rbinom(1,1,p)-1, a, b)
    plot(y, type="o", main="CA2BAbs")
}

#Ta,-a
Ta <- function(x0, p, n, a) {
    y <- c(0)
    y[1] <- x0
    if (x0==a || x0==-a)
        return(0)
    for(i in 2:n) {
        y[i] <- f5(y[i-1], 2*rbinom(1,1,p)-1, a, -a)
        if (y[i]==a || y[i]==-a)
            return(i)
    }
    return(n)
}
