f1 <- function(x, y) x+y;

CA.simple <- function(x0, p, n) {
    y <- c(0)
    y[1] <- x0
    for (i in 2:n) {
        y[i] <- f1(y[i-1], 2*rbinom(1,1,p)-1)
    }
    plot(y, type="o", col="red", main="Caminata Aleatoria Simple",
         xlab="tiempo", ylab="");
}

f2 <- function(x, y, a, b) {
    if((x+y) > b) {
        c <- b;
    } else {
        if((x+y) < a) {
            a -> c;
        } else {
            c <- x + y;
        }
    }
    return(c);
}

CA.breflej <- function(x0, p, n, a, b) {
    y <- c(0)
    y[1] <- x0
    for (i in 2:n) {
        y[i] <- f2(y[i-1], 2*rbinom(1,1,p)-1, a, b)
    }
    plot(y, type="o", col="blue", main="Caminata aleatoria con barrera reflejante",
         xlab="tiempo", ylab="");
}
