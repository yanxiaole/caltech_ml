gen.f <- function() {
    x <- runif(2, min=-1, max=1)
    if (x[1] == x[2])
        return(-x[1], 1, 0)
    y <- runif(2, min=-1, max=1)
    if (y[1] == y[2])
        return(-y[1], 0, 1)
    
    ## ax + y + c = 0
    a <- (y[2] - y[1]) / (x[1] - x[2])
    c <- -y[1] - a * x[1]
    c(1., a, 1)
}

gen.data.set <- function(f, n) {
    X <- matrix(data=runif(n*2, min=-1, max=1), ncol=2)
    colnames(X) <- c("x1", "x2")
    X <- cbind(x0=1., X)
    y <- my.sign(X %*% f)
    data.frame(X, y)
}

my.sign <- function(v) {
    s <- sign(v)
    if (any(s==0)) {
        s[which(s==0)] = 1
    }
    s
}
my.perceptron <- function(data, iter=10, f) {
    X <- as.matrix(data[, -ncol(data)])
    y <- as.matrix(data[, ncol(data)])
    w <- c(0., 0., 0.)
    
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    for (i in 1:iter) {
        #browser(expr = (i>100))
        h <- my.sign(X %*% w)
        if (any(h != y)) {
            idx <- resample(which(h != y), 1)
            w <- w + y[idx] * X[idx, ]
        } else {
            return(c(i, w))
        }
    }
    c(i, w)
}

library('ggplot2')
plot.fun <- function(data, f, w) {
    p <- ggplot(data, aes(x=x1, y=x2, color=factor(y)))
    p + geom_point() + 
        geom_abline(intercept=-f[1]/f[3], slope=-f[2]/f[3]) +
        geom_abline(intercept=-w[1]/w[3], slope=-w[2]/w[3], linetype=2)
}

accurate <- function(f, w) {
    data <- gen.data.set(f, 10000)
    X <- as.matrix(data[, -ncol(data)])
    y <- as.matrix(data[, ncol(data)])
    h <- my.sign(X %*% w)
    
    disagree.p <- length(which(h != y)) / length(y)
}

main.process <- function(n, iter, runs) {
    iters <- 1:runs
    accus <- 1:runs
    for (i in 1:runs) {
        f <- gen.f()
        data <- gen.data.set(f, n)
        ret <- my.perceptron(data, iter, f)
        iters[i] <- ret[1]
        w <- ret[-1]
        accus[i] <- accurate(f, w)
        if (i %% 100 == 0) print(i)
    }
    c(mean(iters), mean(accus))
}