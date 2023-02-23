A = matrix(c(2,0,1,3), ncol=2) 
B = matrix(c(5,2,4,-1), ncol=2)
A + B
A - B
x <- c(4,1,2,3)
diag(x)

d <- diag(3, 5)
d
d <- sweep(d,1,c(1, 0, 0, 0, 0), "+")
d
d <- sweep(d,2,c(2, 0, 0, 0, 0), "+")
d
d[1,1] <- 3
d




