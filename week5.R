A <- matrix(1:100, nrow=10)
B <- matrix(1:1000, nrow=10, ncol = 10)

solve(A)
solve(B)

#Transpose A and B  
t(A)
t(B)


#create two vectors (a and b)
a <- c(2:8)
b <- c(1:50)
a 
b

#multiply matrices by vectors with X and Y
Aa <- A * a
Bb <- B * b

#re-assign the vectors a and b to equal the number of rows of the column for the corresponding matrix
a <- c(2:4)
b <- c(3:5)

#Multiply the matrix by a matrix hit %*%
C <- Aa %*% Bb 
C_sq <- C[,1:10]
det(C_sq)
solve(C_sq)
  
#Inverse a matrix 
S <- matrix(3:10, nrow= 3, ncol = 3)
det(S)
solve(S)

#check det(Your turn)
det(S)
