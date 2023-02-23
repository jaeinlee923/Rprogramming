# Create an  S3 object
b <- list(first = "Flour", second="Water", third="Yeast", hydration = 75) 

b

# Test if it is an S4 object 
isS4(b)
# Check the type
typeof(b)
class(b)
str(b)

# set class
setClass("Ingredients", slots = list(first = "character", second = "character", third = "character", hydration = "numeric"))

# create an S4 object
baguette <- new("Ingredients", first="Flour", second="Water", third="Yeast", hydration = 75)

baguette

# Test if it is an S4 object 
isS4(baguette)
# Check the type
typeof(baguette)
class(baguette)
str(baguette)

# UseMethod
doctor <- function(l,h,n) {
  value <- list(lastName = l, hospital = h, network = n)     
  attr(value, "class") = "doctor"
  value
}

info <- function(obj) {
  UseMethod("info")
}

info.default <- function(obj) {
  cat("This is a generic function\n")
}

info.doctor <- function(obj) {
  if(obj$n == "United Health" || obj$n == "MediK") {
    cat("Doctor is in network.")
  } else {
    stop("Doctor is not in network.")
  }
}

j <- doctor("Johnson", "Meridian Hospital", "MediK")
info.doctor(j)
k <- doctor("Kirkland", "SouthCrest Hospital", "NHC")
info.doctor(k)

# SetGeneric 
setGeneric("multiply", function(x,y) standardGeneric("multiply"))
setMethod("multiply", signature("matrix"), function(x,y) "matrix")
setMethod("multiply", signature("matrix"), function(x,y) {x * y})
x <- matrix(1:9, nrow=3, ncol=3, byrow=TRUE)
y <- matrix(2:10, nrow=3, ncol=3, byrow=TRUE)
multiply(x,y)
