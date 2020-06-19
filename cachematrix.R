#version.string R version 4.0.0 (2020-04-24)

# Caching the Inverse of a Matrix

# Createa s "matrix" object that can cache its inverse
makeCacheMatrix <- function (x = matrix()) {
  i <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the Inverse Matrix
  setinverse <- function(inverse) i <<- inverse
  
  # Get the value of the Inverse Matrix
  getinverse <- function() i
  list(set=set,get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}

#Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
#x <- makeCacheMatrix()
#cacheSolve(x)

#Example
#> x<- c(1,0,0)
#> y<- c(2,4,0)
#> z<- c(3,5,6)
#> cbind(x,y,z)
#x y z
#[1,] 1 2 3
#[2,] 0 4 5
#[3,] 0 0 6
#> a <- makeCacheMatrix(cbind(x,y,z))
#> cacheSolve(a)
#[,1]  [,2]        [,3]
#x    1 -0.50 -0.08333333
#y    0  0.25 -0.20833333
#z    0  0.00  0.16666667

