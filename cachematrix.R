##The following functions aim to compute the inverse of a matrix 
# reducing the time of computation by retrieving the inverse from 
# the cache when it has been already computed. 

##The first function makeCacheMatrix is a function that creates a special "matrix"
# object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function() inv 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve is a function that computes the inverse of the special
# "matrix created by the makeCacheMatrix function above. If the inverse
# has already been calculated, then cacheSolve returns the inverse from
# the cache, otherwise it computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##Example
mat <- matrix(c(1:4), 2, 2)
mat1 <- makeCacheMatrix(mat)
mat2 <- cacheSolve(mat1)
mat2
