## Put comments here that give an overall description of what your
## functions do


# example
#(cacheSolve(makeCacheMatrix(matrix(1:4,2,2))))
 #    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
## Cache the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## get the inverse. If the inverse is not null, get the cached data; else it should calculate the 
## the inverse using the sovle function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
