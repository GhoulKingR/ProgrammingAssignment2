## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that allows
## its inverse to be cached with the cacheSolve
## function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function uses caching techniques to
## optimize the process of getting the
## inverse of a matrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
