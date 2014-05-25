## This file contains two functions. The first (makeCacheMatrix) creates a matrix object that calculates its inverse
## and stores the result in the cache.
## The second one (cacheSolve) either calculates the inverse of the matrix object from makeCacheMatrix or, if 
## the inverse has already been calculated and the matrix hasnt changed, it retrieves the result from the cache.

## The function makeCacheMatrix gets an invertible matrix as input. The function then calculates the inverse of this
## matrix and stores the result in cache.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
      
}


## The function cacheSolve takes a matrix as an argument and checks whether the inverse has already been calculated.
## If so, it returns the result from the cache. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
      
}
