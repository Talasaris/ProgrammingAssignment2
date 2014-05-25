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
      setsolve <- function(solve) s <<- solve # Stores the inverse of the matrix in the cache.
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
      
}


## The function cacheSolve takes a matrix as an argument and checks whether the inverse has already been calculated.
## If so, it returns the result from the cache. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) { ## This checks whether or not there already is an existing solution.
            message("getting cached data")
            return(s)
      }
      data <- x$get() ## If on line 30 there is no already existing solution, or the matrix has been modified, another calculation is started.
      s <- solve(data, ...)
      x$setsolve(s)
      s
      
}
