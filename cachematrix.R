## -------------------------------------------------------------
## Programming assignment 2: Caching the Inverse of a Matrix
##
## This program contains 2 functions and applies the given example
## to the inversion of matrices
## -------------------------------------------------------------

## This function creates a list with functions to access the caching key and
## the cached value. The function operates on a given matrix mtx.
makeCacheMatrix <- function(mtx = matrix()) {
  cachedValue <- NULL
  list(set = function(newMtx) {mtx <<- newMtx; cachedValue <<- NULL}, 
       get = function() {mtx},
       setsolve = function(solve) {cachedValue <<- solve},
       getsolve = function(){cachedValue}
  )
}


## This function retrieves a list-object, which was necessarily created by the
## function makeCacheMatrix and returns the result of the matrix inversion. The
## matrix inversion itself is executed only once.
cacheSolve <- function(x, ...) {
  result <- x$getsolve()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  result <- solve(x$get(), ...)
  x$setsolve(result)
  result
}
