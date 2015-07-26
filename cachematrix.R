## This script contains a pair of functions that cache the inverse of a matrix

## The "makeCacheMatrix" function creates a special "matrix" object
## that stores values in order to cache its inverse.
## The Output for the "makeCacheMatrix" function is a List of 4 functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(givenmatrix) {
    ## Sets the value to the given matrix for which 
    ## the inverse is to be computed
    x <<- givenmatrix
    i <<- NULL
  }
  get <- function() x
  ## Returns the Set matrix value
  
  setinv <- function(givenmatrixinv) i <<- givenmatrixinv
  ## Sets the value to the given matrix inverse
  getinv <- function() i
  ## Returns the Set matrix inverse value
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  ## Returns a List containing the 4 above-defined functions
}


## The "cacheSolve" function computes the inverse of
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the "cacheSolve" function should 
## retrieve the inverse from the cache.

cacheSolve <- function(specialmatrix, ...) {
  ## The input "specialmatrix" is a value that is to be obtained 
  ## from the "makeCacheMatrix" function and will be the List of 4 functions.
  ## i.e., if "x" is the matrix for which the inverse is to be computed,
  ## specialmatrix should be "makeCacheMatrix(x)"
  
  i <- specialmatrix$getinv()
  if(!is.null(i)) {
    message("Retrieving the cached matrix data")
    return(i)
  }
  data <- specialmatrix$get()
  i <- solve(data, ...)
  specialmatrix$setinv(i)
  i
}