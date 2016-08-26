## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly.
## 
## Here are a pair of functions that cache the inverse of a matrix.
##
## To test:
##
## source("cachematrix.R")
## testMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## testMatrix$get() ##this should get a 2x2 matrix
## testMatrix$getsolve() ##NULL the first time
## cacheSolve(testMatrix) ##should return the inverse without any message
## run one more time: cacheSolve(testMatrix) ##should return same results with "getting cached data" message
## testMatrix$getsolve() ##return the same
##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
