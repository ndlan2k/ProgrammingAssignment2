## This contains a pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly 
## For this assignment, assume that the matrix supplied is always invertible.

## This function takes an input matrix and creates a special "matrix" object
## that can cache its inverse. The returned object is really a list containing
## functions to get/set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse matrix
  setinv <- function(inver) inv <<- inver
  # get the value of the inverse matrix
  getinv <- function() inv
  
  # return the  special "matrix" object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves the inverse from the cache,
## without actually doing the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  
  # Compute the inverse of a square invertible matrix
  inv <- solve(matr)
  
  x$setinv(inv)
  
  # Return a matrix that is the inverse of 'x'
  inv
}
