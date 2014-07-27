## This file contains two functions makeCacheMatrix and cacheSolve.
## The function makeCacheMatrix caches the inverse of a matrix,gets and
## sets the matrix whose inverse is to be calculated. The function
## cacheSolve calculates the inverse of a matrix, updates the inverse
## of the cache and gets the inverse if already cached

## this function creates a vector which is list containing function to
## get, set value of a matrix and get and set value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverseMatrix <<- solve
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function caclulates the inverse of the matrix created by
## the makeCacheMatrix function. If the inverse has already been calclulated,
## it is fetched from the cache else inverse is calculated and set 
## via setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
