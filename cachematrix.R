## R Programming, Programming Assignment 2

## makeCacheMatrix calculates the inverse of the matrix and cahces it when cacheSolve 
## is called. cacheSolve will either ask makeCacheMatrix to calculate the inverse of 
## the matrix, or it will access the cahced inverse if the inverse is already calculated.

## makeCacheMatrix takes a matrix as an argument initialized as x. The matrix must be 
## invertible for the function to be carried out properly.

## m is initiallized as NULL and function(y) lexically returns m to NULL if a new argument
## is passed to makeCacheMatrix. Once invoked makeCacheMatrix inverts the matrix using the 
## solve() function and lexically stores the inverse in m.

## A named list of arguments allows users and other functions to call parts of the function.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInvert <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInvert = setInvert,
           getInverse = getInverse)
}


## cacheSolve passess arguments to makeCacheMatrix to return the inverse of a matrix.
## If makeCacheMatrix already has the inverse of the current matrix cached, cacheSolve
## returns the cached value. If the inverse of the current matrix is not cached, cacheSolve
## has makeCacheMatrix solve for the inverse of the matrix and returns the value.

## This prevents calculating the inverse of the matrix more than once.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInvert(m)
      m
}
