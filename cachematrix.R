## cachematrix.R
##
## This file contains two functions for Programming Assignment 2:
##
##  - makeCacheMatrix: Returns a 'cached matrix' represented as a list of
##                     methods. Caches the original matrix and its
##
##  - cacheSolve:      Returns the inverse of the 'cached matrix' created
##                     with makeCacheMatrix


## makeCacheMatrix(x = matrix(), ...)
##
## Creates a cached matrix from matrix x that caches x and can cache
## the inverse of x.
##
## Arguments
## x - matrix (assumed to be invertible)
##
## Returns cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(x, ...)
##
## Computes the inverse of the cached matrix x returned by makeCacheMatrix.
## If this function is called for the first time on a cached matrix x,
## it will compute and cache the inverse of the matrix in x.
##
## Arguments
## x - cached matrix returned by makeCacheMatrix
##
## Returns inverse of the special matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    inverse
}
