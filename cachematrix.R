## The objective of this script is to cache the inverse of a matrix in a 
## special "matrix" object created by makeCacheMatrix(), and avoid a
## re-calculation of the inverse for the same matrix if it already was 
## calculated, when it is requested through cacheSolve(). 


## makeCacheMatrix creates a special "matrix", which is actually holds a list 
## that holds 4 functions that set and get the value of the stored matrix, 
## and set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a special matrix object created with makeCacheMatrix as 
## the argument, and first checks if the inverse of the matrix is already 
## calculated, if not, calculates it and also puts it in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}