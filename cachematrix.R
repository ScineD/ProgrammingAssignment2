## The two functions created in this assignment allow to cache the inverse 
## of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        getinverted <- function() inverted
        setinverted <- function(inverse) inverted <<- inverse
        list(get = get,
             set = set,
             getinverted = getinverted,
             setinverted = setinverted)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverted <- x$getinverted()
        if (!is.null(inverted)) {
                message("getting cached matrix")
                return(inverted)
        }
        matr <- x$get()
        inverted <- solve(matr, ...)
        x$setinverted(inverted)
        inverted
}

## Return a matrix that is the inverse of 'x'

source("cachematrix.R")
my_matrix<-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
my_matrix$get()
my_matrix$getinverted
cacheSolve(my_matrix)
