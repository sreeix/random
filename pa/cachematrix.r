# Implements a cached matrix inversion.
# Usage
# p <- matrix(rnorm(100), 10, 10)
# cm <- makeCacheMatrix(p)
# cacheSolve(cm)
# 

## Creates a wrapper for a matrix and exports a list that ha
# a getter/setter for the original matrix and a getter/setter for the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() { 
        x
    }
    setInverse <- function(inverse) { 
        invX <<- inverse
    }
    getInverse <- function() {
        invX
    }
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Checks if the Cache Exists and if it does not computes the inverse and
# stashes it so that it can be retrived later and returns the inverse

cacheSolve <- function(x, ...) {
    invX <- x$getInverse()
    if (!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    data <- x$get()
    invX <- solve(data, ...)
    x$setInverse(invX)
    invX
}