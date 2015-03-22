## The functions in this file are designed to create a matrix object
## and cache its inverse.

## The makeCacheMatrix function is a list of functions that:
##      1. sets the values of the matrix
##      2. gets the values of the matrix
##      3. sets the inverse of the matrix
##      4. gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix
## created using the above function. It first checks to see if the
## inverse matrix has already been created. If so, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it 
## calls the solve function to calculate the inverse of the matrix
## and sets this to the cache using the setinverse function

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
