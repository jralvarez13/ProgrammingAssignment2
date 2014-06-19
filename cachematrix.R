## These functions demonstrate how to improve performance by
## caching data when calling a function multiple times
##
## When cachesolve is called more than one with the same data,
## operations are not performed again and only the results
## are returned

## makeCacheMatrix sets the data and the function to be
## executed, in this case matrix inversion with 'solve()'
##
## A good way to test this is by creating the proper
## square matrix like this:
##
##		hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##		M <- hilbert(4)
##		solve(M)
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns the inverse of a square matrix
## and caches the results when called repeatedly

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
