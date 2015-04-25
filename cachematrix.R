## makeCacheMatrix and cacheSolve solve the inverse
## of an invertible matrix, store the inverse in cache and 
## return the cached matrix when the inverse is requested again.


## makeCacheMatrix creates a list of functions which allow
## the storage of the inverse of a matrix in cache. X
## and s are defined so that they can be accessed outside of
## the function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInv <- function(solve) s <<- solve
        getInv <- function() s
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve checks to see if the inverse of a requested matrix
## is stored in cache.  If it is, it retrieves it.  If it is not,
## it computes it and then calls on the setInv() function from the
## list created by makeCacheMatrix to cache the result.  In either
## circumstance, it returns the matrix's inverse.
cacheSolve <- function(x, ...) {
        s <- x$getInv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInv(s)
        s
}
