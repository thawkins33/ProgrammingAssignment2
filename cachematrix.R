## Creates a system for cache matrix inversions after they
##    been solved once.  Saves CPU time used to repeatedly
##    calculate the same inverse.
##    4/22/14 - Tom Hawkins

## makeCacheMatrix() - creates an object that contains the
##    original matrix plus a cached version of it's inverse
##    after the first time it has been calculated.
##    Input - a matrix
##    Output - a list of functions to operate on the object

makeCacheMatrix <- function(x = matrix()) {
        inv1 <- NULL
        set <- function(y) {
                x <<- y
                inv1 <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv1 <<- inv
        getinv <- function() inv1
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Either calculates teh inverse or returns the previously
##    calculated inverse for a CacheMatrix object
##    Input - a CacheMatrix object
##    Output - the inverse of the Cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv1 <- x$getinv()
        if(!is.null(inv1)) {
                message("getting cached data")
                return(inv1)
        }
        data <- x$get()
        inv1 <- solve(data, ...)
        x$setinv(inv1)
        inv1
}
