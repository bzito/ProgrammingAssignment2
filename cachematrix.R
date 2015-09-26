## This program caches the inverse of a matrix and then returns the cached
## value.

## makeCacheMatrix is setting up the functions that will be used in the 
##cachesolve fucntion.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
## defines the operations that will take place in cachesolve
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
## list allows us to hold the collection of values for set, get, setsolve, getsolve
}

## The cacheSolve function inverts the matrix and returns the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if (!is.null(m)) {
                message ("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve (data, ...)
        x$setsolve(m)
        m
}