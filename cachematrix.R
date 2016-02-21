
## My functions here can cache the inverse of a matrix rather than recompute it.

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse,
## which contains setting the matrix, getting the matrix, setting the inverse of the matrix and
## getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## The following function returns the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the
## inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}




