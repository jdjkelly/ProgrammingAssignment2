## A pair of functions to create cache-able inverse matricies

## Creates a cached matrix wrapper. Accepts a matrix as an argument. 
## Has functions for setting/getting the underlying matrix, and 
## functions for setting/getting the matrix' inverse. Returns a list
## of its available functions.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
        setSolve = setSolve,
        getSolve = getSolve)
}


## Caches solved matrices. Accepts an instance of `makeCacheMatrix` as
## an argument, determines if it contains a cached solved value, returns
## it if it does, otherwise solving/storing it, and returning the newly
## cached value.

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached inverse")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
