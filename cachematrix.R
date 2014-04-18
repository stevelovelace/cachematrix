## A facility for caching the inverse of a matrix
    ## the following was a big help understanding scoping:
    ##   http://www.r-bloggers.com/environments-in-r/
    ## as well as the original Ihaka/Gentleman paper, which I found here:
    ##   http://www.tandfonline.com/doi/pdf/10.1080/10618600.1996.10474713

## makeCacheMatrix() takes a matrix as its parameter and returns
## a List of functions for manipulating the matrix and its
## cached inversed, which is set or retrieved by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    ## clear cache and create set/get functions
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) inv <<- xinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve() takes a matrix cache (produced by makeCacheMatrix()) and
## returns either a cached inverse or a newly calculated one

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    message("calculating inverse")
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
