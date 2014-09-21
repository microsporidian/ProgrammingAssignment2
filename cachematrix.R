
## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function()m
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
    
}

##cacheSolve checks whether the inverse of x has been solved and cached
##if yes - returns the cached result, if no - solves the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inv")
        return(m)
        
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    return(m)
    
}
