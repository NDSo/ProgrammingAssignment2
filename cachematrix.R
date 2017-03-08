## Put comments here that give an overall description of what your
## functions do:
## Creates a matrix object with cache capability for the solve function

## Write a short comment describing this function:
## Creates an object with a list of functions for accessing a matrix and cached return values
## for functions on that matrix ('Solve' being the only function implemented)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invers) inv <<- invers
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function:
## Creates a 'solve' function that can access cached 'solve' return values from the input CacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
