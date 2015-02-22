## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setInverted <- function(newInverted) inverted <<- newInverted
        getInverted <- function() inverted
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getInverted()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data, ...)
        x$setInverted(inverted)
        inverted
}