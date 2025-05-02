## MAKECACHEMATRIX

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the matrix
    set <- function(matrix) {
        x <<- matrix
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getInverse <- function() inv
    
    # Return list of functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## CACHESOLVE

cacheSolve <- function(x, ...) {
    # Get cached inverse
    inv <- x$getInverse()
    
    # If inverse is cached, return it with a message
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If not cached, calculate inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    inv
}
