## These two functions allow (1) creating a special matrix object and 
## (2) computing the inverse of that matrix in a way that can be cached
## for quick retrieval.

## Example:
##   a <- makeCacheMatrix(matrix(rnorm(100), nrow=10, ncol=10))
##   a$get()
##   cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
    # Create a matrix object that can store its own inverse and 
    # retrieve it from a cache
    
    # Initialize the cache to null 
    cache <- NULL
    set <- function(y) {
        # Set the matrix data
        x <<- y
        # Since data is being changed, invalidate the cache
        cache <<- NULL
    }
    get <- function() {
        # Just return the matrix
        return(x)
    } 
    setInverse <- function(theMatrix) {
        # Store the inverse in the cache
        cache <<- theMatrix
    }
    getInverse <- function() {
        # Retrieve the inverse from the cache
        return(cache)
    }
    # Return a list of all members
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'. If the inverse has already been computed,
    # then return it from cache.
    
    # Try to get the cached value
    cache <- x$getInverse()
    if (!is.null(cache)) {
        # Found data in the cache, so return it and let the user know it was cached.
        message("Getting cached data.")
        return(cache)
    }
    # Matrix inverse had not already been computed (or data changed), so 
    # compute the inverse and store it in the cache.
    data <- x$get()
    # Do the actual inversion
    cache <- solve(data)
    # Store it in cache
    x$setInverse(cache)
    return(cache)
    
}

