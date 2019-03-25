## The following functions can create a "special" matrix and to calculate the inverse 
## of that "special" matrix. The inverse of the "special" matrix is cached.

## Creates a "special" matrix, which can cache its inverse. Returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(new_inverse) inverse <<- new_inverse
        get_inverse <- function() inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

##Calculates and returns the inverse of a CacheMatrix.
##If the inverse of the matrix is already cached,it returns the cached value.
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached inverse of matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
