# These functions are used to cache the result of calculating the inverse of a
# square invertible matrix. Use function makeCacheMatrix to wrap your matrix and
# function cacheSolve to retrieve the cached inverse if available, or the
# calculated inverse otherwise.


# This function wraps a matrix in order to make it cachaeble. It takes a matrix
# as argument and returns a list of functions to interact with the matrix and
# the cache.
makeCacheMatrix <- function(x = matrix()) {
    # initiate cache
    cache <- NULL
    
    # function to get the wrapped matrix
    get <- function() x
    
    # function to set the wrapped matrix
    set <- function(data) {
        x <<- data
        cache <<- NULL
    }
    
    # function to get the cached data
    getcache <- function() cache
    
    # function to set the cached data
    setcache <- function(data) {
        cache <<- data
    }
    
    # return list
    list(
        get = get,
        set = set,
        getcache = getcache,
        setcache = setcache
     )
}


# This function return the cached inverse of the warpped matrix provided. If no
# cache is available, inverse is calculated, stored in cache and returned.
cacheSolve <- function(x, ...) {
    # look up cached inverse
    inverse <- x$getcache()
    
    # return cached inverse if available
    if (!is.null(inverse)) {
        message("returning cached inverse")
        return(inverse)
    }
    
    # get data and calculate inverse
    data <- x$get()
    inverse <- solve(data, ...)
    
    # store calculated inverse in cache
    x$setcache(inverse)
    
    # return calculated inverse
    message("returning calculated inverse")
    inverse
}

# Example usage: (uncomment lines below):
#
# cachematrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# print(cacheSolve(cachematrix))
# print(cacheSolve(cachematrix))
# 
# cachematrix$set(matrix(5:8, 2, 2))
# print(cacheSolve(cachematrix))
# print(cacheSolve(cachematrix))
