## These functions allow the user to create a function that stores an 
## inversible matrix and its inverse. cacheSolve is a function that acts
## on a makeCacheMatrix object to find the inverse of an object and cache
## it for future retrieval.

## Creates a makeCacheMatrix object, which stores an inversible matrix
## (provided as an argument when calling the function) and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # The cached inverse
    inv <- NULL
    
    # Modifies the original matrix (x) and removes any old cached inverses
    modify <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Returns the original matrix (x)
    get <- function() x
    
    # Sets the inverse of x equal to the supplied matrix x_solved
    set_inverse <- function(x.solved) inv <<- x.solved
    
    # Returns the cached inverse
    get_inverse <- function() inv
    
    # The functions that can be used on a makeCachedMatrix object
    list(
        modify = modify,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


## A function that finds the inverse of a matrix stored in a
## makeCacheMatrix object, and caches the inverse back in the
## mCM object. If the inverse is already cached, it simply 
## retrieves the cached inverse.

cacheSolve <- function(x, ...) {
    ## Retrieves cached inverse from x
    inv <- x$get_inverse()
    
    ## If cached inverse already exists, return inverse
    if(!is.null(inv)) {
        message("retrieving cached inverse")
        return(inv)
    }
    
    ## If cached inverse does not already exist,
    ## retrieve original matrix, invert it, store the
    ## inverse back in x, and return the inverse
    mat <- x$get()
    inv <- solve(mat)
    x$set_inverse(inv)
    inv
}
