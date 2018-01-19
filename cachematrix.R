## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    getmatrix <- function() x
    
    setinverse <- function(solve) inv <<- solve
    
    getinverse <- function() inv
    
    list(
        setmatrix = setmatrix,
        getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## Returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("retrieving cached inverse")
        return(inv)
    }
    mat <- x$getmatrix()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}
