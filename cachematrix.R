## Assignment: Caching the Inverse of a Matrix
## Computing the Inverse of a matrix can be time consuming.
## These functions can compute the Inverse of a Matrix and store it in a cache for future use.
## Prior to performing the time consuming Inversion calculation, the cache is checked to see if
## it has been already calculated, in which case it is extracted from the cache.
## Only if the cache is empty is the calculation performed

## makecacheMatrix This creates a matrix object that can cache its inverse
## Input: A Matrix
## outputs a list object with 4 functions as its elements.

makeCacheMatrix <- function(theMatrix = matrix()) {
    # initialize 
    inv <- NULL
    
    #Create the "Set" function
    set <- function(y) {
        theMatrix <<- y
        inv <<- NULL
    }
    
    #Create the "Get" function
    get <- function() theMatrix
    
    setinverse <- function(iv) inv <<- iv
    
    getinverse <- function() inv
    
    #Create the "special matrix" list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    

}


## CacheSolve can calculate the matrix inverse - but will only do that if it is not already cached.
## If already cached it returns it from the cache.
## The output of makeCacheMatrix is the Input to CacheSolve
## The Output of CachSolve is the Inverse of the initial Matrix

cacheSolve <- function(x, ...) {
    
    iv <- x$getinverse()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data)
    x$setinverse(iv)
    iv
}
