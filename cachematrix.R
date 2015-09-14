## Assignment: Caching the Inverse of a Matrix 
## Computing the Inverse of a matrix can be time consuming.
## These functions can compute the Inverse of a Matrix and store it in a cache for future use.
## Prior to performing the time consuming Inversion calculation, the cache is checked to see if ## it has been already calculated, in which case it is extracted from the cache.
## Only if the cache is empty is the calculation performed
## makecacheMatrix This creates a matrix object that can cache its inverse 

## Input: A Matrix 
## outputs a list object with 4 functions as its elements.
makeCacheMatrix <- function(theMatrix = matrix()) {
    # initialize
    inv <- NULL
    Mat <- NULL
    #Create the "Set" function. This stores bthe Matrix and invalidates any inverse that might be stored.
    set <- function(y) {
        Mat <<- y
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
## When we cache the Inverse, we also Cache the Matrix, using the SET function.
## This ensures the matrix and inverse are a matched pair.
## The output of makeCacheMatrix is the Input to CacheSolve 
## The Output of CachSolve is the Inverse of the initial Matrix
cacheSolve <- function(x, ...) {
    ##Get the inverse. First time thru this is NULL
    iv <- x$getinverse()
    ##Get the Matrix. 
    sm <- x$get() 
    ##Its best to check for NULL first, since that is the initial state and makes for cleaner code!
    if(is.null(iv)) { 
        ##Cache the Matrix
        x$set(sm) 
        ##compute the inverse
        iv <- solve(sm)  
        ##cache the Inverse
        x$setinverse(iv)
    ##We HAVE the inverse cached.
    }else { 
        message("getting cached data")
    } 
    iv
}