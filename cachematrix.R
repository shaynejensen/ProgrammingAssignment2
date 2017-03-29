## The following two functions can be used to produce an object that
## can take its own inverse and cache the inverse.


## makeCacheMatrix (x = matrix()) 
## Input: an invertible matrix
## Output: a CacheMatrix object that includes methods for changing the 
## cached matrix and the cached inverse of the matrix

## METHODS:
## set(x=matrix()) stores a matrix

## get() returns the cached matrix 

## setinverse() calculates the inverse of the stored matrix and caches the result

## getinverse() returns the cached inverse

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    
    set <- function (m) {
        m <<- m
        i <<- NULL
    }
    
    get <- function() m
    
    setinverse <- function(z) {
        i <<- z
    }
    
    getinverse <- function () i
    
    list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## CacheSolve: calculates and caches the inverse of a CacheMatrix" object 
## Inputs: x = invertible matrix wrapped in a CacheMatrix object
## Output: m = altered copy of the input CacheMatrix object containing cached inverse

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- m$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- m$get()
    i <- solve(data, ...)
    m$setinverse(i)
    i
}


