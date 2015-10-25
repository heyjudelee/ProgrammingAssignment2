## Put comments here that give an overall description of what your
## functions do
##
## This fucntion creates a cache matrix object that can be used to repeatedly solve 
## the inverse of the matrix. In addition it calculates the inverse once.
##
## Usage:
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)
##
## cacheMatrix$set(M)                   # Change the matrix being cached.
## M <- cacheMatrix$get()               # Returns the matrix being cached.
##
## cacheMatrix$setInverse(solve(data, ...))
## cacheMatrix$getInverse()

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get
                setInverse = setInverse,
                getInverse = getInverse)
        
}


## It returns the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invFunc <- x$getInverse()
        if(!is.null(invFunc)) {
         message("getting cached data")
         return(invFunc)
        }
        data <- x$get()
        invFunc <- solve(data, ...)
        x$setInverse(invFunc)
        invFunc
}
