## This functions creates a matrix object that can cache his inverse.


## Constractor for Matrix that contains:
##      1.set(y)-constract a CacheMatrix from y.
##      2.get() -  returns the CacheMatrix.
##      3.setinverse(solve) set the value of inverse of the matrix
##      3.getinverse() returns the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        updatedCache <- NULL
       set <- function(y) {
               x <<- y
               updatedCache <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) updatedCache <<- solve(x)
       getinverse <- function() updatedCache
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This fuction check if there is a updated cache of the inverse matrix.
## If it's exist it return the cache, if not it will computes the inverse,
## store it in cache and return the inverse.

cacheSolve <- function(x, ...) {
        updatedCache <- x$getinverse()
        if(!is.null(updatedCache)) {
                message("getting cached inverse")
                return(updatedCache)
        }
        data <- x$get()
        updatedCache <- solve(data, ...)
        x$setinverse(updatedCache)
        updatedCache
        ## Return a matrix that is the inverse of 'x'
}
