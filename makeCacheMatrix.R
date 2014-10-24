##This assignment is to write a pair of functions that cache the inverse of a matrix.

##This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
        x <<- y
        m <<- NULL
}
get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() i

list(set=set, get=get,
        setinverse=setinverse,
        getinverse=getinverse )
}
## This function computes the inverse of a special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
##  should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if (!isnull(i)) {
                message("getting cached data")
                return(i)
                }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
