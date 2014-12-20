## Caching the inverse of a matrix. 
## Once the inverse is computed it being saved so that when it 
## is needed again it can be looked up instead of recompouted.

## This function creates a matrix that has a set of functions
## to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of the matrix returned by the 
## 'makeCacheMatrix' and saves the inverse into the cache. If the 
## inverse has already been calculated before, it retrieves it 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
