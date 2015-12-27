## Assignment 2: Caching the Inverse of a Matrix
## These functions allow the user to store the inverse of a matrix (or matrices)
## in the cache memory, in order to save CPU time.

## creates a matrix object that can cache its own inverse

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


## computes the inverse of matrix X or loads it from cache memory

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv) ## Return the inverse stored by makeCacheMatrix
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv  ## Return a matrix that is the inverse of 'x'
}
