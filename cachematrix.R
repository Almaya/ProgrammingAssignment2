## Assignment 2: Caching the Inverse of a Matrix
## These functions allow the user to store the inverse of a matrix (or matrices)
## in the cache memory, in order to save CPU time.

## creates a matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) { ## lets the user store a matrix y in the object
            x <<- y
            inv <<- NULL
      }
      get <- function() x ## lets the user retrieve the stored matrix
      setinv <- function(inverse) inv <<- inverse
            ## lets the user store the inverse of the matrix in cache memory
      getinv <- function() inv
            ## lets the user retrieve stored inverse
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## computes the inverse of matrix X or loads it from cache memory

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv) 
      } ## returns the inverse matrix if already stored by makeCacheMatrix
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv  ## Return a matrix that is the inverse of 'x'
}
