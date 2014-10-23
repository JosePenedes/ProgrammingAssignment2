## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function computes the inverse of the special "matrix"
## It retrieves the inverse from the cache, if the inverse has already been calculated (and the matrix has not changed),

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      ## It checks if the inverse is previously computed
      if (!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      ## If the inverse was not computed, compute it and put it in the cache
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      ## Return the inverse matrix
      inv
}
