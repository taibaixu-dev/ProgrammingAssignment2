## Put comments here that give an overall description of what your
## functions do

## The cacheable matrix. A matrix and its inverse can be set and get
## from this object.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  ## Sets the matrix.
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  ## Gets the matrix.
  get <- function() m
  ## Sets the inverse of the matrix.
  setinverse <- function(inverse) inv <<- inverse
  ## Gets the inverse of the matrix.
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The function to calculate the inverse of a cacheable matrix.
## If the inverse of the cacheable matrix is present, will return
## the cache, otherwise the inverse will be calculated and saved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  ## Calculates and saves the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
