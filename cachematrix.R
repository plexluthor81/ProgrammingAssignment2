## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Following the example in the assignment, we create a 4-element
# vector of getter/setter functions for the data matrix and its
# inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# Following the example in the assignment, we check if the cached
# value is null. If not, return the cached value. Otherwise,
# compute the inverse, cache it, and return it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}