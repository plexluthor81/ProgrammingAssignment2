## Functions for R Programming Week 2 assignment 2. These functions
# cache the computationally expensive matrix inversion operation.

## Write a short comment describing this function
# Following the example in the assignment, we create a 4-element
# vector of getter/setter functions for the data matrix and its
# inverse.

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

## Write a short comment describing this function
# Following the example in the assignment, we check if the cached
# value is null. If not, return the cached value. Otherwise,
# compute the inverse, cache it, and return it.
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

# Testing Code:
m = matrix(c(1,0,0,0,1,0,0,0,1),3,3)
cm <- makeCacheMatrix(m)
cacheSolve(cm)
cacheSolve(cm) # This one should print out the "getting cached data message"