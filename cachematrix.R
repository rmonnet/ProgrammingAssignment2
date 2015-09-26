## These functions provides a matrix object that can cache its inverse.
##
## The first function, makeCacheMatrix, creates the matrix object with its
## associated methods to get/set the matrix and get/set the inverse.
## The second function, cacheSolve, computes the inverse if not already available
## or return the cached value otherwise.
##
## Note: this follows the example for this exercise with a separate cacheSolve
## function but another (better) strategy would be to implement the logic in
## cache solve within the getinv() method. This way there will be no need to manually
## call cacheSolve before getinv(), we could also remove the setinv() from the matrix
## object interface which would prevent users to store an invalid inverse value in the 
## matrix object.

## makeCacheMatrix creates a matrix object with the following methods:
##
## - set, sets or resets the value of the matrix.
## - get, gets the value of the matrix.
## - setinv, set the value of the matrix inverse
## - getinv, get the value of the matrix inverse

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(x) {
    mat <<- x
    inv <<- NULL
  }
  get <- function() {
    mat
  }
  setinv <- function(i) {
    inv <<- i
  }
  getinv <- function() {
    inv
  }
  list(
    set = set, get = get, setinv = setinv, getinv = getinv
  )
}

## cacheSolve returns the value of the matrix inverse.
## If the value has already been computed and the matrix hasn't
## changed it returns the cached value, else it computes and cache the inverse.

cacheSolve <- function(x, ...) {
  res <- x$getinv()
  if (!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  i
}
