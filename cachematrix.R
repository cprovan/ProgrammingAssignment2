## Pair of functions to store, compute, and cache the inverse of a nonsingular matrix

## Function makeCacheMatrix(x) creates a list of functions to set and get
##  the value of the matrix x and the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse to NULL
  xinv <- NULL;
  # set function stores matrix as x and reverts inverse to NULL
  set <- function(y) {
    x <<- y;
    xinv <<- NULL;
  }
  # get function returns matrix
  get <- function() x
  # setinv function stores inverse matrix as xinv
  setinv <- function(inv) xinv <<- inv;
  # getinv returns inverse matrix
  getinv <- function() xinv
  # return list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve returns the cached inverse matrix if it exists and computes
##   and caches the inverse matrix otherwise

cacheSolve <- function(x, ...) {
  # get cached inverse
  xinv <- x$getinv();
  # if cached inverse is not NULL then return this matrix
  if(!is.null(xinv)) {
    message("returning cached inverse");
    return(xinv);
  }     
  # if cached inverse is NULL then compute, cache, and return inverse
  xinv <- solve(x$get());
  x$setinv(xinv);
  xinv
}
