## Cache and compute inverse of a matrix

## "makeCacheMatrix" creates a special "matrix" object 
## that cache its inverse

makeCacheMatrix <- function(matx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    matx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(matx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## "cacheSolve" Computes the inverse of the special matrix
## returned by "makeCacheMatrix"

cacheSolve <- function(matx, ...) {
  inverse <- matx$getinv()
  if(!is.null(inverse)) {
    return(inverse)
  }
  data <- matx$get()
  inverse <- solve(data, ...)
  matx$setinv(inverse)
  return(inverse)
}
