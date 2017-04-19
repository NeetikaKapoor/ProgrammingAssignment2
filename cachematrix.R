## Cache inverse of a matrix so it need not be calculated repeatedly

## Create a matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute inverse once else retrieve from cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("cached data available")
    return(inv)
  }
  my_mat <- x$get()
  inv <- solve(my_mat, ...)
  x$setInverse(inv)
  inv
}