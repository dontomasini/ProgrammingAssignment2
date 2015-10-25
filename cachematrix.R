## Matrix inverse will be cached as recomputing it is costly

## Write a short comment describing this function
## Matrix object is created that can cache the inverse

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

## cacheSolve calculates the inverse of the Matrix created in the function makeCacheMatrix.
## If inverse has already been calculated it is not recalculated but retrrieved from the cache

cacheSolve <- function(x, ...) {
  ## Returns inverse matrix 
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("cached data is used")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
