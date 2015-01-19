## two exemplary functions of matrix manipulation
## in order to introduce the concept of caching

## "makeCacheMatrix" is creating a list containing 4 functions to 
##  manipulate a matrix and to get its inverse (if cached before)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(val) inv <<- val
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## "cacheSolve" is calculating and caching the inverse of a matrix

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
