## Author: Ron Katriel

## The following pair of functions cache the inverse of a matrix to optimize the computation:
## 1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## 2. cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  ## Arguments: x is a square (invertible) matrix
  ## Creates a special "matrix", which is really a list containing functions to
  ## 1. set the value of the vector
  ## 2. get the value of the vector
  ## 3. set the value of the inverse
  ## 4. get the value of the inverse

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Arguments: x is a special "matrix" (created with makeCacheMatrix)
  ## Calculates the inverse of the special "matrix" x by
  ## First checking to see if the inverse has already been calculated:
  ##   If so, gets the inverse from the cache (via getinv) and skips the computation;
  ##   Otherwise, calculates the inverse of the data and sets the inverse in the cache (via setinv)

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
