## The functions are implemented to provide caching of the inverse of a matrix
## to prevent repeated computations of inverse operation on a matrix

## This function, makeCacheMatrix creates a special matrix, which is really a list 
## containing functions to

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inv <<- inverse_matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
