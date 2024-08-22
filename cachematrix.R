# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse matrix as NULL
  
  set <- function(y) {
    x <<- y  # Set the value of the matrix
    inv <<- NULL  # Reset the inverse when a new matrix is set
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse matrix
  
  getInverse <- function() inv  # Return the inverse matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse using solve()
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse matrix
}
