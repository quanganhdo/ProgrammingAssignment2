## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. These functions help cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(matrix) m <<- matrix
  getinversematrix <- function() m
  list(set = set, get = get, 
       setinversematrix = setinversematrix, 
       getinversematrix = getinversematrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## Retrieve the inverse from the cache if possible.

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if (!is.null(m)) {
    message("Getting inverse matrix from cache")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  a$setinversematrix(m);
  m
}
