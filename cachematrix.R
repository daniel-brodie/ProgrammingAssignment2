## Creates a cacheable function to return the inverse of a matrix, 
## The functions cache the result of the inverse of the matrix to improve performance

## makeCacheMatrix creates the underlying framework to store the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list (set = set, get=get, setMatrix = setMatrix, getMatrix=getMatrix)
  
}


## Returns the inverse of the provided matrix, allowing for cacheing of the solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if (!is.null(m)) {
    ##if the matirx has already been cached, return that
    message ("getting cached matrix")
    return(m)
  }
  data <- x$get()
  ## calculate the inverse
  m <- solve(x)
  ## store the inverse in the cache
  x$setMatrix(m)
  m
}
