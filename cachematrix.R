## This function will convert a matrix into a matrix that can be cached and used
## in the cacheSolve function
## e.g. a = matrix(1:9, nrow = 3, ncol = 3)
##      b = makeCacheMatrix(a)

## Converts a matrix to a cachable matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will return the inverse of the matrix x
## it will return the cached result if already calculated earlier
## e.g. result = cacheSolve(b), where b = makeCacheMatrix(a) and 'a' is a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
          
          message("getting cached data")
    return(m)
  }
  data <- x$get()
  print(data)
  m <- solve(data)
  x$setInverse(m)
  m
}
