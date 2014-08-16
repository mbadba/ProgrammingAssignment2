## The functions below fulfill the requirements of Programming Assignment 2: Lexical Scoping
## for the R Programming course rprog-006

## Matrix inversion may be computationally-intentsive. In situations where 
## an inverse of a matrix needs to be used repeatedly, there may be a benefit 
## to caching the inverse of a matrix rather than computing it multiple times.

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

## Note, this function assumes that the matrix supplied is always invertible; therefore, please
## ensure that the matrix is invertible prior to invoking this function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
