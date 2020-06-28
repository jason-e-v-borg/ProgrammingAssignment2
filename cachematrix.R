## R Programming Assignment 2: Caching the inverse of a matrix

## The makeCacheMatrix command creates a custom matrix.
## The matrix is a list that holds a function that
## defines and gets the value of the matrix
## and sets and gets the inverse value of the matrixx


makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the matrix that was created with the makeCacheMatrix function.
## cacheSolve first checks to see if the inverse has been calculated.
## If it has, the inverse is gained from the cache, skipping the process
## If not, it calculates the matrix's inverse and sets its value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  ## Return the inverse of a matrix, which is 'z'
  z <- x$getinverse()
  if(!is.null(z)) {
    message("wait, compiling cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
