## R Programming Assignment 2: Caching the inverse of a matrix

## makeCacheMatrix creates a matrix that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve calculates the matrix's inverse
## A condition checks the inverse for a value
## If there is a value, the inverse is retrieved from the cache
## If there is no value, a message is generated while the value is being retrieved

cacheSolve <- function(x, ...) {
  z<- x$getinv()
  if (!is.null(z)){
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setinv(z)
  inv_result
}

