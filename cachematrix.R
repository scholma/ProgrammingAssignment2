## Put comments here that give an overall description of what your
## functions do

## makecachematrix creates a "vector", 
## which is a list containing a function to
## 
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse
## get the value of the inverse


makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}


## cachesolve calculates the inverse of a matrix created with the above function. 
## however, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function.

cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
