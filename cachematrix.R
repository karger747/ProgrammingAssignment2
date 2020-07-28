## Given that inverting a matrix can be costly in terms of computation
## which can slow down execution, pair of functions below enable inverse
#to be retrieved if it already exists
#NB we assume that inputted matrix is not singular
#should really include case where input is singular

## this first function creates a matrix object, that stores its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function finds inverse of object matrix returned by function above
# If inverse has already been computed then it is retrieved from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
#code below tests functions above
mat <- matrix(c(1,2,3,4),nrow = 2)

a <- makeCacheMatrix(mat)
b <- makeCacheMatrix(mat)

cacheSolve(a)
cacheSolve(a)

cacheSolve(b)
cacheSolve(b)
