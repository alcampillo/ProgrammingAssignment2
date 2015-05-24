## These two functions are used to create a special object that stores a square matrix
## and cache's its inverse

## The first function, makeCacheMatrix, is a function that creates a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x # gets the value of the matrix
  setinv <- function(inv) m <<- inv # sets the value of the inverse
  getinv <- function() m # gets the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function, cacheSolve, calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() # we first read if there's any value in the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # if there is, we don't calculate the inverse and return the cached value
  }
  data <- x$get() # if there isn't any cached value, we retrieve the matrix
  m <- solve(data, ...) # and then calculate its inverse using the "solve" function
  x$setinv(m) # we then set the inverse to the calculated value
  m     # and return it   
}