## These two functions will collectively check if the inverse of the matrix is stored
## in cache and if not then calculate and store it

## This function will reset the value of "m", which can be different from null if the functions have run prviously.
## It will then define the functions that recalls the inverse from cache and another that stores it.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, 
       setsolve=setsolve,
       getsolve=getsolve)
}
## This function will check if the result is saved in the global invironment and return it if that is the case
## If not it will invert the matrix and both save and print the result in "m".
cacheSolve <- function(x, ...) {
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
## Returns a matrix that is the inverse of 'x'
