## This function calculates and stores the value of an inversed matrix and makes it available for later use  
## without rerunning the function

## makeCacheMatrix creates a vector containing a function to set and get the value of the vector as well as
## set and get the value of the vector.  It stores the object's value in an environment different from the current one
## for later use.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setsolve <- function(solve) m <<- solve
  getsolve <- function () m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve checks to see if a matrix that is the inverse of 'x' has already been calculated with makeCacheMatrix.
## If so, cacheSolve returns the value from makeCacheMatrix. If not,  cacheSolve calculates and return a matrix that 
## is the inverse of 'x'.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data,...)
  x$setmean(m)
  m
}