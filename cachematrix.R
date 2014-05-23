##  If the contents of a vector are not changing, the value of the mean can be 
##  cache so that when it is needed again, it can be looked up in the cache rather
##  than recomputing it

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(b = numeric()) {
   v <- NULL
  set <- function(y) {
    b <<- y
    v <<- NULL
  }
  get <- function() b
  setsolve <- function(solve) v <<- solve
  getsolve <- function() v
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  }

## This function computes the inverse of the special "matrix" 
cacheSolve <- function(b, ...) {
  ## Return a matrix that is the inverse of 'b'
   v <- b$getsolve()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- b$get()
  v <- solve(data, ...)
  b$setsolve(v)
  v
 }
