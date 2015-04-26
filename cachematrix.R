
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
## This function will return the Inverse of a matrix by checking first if it has been already calculated and stored.
## if yes it will return from the cache else it will compute the inverse, store it in the cache for next time and return it .

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return (inv)
  } 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
