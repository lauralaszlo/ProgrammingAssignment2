## this is a slightly modified version of the given code of the 
## Coursera R programming course's 2nd Assignement 
## of calculate and cache the mean of a vector

## makeCacheMatrix calculates the inverse of the given matrix 
## and stores it accessibly in case of further use


makeCacheMatrix <- function(x = matrix()) {
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
       getinverse = getinverse)
}


## the cacheSolve function returns the previously calculated inverse of the given matrix 
## OR calculates and stores the inverse in case of it's absence

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}
