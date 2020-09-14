## makeCacheMatrix and cacheSolve functions pair calculate and cache the inverse of a matrix

## creates a special "matrix" object that can cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL 
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(solve) i <<- solve
  
  # get the value of the inverse
  getinverse <- function() i
  
  list(set = set
       , get = get
       , setinverse = setinverse
       , getinverse = getinverse)
}

## Returns the inverse of x
## If inverse is already calculated, retrieve from cache
## else, calculate inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get inverse of x from cache
  i <- x$getinverse()
  
  # if retrieved from cache, return inverse
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # else calculate and return inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
}
