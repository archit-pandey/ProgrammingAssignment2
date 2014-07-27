## Put comments here that give an overall description of what your
## functions do

## A function to wrap the matrix in a set of funtions to return inverse
## of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Return data inside the cached list
  get <- function() x
  
  # set the inverse in the m var through <<- operation to be available
  # across environments
  setinverse <- function(inverse) m <<- inverse
  
  #get the stored/cache inverse for the matrix x
  getinverse <- function() m
  
  #list with the implemented functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## A cached version of the solve function that looks up in cache if the
## inverse of the matrix is available; if not compute inverse and 
## add it to cache before returning the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  #var m not null then cache is hit returned from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #retrieve matrix and calculate inverse
  data <- x$get()
  m <- solve(data,...)
  
  #store the inverse
  x$setinverse(m)
  m
}
