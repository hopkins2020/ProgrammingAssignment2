## Two functions to cache the inverse of a matrix

## makeCacheMatrix function creates a special matrix that can cashe its inverse.
makeCacheMatrix <- function(m = matrix()) {
  
  #start the inverse property
  i <- NULL 
  
  #1# set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  #2# get the matrix
  get <- function() {
    m
  }
  
  #3# set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #4# get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  #return a list of the previous functions #1:4#
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## this function calculates the inverse of the special matrix that was created
## with the above function (makeCacheMatrix). However, it first checks to see if
## the inverse of the matrix has already been calculated. If the inverse has already
## been calculated and the value of the matrix did not change, then (cacheSolve)
## will retrieve the already chased value of the inverse. Otherwise, it will
## calculate the inverse and sets the value in the cashe.

cacheSolve <- function(x, ...) {
  
  ## Return the inversed matrix of "x"
  m <- x$getInverse() 
  
  ## an if statement to check if the inverse value is already calculated
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix from the object
  data <- x$get()
  
  ## calculating inverse using matrix multiplication 
  m <- solve(data) %*% data
  
  ## set the inverse to the object
  x$setInverse(m)
  
  ## return the matrix
  m
  
}







