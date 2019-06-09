## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    inv_x <<- inverse
  }
  getinverse <- function()    inv_x
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
 ## creates a matrix for storing the inverse
 ## assumed that the matrix input is always inverible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if value of inverse not null in cache, return from cache
        ## else compute and return
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  matrix <- x$get()
  inv_x <- solve(matrix)
  x$setinverse(inv_x)
  inv_x
  
}
