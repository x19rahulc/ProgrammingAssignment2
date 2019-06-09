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

## check output
## mat<- matrix(rnorm(25),5,5)
## interm <- makeCacheMatrix(mat)
## cacheInverse(interm)
##[,1]       [,2]        [,3]       [,4]       [,5]
##[1,]  0.2069929  1.8266104  0.77566665 -2.2466238 -0.3316711
##[2,] -0.7319167 -0.9466429  0.03012671  1.3455002  0.3656258
##[3,]  0.1623671 -0.1682259  0.52507269 -0.4668035 -0.2002680
##[4,] -0.5227514 -1.4962771 -0.13521622  1.0008528  0.2080054
##[5,]  0.1857252  0.8352801  0.30440813 -0.7227350  0.2793849
