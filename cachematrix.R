
##This function creates a special "matrix" object that can cache its inverse.
## The first function creates a list of functions which are then used in the second function to find out if the inverse has already been set 
## if the inverse variable m is null then it will calculate and print out the inverse

##makeCache matrix takes an inversible matrix as input and produces a matrix of functions for setting and getting the variables needed in cache inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## if the inverse of the matrix x has not already been cached then cacheinverse will calculate and print it out

cacheinverse <- function(makeCacheMatrix(x), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
