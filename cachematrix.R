## Put comments here that give an overall description of what your
## functions do
#Assignment Week 3: write the following functions: makeCacheMatrix and cacheSolve

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" that can cache its inverse 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<-function() x 
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #This function computes the inverse of a special "matrix" returned by makeCacheMatrix 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}
