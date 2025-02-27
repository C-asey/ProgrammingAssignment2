## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix is used to create a special matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  
  setinverse <- function(inverse){
    m <<- inverse
  }
  getinverse <- function(){
    m
  }
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve is used to compute the inverse of the special matrix
## that created above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  res <- x$getinverse()
  if(!is.null(res)){
    message("getting cached data")
    return(res)
  }
  data <- x$get()
  res <- solve(x, ...)
  x$setinverse(res)
  res
}
