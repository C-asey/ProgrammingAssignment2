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
  m <- x$getinverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  det <- det(x)
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      if((i%%2 == 0 && j%%2 == 1)||(i%%2 == 1 && j%%2 == 0)){
        x[i,j] <- -x[i,j]
      }
      temp1 <- x[i,j];
      x[i,j] <- x[j,i]
      x[j,i] <- temp1
    }
  }
  x <- x/det
}
