## The following pair of functions can be used to 
## store a matrix and cache its inverse.

## makeCacheMatrix is a function  that creates a special 
## "matrix" object that can cache its inverse
## It contains functions to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL  
  setM<-function(y){   
    x<<-y
    invm<<-NULL
  }
  getM<-function()x   
  setinverse<-function(solve)invm<<-solve
  getinverse<-function()invm
  list(setM=setM, getM=getM, setinverse=setinverse, getinverse=getinverse)
}


## The following function computes the inverse of the matrix created with the 
## makeCacheMatrix function above.
## If the inverse has already been computed (and the matrix has not changed), 
## then the `cacheSolve` should retrieve the inverse from the cache.
## Otherwise, if the cached value does not exist, the inverse is computed and cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm<-x$getinverse()
  ## Return the cached value if it exists
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  } 
  ## If cached value does not exist
  ## compute the inverse and cache it
  data<-x$getM()
  invm<-solve(data, ...)
  x$setinverse(invm)
  ## Return the inverse
  invm
}
