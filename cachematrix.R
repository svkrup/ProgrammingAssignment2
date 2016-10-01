

## makeCacheMatrix() returns a list of functions that either 
##1) gets or sets 'x' in the environment of the makeCacheMatrix function and
##2) gets or sets Inverse of 'x'in the environment of the makeCacheMatrix function


makeCacheMatrix <- function(x = matrix()) {
  ## set the value of inverse(x) to NULL
  Inverse_x<-NULL  
  
  ## Define set(x) function
  set <-function(y=matrix()){
    x<<-y
    Inverse_x<<-NULL  
  }
  
  ## Define get(x) function
  get<-function() x
  
  ## Define set(inverse of x) function
  setInverse<-function(Invx) Inverse_x<<-Invx
  
  ## Define get(inverse of x) function
  getInverse<-function() Inverse_x
  
  
  ## Return list of functions  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## CacheSolve checks if there is a cached value of the inverse of 'x'
## If so, returns cached value and indicates that a cached value is being seen
## If no cached value, computes and sets the inverse of 'x' in parent function
## Assumes that the input matrix is inversible

cacheSolve <- function(x, ...) {
  
  ## Step 1: Get the value of the inverse(x) using the get() of makeCacheMatrix
  Invx<-x$getInverse()
  
  ## Step 2: Check if there is a cached value of the inverse and return if yes
  if (!is.null(Invx)){
    message("Returning cached inverse value")
    return(Invx)
  }
  ## Step 3: If no cached value, compute the inverse of input matrix
  
  data<-x$get()
  
  Invx<-solve(data)
  
  ## Step 4: Set the inverse value IV in the parent function makeCacheMatrix
  x$setInverse(Invx)
  
  Invx
}
