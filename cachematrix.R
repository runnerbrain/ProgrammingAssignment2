## This file contains two functin makeCacheMatrix and cacheSolve
## used to load a matrix and calculate its inverse or retrieve it
## from the cache if it had been previously calculated.

## -------------------------------------------------------------------
## function makeCacheMatrix gets passed a matrix as input
## It consitst of four functions which are getters and setters
## of the sqare matrix and its inverse.
## These functions are passed as arguments to a list object.

## parameters :
## x square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(thisinv) inv <<- thisinv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## -------------------------------------------------------------------
## function cacheSolve returns matrix that is the inverse 
## of the square matrix x passed in as the first parameter.
## The function first checks to determine if an inverse already 
## exists for the square matrix passed in. If it does this inverse 
## matrix is returned and the execution of the function is halted.
## If the inverse does not exist it gets caluclated and stored in using 
## one of makeCacheMatrix methods "setinv".
##
## parameters: 
## x  square matrix
## 

cacheSolve <- function(x, ...) {
    ## Get inv using makeCacheMatrix function getInv    
    inv <- x$getinv()
    
    ## if found return it and report that the cached data was returned.
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    
    ## Othewise retrieve the matrix
    passedMatrix <- x$get()
    
    ## find its inverse
    inv <- solve(passedMatrix,...)
    
    ## set the inverse using the setinv function of makeCacheMatrix
    inv <- x$setinv(inv)
    inv
}
