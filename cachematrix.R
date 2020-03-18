## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## set the inverse matrix data in the myInvMat variable
  myInvMat <- NULL
  
  ## to set the matrix
  setMat <- function(y){
    x <<- y
    myInvMat <<- NULL
  }
  
  ## to get the matrix
  getMat <- function() x
  
  ## to set the inverse of matrix
  setInvMat <- function(inv) myInvMat <<- inv
  
  ## to get the inverse of matrix
  getInvMat <- function() myInvMat
  
  list(setMat = setMat, getMat = getMat,
       setInvMat = setInvMat, getInvMat = getInvMat)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## get the cached inverse of matrix
  invMat <- x$getInvMat()
  
  ## check is the inverse was cached previously
  if(!is.null(invMat)){
    message("getting cached inversed matrix")
    return(invMat)
  }
  
  ## get the matrix
  invData <- x$getMat()
  
  ## solve the inverse
  invMat <- solve(invData, ...)
  
  ## cache the inverse
  x$setInvMat(invMat)
  
  invMat
  
}










