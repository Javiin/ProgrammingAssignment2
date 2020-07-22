## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates a matrix object can cache it inverse
makeCacheMatrix <- function(x = matrix()) {
  #invMatrix - inverse matrix value
  invMatrix <- NULL #default value - NULL
  #setMatrix function: matrix and reset invMatrix
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  #getMatrix function: get matrix
  getMatrix <- function() x
  #setInvMatrix function: set inverse matrix
  setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
  #getInvMatrix function: get inverse matrix
  getInvMatrix <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix=getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## Write a short comment describing this function
#cachesolve computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInvMatrix()
 #check if it is not null
  if (!is.null(invMatrix)){
    message("getting cached data")
    return (invMatrix)
  }
  #if it is null, calculate the inverse
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInvMatrix(inv) #save it
  inv
}
