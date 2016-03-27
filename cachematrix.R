## Below methods will be used to cache a matrix inverse, as cost to
## create the matrix inverse is high. To cache the mechanism to improve
## the programming performance.

## The method makeCacheMatrix is used to set and get the cache
## inverse matrix

makeCacheMatrix <- function(mx = matrix()) {
  invMatrix <- NULL
  #set the matrix
  setMatrix <- function(matx) {
    mx <<- matx
    invMatrix <<- NULL
  }
  #get the matrix
  getMatrix <- function(){
    mx
  } 
  
  #set the inverse matrix
  setInverseMatrix <- function(inverseMatrix) {
    invMatrix <<- inverseMatrix
  }
  #get the inverse matrix
  getInverseMatrix <- function() {
    invMatrix
  }
  
  #method list
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## Below method is used to make the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmx <- x$getInverseMatrix()
  if(!is.null(invmx)) {
    message("getting cached data")
    return(invmx)
  }
  #met the matrix data
  data <- x$getMatrix()
  #solve() method is used to find the inverse of a matrix.
  invMatrix <-solve(data) %*% data  ##mean(data, ...)
  x$setInverseMatrix(invMatrix)
  invMatrix
}

## Test Case 1. Uncomment the below lines and run it to test

#a <- makeCacheMatrix( matrix(c(1,0,-2,3,1,-2,-5,-1,9), nrow = 3, ncol = 3) )
#b<-matrix(a,3,3,byrow=TRUE)
#makeCacheMatrix(a)
#cacheSolve(a)

