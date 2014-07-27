##########################################################################
#Function:      makeCacheMatrix
#Description:   Create a Martix Object which caches the inverse
#Input:         A matrix (Empty 1x1 matrix is default)
#Output:        List of follwoing functions:
#               a) get - Get the matrix stored in the object
#               b) set - Set a new matrix in the object
#               c) getinverse - Get the cached inverse of the matrix
#               d) setinverse - Set the inverse of matrix & stored it
##########################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  
  matInv <- NULL
  set <- function(y) {
      x <<- y
      matInv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matInv <<- inverse
  getinverse <- function() matInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

##########################################################################
#Function:      cacheSolve
#Description:   Create an inverse of martix in the matrix object & cache it
#Input:         A matrix object (as returned by makeCacheMatrix)
#Output:        Returns the inverse if cached previously; else calculates 
#               the inverse, caches and returns it
##########################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
  
}