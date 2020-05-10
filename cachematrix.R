## makeCacheMatrix will cache the inverse of a matrix
## get will get the matrix of type makeCacheMatrix
## set will set the matrix of type makeCacheMatrix and initialize its inverse
## getinverse will get the inverse of the matrix
## setinverse will set the inverse of the matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function(){
      x
  }  
  setInverse <- function(inverse){ 
      inv <<- inverse
  }
    getInverse <- function(){ 
      inv
  }    
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}