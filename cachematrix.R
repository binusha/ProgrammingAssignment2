## This function creates a special "matrix" object that can cache its inverse.
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  invx <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) invx <<- solve
  getinverse <- function() invx
  
  ## get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix  
  invx <- x$getinverse()
  
  ## check if there is the matrix 
  if(!is.null(invx)) {
      message("getting cached inverse data")
      return(invx)
  } 
  
  ## if not get the inverse of the matrix   
  data <- x$get()
  invx <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(invx)
  invx
}