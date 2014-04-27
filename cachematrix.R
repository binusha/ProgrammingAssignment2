## This function creates a special "matrix" object that can cache its inverse.
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## variable to store inverse cache
  cinv <- NULL
  
  ## set the new matrix value and invalidate the cache
  set <- function(y) {
      x <<- y
      cinv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) cinv <<- inverse
  
  ## return the cached inverse
  getinverse <- function() cinv
  
  ## get the special matrix
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
  inv <- x$getinverse()
  
  ## check if there is the matrix 
  if(!is.null(inv)) {
      message("getting cached inverse data")
      
      ## return the cached data
      return(inv)
  } 
  
  ## if no cached data get the inverse of the matrix and store it and return it   
  data <- x$get()
  inv <- solve(data)
  ## set the inverse of the matrix 
  x$setinverse(inv)
  
  ## return the inverse
  inv
}