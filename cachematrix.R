## This function creates a special "matrix" object that can cache its inverse.
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invx <<- solve
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)) {
      message("getting inverse data")
      return(invx)
  } else {
      invx <- solve(x$get())
      x$setinverse(invx)
      return(invx)
  }
}