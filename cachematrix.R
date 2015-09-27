## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly

makeCacheMatrix <- function(x = matrix()){
  invMatrix <- NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cacheSolve <- function(x, ...){
  invMatrix <- x$getinverse()
  if(!is.null(invMatrix)){
    message("getting the inverse of a Matrix")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setinverse(invMatrix)
  invMatrix
}