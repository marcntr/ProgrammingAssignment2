## this is pair of functions that caches the inverse of a matrix

## makeCacheMatrix creates a special matrix object
## which contains functions to get/set the value of the matrix and get/set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setinv <- function(mat) inv <<- mat
  getinv <- function() inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data!")
    return(inv)
    
  }
  data<-x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
          
}
