## makeCacheMatrix and CacheSolve are functions used to store
## and create inverses of square matrix which does not get modified

## makeCacheMatrix is used to create a list which caches the input
## matrix as well as its inverse 

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y){
    x <<- y
    t <<- NULL
    
  }
  
  get <- function() x
  setsolve <- function(xt) t <<- xt
  getsolve <- function() t  
  list(set = set, get = get, getsolve = getsolve, setsolve = setsolve)  
}


## cachesolve checks if the Inverse of the matrix inverse already exists.
## in the Cache. If not, it would create the Inverse and Cache it
## for future use.

cacheSolve <- function(x, ...) {
  Inv <- x$getsolve()
  if(!is.null(Inv)){
    message("getting cached inverse matrix")
    return(Inv)    
  }
  m <- x$get()
  Inv <- solve(m)
  x$setsolve(Inv)
  Inv
        ## Return a matrix that is the inverse of 'x'
}

