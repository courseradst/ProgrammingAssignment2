
## These two functions are used to compute the inverse of a square matrix
## It will internally cache the inverse matrix after first calculation
## in subsequent function call, result will be retrieved from cache
## instead of caculating again

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of squre matrix
## get the inverse of squre matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(v) {
    inv <<- v
  }
  getinv <- function() {
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get from cache")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
}

## additional function impl to achieve similar functionality as above two functions
## when invoke the function, a matrix operation function is provided, e.g. solve
## it returns another function that will apply the operation 
## 1st time execution will engage a computation
## subsequent execution will retrieve from closure

cacheMatrixOp <- function(func) {
  cache <- NULL
  function(x) {
    if(is.null(cache)) {
      (cache <<- func(x))
    }
    else {
     message("get from cache")
     cache
    }
  }
}
