## Functions below calculate the inverse of matrix. 
## These functions can accelerate the repeated 
## calculation proccess incorporated in long-lasting loops (like for(i ..))
## Thanks to the application of super-assignment "<<-" operator certain 
## variables are accessible in both functions (make.., and cache...).

## Argument of function makeChaceMatrix is an inversible, 
## simple matrix like tf <- matrix(1:4,2,2).Function is to be invoked like
## a <- makeCacheMatrix(tf). In that case object "a" consists of list of 
## nested,subfunctions (set, get ... to be called later from cacheSolve) 
## and matrix. Function also sets m <- NULL - the cache variable signals,
## that the cache is empty.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve invoked for the first time (CacheSolve(a)) 
## verifies the value of m through subfunction getinv (internal value 
## of m in makeCacheMtrix), as m equals to NULL function calculates 
## the inverse (previosly using get subf) of matrix. 
## The value of inverse is super-assigned to m with help of setinv
## subfunction. Every next time cache variable m is greater than zero, so the
## inverse is not calculated every time the the loop passes. 
## It is possible to change the underlying matrix with help of subfunction
## set, during e.g. the loop process in a "dynamic" way (function set is not
## directly invoked in cacheSolve, can be called as a$set()).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
