## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      get <- function() x
      setinv <- function(inv) m<<-inv
      getinv<- function() m
      list(get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if (!is.null(m)) return(m)
      inv <- solve(x$get())
      x$setinv(inv)
      inv
}
