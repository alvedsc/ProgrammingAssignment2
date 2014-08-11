## First, a list is created from a matrix through makeCacheMatrix with the 
## functions needed for cacheSolve. This is done so that any of this functions
## can be used as part of this list whenever cacheSolve needs to get or set a 
## value from or to the cache. 


## m is created as NULL so that when it is called using getinv() it returns 
## FALSE through !is.na() as long as setinv() is not called. Get just returns 
## the value of the matrix that was previously entered on makeCacheMatrix. 
## setinv asings the new value to m, which is supposed to be the inverse.
## getinv returns the value of the cached inverse. at the end this function 
## returns a list of functions so that they can be called from cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      get <- function() x
      setinv <- function(inv) m<<-inv
      getinv<- function() m
      list(get=get,setinv=setinv,getinv=getinv)
}


## This function tests the list created from makeCacheMatrix to check if the 
## inverse had been calculated before (it will be NULL as long as setinv() is 
## not called) . If there is a cached value then it just returns the value. If 
## not, it calculates it and returns the value through setinv() which
## automatically caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if (!is.null(m)) print("Cached value");return(m)
      inv <- solve(x$get())
      x$setinv(inv)
      inv
}
