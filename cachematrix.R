## The functions makeCacheMatrix and cacheSolve create a matrix 
## and its inverse and store the inverse in cache.
## When the inverse of a matrix is cached usually is giving
## back the results faster than computing the inverse again and again.


##  makeCacheMatrix creates a function (actually a list) to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## The double arrows operator implies that 
## the variables "inv" and "x" can be searched  
## outside the scope of the current function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix. 
## If the inverse is already calculated it returns the cached inverse.
## If the inverse is not yet calculated it calculates it
## caches it and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
  if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}



## testing the functions.
## A 70 by 70 matrix was created and
## some code was written to measure 
## how much time it takes for the no cache
## and the cache inverse matrix to be computed.

## the code for the matrix and its inverse
## the m$get() is not displayed for space purposes

set.seed(150)
r = rnorm(4900)
x<-matrix(r, nrow=70, ncol=70)
m<-makeCacheMatrix(x)
m$get() 

## the "no-cache" first run and the time elapsed
## for the computation

start.time <- Sys.time()
cacheSolve(m)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## result:Time difference of 0.05500293 secs

## the cache run and time elapsed
## "getting cached data" not displayed for space purposes

start.time <- Sys.time()
cacheSolve(m)
getting cached data
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## result: Time difference of 0.03800201 secs
 

