## Suppose we have a matrix invertible.
## The function solve return the inverse matrix, 
## but when the inverse matrix is used repeatedly 
## it is more efficient to cache the result.


## makeCacheMatrix of a matrix creates an object 
## (a list of 4 functions) that can cache 
## its inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) {m <<- inv}
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the object returned 
## by makeCacheMatrix above. If the inverse has already been calculated,
## then cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse and sets the risult 
## in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}


##  Example
##> x<-matrix(c(1,0,0,0,3,4,0,1,1),3,3)
##> x
##     [,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    3    1
##[3,]    0    4    1

##> cacheSolve(makeCacheMatrix(x))
##     [,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0   -1    1
##[3,]    0    4   -3
