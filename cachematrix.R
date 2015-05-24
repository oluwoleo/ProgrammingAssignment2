## Put comments here that give an overall description of what your

#Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Write a short comment describing this function
## function creates a special "matrix" object that can cache its inverse through the.

#1.set the value o the matrix
#2. get the value of the matrix
#3.set the value of inverse of the matrix 
#4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setmean,
       getinverse=getinverse)
}

## Write a short comment describing this function
## This function returns the inverse of created matrix. 
# Checks firstly if the inverse of the matrix have already been computed
# if so , it gets the result and skips the inverse computation 
#If not, it computes the inverse of the matrix and set the value in the cache 
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
