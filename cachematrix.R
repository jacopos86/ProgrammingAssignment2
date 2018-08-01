## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special matrix
## that is a list containing a function to
## 1- Set the value of the matrix
## 2- get the value of the matrix
## 3- Set the value of the inverse
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invert <- NULL
  
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  
  get <- function () x
  
  setinv <- function(inv) invert <<- inv
  
  getinv <- function() invert
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invert <- x$getinv()
        
        if (!is.null(invert)) {
          message("getting cached data")
          return(invert)
        }
        
        mat <- x$get()
        invert <- solve(mat, ...)
        x$setinv(invert)
        
        invert
}

