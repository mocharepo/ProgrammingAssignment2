## The functions below take a matrix and cache of the inverse of the matrix

## This function takes a matrix, and creates a list of functions for storing matrix
## and caching its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {   # set sets the first element of the list to a null
          x <<- y
          s <<- NULL
      }
      get <- function() x  # get takes the input matrix and stores
      setsolve <- function(solve) s <<- solve  # setsolve is a function that stores 
      # the inverse of the input matrix into the null s of set function
      
      getsolve <- function() s   # getsolve function takes the value of s
      list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## The following function takes the list created by the makeCacheMatrix 
## to cache the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()  
      if(!is.null(s)) {       
          message("getting cached data")     # if the inverse of matrix has already..
          return(s) # ..been calculated, it returns the value after the message
      }
      data <- x$get()
      s <- solve(data, ...) # if the inverse was not calculated before, it now ..
      x$setsolve(s) # .. calculates and stores in the list of makeCacheMatrix
      s
}
