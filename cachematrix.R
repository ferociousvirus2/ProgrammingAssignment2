## A pair of function that would cache the inversion of a matrix

## It creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  i<-NULL
  
  set <- function(matrix) {
    m<<- matrix
    i<<-NULL
  }
  get <- function () {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse<- function() {
    i
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function computes the inverse of the special matrix returned by 'makecacheMatrix'above.
## If the inverse has already been calculated (and the matrix has not changed), then the "cache solce"
## should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
       m <-x$getInverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data<- x$get()
       
       i<- solve(data) %*% data
       
       x$setInverse(m)
       
       m
}
