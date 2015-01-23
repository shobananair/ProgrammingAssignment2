## Provides functionality to inverse the matrix and store the value in cache. Also provide the option to retrieve the value from cache
## whenever required to avoid computing time

## This function takes matrix as an argument and return a list with functions that provides getters and setters 
## to store the inverse of matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  invMatrix<-NULL
  set<-function(y){
    x<<-y
    invMatrix<<-NULL
  }
  get<-function()x
  setInverse<-function(inv)invMatrix<<-inv
  getInverse<-function()invMatrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## caclculates the inverse of matrix and stores in cache to avoid any future calculations required for inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix<-x$getInverse()
  if(!(is.null(invMatrix))){
    message("returning cached data")
    return(invMatrix)
  }
  data<-x$get()
  invMatrix<-solve(data)
  x$setInverse(invMatrix)
  invMatrix
}
