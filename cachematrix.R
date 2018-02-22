## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix function returns a list of functions for getting and setting the matrix and its  inverse
##setInverse() sets inverse and getInverse() gets inverse of the matrix
##get() returns matrix and set() sets the matrix value

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  
  setInverse<-function(inverse) i<<-inverse
  
  getInverse<-function() i
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  

}


## Write a short comment describing this function

##cacheSolve is a cache function. it checks if there is an inverse value already else it finds the inverse of a matrix by using solve()

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  m<-x$get()
  i<-solve(m,...)
  x$setInverse(i)
  i
}
