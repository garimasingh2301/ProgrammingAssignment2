#1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
    #makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
    #not changed), then the cacheSolve should retrieve the inverse from the cache.

## This function creates special "matrix" object that is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
  inv<-NULL                     ## initialize inv as NULL; will hold value of matrix inverse 
  setm<-function(y){            ## define the setm function to assign new 
    x<<-y                       ## value of matrix in parent environment
    inv<<-NULL                  ## if there is a new matrix, reset inv to NULL
  }
  getm<-function(){x}           ## define the getm fucntion - returns value of the matrix argument
  setInverse<-function(inverse){inv<<-inverse}  ## assigns value of inv in parent environment
  getInverse<-function(){inv}                  ## gets the value of inv where called
  list(setm=setm,getm=getm,setInverse=setInverse,getInverse=getInverse) ## you need this in order to refer to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  mat<-x$getm()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
