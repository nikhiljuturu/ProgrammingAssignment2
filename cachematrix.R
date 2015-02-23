## The first function makeCacheMatrix creates the object for the matrix and caches its resultant inverse
## The second function cacheSolve checks the object , if its cache exists, it returns it else returns solve(matrix), assuming the matrix input is always square

## The first function houses the four functions that deal with storing the cache of an input matrix and reinitialize the cached value to null when a new input is provided and for setting and getting the values

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL  ## i is initialized to null to clear out any possible values it might be holding
  
##the four underlying functions of makeCacheMatrix are defined below
  
	##set function defined below takes y which is a new input matrix as an argument and assigns it to x
	##it also reinitializes i to NULL because the matrix has changed and the cached inverse matrix would be incorrect
  set<-function(y){
    x <<- y
    i <<- NULL
  }
  
  
  get<-function()x  				##get calls for the value of matrix x
  setinv<-function(inv) i<<-inv 	##takes inv as an argument and assigns it to i
  getinv<-function() i				##prints the inverse matrix i
  
##below is the list which is the return value of makeCacheMatrix, which enables it to call each individual function as an element by x$get, x$setinv, etc.
	list(set=set,get=get,
		setinv=setinv,
		getinv=getinv)

}


## This function checks the getinv of the object created above, if the cache exists it returns it. Else, it computes the inverse of the matrix using the solve() and storing it to i, sets i to that value,

cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if (!is.null(i)){
    message("Getting Cached Data")
    return(i)
    
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}