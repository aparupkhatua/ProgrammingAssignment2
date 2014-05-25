## This function caches the inverse value of x
## and creates a list of functions get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
  
  DM<<-x
  inversematrix<-NULL
  
  get<-function() DM
  # setInverse sets the inverse of the original matrix and store it
  setInverse<-function(inverse) inversematrix<<-inverse
  
  getInverse<-function() inversematrix
  
  list(get=get,
       setInverse=setInverse,
       getInverse=getInverse
  )
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # retrieves the inverse of the matrix
  inverse<-x$getInverse()
  # Check if the inverse has been calculated
  if(!is.null(inverse)) {
    message("check")
    # returns the inverse that has been calculated and cached
    return(inverse)
  }
   inverse<-solve(x$get(), ...)
  # caches the calculated inverse matrix
  x$setInverse(inverse)
  # returns the inverse matrix
  inverse
}



# Testing 
m<-matrix(c(1:4),ncol=2) 
c<-makeCacheMatrix(m) 
r<-cacheSolve(c)
