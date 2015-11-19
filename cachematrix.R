
## function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #CacheMatrix has variables x and m, matrix and solved matrix
  #m is the solved matrix, which is initially unsolved
  m<-NULL
  
  #If setting the matrix, then the solved would need to be reset
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  
  #Returns the matrix (unsolved)
  get<-function()
  {
    x
  }
  
  #sets the passed in solved value
  setmatrix<-function(solve) 
  {
    m<<- solve
  }
  
  #returns solved matrix
  getmatrix<-function()
  {
    m
  }
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  #Get solved matrix if available in special matrix item
  m<-x$getmatrix()
  if(!is.null(m))
  {
    m
  }
  
  #If not available, grab the matrix
  matrix<-x$get()
  #Assign the solved matrix
  m<-solve(matrix, ...)
  #Cache matrix
  x$setmatrix(m)
  #Return solved matrix
  m
}
