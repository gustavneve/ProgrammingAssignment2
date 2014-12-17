## makeCacheMatrix creates matrix object that can cache its inverse
## 

## the function will create a list containing functions that:
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set value of inverse of the matrix
## 4. get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse<-NULL
  
  ##function to set value of matrix with NULL inverse
  set<-function(y){
    x<<-y 
    inverse<<-NULL
  }
  
  ##function to retrieve matrix
  get<-function()x
  
  ##function calculate and cache inverse of matrix
  setinv<-function(solve)inverse<<-solve ##solve function calculates inverse of matrix
  
  ##function to retrieve inverse
  getinv<-function() inverse
  
  ##create list containing funcitons defined above
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
  
}


## cacheSolve returns the inverse of the matrix.
## 1. Check if inverse has already been computed
## 2. Will skip computation of the inverse if inverse is cached
## 3. Will calculate inverse if not cached, set value in cache
## 4. Assumes matrix is always invertible 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinv()
  
  ##check to see if x$getinv() above was able to retrieve the ivnerse from cache
  if(!is.null(inverse)){
    
    ##if the inverse was cached, display a message that says the inverse is being retrieved from cached data, return cached inverse
    message("getting cached data")
    return(inverse)
    
  }
  
  ##if the inverse was not cached, use x$get to retrieve matrix
  data<-x$get()
  
  ##solve the matrix
  inverse<-solve(data,...)
  
  ##x$setinv(inverse) to cache inverse
  x$setinv(inverse)
  
  ##return the inverse of the matrix
  inverse

}