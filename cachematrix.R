## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inversematrix<-NULL
    setinversematrix<-function(y){
      inversematrix<<-y
    }
    
    getinverse<-function() inversematrix
    getmatrix<-function() x
    
    list(setinversematrix=setinversematrix,getinverse=getinverse,getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversevalue<-x$getinverse()
        if(!is.null(inversevalue)){
          message("getting cached data")
          return(inversevalue)
        }
        
        data<-x$getmatrix()
        if(det(data)!=0)
           inversevalue<-solve(data)
        x$setinversematrix(inversevalue)
        inversevalue
}
