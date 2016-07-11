## I have two functions: the makeCacheMatrix is used to set the value of the inverse of the given matrix in the cache, and it is also used to get matrix itself
## Also, makeCacheMatrix return a list finally so that we can use it in the second function CacheSolve function
## The second function cacheSolve is used to get retrive inverse value and return it! if we have already computed the inverse of the martix, we do not need to compute it again, we can just retrive it from the cache. if not, we need to compute it! 

## Write a short comment describing this function
## This function is used to set inverse of the matrix and assign it in the cache!
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
## This function is used to compute the inverse of the matrix, and return its value!
cacheSolve <- function(x, ...) {
        
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
