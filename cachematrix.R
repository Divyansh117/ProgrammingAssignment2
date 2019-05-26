## Caching the inverse of a matrix

## makeCacheMatrix: creates a special matrix object that can cache its reverse
makeCacheMatrix <- function(x = matrix()) {
            cacheM<-NULL
            
            set<-function(y){
              x <<- y
              cacheM <<- NULL
            }
            
            get<-function()x
            setInverse<-function(inverse) cacheM<<-inverse
            getInverse<-function()cacheM
            
            list(set =set, get= get, setInverse = setInverse, getInverse =getInverse)
}


## cacheSolve:This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
              cacheM<-x$getinverse()
              if(!is.null(cacheM)){
                    message("Loading cached matrix..")
                    return(cacheM)
              }
              else{
                data<-x$get()
                cacheM<-solve(data,...)
                x$setinverse(cacheM)
                return(cacheM)
              }
}
