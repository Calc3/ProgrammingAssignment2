## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL 
   set <- function(y) { #assigning value of matrix
     x <<- y
     inv <<- NULL
   }
   get <- function() x #value of matrix
   setInverse <- function(inverse) inv <<- inverse  #setting value of inverse  
   getInverse <- function() inv #value of inverse  
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
           
}


## Write a short comment describing this function
         
#inverse of the matrix above
#if the inverse has been calculated, and the matrix remains unchanged then it should find the inverse from the existing cache        

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
inv <- x$getInverse() # looking at subset getInverse
if (!is.null(inv)) {
     message("getting cached data") # if found in cache 
     return(inv) # if not found in cache
   }
   mat <- x$get() 
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}
