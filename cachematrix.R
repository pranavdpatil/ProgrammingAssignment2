## First run objt<-makeCacheMatrix(), where you would be assigning the function to any object
## Then run objt$set(matrix(20:23,2,2)) to set the matrix. You can use a different matrix- this is just as an example
## As a third step, run cacheSolve(objt)- notice that it returns the matrix without the "cached message" when you run it for the first time
## When you invoke this function again, it gets the inverted cached matrix and displays it with a message "getting cached data"

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matx = matrix()) 
{
    m <- NULL
        
    ## Use this function to set the matrix
    set <- function(y) 
         matx <<- y
         m <<- NULL
        }
   
    get <- function() matx
        
    setInverse <- function(invertedMatrix) m <<- invertedMatrix
    getInverse <- function() m
        
    list(set=set, get=get,
    setInverse=setInverse,
    getInverse=getInverse)
 }


## Return a matrix that is the inverse of 'matx'
cacheSolve <- function(matx=matrix(), ...) 
{
    m<-matx$getInverse()  ## Attempt to get inverted matrix 
    if(!is.null(m)) 
    {
        ## The lines below will get invoked when running the function for the second time for the same object
        message("getting cached data")
        return(m)
    }
    data <- matx$get()
    m <- solve(data, ...)  ## whenever it is not stored, calculates the inverse and caches it to re-use it for the second time
    matx$setInverse(m)
    m
}