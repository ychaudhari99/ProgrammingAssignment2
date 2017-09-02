## Assignment: Caching the Inverse of a Matrix
## The function below perform matrix inversion, a cached value is 
## returned if an inverse is already performed

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y){
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInvrs <- function(inverse) invrs <<- inverse
        getInvrs <- function() invrs
        
        ## Return a list with 1.set the matrix, 2.get the matrix, 
        ## 3.set the inverse, 4.get the inverse
        list(set = set, get = get, setInvrs = setInvrs, getInvrs = getInvrs)

}


##2. cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInvrs()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        
        data <- x$get()
        invrs <- solve(data, ...)
        x$setInvrs(invrs)
        return (invrs)
}
