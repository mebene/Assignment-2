# We set the value of the matrix the set function;
#Then we get the value of the matrix with the get function;
#then we set the value of the inverse matrix with setinverse function;
#and finally we get the value of the inverse matrix with the getinverse function.


# Calculate the inverse of a matrix from another environment and stores it.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
                
                
        }
        
        get <- function()  x    
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list(set=set, get = get, setinverse = setinverse ,getinverse = getinverse)
}


# calculate the value of an inverse matrix through the MakecacheMatrix function.
# if the value is already calculated produce it without further calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
