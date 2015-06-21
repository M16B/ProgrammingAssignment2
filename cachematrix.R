#The function makeCacheMatrix will return a list of functions for getting and setting both
#the matrix and matrix inverse from and to a cached variable.

#The function cacheSolve checks for an existing cached matrix inverse value.
#If there is no cached value is present, then the function
#caclucates the inverse and sets the cached value.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    #set the value of the matrix cached variable x
    setx <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    #get the value of the matrix cached variable x
    getx <- function() x
    #set the value of the matrix inverse cached variable inverse
    setinverse <- function(matinverse) inverse <<- matinverse
    #get the value of the matrix inverse cached variable inverse
    getinverse <- function() inverse
    list(setx=setx, getx=getx, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse() #check for existing matrix inverse
    if(!is.null(inverse)){
        #return cached value
        message("getting cached result")
        return(inverse)
    }
    data <- x$getx()
    inverse <- solve(data, ...) #calculates inverse
    x$setinverse(inverse)
    inverse
}
