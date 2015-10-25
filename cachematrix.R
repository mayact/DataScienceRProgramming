## Put comments here that give an overall description of what your
## functions do
##a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m<- NULL #set the value of m to NULL
    y<- NULL #set the value of y to NULL
    set <- function (y) { # set the value of the matrix
            x <<- y    # cache the input matrix for cacheSolve function to check if any changes to the matrix 
            m <<- NULL # set the value of the matrix inverse m to NULL       
    }
    get <- function() x # get the value of the matrix
    setinverse <- function(inverse) m <<- inverse  # set the value of the matrix inverse m to inverse 
    getinverse <- function() m # get the iverse value 
    list( set = set, get =get ,setinverse = setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<- x$getinverse()                  # get the inverse 
    if(!is.null(m)){                    # check if the inverse exists
           message("getting cached data") # if the inverse exists, then send a message and return the cached inverse 
           return(m)  
    } 
    else{       
    # if the inverse doesn't exist       
    y<-x$get()# get the input matrix
    x$set(y)  # cache the input matrix
    m <- solve(y, ...) # calculate the inverse of the input matrix
    x$setinverse(m)    # cache the calculated inverse
    m # return the inverse 
    } 
}
