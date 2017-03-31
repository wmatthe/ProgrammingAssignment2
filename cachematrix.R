makeCacheMatrix <- function(x = matrix()) {
        
## Function creates a special matrix object that can cache its inverse. 
## @x: a square invertible matrix
## return: a list containing functions to 
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
## this list will be used as the input to cacheSolve()


        m <- NULL
        set <- function(y) {
                # use '<<-' to assign a value to an object in an
                # environment different from the current environment. 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the matrix inverse is
## already calculated. If it is it gets the inverse from
## the casche. otherwise, it calculates the inverse and sets
## the value of the inverse in the cache via the setinverse
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        m <- x$getinverse()
        
        # if the inverse has already been calculated
        if(!is.null(m)) {
                # get it from the cache & skip the computation.
                message("getting cached data")
                return(m)
        }
        
        # otherwise, calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        
        # sets the value of the inverse in the cache via setinverse function
        x$setinverse(m)
        m
}
