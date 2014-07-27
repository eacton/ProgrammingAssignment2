## The makeCacheMatrix() function takes a matrix as input and creates a cache in which to store its inverse, as well as a list of functions that can 
## be used outside the makeCacheMatrix environment to make changes to the cached versions of the matrix and its inverse (such that the inverse 
## does not need to be recalculated). 
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL               #the cache for the inverse matrix is originally empty
        set <- function(y) {                 #sets a new value for the matrix
                x <<- y
                inverse_matrix <<- NULL      #the cached value for the inverse matrix is reset, since there is a new matrix
        }
        get <- function() x                  #get the matrix
        setinverse <- function(z) inverse_matrix <<- z    #set the value for the inverse matrix, obtained from cacheSolve()
        getinverse <- function() inverse_matrix           #get the value for the inverse matrix (will be empty if setinverse() 
        #has not been called)
        list(set = set, get = get,                        #returned list of functions for use outside the makeCacheMatrix environment
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve() function takes the list of functions returned by makeCacheMatrix(). cacheSolve() then uses the getinverse() function to check if an inverse 
## matrix has been stored to a cache (which it would then return).  If that cache is empty, cacheSolve() will get the original matrix (using get()) 
## and calculate its inverse.  It will then set the cache to the inverse matrix by calling setinverse().
cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()          #calls the getinverse() function to access the inverse matrix cache
        if(!is.null(inverse_matrix)) {            #if the inverse has already been cached (ie. the cache is not NULL), the inverse is retrieved from the cache 
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()                    #if the inverse has not been cached, get the matrix
        inverse_matrix <- solve(data, ...) #calculate the inverse
        x$setinverse(inverse_matrix)       #save the inverse matrix by setting it to the cache
        inverse_matrix                     #return the inverse matrix
}

