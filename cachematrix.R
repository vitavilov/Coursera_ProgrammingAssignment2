
## Create an object that contains 
## a matrix and it's inversed form in local environment
## and a list of 4 functions in global environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     #set cache for inversed value to NULL
        set <- function(y) {            #function to replace original batrix in the object
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}           # function that returns original matrix
        setinv <- function(inversed) {  #function to set inversed matrix to cache
                inv <<- inversed
        } 
        getinv <- function(){inv}       #function to return inversed value from cache
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
}


## Return a matrix that is the inverse of 'x'
## (assumed that the matrix supplied is always invertible)
cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()       #look for cached vlue in x object
        if(!is.null(inv)) {     #if cached value exists (not NULL)...
                message("getting cached data")
                return(inv)     #return cached value end finish the function
        }
        # if there is no cached value...
        data <- x$get()         #take the original matrix from x
        inv <- solve(data, ...) #use solve function to inverse it
        x$setinv(inv)           #put calculated inversed matix to cache in x object
        inv                     #return inversed matrix
}
