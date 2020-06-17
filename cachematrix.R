## First function : "makeCacheMatrix" is creating matrix and making a list that contains all the functions with names
## Second function : "cacheSolve " checks if the inverse already exists and if not calculates and returns the inverse

## Function makeCacheMatrix is creating a matrix initially setting the inverse - "i" to NULL 
## set defines a function to set the matrix x to a new matrix object y and also resets the inverse i to NULL
## get function simply retiurns the matrix x
## setinverse stores the inverse in i
## getinverse returns inverse of matrix
## list returns a special vector, containing all the above functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## calls getinverse on matrix x and holds the value in i,
## if the value is already calculated and exists then get the chached value otherwise calculate in the following steps
## data holds the value of matrix x
##  solve returns the inverse which is stored in i with setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
