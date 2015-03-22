#Creates a special "matrix" which is a list containing functions to:
#1 set the value of the matrix
#2 get the value of the matrix
#3 set the value of the inverse matrix
#4 get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <- NULL
    }
    get <- function() x
    setinvm <- function(solve) invm <<- solve
    getinvm <- function() invm
    list(set = set, get = get, setinvm = setinvm, getinvm=getinvm)

}


#Calculates the inverse of the special "matrix" created with 
#makeCacheMatrix()
#First checks if the inverse has already been calculated and, if so,
#gets the cached inverse.
#Else it calculates the inverse and sets the value of the inverse
#in the cache with setinvm()
cacheSolve <- function(x, ...) {
    invm <- x$getinvm()
    if(!is.null(invm)){
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}
