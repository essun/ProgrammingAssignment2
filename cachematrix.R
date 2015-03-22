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


#Checking results example:

#Create a matrix:
# > m <- matrix(c(-1, -2, 1, 1), 2,2)

#Create the special "matrix":
# > x <- makeCacheMatrix(m)

# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1

#Solve the inverse of the matrix by using cacheSolve()
# > inv <- cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1

#Solve again the inverse. This time, it reads the cached data from 
#previous computation:
# > inv <- cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
