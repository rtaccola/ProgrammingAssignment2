## Both functions work together:

## makeCacheMatrix function creates a vector that works as an object
## with methods, here named setters and getters.
## Using "$" we can call these methods and interact with the object


makeCacheMatrix <- function(x = matrix()) {

     #set inverse matrix as null
     inv <- NULL
     
     #set setter "set" a new value (y) to x
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     #return x when getter "get" is called 
     get <- function() x
     
     #setinverse is used when called by cacheSolve to set the inverse matrix
     setinverse <- function(inverse) inv <<- inverse
     
     #return inverse matrix "inv" when called as a getter of the object of 
     #type makeCacheMatrix
     getinverse <- function() inv
     
     #name the elements of the list to allow them to be called as getters and  
     #setters of the makeCacheMatrix object
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
     
}


## cacheSolve Calculate the inverse matrix of x using the setter method setinverse
## of the object makeCacheMatrix if there is no inverse matrix in cache. If there is
## a cached matrix it will return it to the user.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     #set the inverse matrix to "inv" if it alreadt exists in cache
     inv <- x$getinverse()
     
     #check if "inv" is not null (already in cache)
     #if it is already in cache leaves the function and returns "inv"
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     #if is null (not in cache) get x (matrix in makeCacheMatrix) and assign to "data"
     data <- x$get()
     
     #calculate inverse using the solve function
     inv <- solve(data)
     
     #set inverse matrix using "setinverse" method from makeCacheMatrix
     x$setinverse(inv)
     
     #finally returns inv (inverse matrix)
     inv
     
}
