## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#define the input as matrix to be passed in the function makeCacheMatrix.
#initialize invrs as NULL; this will hold value of inverted matrix.
#set function takes an argument that is named as y.
#when set function is executed, it assigns the input argument to the x object in the parent environment,
#and assigns the value of NULL to the invrs object in the parent environment.
#It clears any value of invrs that had been cached by a prior execution of cacheSolve().
#define the get fucntion, it returns value of the matrix argument.
#assigns value of invrs in parent environment.
#gets the value of invrs where called.
#each of these functions as an element within a list(), and returns it to the parent environment.
#this allows us to use the $ form of the extract operator to access the functions by name.

makeCacheMatrix <- function(x = matrix()) {          
    invrs <- NULL                                    
    set <- function(y) {                             
        x <<- y                                       
        invrs <<- NULL                               
    }                                                
    get <-function() x                               
    setinvrs <- function(inverse) invrs <<- inverse  
    getinvrs <- function() invrs                    
    list(set = set, get = get,                       
         setinvrs = setinvrs,                        
         getinvrs = getinvrs)
}

## Write a short comment describing this function

#cacheSolve() is required to populate or retrieve the mean from an object of type makeCacheMatrix().
#cacheSolve() starts with a single argument, x, and an ellipsis that allows the caller to pass additional arguments into the function.
#Next, the function attempts to retrieve a inverse from the object passed in as the argument. First, it calls the getinvrs() function on the input object.
#Then it checks to see whether the result is NULL. Since makeCacheMatrix() sets the cached value of invrs to NULL whenever a new matrix is set into the object, 
#if the value here is not equal to NULL, we have a valid, cached invrs value and can return it to the parent environment.
#If the result of !is.null(invrs) is FALSE, cacheSolve() gets the matrix from the input object, calculates a Solve(), 
#uses the setinvrs() function on the input object to set the inverted matrix in the input object, 
#and then returns the value of the invrs to the parent environment by printing the inverted matrix object.

cacheSolve <- function(x, ...) {                     
        ## Return a matrix that is the inverse of 'x'
    invrs <- x$getinvrs()
    if(!is.null(invrs)) {
        message("getting cached inversed Matrix")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinvrs(invrs)
    invrs
}
