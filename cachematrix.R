## Put comments here that give an overall description of what your
## functions do

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculating the inverse of a matrix can be costly if done
## repeatedly with large datasets. The following pair of functions
## will together cache the inverse of a matrix, meaning if the
## inverse has previously been calculated, it will be retrieved
## rather than being recalculated.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Write a short comment describing this function

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This function will create a list of 4 functions:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv_result) i <<- inv_result
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This function uses the above function (provided as argument "x")  
## to check if the inverse of the matrix has already been calculated
## (if so, it would be in the list). If it has been calculated, this 
## function returns the inverse, otherwise, it calculates the inverse, 
## stores it in the list, and then returns the inverse.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
