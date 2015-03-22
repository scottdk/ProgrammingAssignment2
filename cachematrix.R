## Caching the inverse of a matrix. 
## Calculate the inverse of a matrix, however if the inverse has already been
##   calculated, return the cached version.
##
## Usage example:
## > m1 <- matrix(1:4, c(2,2))
## > m1c <- makeCacheMatrix(m1)
## > m1c$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > 
## > cacheSolve(m1c)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m1c)
## returning cached matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## makeCacheMatrix(m)
##  Returns a cached matrix with associated functions set, get, setInv and getInv

makeCacheMatrix <- function(m = matrix()) {
        
        # set i, the cached inverse of the matrix to be initially NULL 
        i <- NULL
        
        # set(y) : Function, takes parameter y, setting it as the value for 'm'
        #          Reset the cached value for the inverse 'i'
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
      
        # get() : Function to return the matrix 'm' stored at initialisation 
        #           or using the set(y) function above
        get <- function() m
      
        # setInv(inv) : Function to cache the inverse inv of a matrix
        setInv <-function(inv) i <<- inv
          
        # getInv() : Function to return the cached inverse.
        getInv <- function() i
        
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## Return a matrix that is the inverse of 'x'
## 'x' must be a cached matrix, from the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
                
        #Before calculating the inverse, look for the cached inverse
        i <- x$getInv()
        
        #If this has been set, return the cached value and stop
        if(!is.null(i)) {
                message("returning cached matrix inverse")
                return(i)
        }
        
        ##The cached value wasn't found, so create it:
        
        #retrieve the matrix stored
        m <- x$get()
        
        #get the inverse of the matrix
        i <- solve(m)
        
        #cache the inverse so if will not need to be recalculated next time
        x$setInv(i)
        
        #Finally, return the inverse matrix
        i
}
