
makeVector <- function(x = matrix()) { 
    p <- NULL 
    set <- function(y) { 
        x <<- y 
        p <<- NULL 
    } 
    
    get <- function() x 
    setinverse <- function(inverse) p <<- inverse
    getinverse <- function() p
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

cacheinverse <- function(x, ...) { 
    p <- x$getinverse() 
    if (!is.null(p)) {
        message("Getting cached data")
        return(p)
    } 
    
    data <- x$get()
    p <- solve(data, ...)
    x$setin(p)
    p
    
}