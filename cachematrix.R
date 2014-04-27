##the makeCacheMatrix function create a special matrix object
##that caches its inverse calculated by casheSolve
##function. 

##this function create a special matrix object
makeCacheMatrix <- function(x = matrix()) {
        Inverse<-NULL
        set<-function(y){
                x<<-y
                Inverse<<-NULL
        }
        get <- function(){
                x
        }
        setInverse<-function(z){
                Inverse<<-z
        }
        getInverse<-function(){
                Inverse
        }
        list(set = set,get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##this function checkes whether the matrix inverse
##is already cached. If so, it will return the matrix
##from cache, or else it will calculate the inverse
##and store it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse<-x$getInverse()
        if(!is.null(Inverse)){
                message("getting cached data")
                return(Inverse)
        }
        data<-x$get()
        Inverse<-solve(data,...)
        x$setInverse(Inverse)
        Inverse
}
