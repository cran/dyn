dyn <- function(x) {
#       cat("We are in THE DYN function TO CHANGE CLASS \n")    
#       browser()
	class(x) <- unique(c("dyn", oldClass(x)))
#       cat("We are RIGHT AFTER WE CHANGED CLASS TO DYN \n")            
	x
}

dyn <- dyn(dyn)

"$.dyn" <- function(x, fun) {
#       cat("We are in $.dyn \n")
#       browser()
	e <- parent.frame()
#       if (!identical(x, dyn)) return(eval(substitute(unclass(x)$fun),e))
#       if (!identical(x, dyn)) return(eval(substitute(unclass(x)$index),list(index=fun)))
# 11-12-16:        
# PATCH FOR LINE BELOW PROVIDED BY THOMAS KALIBERA BUT PAREN AFTER INDEX NEEDED TO BE REMOVED        
        if (!identical(x, dyn)) return(eval(substitute(unclass(x)$index,list(index=fun))))       
	f <- substitute(function(...) {
#               browser()
		cl <- match.call()
		cl[[1]] <- as.name(fun)
		cl[[2]] <- as.call(list(as.name("dyn"), cl[[2]]))
		result <- eval.parent(cl)
		dyn(result)
	})
#       browser()
	eval(f, e)
}
