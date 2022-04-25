.Last <-
structure(function (...) 
{
    tryCatch({
        if (exists("finalizeSession", mode = "function")) 
            finalizeSession()
    }, error = function(ex) {
        message("Ignoring error occured in .Last(): ", as.character(ex))
    })
}, finalizeSession = TRUE, finalizeSessionVersion = "2.6.0")
