
# function to push new element to end of list
list.push <- function(lst, newitem){

    lst[[length(lst) + 1]] <- newitem
    return(lst)
}

# function to pop last item from list
list.pop <- function(lst){

    lst_length <- length(lst)
    if(lst_length > 1)
        return(lst[1:lst_length - 1])
    else if(lst_length == 1)
        return(list())
}

# Quick grid for testing purposes
tg1 <- expand.grid(nrounds = c(15),
                   eta = c(0.1),
                   max_depth        = seq(1, 5, by = 1),
                   min_child_weight = seq(5, 10, by = 5),
                   colsample_bytree = seq(0.8, 1, by = 0.1),
                   gamma            = seq(0, 20, by = 20))
