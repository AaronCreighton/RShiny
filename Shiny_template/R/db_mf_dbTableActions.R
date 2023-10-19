
# this is a module function files example for doing actions to a Table
# used if you need the global variables in r for the users session


#notes on name spaces for module actions
# could be used to differentiate a delete action or update action (change what is done)
# could be used to provide context to inform the error handeling


mf_getTable <- function (id, r,
                         in_value = NULL,
                         in_value2,){
  moduleServer(id, function(input, output, session) {


    return(out_value)
  })
}

mf_updateTable <- function (id, r,
                         in_value = NULL,
                         in_value2,){
  moduleServer(id, function(input, output, session) {


    return(out_value)
  })
}

