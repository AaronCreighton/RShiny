
# this is a module function files example for doing actions to a single db Table
# used if you need the global variables in gr for the users session

# notes on file naming:
# db: database, purpose is to interect with the db.
# mf: module functions, server modules that can receive and return variables.



#notes on name spaces for module actions:
# could be used to differentiate a delete action or update action (change what is done)
# could be used to provide context to inform the error handeling


mf_getTable <- function (id, gr,
                         in_value = NULL,
                         in_value2 = NULL){
  moduleServer(id, function(input, output, session) {

    # is filtering needed?

      # if yes filter the table
      # if no return table


    return(table)
  })
}

mf_validateTableInput <- function (id, r,
                            in_value = NULL,
                            in_value2){
  moduleServer(id, function(input, output, session) {

    # add code to validate if rows going into the table are correct format etc.


    return(out_value)
  })
}

mf_updateTable <- function (id, r,
                         in_value = NULL,
                         in_value2){
  moduleServer(id, function(input, output, session) {

    # add code to update table


    return(out_value)
  })
}

mf_deleteEntry <- function (id, r,
                            entryId = NULL){
  moduleServer(id, function(input, output, session) {

    # add code to delete entry


    return(out_value)
  })
}

