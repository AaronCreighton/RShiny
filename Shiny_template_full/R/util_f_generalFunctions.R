# file for genetic functions used across multiple modules.

# Dynamic SQL ----

dbUpdateCustom = function(x, conditions_cols, con, schema_name, table_name) {
  # dynamically create sql update statement
  #x: 1-row dataframe that contains 1+ key columns, and 1+ update columns.
  #conditions_cols: character vector, of 1 or more column names that are the conditions for the WHERE clause.

  # WARNING: if conditions_cols do not result unique row it should update multiple rows.

  if (nrow(x) != 1) stop("Input dataframe must be exactly 1 row")
  if (!all(conditions_cols %in% colnames(x))) stop("All columns specified in 'key_cols' must be present in 'x'")

  # Build the update string --------------------------------------------------

  df_key     <- dplyr::select(x,  one_of(conditions_cols))
  df_upt     <- dplyr::select(x, -one_of(conditions_cols))

  set_str    <- purrr::map_chr(colnames(df_upt), ~glue::glue_sql("{`.x`} = {x[[.x]]}", .con = con))
  set_str    <- paste(set_str, collapse = ", ")

  where_str  <- purrr::map_chr(colnames(df_key), ~glue::glue_sql("{`.x`} = {x[[.x]]}", .con = con))
  where_str  <- paste(where_str, collapse = " AND ")

  update_str <- glue::glue("UPDATE {schema_name}.{table_name} SET {set_str} WHERE {where_str}")

  # Execute ------------------------------------------------------------------

  query_res <- DBI::dbSendQuery(con, update_str)
  rowsAffected <- dbGetRowsAffected(query_res)
  DBI::dbClearResult(query_res)

  return (rowsAffected)
}


dbDeleteCustom = function(x, con, schema_name, table_name) {
  # Dynamically generate sql delete statement
  #x: 1-row dataframe that contains 1 row for deletion.
  #returns: rows affected by deletion

  # WARNING: the columns/values of x are the conditions for deletion
  if (nrow(x) != 1) stop("Input dataframe must be exactly 1 row")

  where_str  <- purrr::map_chr(colnames(x), ~glue::glue_sql("{`.x`} = {x[[.x]]}", .con = con))
  where_str  <- paste(where_str, collapse = " AND ")

  update_str <- glue::glue("DELETE FROM {schema_name}.{table_name} WHERE {where_str}")

  # Execute ------------------------------------------------------------------

  query_res <- DBI::dbSendQuery(con, update_str)
  rowsAffected <- dbGetRowsAffected(query_res)
  DBI::dbClearResult(query_res)

  return (rowsAffected)
}


