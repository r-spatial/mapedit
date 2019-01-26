#' Merge 'sf' Edits
#'
#' Internal function used with \code{editFeatures} to apply edits
#'   to a \code{sf} object.
#'
#' @param orig \code{sf} with the original or source data to which
#'          deletes should apply
#' @param edits \code{sf} with sf data to edit
#' @param by named \code{vector} with the name of the vector representing
#'          the column in orig that we will use to match and the value of
#'          the vector representing the column in edits that we will
#'          use to match.  The argument is intended to work like
#'          the \code{*join} functions in \code{dplyr}. Note, this function
#'          will only use the first name
#'          and first value of the vector for matching.
#' @keywords internal

merge_edit <- function(
  orig = NULL, edits = NULL, by = c("id" = "layerId")
) {

  # if edits is empty return orig sf
  if(is.null(edits)) {
    return(orig)
  }
  # if orig is empty then just return drawn sf
  if(is.null(orig)) {
    return(edits)
  }

  # make a copy
  orig2 <- orig

  orig_ids = orig2[[names(by)[1]]]

  edit_ids = edits[, by[[1]], drop=TRUE]

  mapply(
    function(ed, ed_id) {
      matched_id_row = which(orig_ids == ed_id)

      # get type of original
      orig_type <- as.character(sf::st_geometry_type(
        sf::st_geometry(orig[matched_id_row,])
      ))

      tryCatch(
        sf::st_geometry(orig2)[matched_id_row] <<- sf::st_geometry(sf::st_cast(
          sf::st_sfc(ed),
          orig_type
        )),
        error = function(e) {
          sf::st_geometry(orig2)[matched_id_row] <<- ed
          warning(
            paste0("Unable to cast back to original type - ", e$message, " - but this is often caused by intermediate step."),
            call. = FALSE
          )
        }
      )
      return(NULL)
    },
    sf::st_geometry(edits),
    edit_ids
  )

  #matched_id_rows = which(orig_ids %in% edit_ids)

  # cast edits to original type
  #sf::st_geometry(edits) <- sf::st_sfc(mapply(
  #  function(ed, type) {
  #    sf::st_cast(ed, type)
  #  },
  #  sf::st_geometry(edits),
  #  as.character(
  #    sf::st_geometry_type(
  #      sf::st_geometry(orig2)
  #    )[matched_id_rows]
  #  ),
  #  SIMPLIFY = FALSE
  #))

  #sf::st_geometry(orig2)[matched_id_rows] <- sf::st_geometry(edits)

  orig2
}


#' Merge 'sf' Deletes
#'
#' Internal function used with \code{editFeatures} to apply deletes
#'   to a \code{sf} object.
#'
#' @param orig \code{sf} with the original or source data to which
#'          deletes should apply
#' @param deletes \code{sf} with sf data to delete
#' @param by named \code{vector} with the name of the vector representing
#'          the column in orig that we will use to match and the value of
#'          the vector representing the column in deletes that we will
#'          use to match.  The argument is intended to work like
#'          the \code{*join} functions in \code{dplyr}. Note, this function
#'          will only use the first name
#'          and first value of the vector for matching.
#' @keywords internal

merge_delete <- function(
  orig = NULL, deletes = NULL, by = c("id" = "layerId")
) {

  if(is.null(deletes)) {
    return(orig)
  }

  orig_ids = orig[,names(by)[1], drop = TRUE]
  del_ids = deletes[,by[[1]], drop=TRUE]

  orig[which(!(orig_ids %in% del_ids)),]
}


#' Merge 'sf' Adds
#'
#' Internal function used with \code{editFeatures} to apply adds or drawn
#'   to a \code{sf} object.
#'
#' @param orig \code{sf} with the original or source data to which
#'          adds should apply
#' @param drawn \code{sf} with sf data to add to orig
#' @param by not used in merge_add.  This argument only exists
#'          for symmetry with the other merge functions.
#'
#' @keywords internal
merge_add <- function(orig = NULL, drawn = NULL, by = NULL) {

  # if drawn is empty return orig sf
  if(is.null(drawn)) {
    return(orig)
  }

  # if orig is empty then just return drawn sf
  if(is.null(orig)) {
    return(drawn)
  }

  # use mapedit internal function to combine orig and drawn
  combine_list_of_sf(
    list(orig, drawn)
  )
}

