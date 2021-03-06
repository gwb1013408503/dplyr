#' Create or transform variables
#'
#' `mutate()` adds new variables and preserves existing ones;
#' `transmute()` adds new variables and drops existing ones.  Both
#' functions preserve the number of rows of the input.
#' New variables overwrite existing variables of the same name.
#'
#' @section Useful functions available in calculations of variables:
#'
#' * [`+`], [`-`], [log()], etc., for their usual mathematical meanings
#'
#' * [lead()], [lag()]
#'
#' * [dense_rank()], [min_rank()], [percent_rank()], [row_number()],
#'   [cume_dist()], [ntile()]
#'
#' * [cumsum()], [cummean()], [cummin()], [cummax()], [cumany()], [cumall()]
#'
#' * [na_if()], [coalesce()]
#'
#' * [if_else()], [recode()], [case_when()]
#'
#' @section Grouped tibbles:
#'
#' Because mutating expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped mutate:
#'
#' ```
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' The former normalises `mass` by the global average whereas the
#' latter normalises by the averages within gender levels.
#'
#' `mutate()` does not evaluate the expressions when the group is empty.
#'
#' @section Scoped mutation and transmutation:
#'
#' The three [scoped] variants of `mutate()` ([mutate_all()],
#' [mutate_if()] and [mutate_at()]) and the three variants of
#' `transmute()` ([transmute_all()], [transmute_if()],
#' [transmute_at()]) make it easy to apply a transformation to a
#' selection of variables.
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Name-value pairs of expressions,
#'   each with length 1 or the same length as the number of rows in the group
#'   (if using [group_by()]) or in the entire input (if not using groups).
#'   The name of each argument will be the name of a new variable, and the
#'   value will be its corresponding value. Use a `NULL` value in `mutate`
#'   to drop a variable.  New variables overwrite existing variables
#'   of the same name.
#' @family single table verbs
#' @return An object of the same class as `.data`.
#' @examples
#' # Newly created variables are available immediately
#' mtcars %>% as_tibble() %>% mutate(
#'   cyl2 = cyl * 2,
#'   cyl4 = cyl2 * 2
#' )
#'
#' # You can also use mutate() to remove variables and
#' # modify existing variables
#' mtcars %>% as_tibble() %>% mutate(
#'   mpg = NULL,
#'   disp = disp * 0.0163871 # convert to litres
#' )
#'
#'
#' # window functions are useful for grouped mutates
#' mtcars %>%
#'  group_by(cyl) %>%
#'  mutate(rank = min_rank(desc(mpg)))
#' # see `vignette("window-functions")` for more details
#'
#' # You can drop variables by setting them to NULL
#' mtcars %>% mutate(cyl = NULL)
#'
#' # mutate() vs transmute --------------------------
#' # mutate() keeps all existing variables
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237)
#'
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute(displ_l = disp / 61.0237)
#'
#'
#' # The mutate operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' # The following normalises `mass` by the global average:
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Whereas this normalises `mass` by the averages within gender
#' # levels:
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Note that you can't overwrite grouping variables:
#' gdf <- mtcars %>% group_by(cyl)
#' try(mutate(gdf, cyl = cyl * 100))
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])
#' # Learn more in ?dplyr_tidy_eval
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

#' @export
mutate.tbl_df <- function(.data, ...) {
  new_columns <- mutate_new_columns(.data, ...)
  mutate_finish(.data, new_columns)
}

#' @export
mutate.data.frame <- function(.data, ...) {
  as.data.frame(mutate(as_tibble(.data), ...))
}


#' @export
transmute.tbl_df <- function(.data, ...) {
  new_columns <- mutate_new_columns(.data, ...)

  out <- .data[, group_vars(.data), drop = FALSE]
  new_column_names <- names(new_columns)
  for (i in seq_along(new_columns)) {
    if (!inherits(new_columns[[i]], "rlang_zap")) {
      out[[new_column_names[i]]] <-  new_columns[[i]]
    }
  }

  # Preserve order of existing variables
  out <- out[union(intersect(names(.data), names(out)), names(out))]

  # copy back attributes
  # TODO: challenge that with some vctrs theory
  atts <- attributes(.data)
  atts <- atts[! names(atts) %in% c("names", "row.names", "groups", "class")]
  for(name in names(atts)) {
    attr(out, name) <- atts[[name]]
  }

  out
}

#' @export
transmute.data.frame <- function(.data, ...) {
  as.data.frame(transmute(as_tibble(.data), ...))
}

# Helpers -----------------------------------------------------------------

mutate_new_columns <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }
  rows_lengths <- .Call(`dplyr_vec_sizes`, rows)

  o_rows <- vec_order(vec_c(!!!rows, .ptype = integer()))
  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))
  if (length(dots) == 0L) {
    return(list())
  }

  new_columns <- list()

  for (i in seq_along(dots)) {
    # a list in which each element is the result of
    # evaluating the quosure in the "sliced data mask"
    # recycling it appropriately to match the group size
    #
    # TODO: reinject hybrid evaluation at the R level
    c(chunks, needs_recycle) %<-% mask$eval_all_mutate(dots[[i]], dots_names, i)

    if (is.null(chunks)) {
      if (!is.null(dots_names) && dots_names[i] != "" && dots_names[[i]] %in% c(names(.data), names(new_columns))) {
        new_columns[[dots_names[i]]] <- zap()
        mask$remove(dots_names[i])
      }
      next
    }

    if (needs_recycle) {
      chunks <- map2(chunks, rows_lengths, function(chunk, n) {
        vec_recycle(chunk, n)
      })
    }
    result <- vec_slice(vec_c(!!!chunks), o_rows)

    not_named <- (is.null(dots_names) || dots_names[i] == "")
    if (not_named && is.data.frame(result)) {
      new_columns[names(result)] <- result

      # remember each result separately
      map2(seq_along(result), names(result), function(i, nm) {
        mask$add(nm, pluck(chunks, i))
      })
    } else {
      name <- if (not_named) auto_named_dots[i] else dots_names[i]

      # treat as a single output otherwise
      new_columns[[name]] <- result

      # remember
      mask$add(name, chunks)
    }

  }

  new_columns
}

mutate_finish <- function(.data, new_columns) {
  if (!length(new_columns)) {
    return(.data)
  }

  out <- .data
  new_column_names <- names(new_columns)
  for (i in seq_along(new_columns)) {
    out[[new_column_names[i]]] <- if (!inherits(new_columns[[i]], "rlang_zap")) new_columns[[i]]
  }

  # Re-group if needed
  groups <- group_vars(.data)
  if (any(groups %in% names(new_columns))) {
    out <- grouped_df(out, intersect(groups, names(.data)))
  }

  # copy back attributes
  # TODO: challenge that with some vctrs theory
  atts <- attributes(.data)
  atts <- atts[! names(atts) %in% c("names", "row.names", "groups", "class")]
  for(name in names(atts)) {
    attr(out, name) <- atts[[name]]
  }

  out
}
