#' Run bestNormalize transformation for \code{recipes} implementation
#' 
#' @aliases step_bestNormalize step_bestNormalize_new
#'
#' @description `step_best_normalize` creates a specification of a recipe step
#'   (see `recipes` package) that will transform data using the best of a suite
#'   of normalization transformations estimated (by default) using
#'   cross-validation.
#'
#' @param recipe A formula or recipe
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details. For the `tidy`
#'   method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param transform_info A numeric vector of transformation values. This (was
#'   transform_info) is `NULL` until computed by [prep.recipe()].
#' @param transform_options options to be passed to bestNormalize
#' @param num_unique An integer where data that have less possible values will
#'   not be evaluate for a transformation.
#' @param trained For recipes functionality
#' @param skip For recipes functionality
#' @param id For recipes functionality
#'
#'
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms` (the selectors or variables selected) and `value` (the
#'   lambda estimate).
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#'
#' @details The bestnormalize transformation can be used to rescale a variable
#'   to be more similar to a normal distribution. See `?bestNormalize` for more
#'   information; `step_best_normalize` is the implementation of `bestNormalize`
#'   in the `recipes` context. 
#'   
#'   As of version 1.7, the `butcher` package can be used to (hopefully) improve 
#'   scalability of this function on bigger data sets. 
#'
#' @examples
#'
#' library(recipes)
#' rec <- recipe(~ ., data = as.data.frame(iris))
#'
#' bn_trans <- step_best_normalize(rec, all_numeric())
#'
#' bn_estimates <- prep(bn_trans, training = as.data.frame(iris))
#'
#' bn_data <- bake(bn_estimates, as.data.frame(iris))
#'
#' plot(density(iris[, "Petal.Length"]), main = "before")
#' plot(density(bn_data$Petal.Length), main = "after")
#'
#' tidy(bn_trans, number = 1)
#' tidy(bn_estimates, number = 1)
#'
#' @seealso  \code{\link[bestNormalize]{bestNormalize}} \code{\link{orderNorm}},
#'   [recipe()] [prep.recipe()] [bake.recipe()]
#'
#' @importFrom recipes recipe rand_id add_step ellipse_check step
#'   
step_best_normalize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           transform_info = NULL,
           transform_options = list(),
           num_unique = 5,
           skip = FALSE,
           id = rand_id("best_normalize")) {
    add_step(
      recipe,
      step_best_normalize_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        transform_info = transform_info,
        transform_options = transform_options,
        num_unique = num_unique,
        skip = skip,
        id = id
      )
    )
  }

step_best_normalize_new <-
  function(terms, role, trained, transform_info, transform_options, num_unique, skip, id) {
    step(
      subclass = "best_normalize",
      terms = terms,
      role = role,
      trained = trained,
      transform_info = transform_info,
      transform_options = transform_options,
      num_unique = num_unique,
      skip = skip,
      id = id
    )
  }

#' @export
#' @importFrom recipes prep recipes_eval_select check_type
prep.step_best_normalize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])
  
  values <- apply(
    training[, col_names],
    2, 
    estimate_bn,
    transform_options = x$transform_options,
    num_unique = x$num_unique
  )
  
  step_best_normalize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    transform_info = values,
    transform_options = x$transform_options,
    num_unique = x$num_unique,
    skip = x$skip,
    id = x$id
  )
}

#' @export
#' @importFrom recipes bake
#' @importFrom tibble as_tibble
bake.step_best_normalize <- function(object, new_data, ...) {
  if (length(object$transform_info) == 0)
    return(as_tibble(new_data))
  param <- names(object$transform_info)
  for (i in seq_along(object$transform_info))
    new_data[, param[i]] <- 
      predict(object$transform_info[[param[i]]], getElement(new_data, param[i]), warn = FALSE)
  as_tibble(new_data)
}
#' @export
#' @importFrom recipes print_step
print.step_best_normalize <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "bestNormalize transformation on "
    print_step(names(x$transform_info), x$terms, x$trained, width = width, title = title)
    invisible(x)
  }

## estimates the transformations
estimate_bn <- function(dat,
                        transform_options = list(),
                        num_unique = 5) {
  
  # Returns the identity transformation if not enough unique values
   if (length(unique(dat)) < num_unique)
    return(no_transform(dat))
  
  transform_options$x <- dat
  
  # Set some new defaults to bestNormalize unless otherwise specified 
  if(is.null(transform_options$r))
     transform_options$r <- 1 
  if(is.null(transform_options$warn))
     transform_options$warn <- FALSE 
  if(is.null(transform_options$quiet))
     transform_options$quiet <- TRUE
  
  res <- do.call(bestNormalize, transform_options)
  res
}

#' @rdname step_best_normalize
#' @param x A `step_best_normalize` object.
#' @export
#' @importFrom recipes tidy is_trained sel2char
#' @importFrom tibble tibble
#' 
tidy.step_best_normalize <- function(x, ...) {
  if (is_trained(x)) {
    val <- lapply(x$transform_info, tidy)
    chosen <- sapply(val, function(xx) xx$transform[xx$chosen == 1])
    res <- tibble(terms = names(x$transform_info),
                  chosen_transform = chosen,
                  cv_info = val)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, chosen = as.double(NA), cv_info = as.double(NA))
  }
  res$id <- x$id
  res
}

#' @export
step_bestNormalize <- function(...) {
  .Deprecated("step_best_normalize", package = "bestNormalize", old = "step_bestNormalize") 
  step_best_normalize(...)
}

#' @export
step_bestNormalize_new <- function(...) {
  .Deprecated("step_best_normalize_new", package = "bestNormalize", old = "step_bestNormalize_new") 
  step_best_normalize_new(...)
}

#' @rdname step_best_normalize
#' @param x A `step_best_normalize` object.
#' @importFrom butcher axe_env
#' @importFrom purrr map
#' @export
axe_env.step_best_normalize <- function(x, ...) {
  x$terms <- purrr::map(x$terms, function(z) butcher::axe_env(z, ...))
  x
}

#' @export
required_pkgs.step_best_normalize <- function(x, ...) {
  c("bestNormalize")
}
