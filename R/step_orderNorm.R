#' ORQ normalization (orderNorm) for \code{recipes} implementation
#'
#' @description `step_orderNorm` creates a specification of a recipe step (see
#'   `recipes` package) that will transform data using the ORQ (orderNorm)
#'   transformation, which approximates the "true" normalizing tranformation if
#'   one exists. This is considerably faster than `step_bestNormalize`.
#'
#' @param recipe A formula or recipe
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details. For the `tidy`
#'   method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param transform_info A numeric vector of transformation values. This (was
#'   transform_info) is `NULL` until computed by [prep.recipe()].
#' @param transform_options options to be passed to orderNorm
#' @param num_unique An integer where data that have less possible values will
#'   not be evaluate for a transformation.
#' @param trained For recipes functionality
#' @param skip For recipes functionality
#' @param id For recipes functionality
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
#' @details The orderNorm transformation can be used to rescale a variable to be
#'   more similar to a normal distribution. See `?orderNorm` for more
#'   information; `step_orderNorm` is the implementation of `orderNorm` in the
#'   `recipes` context.
#'   
#'   As of version 1.7, the `butcher` package can be used to (hopefully) improve 
#'   scalability of this function on bigger data sets. 
#'
#' @examples
#' library(recipes)
#' rec <- recipe(~ ., data = as.data.frame(iris))
#'
#' orq_trans <- step_orderNorm(rec, all_numeric())
#'
#' orq_estimates <- prep(orq_trans, training = as.data.frame(iris))
#'
#' orq_data <- bake(orq_estimates, as.data.frame(iris))
#'
#' plot(density(iris[, "Petal.Length"]), main = "before")
#' plot(density(orq_data$Petal.Length), main = "after")
#'
#' tidy(orq_trans, number = 1)
#' tidy(orq_estimates, number = 1)
#'
#'
#' @seealso  \code{\link[bestNormalize]{orderNorm}} \code{\link{bestNormalize}},
#'   [recipe()] [prep.recipe()] [bake.recipe()]
#'
#' @references Ryan A. Peterson (2019). Ordered quantile normalization: a
#'   semiparametric transformation built for the cross-validation era. Journal
#'   of Applied Statistics, 1-16.
#'
#' @importFrom recipes recipe rand_id add_step ellipse_check step
#'   
step_orderNorm <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           transform_info = NULL,
           transform_options = list(),
           num_unique = 5,
           skip = FALSE,
           id = rand_id("orderNorm")) {
    add_step(
      recipe,
      step_orderNorm_new(
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

step_orderNorm_new <-
  function(terms, role, trained, transform_info, transform_options, num_unique, skip, id) {
    step(
      subclass = "orderNorm",
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
#' @importFrom recipes prep terms_select check_type
prep.step_orderNorm <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])
  
  values <- apply(
    training[, col_names],
    2, 
    estimate_orq,
    transform_options = x$transform_options,
    num_unique = x$num_unique
  )
  
  step_orderNorm_new(
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
#' @importFrom tibble as_tibble
bake.step_orderNorm <- function(object, new_data, ...) {
  if (length(object$transform_info) == 0)
    return(as_tibble(new_data))
  param <- names(object$transform_info)
  for (i in seq_along(object$transform_info))
    new_data[, param[i]] <- 
      predict(object$transform_info[[param[i]]], getElement(new_data, param[i]), warn = FALSE)
  as_tibble(new_data)
}

#' @export
#' @importFrom recipes printer
print.step_orderNorm <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("orderNorm transformation on ", sep = "")
    printer(names(x$transform_info), x$terms, x$trained, width = width)
    invisible(x)
  }

## estimates the transformations
estimate_orq <- function(dat,
                        transform_options = list(),
                        num_unique = 5) {
  
  # Returns the identity transformation if not enough unique values
  if (length(unique(dat)) < num_unique)
    return(no_transform(dat))
  
  transform_options$x <- dat
  
  # Set some new defaults to orderNorm unless otherwise specified 
if(is.null(transform_options$warn))
    transform_options$warn <- FALSE 
  
  res <- do.call(orderNorm, transform_options)
  res
}

#' @rdname step_orderNorm
#' @param x A `step_orderNorm` object.
#' @export
#' @importFrom recipes tidy is_trained sel2char
#' @importFrom tibble tibble
tidy.step_orderNorm <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$transform_info),
                  value = x$transform_info)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = as.double(NA))
  }
  res$id <- x$id
  res
}

#' @rdname step_orderNorm
#' @param x A `step_orderNorm` object.
#' @importFrom butcher axe_env
#' @importFrom purrr map
#' @export
axe_env.step_orderNorm <- function(x, ...) {
  x$terms <- purrr::map(x$terms, function(z) butcher::axe_env(z, ...))
  x
}
