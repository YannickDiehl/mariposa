# =============================================================================
# Package load hook — conditional S3 method registration for broom
# =============================================================================
# linear_regression / logistic_regression results inherit from "lm" / "glm",
# so broom's default lm/glm tidiers dispatch — but those tidiers internally
# call summary(x), which (with our specialised summary.linear_regression /
# summary.logistic_regression methods overriding the lm/glm ones) returns
# mariposa's SPSS-style summary object instead of the lm/glm summary that
# broom expects. The result: broom::glance() raises "object 'r.squared' not
# found"; broom::tidy() returns a truncated 4-column tibble instead of the
# expected std.error / statistic / p.value columns.
#
# The fix: register explicit tidy / glance / augment methods on our classes,
# which strip the linear_regression / logistic_regression class and delegate
# to broom's lm / glm tidiers. With our class removed, the inner summary()
# call inside broom's tidier dispatches correctly to summary.lm / summary.glm.
#
# Registration uses the standard vendored s3_register() pattern (originally
# from rlang's standalone-s3-register helper) so that broom is only a soft
# (Suggests) dependency: if broom is not installed, nothing is registered.

.onLoad <- function(libname, pkgname) {
  s3_register("broom::tidy",    "linear_regression")
  s3_register("broom::glance",  "linear_regression")
  s3_register("broom::augment", "linear_regression")
  s3_register("broom::tidy",    "logistic_regression")
  s3_register("broom::glance",  "logistic_regression")
  s3_register("broom::augment", "logistic_regression")
}


# Vendored from rlang's standalone-s3-register.R helper (MIT). Registers an
# S3 method for a generic that lives in a Suggests-only namespace, deferring
# the registration until the owning package is actually loaded.
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class),   length(class)   == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_fn <- paste0("get_method_", class)
  if (is.null(method)) {
    method_fn <- get(paste0(generic, ".", class), envir = caller)
  } else {
    stopifnot(is.function(method))
    method_fn <- method
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)
      method_fn <- get(paste0(generic, ".", class), envir = caller)
      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}
