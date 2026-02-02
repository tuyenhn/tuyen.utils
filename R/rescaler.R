# Rescale any vector from 0 to 1 (optionally with a multiplier)

#
rescaler <- function(x, mult = 1) {
  .validate_rescale_input(x)
  .rescaled <- ((x - min(x)) / (max(x) - min(x))) * mult

  structure(
    list(
      vec = .rescaled,
      orig_min = min(x),
      orig_max = max(x),
      mult = mult
    ),
    class = "rescaled"
  )
}

.validate_rescale_input <- function(x) {
  if (!inherits(x, "numeric")) {
    stop("Input requires a numeric vector")
  }
}

.assert_rescaled <- function(x) {
  if (!inherits(x, "rescaled")) {
    stop("A `rescaled` object is required")
  }
}

unnormalise <- function(x, new_vec = NULL) {
  .assert_rescaled(x)
  .lvs <- unclass(x)
  .vec <- if (is.null(new_vec)) {
    .lvs$vec
  } else {
    new_vec
  }
  ((.vec / .lvs$mult) * (.lvs$orig_max - .lvs$orig_min)) + .lvs$orig_min
}


print.rescaled <- function(x) {
  print(x$vec)
}
