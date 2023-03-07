#' Determined n-tile ranks
#' 
#' @param x A numeric vector.
#' @param n A positive integer.
#' @return An integer vector with n-tile rank indices (1 being the highest).
## after `dplyr::ntile`
ntile <- function (x, n = 5L) {
  x <- rank(x, ties.method = "first", na.last = "keep")
  len <- length(x) - sum(is.na(x))
  n <- as.integer(floor(n))
  n_larger <- as.integer(len%%n)
  n_smaller <- as.integer(n - n_larger)
  size <- len/n
  larger_size <- as.integer(ceiling(size))
  smaller_size <- as.integer(floor(size))
  larger_threshold <- larger_size * n_larger
  bins <- ifelse(x <= larger_threshold, (x + (larger_size - 
      1L))/larger_size, (x + (-larger_threshold + smaller_size - 
      1L))/smaller_size + n_larger)
  as.integer(floor(bins))
}

#' Stratified n-tile ranks
#' 
#' @param x A numeric vector.
#' @param by Values of `x` ntile-d by each unique value of `by` (stratum).
#' @return An integer vector with n-tile rank indices by strata.
zone <- function(x, by) {
  v <- unique(by)
  o <- numeric(length(x))
  for (i in v) {
    o[by == i] <- 6L - ntile(x[by == i])
  }
  o
}

#' Process regions data
#' 
#' @param dat The data list.
#' @param region Character, the region name (one from `names(dat)`).
#' @param inputs A list of input parameters for weights or `NULL`.
#' @param custom_var A user provided variable (`CustomVariable`), used only when it is 
#'   not `NULL` and the vector contains some non `NA` values (comes from user uploaded xlsx file).
#'   Values are expected to match the corresponding data frame's UIDs (e.g. `dat[["Boreal"]]`).
#' @return A data frame with the region's data, including metadata attributes.
wght_zone <- function(dat, region, inputs = NULL, custom_var = NULL) {
  if (is.null(inputs))
    inputs <- list(
      w_B4BNorm = 1,
      w_CUNorm = 1,
      w_NrmPl20 = 1,
      w_NormCor = 1,
      w_UWRNorm = 1,
      w_Custom = 0)

  if (!is.null(custom_var) && !all(is.na(custom_var))) {
    cust_var <- as.numeric(custom_var)
    # range normalization to make sure custom variable is in the 0-1 range
    rg <- range(cust_var, na.rm=TRUE)
    cust_var <- (cust_var - rg[1]) / diff(rg)
    cust_var[is.na(cust_var)] <- 0
    ## check if range matters
  } else {
    ## ignore weights when no custom data provided
    cust_var <- 0
    inputs$w_Custom <- 0
  }

  message("Updating ", region, " | weight = ", inputs$w_Custom)
  #message("call: ", deparse(sys.calls()[[sys.nframe()-1]]))
  
  # boreal: B4BNorm is multiplied with CUNorm
  if (region == "Boreal") {
    d <- dat$Boreal
    if (is.null(custom_var)) {
      # multiplicative formula as in the report w/o custom variable
      d$B4Bwght <- (inputs$w_B4BNorm * d$B4BNorm) * 
        (inputs$w_CUNorm * d$CUNorm)
    } else {
      # additive formula when custom variable is provided
      d$B4Bwght <- (inputs$w_B4BNorm * d$B4BNorm) + 
        (inputs$w_CUNorm * d$CUNorm) +
        (inputs$w_Custom * cust_var)
    }
    d$Zone <- zone(d$B4Bwght, d$HERD_NA)
  }
  # central: B4BNorm added with CUNorm
  if (region == "Central") {
    d <- dat$Central
    d$B4Bwght <- inputs$w_B4BNorm * d$B4BNorm + 
      inputs$w_CUNorm * d$CUNorm +
      (inputs$w_Custom * cust_var)
    d$Zone <- zone(d$B4Bwght, d$HERD_NA)
  }
  # SouthernNorthern (B4BNWgh): B4BNorm, NrmPl20, NormCor, and UWRNorm are added
  # B4BNorm + NrmPl20 + NormCore + UWRNorm
  if (region == "Southern") {
    d <- dat$Southern
    d$B4Bwght <- inputs$w_B4BNorm * d$B4BNorm + 
      inputs$w_NrmPl20 * d$NrmPl20 + 
      inputs$w_NormCor * d$NormCor + 
      inputs$w_UWRNorm * d$UWRNorm +
      (inputs$w_Custom * cust_var)
    d$Zone <- zone(d$B4Bwght, d$HERD_NA)
  }
  if (region == "Northern") {
    d <- dat$Northern
    d$B4Bwght <- inputs$w_B4BNorm * d$B4BNorm + 
      inputs$w_NrmPl20 * d$NrmPl20 + 
      inputs$w_NormCor * d$NormCor + 
      inputs$w_UWRNorm * d$UWRNorm +
      (inputs$w_Custom * cust_var)
    d$Zone <- zone(d$B4Bwght, d$HERD_NA)
  }
  if (!is.null(custom_var)) {
    d$CustomVariable <- custom_var
    print(summary(custom_var * inputs$w_Custom))
  }
  d
}
# str(wght_zone(dat, "Central"))
# str(wght_zone(dat, "Boreal"))
# str(wght_zone(dat, "Southern"))
# str(wght_zone(dat, "Northern"))
