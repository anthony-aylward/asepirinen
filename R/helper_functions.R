#===============================================================================
# helper_functions.R
#===============================================================================

# Imports ======================================================================

#' @import parallel
#' @import plyr




# Functions ====================================================================

#' @title Apply GTM to individual variants
#'
#' @description Apply GTM to each of a list of variants
#'
#' @param counts list containing allele count data for each variant
#' @param pr_beta vector, parameters for beta priors
#' @param pr_intv determines if domain should be truncated, by default it is not
#' @param niter number of iterations
#' @param burnin number of burn-in steps
#' @param two_sided if TRUE, use two-sided beta priors
#' @param independent independent
#' @return list, the GTM result for each variant
#' @export
apply_gtm_per_variant <- function(
  counts,
  pr_beta = c(2000, 2000, 36, 12, 80, 1),
  pr_intv = rep(NA, 6),
  niter = 2000,
  burnin = 10,
  two_sided = TRUE,
  independent = FALSE,
  cores = 1
) {
  mclapply(
    counts,
    function(count_matrix) {
      gtm(
        count_matrix,
        pr.beta = pr_beta,
        pr.intv = pr_intv,
        niter = niter,
        burnin = burnin,
        two.sided = two_sided,
        independent = independent
      )
    },
    mc.cores = cores
  )
}

#' @title Extract Posteriors Row
#'
#' @description Extract a row of the posteriors table from GTM results
#'
#' @param single_variant_result the GTM result for a single variant
#' @return data frame, a row of the posteriors table
extract_posteriors_row <- function(single_variant_result) {
  setNames(
    as.data.frame(
      single_variant_result[["state.posteriors"]],
      stringsAsFactors = FALSE
    ),
    names(single_variant_result[["state.posteriors"]])
  )
}

#' @title Construct posteriors data frame
#'
#' @description convert GTM results to a data frame of variants and posterior
#'   probabilities
#'
#' @param res_list list of GTM results
#' @return data frame, posterior probabilities from GTM for each variant
#' @export
construct_posteriors_frame <- function(res_list) {
  cbind(
    list(ID=names(res_list)),
    as.data.frame(
      lapply(rbind.fill(lapply(res_list, extract_posteriors_row)), unlist)
    )  
  )
}