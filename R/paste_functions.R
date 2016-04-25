#' Paste function pasting two character strings "_" as seperator
#'
#' @param a Character string
#' @param b Character string
#'
#' @return "a_b"
'%_%' <- function(a, b) paste(a, b, sep = "_")

#' Paste function pasting two character strings "." as seperator
#'
#' @param a Character string
#' @param b Character string
#'
#' @return "a.b"
'%.%' <- function(a, b) paste(a, b, sep = ".")

#' Paste function pasting two character strings "/" as seperator
#'
#' @param a Character string
#' @param b Character string
#'
#' @return "a/b"
'%/%' <- function(a, b) paste(a, b, sep = "/")
