## select.mgmt.cnop(mgmt_cnop) --------------------------------------------
select.mgmt.cnop <- function(mgmt_cnop){
  prob_sel <- which(mgmt_cnop$fraction$FRAC >= runif(1))[1]
  mgmt_cnop$fraction$NAME[prob_sel]
}
