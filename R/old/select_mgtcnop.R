## select_cnop(mgt_cnop) --------------------------------------------
select_mgtcnop <- function(mgt_cnop){
  prob_sel <- which(mgt_cnop$fraction$FRAC >= runif(1))[1]
  mgt_cnop$fraction$NAME[prob_sel]
}
