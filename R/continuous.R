pev_fcont <- function(hcl_param) {

  # if type is diverging, construct two sequential scales and compose


  f <- function(x) {
    seqhcl(
      i = x,
      h1 = hcl_param$h1,
      h2 = hcl_param$h2,
      c1 = hcl_param$c1,
      c2 = hcl_param$c2,
      l1 = hcl_param$l1,
      l2 = hcl_param$l2,
      p1 = hcl_param$p1,
      p2 = hcl_param$p2,
      cmax = hcl_param$cmax,
      fixup = hcl_param$fixup
    )
  }

  f
}
