pev_fcont <- function(hcl_param) {

  # if type is diverging, construct two sequential scales and compose
  if (identical(hcl_param$type, "diverging")) {

    hcl_low <- pev_hcl_param(
      type = "sequential",
      h1 = hcl_param$h1,
      c1 = hcl_param$c1,
      c2 = 0,
      l1 = hcl_param$l1,
      l2 = hcl_param$l2,
      p1 = hcl_param$p1,
      p2 = hcl_param$p2,
      cmax = hcl_param$cmax,
      fixup = hcl_param$fixup
    )

    hcl_high <- pev_hcl_param(
      type = "sequential",
      h1 = hcl_param$h2,
      c1 = hcl_param$c1,
      c2 = 0,
      l1 = hcl_param$l1,
      l2 = hcl_param$l2,
      p1 = hcl_param$p1,
      p2 = hcl_param$p2,
      cmax = hcl_param$cmax,
      fixup = hcl_param$fixup
    )

    f <- pev_fcont_div(pev_fcont(hcl_low), pev_fcont(hcl_high))

    return(f)
  }

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

pev_fcont_div <- function(pev_fcont_low, pev_fcont_high) {

  f <- function(x) {
    x_rescale <- abs(x - 0.5) / 0.5

    ifelse(
      x < 0.5,
      pev_fcont_low(x_rescale),
      pev_fcont_high(x_rescale)
    )
  }

  f
}


