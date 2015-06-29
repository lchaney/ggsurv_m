ggsurv_m <- function(
  s,
  CI         = 'def',
  plot.cens  = TRUE,
  surv.col   = 'gg.def',
  cens.col   = 'red',
  lty.est    = 1,
  lty.ci     = 2,
  cens.shape = 3,
  size.est   = 0.5,
  size.ci    = 0.5,
  size.cens  = 2,
  back.white = FALSE,
  xlab       = 'Time',
  ylab       = 'Survival',
  main       = '',
  strata     = length(s$strata)
) {
  n <- s$strata
  
  strataEqualNames <- unlist(strsplit(names(s$strata), '='))
  groups <- factor(
    strataEqualNames[seq(2, 2 * strata, by = 2)]
  )
  
  gr.name <-  strataEqualNames[1]
  gr.df   <- vector('list', strata)
  n.ind   <- cumsum(c(0,n))
  
  for (i in 1:strata) {
    indI <- (n.ind[i]+1):n.ind[i+1]
    gr.df[[i]] <- data.frame(
      time  = c(0, s$time[ indI ]),
      surv  = c(1, s$surv[ indI ]),
      up    = c(1, s$upper[ indI ]),
      low   = c(1, s$lower[ indI ]),
      cens  = c(0, s$n.censor[ indI ]),
      group = rep(groups[i], n[i] + 1)
    )
  }
  
  dat      <- do.call(rbind, gr.df)
  dat.cens <- subset(dat, cens != 0)
  
  pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
    geom_step(aes(col = group, lty = group), size = size.est) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main)
  
  pl <- if(surv.col[1] != 'gg.def'){
    scaleValues <- if (length(surv.col == 1)) {
      rep(surv.col, strata)
    } else{
      surv.col
    }
    pl + scale_colour_manual(name = gr.name, values = scaleValues)
    
  } else {
    pl + scale_colour_discrete(name = gr.name)
  }
  
  lineScaleValues <- if (length(lty.est) == 1) {
    rep(lty.est, strata)
  } else {
    lty.est
  }
  pl <- pl + scale_linetype_manual(name = gr.name, values = lineScaleValues)
  
  if(identical(CI,TRUE)) {
    if(length(surv.col) > 1 && length(lty.est) > 1){
      stop('Either surv.col or lty.est should be of length 1 in order to plot 95% CI with multiple strata')
    }
    
    stepLty <- if ((length(surv.col) > 1 | surv.col == 'gg.def')[1]) {
      lty.ci
    } else{
      surv.col
    }
    pl <- pl +
      geom_step(aes(y = up, lty = group), lty = stepLty, size = size.ci) +
      geom_step(aes(y = low,lty = group), lty = stepLty, size = size.ci)
  }
  
  if (identical(plot.cens, TRUE) ){
    if (length(dat.cens) == 0) {
      stop ('There are no censored observations')
    }
    pl <- pl + geom_point(
      data    = dat.cens,
      mapping = aes(y = surv),
      shape   = cens.shape,
      col     = cens.col,
      size    = size.cens
    )
  }
  
  if(identical(back.white, TRUE)) {
    pl <- pl + theme_bw()
  }
  
  pl
}