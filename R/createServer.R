



tlimits <- function(m, d=NULL) {
  if (is.null(d)) d <- extractModelData(m)
  m0 <- refitModel(m, NULL, d)
  preds <- attr(terms(m), "term.labels")
  an <- anova(m)
  df <- an[-nrow(an),]$Df
  preds <- preds[df ==1]
  fits <- sapply(preds,function(p) {
    f <- update(m0, paste0("~ . + ", p),data=d)
    summary(f)$coefficients[2,3]
  })
  r <- range(fits)
  c(min(r[1],-3), max(r[2],3))*1.05
}

flimits <- function(m, d=NULL) {
  if (is.null(d)) d <- extractModelData(m)
  m0 <- refitModel(m, NULL, d)
  preds <- attr(terms(m), "term.labels")
  fits <- sapply(preds,function(p) {
    f <- update(m0, paste0("~ . + ", p),data=d)
    anova(m0,f)$F[2]
  })
  r <- max(fits)
  c(0, max(r,9)*1.05)
}


slimits <- function(m, d=NULL) {
  if (is.null(d)) d <- extractModelData(m)
  m0 <- refitModel(m, NULL, d)
  preds <- attr(terms(m), "term.labels")
  fits <- sapply(preds,function(p) {
    f <- update(m0, paste0("~ . + ", p),data=d)
    anova(m0,f)[2,4]
  })
  r <- max(fits)
  c(0, r*1.05)
}

cilimits <- function(m, d=NULL) {
  if (is.null(d)) d <- extractModelData(m)
  m0 <- refitModel(m, NULL, d)
  preds <- attr(terms(m), "term.labels")
  an <- anova(m)
  df <- an[-nrow(an),]$Df
  preds <- preds[df ==1]
  fits <- sapply(preds,function(p) {
    f <- update(m0, paste0("~ . + ", p),data=d)
    confint(f)[2,]
  })

  r <- c(min(fits[1,]),max(fits[2,]))
  fudge <- ifelse(r < 0,.95,1.05)
  r*fudge
}

cislimits <- function(m, d=NULL) {
  if (is.null(d)) d <- extractModelData(m)
  m0 <- refitModel(m, NULL, d)
  preds <- attr(terms(m), "term.labels")
  an <- anova(m)
  df <- an[-nrow(an),]$Df
  preds <- preds[df ==1]
  ci <- sapply(preds,function(p) {
    f <- update(m0, paste0("~ . + ", p),data=d)
    confint(f)[2,]
  })
  mmat <- model.matrix(m)
  for (i in 1:nrow(ci)){
    if (preds[i] %in% colnames(mmat)){
      x <- mmat[, preds[i]]
      s <- sd(x)
      ci[i,]<- ci[i,]*s
    }
  }
  r <- c(min(ci[1,]),max(ci[2,]))
  fudge <- ifelse(r < 0,.95,1.05)
  r*fudge
}



#' A function which returns a shiny server for Exploratory Regression
#'
#' @param ERfit the lm fit to be explored
#' @param ERdata the data used to fit the model. If NULL, attempts to extract from ERfit.
#' @param ERbarcols a vector of colours, one per term in lm.
#' Will be expanded via colorRampPalette if not the correct length.
#' @param ERnpcpCols number of colours for the PCP
#' @param pvalOrder if TRUE, re-arranges predictors in order of p-value
#'
#' @return a function
#' @import shiny
#' @import  miniUI
#' @importFrom rlang .data

createERServer <- function(ERfit,ERdata=NULL,ERbarcols=RColorBrewer::brewer.pal(4, "Set2"),ERnpcpCols=4,  pvalOrder=F){

  if (!is(ERfit, "lm") | is(ERfit, "glm"))
    stop("ERfit must be an lm and not a glm")
  if (is.null(ERdata)) ERdata <- extractModelData(ERfit)
function(input, output,session) {

  ERtermcols <- NULL
  plotAlims <-  NULL


  initER <- function(){
    if (pvalOrder)
      ERfit <<- pvalOrder(ERfit, ERdata)
    ERtermcols <<- termColours(ERfit, ERbarcols)
  }



  initER()


  options(warn=-1)

  rv <- reactiveValues(fit0=ERfit,fit1 = ERfit, fit2=revPredOrder(ERfit,ERdata))
  rd <- reactiveValues(mdata=ERdata, pcp_data= NULL,sel=NULL)
  barpYlim <- NULL
  barpXlim <- NULL
  preds <- names(ERtermcols)[1:(length(ERtermcols)-2)]

  updateFit <- function(fit,order, pred){
    fit0 <- rv$fit0
    data <- rd$mdata
    if (order =="Default"){
      fit0
    }
    else if (order == "RevDefault"){
      revPredOrder(fit0,data)
    }
    else if (order == "Forward"){
      fselOrder(fit0,data)
    }
    else if (order == "Backward"){
      bselOrder(fit0, data)
    }
    else if (order == "Random") {
      randomPredOrder(fit0,data)
    }
    else if (order == "Add") {
      addPred(fit, pred, data)
    }
    else if (order == "Remove") {
      removePred(fit, pred, data)
    }
    else if (order == "data") {
      refitModel(fit, attr(terms(fit), "term.labels"), data)
    }
    else fit
  }

  observeEvent(input$all_terms, {
    terms0 <- attr(terms(ERfit), "term.labels")

    rv$fit0 <-refitModel(rv$fit0, terms0, rd$mdata)
    rv$fit1 <- updateFit(rv$fit0, input$order1)
    rv$fit2 <- updateFit(rv$fit0, input$order2)
  })

  observeEvent(input$order1, {
    rv$fit1 <- updateFit(rv$fit1, input$order1)
  })

  observeEvent(input$order2, {
    rv$fit2 <- updateFit(rv$fit2, input$order2)
   })

  observeEvent(input$plot_clickA, {
    click <- input$plot_clickA
    predi <- seq(along=preds)
    p <- which.min(abs(predi - click$y))
    pred <- preds[p]
    terms0 <- attr(terms(rv$fit0), "term.labels")
    change <- NULL
    if  (pred %in% terms0){
      if (length(terms0)> 1){
        # print( paste0("Removing term ", pred))
        change <- "Remove"
        terms0 <- terms0[- match(pred, terms0)]
      }
    }
    else {
      change <- "Add"
      # print( paste0("Adding term ", pred))
      terms0 <- c(terms0, pred)
      terms0 <- preds[sort(match(terms0, preds))]
    }
    rv$fit0 <-refitModel(rv$fit0, terms0, rd$mdata)

    if (! is.null(change)){
    if (input$order1 %in% c("Default","RevDefault", "Forward", "Backward"))
      rv$fit1 <- updateFit(rv$fit0, input$order1)
    else rv$fit1 <- updateFit(rv$fit1, change,pred)

    if (input$order2 %in% c("Default", "RevDefault","Forward", "Backward"))
      rv$fit2 <- updateFit(rv$fit0, input$order2)
    else rv$fit2 <- updateFit(rv$fit2, change,pred)
    }

  })




  observeEvent(input$plot_clickS, {
    click <- input$plot_clickS
   #  print(c(click$x, click$y))
    if (click$x > 1.75 & click$x < 2.25 & input$order2=="Interact") {
      p <- whichPredS(rv$fit2,click$y)
      if (!is.na(p)){
        rv$fit2 <- upPredOrd(rv$fit2,p,rd$mdata)
      }
    }
    else if (click$x > .75 & click$x < 1.25  & input$order1=="Interact"){
      p <- whichPredS(rv$fit1,click$y)
      if (!is.na(p)){
        rv$fit1 <- upPredOrd(rv$fit1,p, rd$mdata)
      }
    }
  })

  observeEvent(input$plot_dblclickA, {
    terms0 <- attr(terms(ERfit), "term.labels")

    rv$fit0 <-refitModel(rv$fit0, terms0, rd$mdata)
    rv$fit1 <- updateFit(rv$fit0, input$order1)
    rv$fit2 <- updateFit(rv$fit0, input$order2)
  }
  )

  observeEvent(input$plot_dblclickS, {
    click <- input$plot_dblclickS
    if (click$x > 1.75 & click$x < 2.25 & input$order2=="Interact") {
      p <- whichPredS(rv$fit2,click$y)
      if (!is.na(p)){
        rv$fit2 <- downPredOrd(rv$fit2,p, rd$mdata)
      }
    }
    else if (click$x > 0.75 & click$x < 1.25  & input$order1=="Interact"){
      p <- whichPredS(rv$fit1,click$y)
      if (!is.na(p)){
        rv$fit1 <- downPredOrd(rv$fit1,p, rd$mdata)
        }
    }
  })



  observeEvent(input$restore_all, {
    rd$mdata <- ERdata
    # rd$sel<- NULL

    rv$fit0 <- updateFit(rv$fit0, "data",pred)
    if (input$order1 %in% c("Interact", "Random"))
      rv$fit1 <- updateFit(rv$fit1, "data",pred)
    else  rv$fit1 <- updateFit(rv$fit1, input$order1)

    if (input$order2 %in% c("Interact", "Random"))
      rv$fit2 <- updateFit(rv$fit2, "data",pred)
    else  rv$fit2 <- updateFit(rv$fit2, input$order2)

  })

  observeEvent(input$remove_brushed, {

    sel <- rd$sel
    if (! is.null(sel)){
      del <- rownames(rd$mdata) %in% sel$case
      rd$mdata <- rd$mdata[!del,]

      rv$fit0 <- updateFit(rv$fit0, "data",pred)
      if (input$order1 %in% c("Interact", "Random"))
        rv$fit1 <- updateFit(rv$fit1, "data",pred)
      else  rv$fit1 <- updateFit(rv$fit1, input$order1)

      if (input$order2 %in% c("Interact", "Random"))
        rv$fit2 <- updateFit(rv$fit2, "data",pred)
      else  rv$fit2 <- updateFit(rv$fit2, input$order2)
      # rd$sel<- NULL
    }
  })

  observeEvent(input$pcp_dblclick, {
    if (! is.null(rd$sel)){
      rd$sel<-NULL
    }
  })

  observeEvent(input$plot_brush, {
    sel <-  brushedPoints(rd$pcp_data, input$plot_brush, "varn", "val")
    if (nrow(sel) ==0)
      rd$sel<-NULL
    else
    rd$sel<-rbind(sel, rd$sel)
    # if (nrow(sel) !=0){
    #     del <- rownames(rd$mdata) %in% sel$case
    #  }


  })




  output$barPlotA <- renderPlot({

     fixedscales <- input$fixedscales
     # fixedscales <- "diff" %in% input$Res

     if (input$stat == "Adj. SS"){
     p <- plotAnovaStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), type="SS")+
     xlab("")+ ylab("")
     if (!fixedscales) p
     else {
       plimsx <- as.list(ggplot_build(p)$layout)$panel_params[[1]]$x.range

       if (is.null(plotAlims$sstat$x.range)){
         lims <- slimits(rv$fit0)
         plotAlims$sstat$x.range <<- c(min(plimsx[1], lims[1]), max(plimsx[2], lims[2]))
         plotAlims$sstat$y.range <<- as.list(ggplot_build(p)$layout)$panel_params[[1]]$y.range
       }
       p$coordinates$default<- TRUE
       p+   coord_flip(ylim=plotAlims$sstat$x.range, xlim=plotAlims$sstat$y.range, expand=F)
     }
    }
    else if (input$stat == "F stat"){
      p <- plotAnovaStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), type="F")+
        xlab("")+ ylab("")


      if (!fixedscales) p
      else {
        plimsx <- as.list(ggplot_build(p)$layout)$panel_params[[1]]$x.range

        if (is.null(plotAlims$fstat$x.range)){
          lims <- flimits(rv$fit0)
          plotAlims$fstat$x.range <<- c(min(plimsx[1], lims[1]), max(plimsx[2], lims[2]))
          plotAlims$fstat$y.range <<- as.list(ggplot_build(p)$layout)$panel_params[[1]]$y.range
        }
       p$coordinates$default<- TRUE
        p+   coord_flip(ylim=plotAlims$fstat$x.range, xlim=plotAlims$fstat$y.range, expand=F)
      }
    }

    else if (input$stat == "t stat") {
      p <-  plottStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha))+
        xlab("")+ ylab("")

       if (!fixedscales) p
      else {
      plimsx <- as.list(ggplot_build(p)$layout)$panel_params[[1]]$x.range

      if (is.null(plotAlims$tstat$x.range)){
        lims <- tlimits(rv$fit0)
        plotAlims$tstat$x.range <<- c(min(plimsx[1], lims[1]), max(plimsx[2], lims[2]))
        plotAlims$tstat$y.range <<- as.list(ggplot_build(p)$layout)$panel_params[[1]]$y.range
      }


      # using coord_flip a second time generates a warning
      # can get rig of with suppressMessages(print but that affects coordinate system
      # next line gets rid of message
      p$coordinates$default<- TRUE

      p+   coord_flip(ylim=plotAlims$tstat$x.range, xlim=plotAlims$tstat$y.range, expand=F)

      }
    }
    else if (input$stat == "CI") {
      p <- plotCIStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), F)+
        xlab("")+ ylab("")

      if (!fixedscales) p
      else {
        plimsx <- as.list(ggplot_build(p)$layout)$panel_params[[1]]$x.range

        if (is.null(plotAlims$cistat$x.range)){
          lims <- cilimits(rv$fit0)
          plotAlims$cistat$x.range <<- c(min(plimsx[1], lims[1]), max(plimsx[2], lims[2]))
          plotAlims$cistat$y.range <<- as.list(ggplot_build(p)$layout)$panel_params[[1]]$y.range
        }
        # p$coordinates$default<- TRUE
        p+   coord_cartesian(xlim=plotAlims$cistat$x.range, ylim=plotAlims$cistat$y.range, expand=F)
      }
    }
    else if (input$stat == "CI stdX") {
      p <- plotCIStats(rv$fit0, ERtermcols, preds,as.numeric(input$alpha), T)+
        xlab("")+ ylab("")

      if (!fixedscales) p
      else {
        plimsx <- as.list(ggplot_build(p)$layout)$panel_params[[1]]$x.range

        if (is.null(plotAlims$cistdstat$x.range)){
          lims <- cislimits(rv$fit0)
          plotAlims$cistdstat$x.range <<- c(min(plimsx[1], lims[1]), max(plimsx[2], lims[2]))
          plotAlims$cistdstat$y.range <<- as.list(ggplot_build(p)$layout)$panel_params[[1]]$y.range
        }
        # p$coordinates$default<- TRUE
        p+   coord_cartesian(xlim=plotAlims$cistdstat$x.range, ylim=plotAlims$cistdstat$y.range, expand=F)
      }
    }

  })

  output$barPlotS <- renderPlot({
    # generate levels based on input$levels from ui.R
    # res <- tidy(anova(rv$fit))
    fits <- NULL
    if (input$order2 == "Best"){
      best <-regsubsetsOrder(rv$fit2,rd$mdata)
      if (! is.null(best)){
        fits <- c("Order1"= list(rv$fit1), best)
       }
    }
    if (is.null(fits))
      fits <- list("Order1"=rv$fit1, "Order2"= rv$fit2)

    if (is.null(barpYlim )){
      p <- plotSeqSS(fits, ERtermcols, legend=F)
      barpYlim <<- ggplot_build(p)$layout$panel_ranges[[1]]$y.range
      barpXlim <<- ggplot_build(p)$layout$panel_ranges[[1]]$x.range
      #print("setting ylim")
      # print(p)
    }
   plotSeqSS(fits, ERtermcols, legend=F) +coord_cartesian(xlim = barpXlim,ylim = barpYlim, expand = FALSE)+ xlab("")+ ylab("")
  })





  output$info <- renderPrint({
    ft1 <- broom::tidy(anova(rv$fit1))
    r1 <- ft1$sumsq
    r1 <- (1 - dplyr::last(r1)/sum(r1))*100
    r1 <- format(r1, digits=3)
    mse <- format(sqrt(dplyr::last(ft1$meansq)/dplyr::last(ft1$df)),digits=2)
    ft2 <- broom::tidy(anova(rv$fit2))
    infoString1 <- paste("Plot2:",paste(ft1$term,sapply(ft1$sumsq, format, digits=2), collapse=" "),
                         paste0("Rsq ", r1, "%", " MSE ", mse))
    infoString2 <- paste("Plot3:",paste(ft2$term,sapply(ft2$sumsq, format, digits=2), collapse=" "),
                         paste0("Rsq ", r1, "%", " MSE ", mse))
    cat(paste0(infoString1,"\n", infoString2))

  })



  output$infoBrushed <- renderPrint({
    sel <- rd$sel
    #d <- rd$mdata
       if (!is.null(sel) && (nrow(sel) !=0)){
         d <- get_all_vars(rv$fit0,rd$mdata)
      # del <- rownames(d) %in% sel$case
      # if (sum(del)> 0)
      #  d[del,]
          del <- na.omit(match(sel$case, rownames(d)))
      if (length(del) > 0)
        d[rev(del),]
      }
  })


  output$pcp <- renderPlot({
    mdata <- rd$mdata
    resD <- "diff" %in% input$Res
    resA <- "abs" %in% input$Res
    cols <- pcpColors(rv$fit0, input$PCP, ERnpcpCols,resA)
    if (input$From == "Plot1"){
      r <- regPCPdata(rv$fit0,mdata,input$PCP,resD,resA, color=cols$color, sequential=F)
    }
    else if (input$From == "Plot2"){
      r <- regPCPdata(rv$fit1,mdata,input$PCP,resD,resA, color=cols$color, sequential=T)
    }
    else {
      r <- regPCPdata(rv$fit2,mdata,input$PCP,resD,resA, color=cols$color, sequential=T)
    }
    dg <- r$pcp_data
    rd$pcp_data <- dg
    sel <-  rd$sel
    # dsel <- dplyr::filter(dg , case %in% sel$case)
    dsel <- dg[dg$case%in% sel$case, ]
     ggplot(dg, aes(x = .data$varn, y = .data$val, group = .data$case,color=.data$color)) +
      geom_line()+ ylab(r$ylab)  +labs(colour= cols$clab)+
      geom_line(data = dsel, color="magenta", size=1.5)+
      scale_x_continuous("", breaks= seq(along=levels(dg$var)), labels=levels(dg$var))

  })

  observeEvent(input$done, {
    stopApp(invisible(list(Plot1 = rv$fit0, Plot2=rv$fit1, Plot3=rv$fit2, data=rd$mdata)))
  })
  observeEvent(input$cancel, {
    stopApp(NULL)
  })



}


}



