require(BiocManager)
# if get bioconductor error do this
options(repos = BiocManager::repositories())

require(shiny)
require(shinyjs)
require(DT)
require(tidyverse)
require(plotly)
require(Hmisc)
require(pwr)
require(samplingbook)
require(xkcd)
# devtools::install_github("slzhao/RnaSeqSampleSize")
require(RnaSeqSampleSize)
require(e1071)
require(caret)
require(pROC)
require(plotROC)
require(ggplot2)
require(shinyAce)
require(binom)




# library(mailR)
# require(sendmailR)
# require(shinyalert)
# Stuff here runs once per app

#  I put everything in a loop as not all the things can take in vetors. So this was jsut easier.
shinyServer(function(input, output, session){
  # output$rctMenu <- renderMenu({
  #   sidebarMenu(
  #     menuItem("Cohort/RCT", tabName = "rct"),
  #
  #     # menuItem("Means", tabName = "mean")
  #   )
  # })

  # observe({
  #   output$hide_panel <- eventReactive(input$sidebarmenu != "rct", T, ignoreInit = TRUE)
  #   outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
  # })
  # observe(input$hide_panel <- F
  # )
  # observe({print(input$hide_panel[1]%%2)})
  #
  #   observeEvent(input$sidebarmenu, {
  #     if (input$sidebarmenu == "rct")
  #       test <<- test+1
  #     print(test)
  #   })

  # observeEvent(input$pwrRCT, {
  #   if (input$pwrRCT == "Sample Size")
  #     shinyjs::disable("pRange")
  #   else
  #     shinyjs::disable("Button1")
  # })

  # plotStd <- function(cc, type){
  #   if (type)
  #     ggplotly(ggplot(cc, aes(x = Power, y = SampleSize)) +
  #                geom_point() +
  #                geom_line() +
  #                theme_xkcd() + xkcdaxis(range(cc$Power), range(cc$SampleSize))) %>%
  #     layout(plot_bgcolor='rgb(254, 247, 234)') %>%
  #     layout(paper_bgcolor='rgb(254, 247, 234)') %>%
  #     layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
  #   else
  #     plot_ly(cc, x = ~Power, y = ~SampleSize, type = 'scatter', mode = 'lines+markers')%>%
  #     layout(plot_bgcolor='rgb(254, 247, 234)') %>%
  #     layout(paper_bgcolor='rgb(254, 247, 234)')
  # }


  #############
  ## rct TAB ##
  #############
  RCTss <- reactive({
    if (input$pwrRCT == "Power"){
      ss <- NULL
      for (pwr in seq(from = input$pRange[1], to = input$pRange[2], by = 0.01)){
        ss.tmp <- bsamsize(input$outcome1, input$outcome2, alpha=input$alp, power=pwr, fraction = input$fraction)
        ss.tmp <- c(pwr, ss.tmp)
        ss.tmp <- c(ss.tmp, input$prev1)
        ss.tmp <- c(ss.tmp, input$prev2)
        ss <- rbind(ss, ss.tmp)
      }
      ss.backup <- data.frame(ss)
      colnames(ss.backup)[1] <- "Power"
      colnames(ss.backup)[4:5] <- c("Prevelance 1", "Prevelance 2")

      ss <- ss.backup

      ss$n1 <- (ss$n1/input$prev1)
      ss$n1 <- ceiling(ss$n1*input$LTFU+ss$n1)
      ss$n2 <- (ss$n2/input$prev2)
      ss$n2 <- ceiling(ss$n2*input$LTFU+ss$n2)

      ss$TotalSampleSize <- ss$n1 + ss$n2
      ss <- gather(ss, key = Group, value = SampleSize,
                   n1, n2, TotalSampleSize)

      if (input$fraction == 0.5 & input$prev1 == input$prev2){
        ss <- ss[ss$Group != "n2",]
        ss$Group <- ifelse(ss$Group == "n1", "n", ss$Group)
      }
    }else{
      ss <- NULL
      for (pwr in seq(from = input$nrct[1], to = input$nrct[2], by = 1)){
        ss.tmp <- bpower(input$outcome1, input$outcome2, alpha=input$alp,
                         # odds.ratio=input$ORrct, percent.reduction=input$PRrct,
                         n=pwr)
        ss.tmp <- c(ss.tmp, pwr)
        ss <- rbind(ss, ss.tmp)
      }
      ss.backup <- data.frame(ss)
      colnames(ss.backup) <- c("Power", "SampleSize")
      ss <- ss.backup
      # ss <- gather(ss, key = Group, value = SampleSize)
    }

    ss
  })

  output$RCTplot <- renderPlotly({
    ss <- RCTss()

    if ("Group"%in%colnames(ss)){
      if (input$plot_xkcd)
        ggplotly(ggplot(ss, aes(x = Power, y = SampleSize)) +
                   geom_point(aes(color = Group, group = Group), data=ss) +
                   geom_line(aes(color = Group, group = Group), data=ss) +
                   theme_xkcd() + xkcdaxis(range(ss$Power), range(ss$SampleSize))) %>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
      else
        plot_ly(ss, x = ~Power, y = ~SampleSize, name = ~Group, type = 'scatter', mode = 'lines+markers')%>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        add_text(text = ifelse(rownames(ss)%in%input$RCTplotDT_rows_selected, paste("SS:", ss$SampleSize, "Power:", ss$Power), ""), textposition = "bottom right")
    }else{
      if (input$plot_xkcd)
        ggplotly(ggplot(ss, aes(x = Power, y = SampleSize)) +
                   geom_point() +
                   geom_line() +
                   theme_xkcd() + xkcdaxis(range(ss$Power), range(ss$SampleSize))) %>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
      else
        plot_ly(ss, x = ~Power, y = ~SampleSize, type = 'scatter', mode = 'lines+markers')%>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        add_text(text = ifelse(rownames(ss)%in%input$RCTplotDT_rows_selected, paste("SS:", ss$SampleSize, "Power:", ss$Power), ""), textposition = "bottom right")
    }

  })

  output$RCTplotDT <- renderDT({
    ss <- RCTss()
    datatable(ss,  extensions = 'Buttons', options = list(scrollX = TRUE, dom = 'Bfrtip',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                          rownames = F)
    )
  })


  ################
  ## propSS TAB ##
  ################
  PropOneSSss <- reactive({
    ss <- NULL
    if (input$pwrpropSS == "Power"){
      for (pwr in seq(from = input$pRangeProp[1], to = input$pRangeProp[2], by = 0.01)){
        ss.tmp <- pwr.p.test(h=input$effProp, power=pwr, sig.level=input$alpProp, alternative=input$altProp)
        ss.tmp <- c(pwr, ss.tmp$n)
        ss <- rbind(ss, ss.tmp)
      }
    }else{
      stpS = ifelse(input$pRangePropSsize[2]-input$pRangePropSsize[1] > 100, 1, 10)
      for (pwr in seq(from = input$pRangePropSsize[1], to = input$pRangePropSsize[2], by = stpS)){
        ss.tmp <- pwr.p.test(h=input$effProp, n=pwr, sig.level=input$alpProp, alternative=input$altProp)
        ss.tmp <- c(ss.tmp$power, pwr)
        ss <- rbind(ss, ss.tmp)
      }
    }

    ss <- data.frame(ss)
    colnames(ss) <- c("Power", "TotalSampleSize")
    ss.backup <- data.frame(ss)
    ss <- ss.backup
    if (input$pwrpropSS == "Power")
      ss$TotalSampleSize <- ceiling((ss$TotalSampleSize*input$LTFUProp)+ss$TotalSampleSize)
    else
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)
    ss
  })

  output$PropOneSSplot <- renderPlotly({
    ss <- PropOneSSss()
    if (input$plot_xkcd)
      ggplotly(ggplot(ss, aes(x = Power, y = TotalSampleSize)) +
                 geom_point() +
                 geom_line() +
                 theme_xkcd() + xkcdaxis(range(ss$Power), range(ss$TotalSampleSize))) %>%
      layout(plot_bgcolor='rgb(254, 247, 234)') %>%
      layout(paper_bgcolor='rgb(254, 247, 234)') %>%
      layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    else
      plot_ly(ss, x = ~Power, y = ~TotalSampleSize,  type = 'scatter', mode = 'lines+markers')%>%
      layout(plot_bgcolor='rgb(254, 247, 234)') %>%
      layout(paper_bgcolor='rgb(254, 247, 234)')%>%
      add_text(text = ifelse(rownames(ss)%in%input$PropOneSSdt_rows_selected, paste("SS:", ss$SampleSize, "Power:", ss$Power), ""), textposition = "bottom right")
  })

  output$PropOneSSdt <- renderDT({
    ss <- PropOneSSss()
    datatable(ss,  extensions = 'Buttons', options = list(scrollX = TRUE, dom = 'Bfrtip',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    )
  })


  ################
  ## propTS TAB ##
  ################
  PropOneTSss <- reactive({
    ss <- NULL
    if (input$pwrpropTS == "Power"){
      for (pwr in seq(from = input$pRangePropTS[1], to = input$pRangePropTS[2], by = 0.01)){
        ss.tmp <- pwr.2p.test(h=input$effPropTS, power=pwr, sig.level=input$alpPropTS, alternative=input$altPropTS)
        ss.tmp <- c(pwr, ss.tmp$n)
        ss <- rbind(ss, ss.tmp)
      }
    }else{
      stpS = ifelse(input$pRangePropSsize[2]-input$pRangePropSsize[1] > 100, 1, 10)
      for (pwr in seq(from = input$pRangePropTSsize[1], to = input$pRangePropTSsize[2], by = stpS)){
        ss.tmp <- pwr.2p.test(h=input$effPropTS, n=pwr, sig.level=input$alpPropTS, alternative=input$altPropTS)
        ss.tmp <- c(ss.tmp$power, pwr)
        ss <- rbind(ss, ss.tmp)
      }
    }
    ss <- data.frame(ss)
    colnames(ss) <- c("Power", "TotalSampleSize")
    ss.backup <- data.frame(ss)
    ss <- ss.backup
    ss
  })

  output$PropOneTSplot <- renderPlotly({
    ss <- PropOneTSss()
    if (input$pwrpropTS == "Power")
      ss$TotalSampleSize <- ceiling((ss$TotalSampleSize*input$LTFUPropTS)+ss$TotalSampleSize)
    else
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)

    if (input$plot_xkcd)
      ggplotly(ggplot(ss, aes(x = Power, y = TotalSampleSize)) +
                 geom_point() +
                 geom_line() +
                 theme_xkcd() + xkcdaxis(range(ss$Power), range(ss$TotalSampleSize))) %>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    else
      plot_ly(ss, x = ~Power, y = ~TotalSampleSize,  type = 'scatter', mode = 'lines+markers')%>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)')
  })

  output$PropOneTSdt <- renderDT({
    ss <- PropOneTSss()
    datatable(ss,  extensions = 'Buttons', options = list(scrollX = TRUE, dom = 'Bfrtip',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })


  ############
  ## PP TAB ##
  ############
  PPss <- reactive({
    ss <- NULL
    n <- ifelse(input$popSizePP == 0, Inf, input$popSizePP)
    for (pwr in seq(from = input$pRangePP[1], to = input$pRangePP[2], by = 0.01)){
      ss.tmp <- sample.size.prop(e=input$alpPP, P=input$fractionPP, N=n, level = pwr)
      ss.tmp <- c(pwr, ss.tmp$n)
      ss <- rbind(ss, ss.tmp)
    }
    ss <- data.frame(ss)
    colnames(ss) <- c("ConfidenceInterval", "TotalSampleSize")
    ss.backup <- data.frame(ss)
    ss <- ss.backup
    ss$TotalSampleSize <- ceiling((ss$TotalSampleSize*input$LTFUPPP)+ss$TotalSampleSize)
    ss
  })


  output$PPplot <- renderPlotly({
    ss <- PPss()
    if (input$plot_xkcd)
      ggplotly(ggplot(ss, aes(x = ConfidenceInterval, y = TotalSampleSize)) +
                 geom_point() +
                 geom_line() +
                 theme_xkcd() + xkcdaxis(range(ss$ConfidenceInterval), range(ss$TotalSampleSize))) %>%
      layout(plot_bgcolor='rgb(254, 247, 234)') %>%
      layout(paper_bgcolor='rgb(254, 247, 234)') %>%
      layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    else
      plot_ly(ss, x = ~ConfidenceInterval, y = ~TotalSampleSize,  type = 'scatter', mode = 'lines+markers')%>%
      layout(plot_bgcolor='rgb(254, 247, 234)') %>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })

  output$PPdt <- renderDT({
    ss <- PPss()
    datatable(ss,  extensions = 'Buttons', options = list(scrollX = TRUE, dom = 'Bfrtip',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })






  ############
  ## DP TAB ##
  ############
  # https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1553-2712.1996.tb03538.x

  output$DPplot <- renderPlotly({

    datDP <- data.frame(N = NA,
                        n_Sn = NA,
                        n_Sp = NA,
                        N_Sn = NA,
                        N_Sp = NA,
                        P = NA,
                        W = NA,
                        SN = NA,
                        SP = NA)

    alpha <- input$alpDC
    W <- input$wDP
    SN <-input$wSn
    SP <-input$wSp
    tails <- 2
    Za2 <- qnorm(1-alpha/tails)

    for (P in seq(from = input$wP[1], to = input$wP[2], by = 0.05)){

      # Calculate the Number with Disease, TP + FN
      TP_FN <- Za2^2*(SN*(1-SN)/W^2)

      # Calculate the Sample Size Required for Sensitivity
      N_Sn <- ceiling((TP_FN)/P)


      # Calculate the Number without Disease, FP + TN
      FP_TN <- Za2^2*(SP*(1-SP)/W^2)

      # Calculate the Sample Size Required for Specificity, N_Sp
      N_Sp <- ceiling((FP_TN)/(1-P))

      N <- ifelse(N_Sn > N_Sp, N_Sn, N_Sp)
      # is the number of randomly selected subjects required to estimate the sensitivity and specificity,
      # in a target population with prevalence of disease P, within W,
      # assuming the sensitivity and specificity are of sizes SN and SP, respectively

      n_Sn <- N_Sn*P
      n_Sp <- N_Sp*P


      datDP <- rbind(datDP,
                     cbind(N,
                           n_Sn,
                           n_Sp,
                           N_Sn,
                           N_Sp,
                           P,
                           W,
                           SN,
                           SP)
      )
    }
    datDP <- datDP[-1,]




    ss <- PPss()
    if (input$plot_xkcd)
      ggplotly(ggplot(datDP, aes(x = P, y = N_Sp)) +
                 geom_point() +
                 geom_line() +
                 theme_xkcd() + xkcdaxis(range(ss$P), range(ss$N_Sp))) %>%
      layout(plot_bgcolor='rgb(254, 247, 234)') %>%
      layout(paper_bgcolor='rgb(254, 247, 234)') %>%
      layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    else
      datDP %>%
        plot_ly(x = ~P*100, y = ~N_Sp, type = 'scatter', mode = 'lines+markers', name = 'Specificity') %>%
        add_trace(y = ~N_Sn, mode = 'lines+markers', name = 'Sensitivity') %>%
        layout(plot_bgcolor='rgb(254, 247, 234)', paper_bgcolor='rgb(254, 247, 234)') %>%
        layout(xaxis = list(title = "Prevalence (%)"),
               yaxis = list(title = "N")
        )
  })





  ############
  ## MM TAB ##
  ############
  MMss <- reactive({
    ss <- NULL
    if (input$altSDMM == 0)
      dM <- abs(input$Mm1-input$Mm2)/(input$SdM1+input$SdM2)
    else
      dM <- input$altSDMM
    if (input$pwrMM == "Power"){
      for (pwr in seq(from = input$pRangeMM[1], to = input$pRangeMM[2], by = 0.01)){
        ss.tmp <- pwr.t.test(d = dM, sig.level = input$alpMM, power = pwr,
                             type = input$typM,
                             alternative = input$altMM)
        ss.tmp <- c(pwr, ss.tmp$n)
        ss <- rbind(ss, ss.tmp)
      }
    }else{
      stpS = ifelse(input$pRangePropSsize[2]-input$pRangePropSsize[1] > 100, 1, 10)
      for (pwr in seq(from = input$pRangeMMpwr[1], to = input$pRangeMMpwr[2], by = stpS)){
        ss.tmp <- pwr.t.test(d = dM, sig.level = input$alpMM, n = pwr,
                             type = input$typM,
                             alternative = input$altMM)
        ss.tmp <- c(ss.tmp$power, pwr)
        ss <- rbind(ss, ss.tmp)
      }
    }
    ss <- data.frame(ss)
    colnames(ss) <- c("Power", "TotalSampleSize")
    ss.backup <- data.frame(ss)
    ss <- ss.backup
    ss
  })

  output$MMplot <- renderPlotly({
    ss <- MMss()
    if (input$pwrMM == "Power")
      ss$TotalSampleSize <- ceiling((ss$TotalSampleSize*input$LTFUPMM)+ss$TotalSampleSize)
    else
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)

    if (input$plot_xkcd)
      ggplotly(ggplot(ss, aes(x = Power, y = TotalSampleSize)) +
                 geom_point() +
                 geom_line() +
                 theme_xkcd() + xkcdaxis(range(ss$Power), range(ss$TotalSampleSize))) %>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    else
      plot_ly(ss, x = ~Power, y = ~TotalSampleSize,  type = 'scatter', mode = 'lines+markers')%>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)')
  })

  output$MMdt <- renderDT({
    ss <- MMss()
    datatable(ss,  extensions = 'Buttons', options = list(scrollX = TRUE, dom = 'Bfrtip',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })


  ###############
  ## RNAss TAB ##
  ###############
  RNAssDt <- reactive({
    ss <- NULL
    withProgress(message = 'Making plot', value = 0, {
      if (input$pwrRNA == 'Power'){
        for (pwr in seq(from = input$pRangeRNA[1], to = input$pRangeRNA[2], by = 0.01)){
          ss.tmp <- sample_size(power=pwr, f=input$alpRNA, rho=input$rhoRNA, lambda0=input$lambda0RNA, phi0=input$phi0RNA)
          ss.tmp <- c(pwr, ss.tmp[1])
          ss <- rbind(ss, ss.tmp)
          # incProgress(pwr/length(seq(from = input$pRangeRNA[1], to = input$pRangeRNA[2], by = 0.01)))
        }
      }else{
        stpS = ifelse(input$pRangePropSsize[2]-input$pRangePropSsize[1] > 100, 1, 10)
        for (pwr in seq(from = input$pRangeRNAss[1], to = input$pRangeRNAss[2], by = stpS)){
          ss.tmp <- est_power(n=pwr, f=input$alpRNA, rho=input$rhoRNA, lambda0=input$lambda0RNA, phi0=input$phi0RNA)
          ss.tmp <- c(ss.tmp[1], pwr)
          ss <- rbind(ss, ss.tmp)
        }
      }
    })
    ss <- data.frame(ss)
    colnames(ss) <- c("Power", "TotalSampleSize")
    ss.backup <- data.frame(ss)
    ss <- ss.backup
    ss
  })

  output$plotRNAss <- renderPlotly({
    ss <- RNAssDt()
    if (input$pwrRNA == 'Sample Size')
      ss$TotalSampleSize <- ceiling((ss$TotalSampleSize*input$LTFUPRNA)+ss$TotalSampleSize)
    else
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)

    if (input$plot_xkcd)
      ggplotly(ggplot(ss, aes(x = Power, y = TotalSampleSize)) +
                 geom_point() +
                 geom_line() +
                 theme_xkcd() + xkcdaxis(range(ss$Power), range(ss$TotalSampleSize))) %>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)') %>%
        layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    else
      plot_ly(ss, x = ~Power, y = ~TotalSampleSize,  type = 'scatter', mode = 'lines+markers')%>%
        layout(plot_bgcolor='rgb(254, 247, 234)') %>%
        layout(paper_bgcolor='rgb(254, 247, 234)')
  })


  output$RNAssdt <- renderDT({
    ss <- RNAssDt()
    datatable(ss,  extensions = 'Buttons', options = list(scrollX = TRUE, dom = 'Bfrtip',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })


  ######################
  ## bayes sucess run ##
  ######################
  output$b_srun <- renderPrint({
    # sucess sample size, Bayes success run theorem
    C <- input$ci_srun #confidence interval
    R <- input$r_srun #reliability
    N <- ceiling(log(1-C)/log(R))

    print(paste0("You will need to test ", as.character(N), " samples to demonstrate a reliability of ", as.character(input$r_srun*100),"% at ", as.character(input$ci_srun*100),"%  confidence level"))



    # # Binomial Reliability Demonstration Test
    # # https://reliabilityanalyticstoolkit.appspot.com/sample_size
    # # Testing 124 items with 2 failures occurring will demonstrate a reliability of 95% at 95% confidence level.
    # # Risk = 1 - C, where C is confidence.  Select sample size n such that the right-hand side is equal to,
    # # or slightly less than, the left-hand side of binomial equation (specified allowable risk).
    # C <- 0.95 #C is confidence
    # r <- 1 - C # risk
    # i <- 0
    # f <- 0 # allowable failures
    # R <- 0.95 #realiability to be demostrated
    # n_in <- 5
    #
    # tmp_rsk <- 1
    #
    # while (tmp_rsk > r){
    #   a <- factorial(n_in)/(factorial(i)*factorial(n_in-i)) # "n choose i"
    #   b <- (1 - R)^i
    #   c <- R^(n_in - i)
    #   tmp_rsk <- a*b*c
    #   n_in <- n_in + 1
    # }
    # n_in-1
    #

  })


  ######################
  ## Seq Success ##
  ######################
  output$seq_sucO <- reactive({
  VAF <- input$vaf
  background_seq_error_rate <- input$ber
  conf_level <- input$seq_conf
  alpha <- 1 - conf_level
  power <- input$seq_pwd

  seq_err <- cloglog.sample.size(p.alt = VAF,
                      p = background_seq_error_rate,
                      power = power,
                      alpha = alpha)


  print(paste0("Null Hypothesis: No variant present, background error rate only ", background_seq_error_rate, "%"))
  print(paste0("Alternative Hypothesis: A variant is present at the given frequency ", VAF, "%"))

  print(paste0("You will need to test ", as.character(seq_err$n), " samples"))


  })


  ######################
  ## Chi Sq or fisher ##
  ######################
  RCinputData <- reactive({
    dat <- read.csv(text=input$rc_input, sep="", na.strings=c("","NA","."))
    rownames(dat) <- dat[,1]
    dat <- dat[,-c(1)]

    dat <- as.matrix(dat)
    dat <- addmargins(dat)

    print(dat)
  })

  output$RCdt <- renderPrint({
    if (input$MCtest){
      print(mcnemar.test(RCinputData()))
    }else{
      if (any(RCinputData()<5))
        print(fisher.test(RCinputData(), workspace = 10000000))
      else
        print(chisq.test(RCinputData()))
    }
  })


  # require(broom)
  # tidy(chisq.test(dat))


  ######################
  ## Diagnostic Tests ##
  ######################
  DTPinputData <- reactive({
    dat <- read.csv(text=input$dtp_input, sep="", na.strings=c("","NA","."))
    rownames(dat) <- dat[,1]
    dat <- dat[,-c(1)]

    dat <- as.matrix(dat)
    # dat <- addmargins(dat)
    dat
  })

  output$DTPdt <- renderPrint({
    dat <- as.table(DTPinputData())
    print(confusionMatrix(dat))
  })


  #########
  ## ROC ##
  #########
  ROCinputData <- reactive({
    dat <- read.csv(text=input$roc_input, sep="", na.strings=c("","NA","."))
    rownames(dat) <- dat[,1]
    dat <- dat[,-c(1)]
    colnames(dat) <- c("Outcome", "Measurement")

    dat[c(1:2)]
  })

  output$ROCdt <- renderPrint({
    dat <- ROCinputData()
    if (length(unique(dat$Outcome)) != 2)
      print("Outcome not Binary!")
    else{
      if (any(!unique(dat$Outcome)%in%c(0,1))){
        slev <- sort(levels(as.factor(dat$Outcome)))
        showNotification(paste0("Outcome not labeled 0/1, assuming ", slev[1],
                                " = 0 and ", slev[2], " = 1!"), duration = NULL)
      }
      dat[c(1,2)]
    }
  })

  output$ROCplot <- renderPlot({
    dat <- ROCinputData()
    txt <- roc(Outcome ~ Measurement, dat, smooth=TRUE)

    last.message <- NULL
    p <- tryCatch(
      ggplot(dat, aes(d = Outcome, m = Measurement)) + geom_roc() + style_roc() +
        annotate("text", label = paste0("AUC: ", round(txt$auc,2)), y=0.25, x=0.75)+
        theme(panel.background = element_rect(fill="#fef7ea", colour="#fef7ea"),
              plot.background = element_rect(fill = "#fef7ea"))
    )
    if (input$plot_xkcd)
      p + theme_xkcd()
    else
      p

  })



  output$frame2 <- renderUI({
    HTML('<div>
    <iframe src="https://rpsychologist.com/d3/NHST/" scrolling="no" style="height: 1675px; border: 0px none; width: 1250px; margin-top: -82px; margin-left: 0px; ">
    </iframe>
    </div>')
  })

  observeEvent(input$sidebarmenu, {
    if (input$sidebarmenu == "WhatIs")
      showModal(modalDialog(easyClose=T,
                            title = "What is a p-value? What is power?",
                            "This is borrowed from the Rphycologist and you can interactively play with paramaters to see what power and significance are/mean."
                            #       footer =
                            #       c('Still need to edit the below text.
                            #       Power is the probability of not making a Type II error (1 – beta). Type II error is the probability of wrongly failing to reject the null  (i.e. you dont see a difference in you test but there is actually a difference). Thus, simply put, power is the probability that the test rejects the null hypothesis (H0) when, in fact, it is false. You want power to be as large as possible.
                            #
                            #
                            # What affects power?
                            # -Significance level (alpha)
                            # -Sample size
                            # -Variability, or variance, in the measured response variable
                            # -Magnitude of the effect
                            #
                            #
                            # p-value: How likley our sample results are under our assumption of the truth. Put another way, what is the probability of being this far or further from the null in either direction (two sided test). So for example, our H0 when comparing two means would be H0=u1-u2=0. Type I error is to falsely infer the existence of something that is not there.
                            # It is the likelihood that you will report a difference as significant when, in reality, it is not. You want this to be as small as possible.')
      ))
  })

  observeEvent(input$sidebarmenu, {
    if (input$sidebarmenu == "cite")
      showModal(modalDialog(
        title = "Citation",
        paste0("Limberis, JD. 2018.", "HowBigHowStrong", "[Online]. Available at: ", "https://semiquant.shinyapps.io/SampleSizeCalculator/", "(Accessed: ", Sys.Date(), ")"),
        easyClose = TRUE,
        footer = NULL
      ))
  })


  # output$CM <- renderImage({
  #   # When input$n is 1, filename is ./images/image1.jpeg
  #   filename <- normalizePath(file.path('www/ConfusionMatrix_MyEdit.png'))
  #
  #   # Return a list containing the filename
  #   list(src = filename)
  # }, deleteFile = FALSE)

})



