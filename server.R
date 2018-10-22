require(shiny)
library(shinyjs)
require(DT)
require(tidyverse)
require(plotly)
require(Hmisc)
require(pwr)
require(samplingbook)
require(xkcd)
require(RnaSeqSampleSize)
require(e1071)
require(caret)
require(pROC)
require(plotROC)
require(ggplot2)
require(shinyAce)
# library(mailR)
# require(sendmailR)
# require(shinyalert)
#Stuff here runs once per app

#  I put everything in a loop as not all the things can take in vetors. So this was jsut easier.
#
#
#
# Define server logic required to draw a histogram
# test <<- 0
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
        ss <- rbind(ss, ss.tmp)
      }
      ss.backup <- data.frame(ss)
      colnames(ss.backup)[1] <- "Power"

      ss <- ss.backup
      ss$n1 <- ceiling((ss$n1*input$LTFU)+ss$n1)
      ss$n2 <- ceiling((ss$n1*input$LTFU)+ss$n2)
      ss$TotalSampleSize <- ss$n1 + ss$n2
      ss <- gather(ss, key = Group, value = SampleSize,
                   n1, n2, TotalSampleSize)

      if (input$fraction == 0.5){
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
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
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
})


