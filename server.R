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
    if (input$pwrRCT == "Power") {
      # Pre-calculate power sequence
      pwr_seq <- seq(from = input$pRange[1], to = input$pRange[2], by = 0.01)
      
      # Vectorized calculations
      ss <- do.call(rbind, lapply(pwr_seq, function(pwr) {
        ss.tmp <- bsamsize(input$outcome1, input$outcome2, alpha=input$alp, 
                          power=pwr, fraction = input$fraction)
        c(pwr, ss.tmp, input$prev1, input$prev2)
      }))
      
      ss <- as.data.frame(ss)
      colnames(ss)[1] <- "Power"
      colnames(ss)[4:5] <- c("Prevelance 1", "Prevelance 2")
      
      # Vectorized adjustments
      ss$n1 <- ceiling((ss$n1/input$prev1) * (1 + input$LTFU))
      ss$n2 <- ceiling((ss$n2/input$prev2) * (1 + input$LTFU))
      ss$TotalSampleSize <- ss$n1 + ss$n2
      
      ss <- gather(ss, key = Group, value = SampleSize,
                  n1, n2, TotalSampleSize)
      
      if (input$fraction == 0.5 & input$prev1 == input$prev2) {
        ss <- ss[ss$Group != "n2",]
        ss$Group <- ifelse(ss$Group == "n1", "n", ss$Group)
      }
      
    } else {
      # Pre-calculate sample size sequence 
      n_seq <- seq(from = input$nrct[1], to = input$nrct[2], by = 1)
      
      # Vectorized power calculations
      ss <- do.call(rbind, lapply(n_seq, function(n) {
        ss.tmp <- bpower(input$outcome1, input$outcome2, alpha=input$alp, n=n)
        c(ss.tmp, n)
      }))
      
      ss <- as.data.frame(ss)
      colnames(ss) <- c("Power", "SampleSize")
    }
    
    return(ss)
  })

  output$RCTplot <- renderPlotly({
    ss <- RCTss()
    
    # Create base plot
    p <- if ("Group" %in% colnames(ss)) {
      if (input$plot_xkcd) {
        ggplot(ss, aes(x = Power, y = SampleSize, color = Group)) +
          geom_point() +
          geom_line() +
          theme_xkcd() + 
          xkcdaxis(range(ss$Power), range(ss$SampleSize)) +
          labs(x = "Power", y = "Sample Size Required")
      } else {
        plot_ly(ss, x = ~Power, y = ~SampleSize, color = ~Group,
                type = 'scatter', mode = 'lines+markers')
      }
    } else {
      if (input$plot_xkcd) {
        ggplot(ss, aes(x = Power, y = SampleSize)) +
          geom_point() +
          geom_line() +
          theme_xkcd() + 
          xkcdaxis(range(ss$Power), range(ss$SampleSize)) +
          labs(x = "Power", y = "Sample Size Required")
      } else {
        plot_ly(ss, x = ~Power, y = ~SampleSize,
                type = 'scatter', mode = 'lines+markers')
      }
    }
    
    # Add common layout settings
    p %>%
      layout(plot_bgcolor = 'rgb(254, 247, 234)',
             paper_bgcolor = 'rgb(254, 247, 234)',
             showlegend = TRUE) %>%
      layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
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
    if (input$pwrpropSS == "Power") {
      pwr_seq <- seq(from = input$pRangeProp[1], to = input$pRangeProp[2], by = 0.01)
      
      ss <- do.call(rbind, lapply(pwr_seq, function(pwr) {
        ss.tmp <- pwr.p.test(h=input$effProp, power=pwr, 
                            sig.level=input$alpProp, alternative=input$altProp)
        c(pwr, ss.tmp$n)
      }))
    } else {
      stpS <- ifelse(input$pRangePropSsize[2]-input$pRangePropSsize[1] > 100, 1, 10)
      pwr_seq <- seq(from = input$pRangePropSsize[1], 
                    to = input$pRangePropSsize[2], by = stpS)
      
      ss <- do.call(rbind, lapply(pwr_seq, function(pwr) {
        ss.tmp <- pwr.p.test(h=input$effProp, n=pwr,
                            sig.level=input$alpProp, alternative=input$altProp)
        c(ss.tmp$power, pwr) 
      }))
    }
    
    ss <- as.data.frame(ss)
    colnames(ss) <- c("Power", "TotalSampleSize")
    
    if (input$pwrpropSS == "Power") {
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize * (1 + input$LTFUProp))
    } else {
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)
    }
    
    return(ss)
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
    # Create sequence of confidence intervals
    ci_seq <- seq(from = 0.8, to = 0.99, by = 0.01)
    
    # Pre-allocate results data frame
    ss <- data.frame(
      ConfidenceInterval = ci_seq,
      TotalSampleSize = NA_real_,
      stringsAsFactors = FALSE
    )
    
    # Get population size (use Inf if 0)
    N <- ifelse(input$popSizePP == 0, Inf, input$popSizePP)
    
    # Calculate sample sizes with progress bar
    withProgress(message = 'Calculating sample sizes...', value = 0, {
      for (i in seq_along(ci_seq)) {
        tryCatch({
          ss_tmp <- sample.size.prop(
            e = input$alpPP,
            P = input$fractionPP,
            N = N,
            level = ci_seq[i]
          )
          
          # Store result
          ss$TotalSampleSize[i] <- ceiling(ss_tmp$n * (1 + input$LTFUPPP))
          
          # Update progress
          incProgress(1/length(ci_seq))
          
        }, error = function(e) {
          warning(paste("Error at CI", ci_seq[i], ":", e$message))
        })
      }
    })
    
    # Remove any rows with NA values
    ss <- ss[!is.na(ss$TotalSampleSize), ]
    
    # Return empty data frame with correct structure if no results
    if (nrow(ss) == 0) {
      return(data.frame(
        ConfidenceInterval = numeric(0),
        TotalSampleSize = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    
    return(ss)
  })


  output$PPplot <- renderPlotly({
    ss <- PPss()
    
    # Check if we have data to plot
    if (nrow(ss) == 0) {
      return(plot_ly() %>% 
        add_annotations(
          text = "No valid sample sizes could be calculated with current parameters",
          showarrow = FALSE
        ))
    }
    
    if (input$plot_xkcd) {
      p <- ggplot(ss, aes(x = ConfidenceInterval, y = TotalSampleSize)) +
        geom_point() +
        geom_line() +
        theme_xkcd() + 
        xkcdaxis(range(ss$ConfidenceInterval), range(ss$TotalSampleSize)) +
        labs(
          x = "Confidence Interval",
          y = "Total Sample Size Required"
        )
      
      ggplotly(p) %>%
        layout(
          plot_bgcolor = 'rgb(254, 247, 234)',
          paper_bgcolor = 'rgb(254, 247, 234)',
          legend = list(bgcolor = 'rgb(254, 247, 234)')
        )
    } else {
      plot_ly(ss, x = ~ConfidenceInterval, y = ~TotalSampleSize, 
              type = 'scatter', mode = 'lines+markers') %>%
        layout(
          plot_bgcolor = 'rgb(254, 247, 234)',
          paper_bgcolor = 'rgb(254, 247, 234)',
          xaxis = list(title = "Confidence Interval"),
          yaxis = list(title = "Total Sample Size Required")
        )
    }
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
    # Create data frame for calculations
    datDP <- data.frame(
      P = seq(from = input$wP[1], to = input$wP[2], by = 0.05),
      stringsAsFactors = FALSE
    )
    
    # Calculate sample sizes
    datDP <- within(datDP, {
      alpha <- input$alpDC
      W <- input$wDP
      SN <- input$wSn
      SP <- input$wSp
      Za2 <- qnorm(1 - alpha/2)
      
      # Calculate sample sizes for sensitivity
      TP_FN <- Za2^2 * (SN * (1-SN) / W^2)
      N_Sn <- ceiling(TP_FN / P)
      n_Sn <- N_Sn * P
      
      # Calculate sample sizes for specificity  
      FP_TN <- Za2^2 * (SP * (1-SP) / W^2)
      N_Sp <- ceiling(FP_TN / (1-P))
      n_Sp <- N_Sp * (1-P)
      
      # Total sample size is max of the two requirements
      N <- pmax(N_Sn, N_Sp)
    })
    
    # Create plot
    if (input$plot_xkcd) {
      p <- ggplot(datDP, aes(x = P)) +
        geom_line(aes(y = N_Sp, color = "Specificity")) +
        geom_line(aes(y = N_Sn, color = "Sensitivity")) +
        theme_xkcd() +
        xkcdaxis(range(datDP$P), range(c(datDP$N_Sn, datDP$N_Sp)))
      
      ggplotly(p) %>%
        layout(plot_bgcolor = 'rgb(254, 247, 234)') %>%
        layout(paper_bgcolor = 'rgb(254, 247, 234)') %>%
        layout(legend = list(bgcolor = 'rgb(254, 247, 234)'))
    } else {
      plot_ly(datDP, x = ~P*100) %>%
        add_trace(y = ~N_Sp, name = 'Specificity', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = ~N_Sn, name = 'Sensitivity', type = 'scatter', mode = 'lines+markers') %>%
        layout(
          plot_bgcolor = 'rgb(254, 247, 234)',
          paper_bgcolor = 'rgb(254, 247, 234)',
          xaxis = list(title = "Prevalence (%)"),
          yaxis = list(title = "Sample Size Required")
        )
    }
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
      stpS = ifelse(input$pRangeMMpwr[2]-input$pRangeMMpwr[1] > 100, 1, 10)
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
    if (input$pwrMM == 'Power') {
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize * (1 + input$LTFUPMM))
    } else {
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)
    }

    # Create base plot
    p <- if (input$plot_xkcd) {
      ggplot(ss, aes(x = Power, y = TotalSampleSize)) +
        geom_point() +
        geom_line() +
        theme_xkcd() + 
        xkcdaxis(range(ss$Power), range(ss$TotalSampleSize)) +
        labs(x = "Power", y = "Sample Size Required")
    } else {
      plot_ly(ss, x = ~Power, y = ~TotalSampleSize,
              type = 'scatter', mode = 'lines+markers')
    }

    # Add common layout settings
    p %>%
      layout(plot_bgcolor = 'rgb(254, 247, 234)',
             paper_bgcolor = 'rgb(254, 247, 234)',
             showlegend = FALSE) %>%
      layout(xaxis = list(title = "Power"),
             yaxis = list(title = "Sample Size Required"))
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
    if (input$pwrRNA == 'Power') {
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize * (1 + input$LTFUPRNA))
    } else {
      ss$TotalSampleSize <- ceiling(ss$TotalSampleSize)
    }

    # Create plot
    p <- if (input$plot_xkcd) {
      ggplot(ss, aes(x = Power, y = TotalSampleSize)) +
        geom_point() +
        geom_line() +
        theme_xkcd() + 
        xkcdaxis(range(ss$Power), range(ss$TotalSampleSize)) +
        labs(x = "Power", y = "Sample Size Required")
    } else {
      plot_ly(ss, x = ~Power, y = ~TotalSampleSize, 
              type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Power"),
          yaxis = list(title = "Sample Size Required")
        )
    }

    # Add common layout settings
    p %>%
      layout(plot_bgcolor = 'rgb(254, 247, 234)',
             paper_bgcolor = 'rgb(254, 247, 234)',
             showlegend = FALSE)
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

  output$srun_interpretation <- renderUI({
    ci <- input$ci_srun
    r <- input$r_srun
    n <- ceiling(log(1 - ci) / log(r))
    
    # Return styled HTML output
    HTML(sprintf(
        "<div style='font-size: 28px; line-height: 1.5; color: #ffffff;'>
            <p>With %d consecutive successes:</p>
            <ul>
                <li>You can be %d%% confident that the true reliability is at least %d%%</li>
                <li>This means if the true reliability were lower than %d%%, the probability of seeing %d consecutive successes would be less than %d%%</li>
            </ul>
        </div>",
        n,
        round(ci * 100),
        round(r * 100),
        round(r * 100),
        n,
        round((1 - ci) * 100)
    ))
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
    # Create matrix from input values
    dat <- matrix(c(input$rc_11, input$rc_12,
                   input$rc_21, input$rc_22), 
                 nrow=2, byrow=TRUE)
    
    # Add row and column names
    rownames(dat) <- c("Row1", "Row2")
    colnames(dat) <- c("Col1", "Col2")
    
    # Convert to table
    dat <- as.table(dat)
    return(dat)
  })

  output$RCdt <- renderPrint({
    # Get contingency table data
    dat <- RCinputData()
    
    # Perform appropriate test based on input
    if (input$MCtest) {
      test_result <- mcnemar.test(dat)
    } else {
      if (any(dat < 5)) {
        test_result <- fisher.test(dat, workspace=2000000)
      } else {
        test_result <- chisq.test(dat)
      }
    }
    
    # Print results
    print(test_result)
  })


  # require(broom)
  # tidy(chisq.test(dat))


  ######################
  ## Diagnostic Tests ##
  ######################
  DTPinputData <- reactive({
    # Create matrix from input values
    dat <- matrix(c(input$dtp_11, input$dtp_12,
                   input$dtp_21, input$dtp_22), 
                 nrow=2, byrow=TRUE)
    
    # Convert to factors with explicit levels
    dat <- as.table(dat)
    dimnames(dat) <- list(
      Reference = factor(c("Positive", "Negative"), levels=c("Positive", "Negative")),
      Predicted = factor(c("Positive", "Negative"), levels=c("Positive", "Negative"))
    )
    
    return(dat)
  })

  output$DTPdt <- renderPrint({
    # Get diagnostic test data
    dat <- DTPinputData()
    
    # Calculate metrics manually for more control
    TP <- dat[1,1]
    FP <- dat[2,1]
    FN <- dat[1,2]
    TN <- dat[2,2]
    
    # Calculate performance metrics
    sensitivity <- TP/(TP + FN)
    specificity <- TN/(TN + FP)
    ppv <- TP/(TP + FP)
    npv <- TN/(TN + FN)
    accuracy <- (TP + TN)/sum(dat)
    
    # Calculate confidence intervals
    sen_ci <- binom.test(TP, TP + FN)$conf.int
    spec_ci <- binom.test(TN, TN + FP)$conf.int
    ppv_ci <- binom.test(TP, TP + FP)$conf.int
    npv_ci <- binom.test(TN, TN + FN)$conf.int
    acc_ci <- binom.test(TP + TN, sum(dat))$conf.int
    
    # Calculate likelihood ratios
    LRplus <- sensitivity/(1-specificity)
    LRminus <- (1-sensitivity)/specificity
    
    # Print standard metrics
    cat("\nDiagnostic Test Performance Metrics:\n")
    cat("=====================================\n")
    cat(sprintf("Sensitivity: %.3f (95%% CI: %.3f-%.3f)\n", 
                sensitivity, sen_ci[1], sen_ci[2]))
    cat(sprintf("Specificity: %.3f (95%% CI: %.3f-%.3f)\n",
                specificity, spec_ci[1], spec_ci[2]))
    cat(sprintf("PPV: %.3f (95%% CI: %.3f-%.3f)\n", 
                ppv, ppv_ci[1], ppv_ci[2]))
    cat(sprintf("NPV: %.3f (95%% CI: %.3f-%.3f)\n", 
                npv, npv_ci[1], npv_ci[2]))
    cat(sprintf("Accuracy: %.3f (95%% CI: %.3f-%.3f)\n", 
                accuracy, acc_ci[1], acc_ci[2]))
    cat(sprintf("Positive Likelihood Ratio: %.2f\n", LRplus))
    cat(sprintf("Negative Likelihood Ratio: %.2f\n", LRminus))
    
    # Print raw counts
    cat("\nConfusion Matrix:\n")
    cat("=================\n")
    print(dat)
    
    # Generate standardized report text
    cat("\nStandardized Report Text:\n")
    cat("=======================\n")
    
    # Break the report into smaller chunks for better formatting
    report_chunks <- c(
        sprintf("Our test demonstrated a sensitivity of %.1f%% (95%% CI: %.1f-%.1f%%) and specificity of %.1f%% (95%% CI: %.1f-%.1f%%).",
            sensitivity*100, sen_ci[1]*100, sen_ci[2]*100,
            specificity*100, spec_ci[1]*100, spec_ci[2]*100),
            
        sprintf("The positive predictive value was %.1f%% (95%% CI: %.1f-%.1f%%) and negative predictive value was %.1f%% (95%% CI: %.1f-%.1f%%).",
            ppv*100, ppv_ci[1]*100, ppv_ci[2]*100,
            npv*100, npv_ci[1]*100, npv_ci[2]*100),
            
        sprintf("Overall accuracy was %.1f%% (95%% CI: %.1f-%.1f%%). The test had a positive likelihood ratio of %.2f and negative likelihood ratio of %.2f.",
            accuracy*100, acc_ci[1]*100, acc_ci[2]*100,
            LRplus, LRminus),
            
        sprintf("Based on the confusion matrix, out of %.0f total cases, there were %.0f true positives, %.0f true negatives, %.0f false positives, and %.0f false negatives.",
            sum(dat), TP, TN, FP, FN)
    )
    
    # Wrap and print each chunk
    wrapped_text <- sapply(report_chunks, function(x) {
        paste(strwrap(x, width = 80), collapse = "\n")
    })
    cat(paste(wrapped_text, collapse = "\n\n"))
    
    # Add interpretation guide with controlled width
    guide_text <- c(
        "\n\nInterpretation Guide:",
        "-------------------",
        "• Likelihood Ratios (LR):",
        "  - LR+ >10 or LR- <0.1: Strong diagnostic evidence",
        "  - LR+ 5-10 or LR- 0.1-0.2: Moderate diagnostic evidence", 
        "  - LR+ 2-5 or LR- 0.2-0.5: Weak diagnostic evidence",
        "  - LR+ 1-2 or LR- 0.5-1: Negligible diagnostic evidence"
    )
    
    cat(paste(guide_text, collapse = "\n"))
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
      ggplot(dat, aes(d = Outcome, m = Measurement)) + 
        geom_roc() + 
        style_roc() +
        annotate("text", label = paste0("AUC: ", round(txt$auc,2)), y=0.25, x=0.75) +
        theme(panel.background = element_rect(fill="#fef7ea", colour="#fef7ea"),
              plot.background = element_rect(fill = "#fef7ea"))
    )

    if (input$plot_xkcd) {
      p + theme_xkcd()
    } else {
      p
    }
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



