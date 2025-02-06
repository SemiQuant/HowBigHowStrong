require(shiny)
require(shinythemes)
require(shinyBS)
require(shinydashboard)
require(DT)
require(dashboardthemes)
require(plotly)
require(shinyjs)
require(shinyAce)
require(shinyalert)
# require(shinyWidgets)
# require(sendmailR)
# devtools::install_github("nik01010/dashboardthemes")
require(dashboardthemes)

#################
dark_grey_edited <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Helvetica Neue, Helvetica, Arial, sans-serif"
  ,appFontColor = "rgb(205,205,205)"
  ,bodyBackColor = "rgb(39,43,48)"

  ### header
  ,logoBackColor = "rgb(70,80,90)"

  ,headerButtonBackColor = "rgb(70,80,90)"
  ,headerButtonIconColor = "rgb(198, 253, 168)"
  ,headerButtonBackColorHover = "rgb(40,50,60)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"

  ,headerBackColor = "rgb(70,80,90)"
  ,headerBoxShadowColor = "rgb(198, 253, 168)"
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "rgb(52,62,72)"
  ,sidebarPadding = 0

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "rgb(205,205,205)"

  ,sidebarSearchBackColor = "rgb(39,43,48)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(39,43,48)"

  ,sidebarTabTextColor = "rgb(205,205,205)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0

  ,sidebarTabBackColorSelected = "rgb(70,80,90)"
  ,sidebarTabTextColorSelected = "rgb(198, 253, 168)"
  ,sidebarTabRadiusSelected = "5px"

  ,sidebarTabBackColorHover = "rgb(55,65,75)"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "5px"

  ### boxes
  ,boxBackColor = "rgb(52,62,72)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(52,62,72)"
  ,boxPrimaryColor = "rgb(200,200,200)"
  ,boxSuccessColor = "rgb(155,240,80)"
  ,boxWarningColor = "rgb(240,80,210)"
  ,boxDangerColor = "rgb(240,80,80)"

  ,tabBoxTabColor = "rgb(52,62,72)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(205,205,205)"
  ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
  ,tabBoxBackColor = "rgb(52,62,72)"
  ,tabBoxHighlightColor = "rgb(70,80,90)"
  ,tabBoxBorderRadius = 5

  ### inputs
  ,buttonBackColor = "rgb(230,230,230)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(50,50,50)"
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(180,180,180)"
  ,buttonTextColorHover = "rgb(50,50,50)"
  ,buttonBorderColorHover = "rgb(50,50,50)"

  ,textboxBackColor = "rgb(68,80,90)"
  ,textboxBorderColor = "rgb(76,90,103)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(80,90,100)"
  ,textboxBorderColorSelect = "rgb(255,255,255)"

  ### tables
  ,tableBackColor = "rgb(52,62,72)"
  ,tableBorderColor = "rgb(70,80,90)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1

)

#####################


# https://rstudio.github.io/shinydashboard/structure.html
title <- "Sample Size Calculator"
#  link not working
header <- dashboardHeader(title =  div(img(src = "sq.png", height="42.885", width="34.29"), title), titleWidth = 300,
                          # dropdownMenu(type = "notifications", icon = icon("medkit"), badgeStatus = NULL,
                          #              notificationItem(text = "Center for Lung Infection and Immunity",
                          #                               href = "http://lunginstitute.co.za/liiu/")
                          # ),
                          dropdownMenu(type = "notifications", icon = icon("copyright"), badgeStatus = NULL,
                                       notificationItem(text = "Copyright 2018")
                          )
)

sidebar <- dashboardSidebar(
  width = 300,
  h3("Select a Test", align = "center"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                convertMenuItem(
                  menuItem("Cohort/RCT", tabName = "rct", icon = icon("calculator"),
                           radioButtons("pwrRCT", "Select type:", inline = T, choices = c("Power", "Sample Size"), selected = "Power"),
                           sliderInput("alp", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.01),
                           bsTooltip("alp", "The significance level (Type I error rate) - probability of rejecting null when it's true", 
                                   placement = "right", trigger = "hover"),
                           sliderInput("outcome1", "Select Outcome 1 expected proportion:", min = 0.1, max = 0.99, 0.5, step = 0.01),
                           bsTooltip("outcome1", "Expected proportion of success/event in group 1",
                                   placement = "right", trigger = "hover"),
                           sliderInput("outcome2", "Select Outcome 2 expected proportion:", min = 0.1, max = 0.99, 0.65, step = 0.01),
                           bsTooltip("outcome2", "Expected proportion of success/event in group 2",
                                   placement = "right", trigger = "hover"),
                           conditionalPanel("input.pwrRCT === 'Power'",
                                            sliderInput("fraction", "Select fraction of observations in Outcome 1 (use 0.5 if equal populations):",
                                                        min = 0.01, max = 0.5, 0.5, step = 0.01),
                                            bsTooltip("fraction", "Proportion of total sample size allocated to group 1",
                                                    placement = "right", trigger = "hover"),
                                            sliderInput("pRange", "Select power range to plot:", min = 0.1, max = 0.99, value = c(0.6, 0.95), step = 0.01),
                                            bsTooltip("pRange", "Range of statistical power values to calculate sample sizes for",
                                                    placement = "right", trigger = "hover"),
                                            sliderInput("LTFU", "Add proportion for LTFU and interim calculations:", min = 0, max = 1, 0, step = 0.01),
                                            bsTooltip("LTFU", "Additional proportion to account for potential loss to follow-up",
                                                    placement = "right", trigger = "hover"),
                                            sliderInput("prev1", "Prevalence Outcome 1:", min = 0, max = 1, 1, step = 0.01),
                                            bsTooltip("prev1", "Disease/condition prevalence in group 1",
                                                    placement = "right", trigger = "hover"),
                                            sliderInput("prev2", "Prevalence Outcome 2:", min = 0, max = 1, 1, step = 0.01),
                                            bsTooltip("prev2", "Disease/condition prevalence in group 2",
                                                    placement = "right", trigger = "hover")
                           ),
                           conditionalPanel("input.pwrRCT === 'Sample Size'",
                                            sliderInput("nrct", "Sample Size:", min = 2, value = c(50, 100), step = 1, max = 10000)
                           )
                  ),"rct"),


                menuItem("Proportions", icon = icon("calculator"),
                         convertMenuItem(
                           menuItem("One Sample Proportion", tabName = "propSS", icon = icon("calculator"),
                                    # conditionalPanel("input.sidebarmenu === 'propSS'",
                                    radioButtons("pwrpropSS", "Select type:", inline = T, choices = c("Power", "Sample Size"), selected = "Power"),
                                    sliderInput("alpProp", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.01),
                                    selectInput(
                                      inputId = "altProp", label = "Alternative:", multiple = F,
                                      choices = c("two.sided", "greater", "less"), selected = "two.sided"),
                                    numericInput("effProp", "Effect Size:", 0.2, min = 0.0001),
                                    conditionalPanel("input.pwrpropSS === 'Power'",
                                                     sliderInput("pRangeProp", "Select power range to plot:", min = 0.1, max = 0.99, value = c(0.6, 0.95), step = 0.1),
                                                     sliderInput("LTFUProp", "Add proportion for LTFU and interim calculations:", min = 0, max = 1, 0, step = 0.1)
                                    ),
                                    conditionalPanel("input.pwrpropSS === 'Sample Size'",
                                                     sliderInput("pRangePropSsize", "Sample Size:", min = 2, value = c(50, 100), step = 1, max = 10000)
                                    )

                           ), "propSS"),

                         convertMenuItem(
                           menuItem("Two Sample Proportion", tabName = "propTS", icon = icon("calculator"),
                                    # conditionalPanel("input.sidebarmenu === 'propTS'",
                                    radioButtons("pwrpropTS", "Select type:", inline = T, choices = c("Power", "Sample Size"), selected = "Power"),
                                    sliderInput("alpPropTS", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.01),
                                    bsTooltip("alpPropTS", "The significance level (Type I error rate) for the test", 
                                              placement = "right", trigger = "hover"),
                                    selectInput("altPropTS", "Alternative:", multiple = F,
                                                choices = c("two.sided", "greater", "less"), 
                                                selected = "two.sided"),
                                    bsTooltip("altPropTS", "Type of alternative hypothesis - two-sided tests for any difference, greater/less tests for specific direction", 
                                              placement = "right", trigger = "hover"),
                                    numericInput("effPropTS", "Effect Size:", 0.2, min = 0.01),
                                    bsTooltip("effPropTS", "The minimum difference in proportions you want to detect", 
                                              placement = "right", trigger = "hover"),
                                    conditionalPanel("input.pwrpropTS === 'Power'",
                                                     sliderInput("pRangePropTS", "Select power range to plot:", min = 0.1, max = 0.99, value = c(0.6, 0.95), step = 0.01),
                                                     sliderInput("LTFUPropTS", "Add proportion for LTFU and interim calculations:", min = 0, max = 1, 0, step = 0.01)
                                    ),
                                    conditionalPanel("input.pwrpropTS === 'Sample Size'",
                                                     sliderInput("pRangePropTSsize", "Sample Size:", min = 2, value = c(50, 100), step = 1, max = 10000)
                                    )
                           ), "propTS"),

                         convertMenuItem(
                           menuItem("Population Proportion (CI)", tabName = "PP", icon = icon("calculator"),
                                    radioButtons("pwrpropPP", "Select type:", inline = T, choices = c("Power"), selected = "Power"),
                                    sliderInput("alpPP", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.01),
                                    bsTooltip("alpPP", "The significance level for confidence interval calculation", 
                                              placement = "right", trigger = "hover"),
                                    sliderInput("fractionPP", "Select likely sample proportion:",
                                              min = 0.01, max = 0.99, 0.5, step = 0.01),
                                    bsTooltip("fractionPP", "Expected proportion in your population of interest", 
                                              placement = "right", trigger = "hover"),
                                    numericInput("popSizePP", "Total population size (leave as 0 if unknown):", value=0, min = 0),
                                    bsTooltip("popSizePP", "Known population size - affects calculation for finite populations", 
                                              placement = "right", trigger = "hover"),
                                    sliderInput("marginPP", "Margin of error:", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
                                    bsTooltip("marginPP", "Desired margin of error for confidence interval", 
                                              placement = "right", trigger = "hover"),
                                    sliderInput("nRangePP", "Sample size range to plot:", 
                                              min = 10, max = 1000, value = c(30, 500), step = 10),
                                    bsTooltip("nRangePP", "Range of sample sizes to evaluate",
                                              placement = "right", trigger = "hover")
                           ), "PP"),
                         convertMenuItem(
                           menuItem("Diagnostic Sens/Spec CI width", tabName = "DP", icon = icon("calculator"),
                                    sliderInput("wP", "Range for the prevalence of disease in the target population:", min = 0, max = 1, step = 0.1,
                                                value = c(0.2, 0.8)),
                                    sliderInput("alpDC", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.01),
                                    numericInput("wDP", "Maximum clinically acceptable width of the 95% CI:", value=0.1, min = 0.01, max = 0.9, step = 0.05),
                                    numericInput("wSn", "Expected sensitivity of the new diagnostic test:", value=0.8, min = 0.01, max = 1, step = 0.05),
                                    numericInput("wSp", "Expected specificity of the new diagnostic test:", value=0.8, min = 0.01, max = 1, step = 0.05)
                           ), "DP")


                ),

                convertMenuItem(
                  menuItem("Two Means", tabName = "MM", icon = icon("calculator"),
                           # conditionalPanel("input.sidebarmenu === 'MM'",
                           radioButtons("pwrMM", "Select type:", inline = T, choices = c("Power", "Sample Size"), selected = "Power"),
                           sliderInput("alpMM", "Select alpha:", min = 0.001, max = 0.2, 0.05, step = 0.01),
                           bsTooltip("alpMM", "The significance level (Type I error rate) for the test", 
                                    placement = "right", trigger = "hover"),
                           selectInput(
                             inputId = "typM", label = "Type:", multiple = F,
                             choices = c("two.sample", "one.sample", "paired"), selected = "two.sample"),
                           bsTooltip("typM", "Type of t-test to perform", placement = "right", trigger = "hover"),
                           selectInput(
                             inputId = "altMM", label = "Alternative:", multiple = F,
                             choices = c("two.sided", "greater", "less"), selected = "two.sided"),
                           numericInput("Mm1", "Mean of group 1:", value=132.86, min = 0),
                           bsTooltip("Mm1", "Expected mean value for first group", placement = "right", trigger = "hover"),
                           numericInput("SdM1", "Standard deviation of group 1:", value=15.34, min = 0),
                           bsTooltip("SdM1", "Expected standard deviation for first group", placement = "right", trigger = "hover"),
                           numericInput("Mm2", "Mean of group 2:", value=127.44, min = 0),
                           numericInput("SdM2", "Standard deviation of group 2:", value=18.23, min = 0),
                           numericInput("altSDMM", "Effect size (Cohen's d; leave this 0 if using the above Means and SDs):", value=0, min = 0),
                           conditionalPanel("input.pwrMM === 'Power'",
                                            sliderInput("pRangeMM", "Power:", min = 0.1, max = 0.99, value = c(0.6, 0.95), step = 0.01),
                                            sliderInput("LTFUPMM", "Add proportion for LTFU and interim calculations:", min = 0, max = 1, 0, step = 0.01)
                           ),
                           conditionalPanel("input.pwrMM === 'Sample Size'",
                                            sliderInput("pRangeMMpwr", "Sample Size:", min = 2, value = c(50, 100), step = 1, max = 10000)
                           )), "MM"
                ),


                convertMenuItem(
                  menuItem("Success Run", tabName = "srun", icon = icon("calculator"),
                           sliderInput("ci_srun", "Select a confidence interval:", min = 0.001, max = 1, 0.95, step = 0.01),
                           bsTooltip("ci_srun", "Desired confidence level for the success run analysis", 
                                     placement = "right", trigger = "hover"),
                           sliderInput("r_srun", "Select the reliability of the test:", min = 0.001, max = 1, 0.95, step = 0.05),
                           bsTooltip("r_srun", "Required reliability level for the success run", 
                                     placement = "right", trigger = "hover")
                  ), "srun"),


                convertMenuItem(
                  menuItem("Seq Success", tabName = "seq_suc", icon = icon("calculator"),
                           sliderInput("vaf", "Allele Freq:", min = 0.01, max = 1, 0.1, step = 0.005),
                           bsTooltip("vaf", "Expected variant allele frequency", 
                                     placement = "right", trigger = "hover"),
                           sliderInput("ber", "Background (sequencing) error rate:", min = 0.001, max = 0.7, 0.01, step = 0.005),
                           bsTooltip("ber", "Expected sequencing error rate for the platform", 
                                     placement = "right", trigger = "hover"),
                           sliderInput("seq_conf", "Confidence interval:", min = 0.05, max = 1, 0.95, step = 0.05),
                           bsTooltip("seq_conf", "Desired confidence level for the analysis", 
                                     placement = "right", trigger = "hover"),
                           sliderInput("seq_pwd", "Power:", min = 0.05, max = 1, 0.90, step = 0.05),
                           bsTooltip("seq_pwd", "Desired statistical power to detect variants", 
                                     placement = "right", trigger = "hover")
                  ), "seq_suc"),



                convertMenuItem(
                  menuItem("RNA Seq", tabName = "RNAss", icon = icon("calculator"),
                           # conditionalPanel("input.sidebarmenu === 'RNAss'",
                           radioButtons("pwrRNA", "Select type:", inline = T, choices = c("Power", "Sample Size"), selected = "Power"),
                           sliderInput("alpRNA", "Select FDR alpha:", min = 0.001, max = 0.2, 0.05, step = 0.001),
                           bsTooltip("alpRNA", "False Discovery Rate threshold for multiple testing correction",
                                     placement = "right", trigger = "hover"),
                           sliderInput("rhoRNA", "Minimal fold change between two groups:", min = 0.5, max = 50, 2, step = 0.5),
                           bsTooltip("rhoRNA", "Minimum fold change in expression you want to detect",
                                     placement = "right", trigger = "hover"),
                           numericInput("lambda0RNA", "Minimal average read counts:", value=5, min = 1),
                           numericInput("phi0RNA", "Maximal dispersion:", value= 0.5, min = 0),
                           conditionalPanel("input.pwrRNA === 'Power'",
                                            sliderInput("LTFUPRNA", "Add proportion for LTFU and interim calculations:", min = 0, max = 1, 0, step = 0.01),
                                            sliderInput("pRangeRNA", "Power:", min = 0.1, max = 0.99, value = c(0.94, 0.95), step = 0.01)
                           ),
                           conditionalPanel("input.pwrRNA === 'Sample Size'",
                                            sliderInput("pRangeRNAss", "Sample Size:", min = 2, value = c(95, 100), step = 1, max = 10000)
                           )
                  ), "RNAss"),


                conditionalPanel("input.sidebarmenu !== 'dashboard'",
                                 checkboxInput("plot_xkcd", "Plot xkcd", value = F)
                ),


                menuItem("Other Cals", tabName = "Ocalc", startExpanded = TRUE,
                         convertMenuItem(
                         menuItem("Contigency (RxC) Tables", tabName = "ChiTab",
                         fluidRow(
                           column(6,
                                  numericInput("rc_11", "Row1 Col1:", value=3),
                                  numericInput("rc_12", "Row1 Col2:", value=1)
                           ),
                           column(6, 
                                  numericInput("rc_21", "Row2 Col1:", value=1),
                                  numericInput("rc_22", "Row2 Col2:", value=3)
                           )
                         ),
                         checkboxInput("MCtest", "Use Mcnemar Test")
                         ), "ChiTab"),

                         convertMenuItem(
                         menuItem("Diagnostic Test Performance", tabName = "DiagTAB",
                         fluidRow(
                           column(6,
                                  numericInput("dtp_11", "True Positive:", value=3),
                                  numericInput("dtp_12", "False Positive:", value=1)
                           ),
                           column(6,
                                  numericInput("dtp_21", "False Negative:", value=1), 
                                  numericInput("dtp_22", "True Negative:", value=3)
                           )
                         )
                         ), "DiagTAB")
                ),

                menuItem("Useful Information", tabName = "Info",
                         menuItem("What is power?", tabName = "WhatIs", icon = icon("info")),
                         menuItem("A confusion matrix", tabName = "CM", icon = icon("info")),
                         menuItem("Reporting Results", tabName = "ReportResults", icon = icon("file-text"))
                ),

                menuItem("Cite", icon = icon("info"), tabName = "cite"
                ),

                menuItem("Website", icon = icon("chrome"),
                         href = "http://www.semiquant.com")
    )
  ),
  br()
)


body <- dashboardBody(
  dark_grey_edited,
  # Add enhanced SEO meta tags
  tags$head(
    # Basic meta tags with expanded keywords and description
    tags$meta(name="description", content="Free online sample size and power calculator for medical research, clinical trials, and scientific studies. Calculate sample sizes for RCTs, proportions, means, RNA-seq, diagnostic tests and more. User-friendly interface with instant results and visualizations."),
    tags$meta(name="keywords", content="sample size calculator, power calculator, clinical trial calculator, medical research calculator, statistical power analysis, RCT sample size, RNA-seq sample size, diagnostic test calculator, research planning tool, biostatistics calculator, clinical research calculator, study design tool, medical statistics, cohort study calculator, proportion calculator, means calculator, free sample size calculator"),
    tags$meta(name="author", content="Jason Limberis/UCSF"),
    
    # Additional meta tags for search engines
    tags$meta(name="robots", content="index, follow"),
    tags$meta(name="language", content="English"),
    tags$meta(name="revisit-after", content="7 days"),
    
    # Enhanced social media meta tags
    tags$meta(property="og:title", content="Free Sample Size & Power Calculator for Medical Research"),
    tags$meta(property="og:description", content="Professional-grade calculator for sample size and power calculations in medical research, clinical trials, and scientific studies. Free, easy to use, and comprehensive."),
    tags$meta(property="og:type", content="website"),
    tags$meta(property="og:url", content="https://www.semiquant.com/"),
    tags$meta(property="og:image", content="https://github.com/SemiQuant/DEAD/blob/master/www/sq.png"),
    tags$meta(property="og:site_name", content="SemiQuant Sample Size Calculator"),
    
    # Enhanced Twitter card meta tags
    tags$meta(name="twitter:card", content="summary_large_image"),
    tags$meta(name="twitter:title", content="Free Sample Size & Power Calculator for Medical Research"),
    tags$meta(name="twitter:description", content="Professional-grade calculator for sample size and power calculations in medical research, clinical trials, and scientific studies. Free, easy to use, and comprehensive."),
    tags$meta(name="twitter:image", content="https://github.com/SemiQuant/DEAD/blob/master/www/sq.png"),
    
    # Enhanced structured data in JSON-LD format
    tags$script(type="application/ld+json",
      '{
        "@context": "https://schema.org",
        "@type": "WebApplication",
        "name": "SemiQuant Sample Size & Power Calculator",
        "description": "Professional online calculator for sample size and power calculations in medical research and clinical trials. Free tool for researchers, clinicians, and scientists.",
        "url": "https://www.semiquant.com",
        "applicationCategory": "Scientific Calculator",
        "operatingSystem": "Any",
        "offers": {
          "@type": "Offer",
          "price": "0",
          "priceCurrency": "USD"
        },
        "featureList": [
          "RCT sample size calculation",
          "Power analysis",
          "Proportion testing",
          "RNA-seq sample size",
          "Diagnostic test calculations",
          "Success run analysis",
          "Two means comparison",
          "Population proportion estimation"
        ],
        "author": {
          "@type": "Person",
          "name": "Jason Limberis",
          "affiliation": "UCSF"
        },
        "about": {
          "@type": "Thing",
          "name": "Medical Research Statistics",
          "description": "Statistical calculations for medical research and clinical trials"
        },
        "audience": {
          "@type": "Audience",
          "audienceType": "Researchers, Clinicians, Scientists"
        },
        "keywords": "sample size calculator, power calculator, clinical trial calculator, medical research, statistical power, RCT sample size, RNA-seq sample size"
      }'
    ),
    
    # Existing style tags
    tags$style(HTML("
      /* Increase main content text size */
      .content-wrapper {
        font-size: 18px !important;
      }
      .content-wrapper h2 {
        font-size: 30px !important;
      }
      .content-wrapper h3 {
        font-size: 24px !important;
      }
      .content-wrapper h4 {
        font-size: 20px !important;
      }
      .content-wrapper p, 
      .content-wrapper li,
      .content-wrapper .box-body {
        font-size: 16px !important;
      }
      /* Preserve existing styles */
      .irs-max {font-family: 'arial'; color: white;}
      .irs-min {font-family: 'arial'; color: white;}
    ")),
    tags$style(HTML("
        .large-interpretation {
            font-size: 28px !important;
            line-height: 1.5 !important;
            color: #ffffff !important;
        }
    "))
  ),
  # fluidRow("hello"),
  tabItems(
    tabItem(tabName = "dashboard",
        fluidRow(
            box(width = 12, status = "primary",
                div(
                    style = "text-align: center; padding: 20px;",
                    h1("Sample Size & Power Calculator", 
                       style = "color: #ffffff; margin-bottom: 30px; font-weight: 300;"),
                    p(style = "font-size: 18px; color: #c8c8c8; margin-bottom: 40px;",
                      "An intuitive tool for researchers to calculate sample sizes and statistical power",
                      "for various study designs and analyses.")
                ),
                
                div(
                    style = "display: flex; justify-content: space-around; flex-wrap: wrap; margin: 20px 0;",
                    
                    # Study Design Card
                    div(
                        style = "background-color: rgb(52,62,72); padding: 20px; margin: 10px; border-radius: 5px; width: 45%; min-width: 300px;",
                        h3("Study Design", style = "color: rgb(198, 253, 168); border-bottom: 1px solid rgb(70,80,90); padding-bottom: 10px;"),
                        tags$ul(
                            style = "color: #c8c8c8; font-size: 16px;",
                            tags$li(strong("Cohort/RCT:"), "Compare proportions in cohort studies or RCTs"),
                            tags$li(strong("One Sample Proportion:"), "Test single proportion against null"),
                            tags$li(strong("Two Sample Proportion:"), "Compare independent group proportions"),
                            tags$li(strong("Population Proportion (CI):"), "Calculate confidence intervals"),
                            tags$li(strong("Two Means:"), "Compare means using t-tests")
                        )
                    ),
                    
                    # Specialized Tests Card
                    div(
                        style = "background-color: rgb(52,62,72); padding: 20px; margin: 10px; border-radius: 5px; width: 45%; min-width: 300px;",
                        h3("Specialized Tests", style = "color: rgb(198, 253, 168); border-bottom: 1px solid rgb(70,80,90); padding-bottom: 10px;"),
                        tags$ul(
                            style = "color: #c8c8c8; font-size: 16px;",
                            tags$li(strong("RNA Seq:"), "Sample size for RNA sequencing analysis"),
                            tags$li(strong("Success Run:"), "Consecutive successes for reliability"),
                            tags$li(strong("Seq Success:"), "Sample size for variant detection"),
                            tags$li(strong("Diagnostic Test:"), "Calculate diagnostic metrics")
                        )
                    )
                ),
                
                # Tutorial Section
                div(
                    style = "text-align: center; margin-top: 40px; padding: 20px; background-color: rgb(52,62,72); border-radius: 5px;",
                    h3("Quick Start Tutorial", style = "color: rgb(198, 253, 168); margin-bottom: 20px;"),
                    tags$video(
                        src = "SampleSizeCalcTut.mp4", 
                        type = "mp4", 
                        controls = TRUE,
                        width = "60%", 
                        poster = "sq_pad.png",
                        style = "border-radius: 5px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"
                    )
                ),
                
                # Footer
                div(
                    style = "margin-top: 40px; padding: 20px; text-align: center; border-top: 1px solid rgb(70,80,90);",
                    p(style = "color: #7a8288;",
                      "Have a suggestion or need a specific test? ",
                      tags$a(
                          href = "mailto:Jason.Limberis@uct.ac.za?subject=Sample Size Calculator Suggestion",
                          "Email us",
                          style = "color: rgb(198, 253, 168);"
                      )
                    ),
                    
                    p(style = "color: #7a8288; margin-top: 20px;",
                      "Powered by R packages: ",
                      tags$span(
                          style = "color: #c8c8c8;",
                          "pwr, samplingbook, RnaSeqSampleSize, Hmisc"
                      )
                    )
                )
            )
        )
    ),


    tabItem(tabName = "rct",
            h3("Cohort/RCT Sample Size Calculator"),
            p("This calculator helps determine sample size requirements for comparing two proportions in a cohort study or randomized controlled trial (RCT)."),
            p("Uses method of Fleiss, Tytun, and Ury (without continuity correction) to estimate:"),
            tags$ul(
              tags$li("Power - given a sample size"),
              tags$li("Sample size - to achieve a desired power")
            ),
            p("For a two-sided test comparing the difference between two proportions."),
            h5("*Reference: Fleiss JL, Tytun A, Ury HK (1980): A simple approximation for calculating sample sizes for comparing independent proportions. Biometrics 36:343–6."),
            plotlyOutput("RCTplot"),
            br(),
            box(title = "Data Table", collapsible = T, collapsed = T, width = 12,
                DTOutput("RCTplotDT"))
    ),


    tabItem(tabName = "propSS",
            h3("One Sample Proportion Test Calculator"),
            p("Calculate sample size or power for testing a single proportion against a null hypothesis value."),
            p("Common uses include:"),
            tags$ul(
              tags$li("Testing if a population proportion differs from a specified value"),
              tags$li("Determining if a new treatment success rate exceeds a threshold"),
              tags$li("Evaluating if an adverse event rate is below an acceptable level")
            ),
            h5("*Reference: Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum."),
            plotlyOutput("PropOneSSplot"),
            br(),
            box(title = "Data Table", collapsible = T, collapsed = T, width = 12,
                DTOutput("PropOneSSdt"))
    ),


    tabItem(tabName = "propTS",
            h3("Two-sample proportion test"),
            p("This calculator helps determine sample size or power when comparing proportions between two independent groups."),
            p("Common applications include:"),
            tags$ul(
                tags$li("Comparing treatment success rates between two groups"),
                tags$li("Testing if proportions differ between populations"),
                tags$li("Evaluating differences in event rates")
            ),
            plotlyOutput("PropOneTSplot"),
            br(),
            box(title = "Data Table", collapsible = T, collapsed = T, width = 12,
                DTOutput("PropOneTSdt"))
    ),


    tabItem(tabName = "PP",
            h3("Population Proportion Calculator"),
            p("Calculate confidence intervals and sample size requirements for estimating population proportions."),
            p("Useful for:"),
            tags$ul(
                tags$li("Prevalence studies"),
                tags$li("Survey research"),
                tags$li("Population characteristic estimation")
            ),
            br(),
            h2("Currently not working!")
            # plotlyOutput("PPplot"),
            # br(),
            # box(title = "Sample Size Summary", width = 12,
            #     verbatimTextOutput("PPsummary")
            # ),
            # box(title = "Data Table", collapsible = T, collapsed = T, width = 12,
            #     DTOutput("PPdt"))
    ),

    tabItem(tabName = "DP",
            h3("Disease Prevalence and Sample Size"),
            h5("*Buderer, N. M. F. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Academic Emergency Medicine."),
            plotlyOutput("DPplot")
    ),

    tabItem(tabName = "MM",
            h3("Two Means Calculator"),
            p("Calculate sample size or power for comparing means between groups using t-tests."),
            p("Supports:"),
            tags$ul(
                tags$li("One sample t-test (comparing to a known mean)"),
                tags$li("Two independent sample t-test"),
                tags$li("Paired t-test")
            ),
            
            plotlyOutput("MMplot"),
            br(),
            box(title = "Data Table", collapsible = T, collapsed = T, width = 12,
                DTOutput("MMdt"))
    ),


    tabItem(tabName = "RNAss",
            h3("RNA-Seq Sample Size Calculator"), 
            p("Calculate sample size or power for RNA sequencing differential expression analysis."),
            p("Key parameters to consider:"),
            tags$ul(
                tags$li("FDR (False Discovery Rate) threshold"),
                tags$li("Minimum fold change of interest"),
                tags$li("Expected read counts and dispersion")
            ),
            
            plotlyOutput("plotRNAss"),
            "This is quite a slow calculation",
            br(),
            box(title = "Data Table", collapsible = T, collapsed = T, width = 12,
                DTOutput("RNAssdt"))
    ),


    tabItem(tabName = "srun",
            h3("Success Run Calculator"),
            
            # Results box with improved formatting
            box(
                title = "Results",
                width = 12,
                status = "primary",
                div(
                    style = "padding: 15px; background-color: rgb(52,62,72); border-radius: 5px;",
                    div(
                        style = "margin-bottom: 20px;",
                        h4("Required Success Run Length:", style = "color: rgb(198, 253, 168);"),
                        verbatimTextOutput("b_srun")
                    ),
                    div(
                        style = "margin-bottom: 20px;",
                        h4("Interpretation:", style = "color: rgb(198, 253, 168);"),
                        uiOutput("srun_interpretation", style = "font-size: 28px; line-height: 1.5; color: #ffffff;")
                    )
                )
            ),
            # Add explanation box
            box(
                title = "About Success Run Analysis",
                width = 12,
                status = "info",
                p("Success run analysis is used to determine the required number of consecutive successes (n) needed to demonstrate reliability (p)."),
                tags$ul(
                    tags$li("Confidence Level: How certain you want to be about your conclusion (e.g., 95%)"),
                    tags$li("Target Reliability: The minimum acceptable reliability level (e.g., 95% reliable)")
                ),
                            p("Applications include:"),
            tags$ul(
                tags$li("Medical device validation"),
                tags$li("Quality control testing"),
                tags$li("Process reliability assessment")
            ),
            ),
            
            box(
                title = "Example Application",
                width = 12,
                status = "success",
                p("Consider testing a medical device where you need to be 95% confident that it is at least 95% reliable:"),
                tags$ul(
                    tags$li("Set confidence interval to 0.95 (95%)"),
                    tags$li("Set reliability to 0.95 (95%)"),
                    tags$li("The calculator will tell you how many consecutive successful tests you need")
                ),
                p("This approach is particularly useful for:"),
                tags$ul(
                    tags$li("Validation testing where failures are costly or time-consuming"),
                    tags$li("Safety-critical applications requiring high reliability"),
                    tags$li("Quality assurance in manufacturing processes")
                )
            )
    ),


    tabItem(tabName = "seq_suc",
            h3("Sequencing Success Calculator"),
            p("Determine sample size requirements for detecting genetic variants through sequencing."),
            p("Key considerations:"),
            tags$ul(
                tags$li("Expected variant allele frequency"),
                tags$li("Background sequencing error rate"),
                tags$li("Required detection confidence")
            ),
            verbatimTextOutput("seq_sucO")
    ),



    tabItem(tabName = "ChiTab",
            h4("Results (if any cell contains a value less than 5, Fishers Exact test is performed as opposed to Persons Chi Square)"),
            verbatimTextOutput("RCdt")

    ),

    tabItem(tabName = "DiagTAB",
            verbatimTextOutput("DTPdt"),
            br(),
            box(title = "Overview of calculations", collapsible = T, collapsed = F, width = 12,
                img(src = "DtestStats.png", width = "100%")
            )

    ),

    tabItem(tabName = "WhatIs",
            htmlOutput("frame2"),

            tags$p('Still need to edit this text
                   Power is the probability of not making a Type II error (1 – beta).'),
            tags$p('Type II error is the probability of wrongly failing to reject the null  (i.e. you dont see a difference in you test but there is actually a difference).'),
            tags$p('Thus, simply put, power is the probability that the test rejects the null hypothesis (H0) when, in fact, it is false. You want power to be as large as possible.'),
            br(),
            tags$p('What affects power?'),
            tags$li('Significance level (alpha)'),
            tags$li('Sample size'),
            tags$li('Variability, or variance, in the measured response variable'),
            tags$li('Magnitude of the effect'),
            br(),

            tags$p('p-value: How likley our sample results are under our assumption of the truth. Put another way, what is the probability of being this far or further from the null in either direction (two sided test).'),
            tags$p('So for example, our H0 when comparing two means would be H0=u1-u2=0. Type I error is to falsely infer the existence of something that is not there.'),
            tags$p('It is the likelihood that you will report a difference as significant when, in reality, it is not. You want this to be as small as possible.')
            # https://rpsychologist.com/d3/NHST/
    ),

    tabItem(tabName = "CM",
            img(src='ConfusionMatrix_MyEdit.png', align = "center", height="80%", width="80%")
    ),

    tabItem(tabName = "ReportResults",
            h3("How to Report Sample Size and Power Calculations"),
            
            box(title = "General Guidelines", width = 12,
                p("When reporting sample size and power calculations, include:"),
                tags$ul(
                    tags$li("The statistical test that will be used"),
                    tags$li("The significance level (α)"),
                    tags$li("The desired power (1-β)"),
                    tags$li("The expected effect size or difference to detect"),
                    tags$li("Any assumptions made about variability or standard deviations")
                )
            ),
            
            box(title = "Clinical Trial Example", width = 12, status = "primary",
                h4("Example 1: RCT comparing two treatments"),
                p("Sample text: 'Sample size was calculated using a two-sided test with α=0.05 and 80% power 
                  to detect a 15% absolute difference in treatment success rate between groups (60% vs 75%). 
                  Assuming a 10% dropout rate, we need to recruit 164 participants (82 per group).'"),
                
                h4("Example 2: Diagnostic test study"),
                p("Sample text: 'To estimate sensitivity with a precision of ±5% (95% CI), assuming an expected 
                  sensitivity of 85% and disease prevalence of 30%, we calculated a required sample size of 
                  200 participants.'")
            ),
            
            box(title = "Basic Science Example", width = 12, status = "success",
                h4("Example 1: RNA-seq study"),
                p("Sample text: 'Sample size calculations for RNA-seq were performed assuming a fold-change 
                  threshold of 2.0, FDR of 5%, average read depth of 20 million reads per sample, and 
                  biological coefficient of variation of 0.4. To achieve 80% power, we determined that 
                  5 biological replicates per group were needed.'"),
                
                h4("Example 2: Laboratory experiment"),
                p("Sample text: 'Power analysis indicated that 8 samples per group would provide 90% power 
                  to detect a 1.5-fold difference in protein expression (α=0.05), assuming a standard 
                  deviation of 0.4 based on preliminary data.'")
            ),
            
            box(title = "Common Mistakes to Avoid", width = 12, status = "warning",
                tags$ul(
                    tags$li("Not stating the effect size or difference you're trying to detect"),
                    tags$li("Omitting key assumptions that went into the calculation"),
                    tags$li("Not accounting for multiple comparisons when relevant"),
                    tags$li("Failing to justify the chosen effect size or power level"),
                    tags$li("Not mentioning adjustments for anticipated dropout/attrition")
                )
            ),
            
            box(title = "Additional Resources", width = 12,
                p("For more detailed guidance, consult:"),
                tags$ul(
                    tags$li(tags$a(href="https://www.equator-network.org/", "EQUATOR Network reporting guidelines")),
                    tags$li(tags$a(href="https://www.consort-statement.org/", "CONSORT Statement for RCTs")),
                    tags$li(tags$a(href="https://www.strobe-statement.org/", "STROBE Statement for observational studies"))
                )
            )
    )





  )
)


dashboardPage(
  title=title,
  header,
  sidebar,
  body
)



