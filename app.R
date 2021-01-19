library(shinythemes); library(shinyBS)
library(DT); library(dplyr)
source("hies_functions.R")
source("hies_data.R")


ui <- navbarPage("", theme = shinytheme("flatly"), collapsible = T,
  header = 
    tags$head(
        includeHTML("google-analytics.js"),
        tags$style(HTML("
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "))
      ),
    
  tabPanel("Calculator", id="test", 
      div(h2("Hyper-IgE Scoring Systems")),
      sidebarLayout(
        sidebarPanel(width = 4,
        bsCollapse(open = "panelfull",
          bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Patient values"), value = "panelfull",
            div(style = "overflow-y:scroll; max-height: 600px", 
            bsCollapse(
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "IgE"), value = "collapse_IGE",
                            numericInput("label_IGE", label ="Max IU/mL", value = 0),
                            selectInput("label_IGE_REF", label = "> 10x reference range?", 
                                        choices = list("No" = 1,
                                                       "Yes" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Skin abscesses"), value = "collapse_ABSCESS",
                            numericInput("label_ABSCESS", label ="Lifetime", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Pneumonias"), value = "collapse_PNA",
                            numericInput("label_PNA", label ="Lifetime", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Lung abnormalities"), value = "collapse_LUNG",
                            selectInput("label_LUNG", label = "", 
                                        choices = list("None" = 1,
                                                       "Bronchiectasis" = 2,
                                                       "Pneumatocele" = 3), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Retained teeth"), value = "collapse_TEETH",
                            numericInput("label_TEETH", label ="", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Scoliosis"), value = "collapse_SCOLIO",
                            numericInput("label_SCOLIO", label ="Max degree curve", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Abnormal fractures"), value = "collapse_FRX",
                            numericInput("label_FRX", label ="To minor trauma", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Eosinophils"), value = "collapse_EOS",
                            numericInput("label_EOS", label ="Max cells/uL", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Facial structure"), value = "collapse_FACE",
                            selectInput("label_FACE", label = "", 
                                        choices = list("Normal" = 1,
                                                       "Intermediate" = 2,
                                                       "Characteristic facies" = 3), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Midline anomaly"), value = "collapse_MIDLINE",
                            selectInput("label_MIDLINE", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Newborn rash"), value = "collapse_RASH",
                            selectInput("label_RASH", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Eczema"), value = "collapse_ECZEMA",
                            selectInput("label_ECZEMA", label = "At worst stage", 
                                        choices = list("Absent" = 1,
                                                       "Mild" = 2,
                                                       "Moderate" = 3,
                                                       "Severe" = 4), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "URIs"), value = "collapse_URI",
                            numericInput("label_URI", label ="Annual", value = 0)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Candidiasis"), value = "collapse_CANDIDA",
                            selectInput("label_CANDIDA", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Oral" = 2,
                                                       "Fingernail" = 3,
                                                       "Systemic" = 4), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Other serious infection"), value = "collapse_SERIOUS",
                            selectInput("label_SERIOUS", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Fatal infection"), value = "collapse_FATAL",
                            selectInput("label_FATAL", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Hyperextensibility"), value = "collapse_HYPER",
                            selectInput("label_HYPER", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Lymphoma"), value = "collapse_LYMPHOMA",
                            selectInput("label_LYMPHOMA", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Nasal width"), value = "collapse_NASAL",
                            selectInput("label_NASAL", label = "In standard deviations", 
                                        choices = list("< 1 SD" = 1,
                                                       "1-2 SD" = 2,
                                                       "> 2 SD" = 3), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "High palate"), value = "collapse_PALATE",
                            selectInput("label_PALATE", label = "", 
                                        choices = list("Absent" = 1,
                                                       "Present" = 2), selected = 1)),
            bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "Age"), value = "collapse_AGE",
                            numericInput("label_AGE", label ="Years (closest 0.1)", value = 10.0))
          )))
        )),
        mainPanel(
          fluidRow(column(12, h3(textOutput("nih_score")))),
          fluidRow(
            column(12, 
                   bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "NIH HIES score components"),
                                   fluidRow(
                                     column(6,div(DT::dataTableOutput("nih_table_p1"), style = "overflow-x: auto;")),
                                     column(6,div(DT::dataTableOutput("nih_table_p2"), style = "overflow-x: auto;"))
                              
                                   )
                                   ),
                   fluidRow(
                     column(1),
                     column(11,
                            tags$li(tags$b("NIH-HIES score > 30: "), "87.5% sensitivity, 80.6% specificity"),
                            tags$li(tags$b("NIH-HIES score > 20: "), "possible"),
                            tags$li(tags$b("NIH-HIES score > 40: "), "probable"),
                            )))),
          fluidRow(
            column(6, 
                   h3(textOutput("stat3_score")),
                   bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "STAT3 score components"),
                                   div(DT::dataTableOutput("stat3_table"), style = "overflow-x: auto;")),
                   fluidRow(
                     column(1),
                     column(11,
                            tags$li(tags$b("Possible criteria"), 
                                    tags$li(style = "padding-left: 50px;", "STAT3 score > 30"),
                                    tags$li(style = "padding-left: 50px;", "IgE >= 1000 IU/L")),
                            tags$li(tags$b("Probable criteria"), 
                                    tags$li(style = "padding-left: 50px;", "Meets possible criteria"),
                                    tags$li(style = "padding-left: 50px;", "Lack of Th17 cells or HIES family history")),
                            tags$li(tags$b("Definitive criteria"), 
                                    tags$li(style = "padding-left: 50px;", "Meets probably criteria"),
                                    tags$li(style = "padding-left: 50px;", "Dominant negative STAT3 mutation"))
                     ))
                   ),
            column(6, 
                   h3(textOutput("dock8_score")),
                   bsCollapsePanel(p(icon("bars"),HTML('&nbsp;'), "DOCK8 score components"),
                                   div(DT::dataTableOutput("dock8_table"), style = "overflow-x: auto;")),
                   fluidRow(
                     column(1),
                     column(11,
                            tags$li(tags$b("Criteria: "), "91.4% sensitivity, 87.5% specificity", 
                                    tags$li(style = "padding-left: 50px;", "DOCK8 score > 30"),
                                    tags$li(style = "padding-left: 50px;", "NIH-HIES score > 20"),
                                    tags$li(style = "padding-left: 50px;", "DOCK8 score > STAT3 score"))))
                   )
          ),
          fluidRow(column(12, h3("References"))),
          fluidRow(
            column(12, tags$ul(
              tags$li("NIH HIES score - Grimbacher ", em("et al."), " (1999)", a("PMID: 10441580", href = "https://pubmed.ncbi.nlm.nih.gov/10441580/")),
              tags$li("STAT3 score - Woellner ", em("et al."), " (2010)", a("PMID: 20159255", href = "https://pubmed.ncbi.nlm.nih.gov/20159255/")),
              tags$li("DOCK8 score - Engelhardt", em("et al."), " (2015)", a("PMID: 25724123", href = "https://pubmed.ncbi.nlm.nih.gov/25724123/"))))
          )
          # fluidRow(
          #   column(4),
          #   column(4, uiOutput("conditionalDecision"))
          # ),
          # div(DT::dataTableOutput("componentTable"), style = "overflow-x: auto;"),
          # fluidRow(
          #   tags$div(tags$ul(
          #     tags$li(tags$span("Sensitivity = % of patients who DID require 2nd IVIG who would NOT be discharged early")),
          #     tags$li(tags$span("Specificity = % of patients who DID NOT require 2nd IVIG who would HAVE been discharged early")),
          #     tags$li(tags$span("PPV = % of patients above threshold that DID need a 2nd IVIG")),
          #     tags$li(tags$span("NPV = % of patients below threshold that DID NOT need a 2nd IVIG"))))
          # )
        )
      )
    )
)

server <- function(input, output) {
  # test <- reactive({input$label_IGE})
  NIH_ige_score <- reactive({score_finder(input$label_IGE, NIH_ige_key)})
  NIH_abcess_score <- reactive({score_finder(input$label_ABSCESS, NIH_abscess_key)})
  NIH_pna_score <- reactive({score_finder(input$label_PNA, NIH_pna_key)})
  NIH_lung_score <- reactive({score_finder(input$label_LUNG, NIH_lung_key)})
  NIH_teeth_score <- reactive({score_finder(input$label_TEETH, NIH_teeth_key)})
  NIH_scolio_score <- reactive({score_finder(input$label_SCOLIO, NIH_scolio_key)})
  NIH_frx_score <- reactive({score_finder(input$label_FRX, NIH_frx_key)})
  NIH_eos_score <- reactive({score_finder(input$label_EOS, NIH_eos_key)})
  NIH_face_score <- reactive({score_finder(input$label_FACE, NIH_face_key)})
  NIH_midline_score <- reactive({score_finder(input$label_MIDLINE, NIH_midline_key)})
  NIH_rash_score <- reactive({score_finder(input$label_RASH, NIH_rash_key)})
  NIH_eczema_score <- reactive({score_finder(input$label_ECZEMA, NIH_eczema_key)})
  NIH_uri_score <- reactive({score_finder(input$label_URI, NIH_uri_key)})
  NIH_candida_score <- reactive({score_finder(input$label_CANDIDA, NIH_candida_key)})
  NIH_serious_score <- reactive({score_finder(input$label_SERIOUS, NIH_serious_key)})
  NIH_fatal_score <- reactive({score_finder(input$label_FATAL, NIH_fatal_key)})
  NIH_hyper_score <- reactive({score_finder(input$label_HYPER, NIH_hyper_key)})
  NIH_lymphoma_score <- reactive({score_finder(input$label_LYMPHOMA, NIH_lymphoma_key)})
  NIH_nasal_score <- reactive({score_finder(input$label_NASAL, NIH_nasal_key)})
  NIH_palate_score <- reactive({score_finder(input$label_PALATE, NIH_palate_key)})
  NIH_age_score <- reactive({score_finder(input$label_AGE, NIH_age_key)})
  
  NIH_score_values <- reactive({c(
    NIH_ige_score(),
    NIH_abcess_score(), 
    NIH_pna_score(), 
    NIH_lung_score(),
    NIH_teeth_score(),
    NIH_scolio_score(), 
    NIH_frx_score(),
    NIH_eos_score(),
    NIH_face_score(),
    NIH_midline_score(),
    NIH_rash_score(),
    NIH_eczema_score(),
    NIH_uri_score(),
    NIH_candida_score(),
    NIH_serious_score(),
    NIH_fatal_score(),
    NIH_hyper_score(),
    NIH_lymphoma_score(),
    NIH_nasal_score(),
    NIH_palate_score(), 
    NIH_age_score()
  )})
  
  ######################### NIH SCORE ######################################
  
  NIH_TOTAL_SCORE <- reactive({sum(NIH_score_values())})
  
  output$nih_score <- renderText({paste("NIH score:", NIH_TOTAL_SCORE(), sep = " ")})
  
  NIH_score_components <- c("IgE","Skin abscesses","Pneumonias","Lung abnormalities","Retained teeth",
                        "Scoliosis","Abnormal fractures","Eosinophils","Facial structure",
                        "Midline anomaly","Newborn rash","Eczema","URIs","Candidiasis",
                        "Other serious infection","Fatal infection","Hyperextensibility","Lymphoma",
                        "Nasal width","High palate","Age")

  NIH_score_df_p1 <- reactive({
    tibble(
      NIH_score_components,
      NIH_score_values()) %>% 
      slice(1:12)
    })
  
  NIH_score_df_p2 <- reactive({
    tibble(
      NIH_score_components,
      NIH_score_values()) %>% 
      slice(13:n())
  })
  
  output$nih_table_p1 <- DT::renderDataTable({
    datatable(NIH_score_df_p1(),
              colnames = c("Component", "Score"),
              rownames = FALSE,
              class = "cell-border",
              options = list(dom = "t",
                             pageLength = 50,
                             ordering = F))
  })
  
  output$nih_table_p2 <- DT::renderDataTable({
    datatable(NIH_score_df_p2(),
              colnames = c("Component", "Score"),
              rownames = FALSE,
              class = "cell-border",
              options = list(dom = "t",
                             pageLength = 50,
                             ordering = F))
  })

  #################################### STAT3 SCORE ################################################
  
  stat3_score_df <- reactive({stat3_score(pna = NIH_pna_score(), 
                                    rash = NIH_rash_score(), 
                                    frx = NIH_frx_score(), 
                                    face = NIH_face_score(), 
                                    palate = NIH_palate_score())})
  
  output$stat3_score <- renderText({
    paste("STAT3 score:", stat3_score_df() %>% pull(values) %>% sum(), sep = " ")
  })
    
  output$stat3_table <- DT::renderDataTable({
    datatable(stat3_score_df(),
              colnames = c("Component", "Score"),
              rownames = FALSE,
              class = "cell-border",
              options = list(dom = "t",
                             pageLength = 50,
                             ordering = F))
  })

  
  #################################### DOCK8 SCORE ################################################
  
  dock8_score_df <- reactive({dock8_score(lung = NIH_lung_score(), 
                                          eos = NIH_eos_score(), 
                                          uri = NIH_uri_score(), 
                                          teeth = NIH_teeth_score(), 
                                          frx = NIH_frx_score())})
  
  output$dock8_score <- renderText({
    paste("DOCK8 score:", dock8_score_df() %>% pull(values) %>% sum(), sep = " ")
  })
  
  output$dock8_table <- DT::renderDataTable({
    datatable(dock8_score_df(),
              colnames = c("Component", "Score"),
              rownames = FALSE,
              class = "cell-border",
              options = list(dom = "t",
                             pageLength = 50,
                             ordering = F))
  })
  
  # p <- reactive({
  #   round(
  #     kdRisk(
  #       WBC = input$labelWBC,
  #       Platelet = input$labelPLAT,
  #       Hgb = input$labelHGB,
  #       AST = input$labelAST,
  #       Na = input$labelNA,
  #       Alb = input$labelALB,
  #       Temp = input$labelTEMP,
  #       Classic = as.numeric(input$labelCLASSIC)),
  #     2
  #   )
  #   # kdRisk(
  #   #   WBC = 14,
  #   #   Platelet = 400,
  #   #   Hgb = 14,
  #   #   AST = 40,
  #   #   Na = 140,
  #   #   Alb = 4,
  #   #   Temp = 39,
  #   #   Classic = 1
  #   # )
  # })
  # 
  # output$conditionalDecision <- renderUI({
  #   if (p() <= cut[as.numeric(input$labelCUT)]){
  #     tags$div(
  #       style="border:solid; background-color:#66c2a550; border-radius: 5px; 
  #              overflow: hidden; text-align:center; vertical-align: middle;
  #              line-height: normal;",
  #       tags$h3("Probability of non-response: ", tags$b(as.character(p())), tags$br(), tags$h2("Safe to discharge"))
  #     )
  #   } else {
  #     tags$div(
  #       style="border:solid; background-color:#d53e4f50; border-radius: 5px; 
  #              overflow: hidden; text-align:center; vertical-align: middle;
  #              line-height: normal;",
  #       tags$h3("Probability of non-response: ", tags$b(as.character(p())), tags$br(), tags$h2("Not safe to discharge"))
  #     )
  #   }
  # })
  # 
  # #Data table generation
  # # output$text <- renderText({ 
  # #   paste0("||| WBC = ", input$labelWBC,
  # #          "||| Platelets = ", input$labelPLAT,
  # #          "||| Hgb = ", input$labelHGB,
  # #          "||| AST = ", input$labelAST,
  # #          "||| Na = ", input$labelNA,
  # #          "||| ALB = ", input$labelALB,
  # #          "||| Temp = ", input$labelTEMP,
  # #          "||| Classic = ", input$labelCLASSIC)
  # # })
  # 
  # output$prob <- renderText({
  #   paste0(as.character(p()))
  #   })
  #   
  # output$componentTable = DT::renderDataTable({
  #     datatable(
  #       data.frame(cut, sens, spec, PPV, NPV, "test" = as.integer(1:4 == input$labelCUT)),
  #       colnames = c("Cut-off", "Sensitivity", "Specificity", "PPV", "NPV", "test"),
  #       rownames = FALSE,
  #       caption = htmltools::tags$caption(
  #         style = 'caption-side: bottom; text-align: right;',
  #         "Bold row indicates selected probability cut-off"),
  #       class = "cell-border",
  #       options = list(dom = "t",
  #                      ordering = F,
  #                      columnDefs = list(
  #                        list(className = 'dt-center', targets = 0:5),
  #                        list(targets = 5, visible = F)))
  #     )  %>%
  #     formatStyle(
  #       columns = 'test',
  #       target = 'row',
  #       backgroundColor = styleEqual(c(0, 1), c('#white', '#00000010')),
  #       fontWeight = styleEqual(c(0,1), c("normal", "bold")))
  #   })
  # #Plot generation
  # # output$plotSBP <- renderPlot({
  # #   S()$plot + ggtitle("Systolic BP percentiles")
  # # })
  # # 
  # # output$plotDBP <- renderPlot({
  # #   D()$plot +ggtitle("Diastolic BP percentiles")
  # # })
}

shinyApp(ui, server)
