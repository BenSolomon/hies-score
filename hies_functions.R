


score_finder <- function(value, score_table){
  score_table$scores[findInterval(value, score_table$cutpoints)]
}


# nih_score <- function(ige=0,
#                       abscess=0,
#                       pna=0,
#                       lung=0,
#                       teeth=0,
#                       scolio=0,
#                       frx=0,
#                       eos=0,
#                       face=0,
#                       midline=0,
#                       rash=0,
#                       eczema=0,
#                       uri=0,
#                       candida=0,
#                       serious=0,
#                       fatal=0,
#                       hyper=0,
#                       lymphoma=0,
#                       nasal=0,
#                       palate=0,
#                       age=1){
#   
#   NIH_score_components <- c("IgE","Skin abscesses","Pneumonias","Lung abnormalities","Retained teeth",
#                             "Scoliosis","Abnormal fractures","Eosinophils","Facial structure",
#                             "Midline anomaly","Newborn rash","Eczema","URIs","Candidiasis",
#                             "Other serious infection","Fatal infection","Hyperextensibility","Lymphoma",
#                             "Nasal width","High palate","Age")
#   
#   NIH_score_values <- unlist(as.list(match.call(expand.dots = F))[-1])
#   
#   NIH_score_df <- tibble(NIH_score_components, NIH_score_values)
#   NIH_score_df
#   # NIH_score_total <- sum(NIH_score_values)
#   # 
#   # list(total = NIH_score_total, table = NIH_score_df)
# }

stat3_score <- function(pna = 0, rash = 0, frx = 0, face = 0, palate = 0){
  stat3_pna <- pna * 2.5
  stat3_rash <- rash*2.08
  stat3_frx <- frx*3.33
  stat3_face <- face*3.33
  stat3_palate <- palate*2.5
  
  stat3_score_components <- c("Pneumonias","Abnormal fractures","Facial structure",
                            "Newborn rash","High palate")
  
  stat3_score_values <- c(stat3_pna,stat3_frx,stat3_face,stat3_rash,stat3_palate)
  
  tibble(components = stat3_score_components, values = stat3_score_values)
}


dock8_score <- function(lung = 0, eos = 0, uri = 0, teeth = 0, frx = 0){
  dock8_lung <- lung*(-5)
  dock8_eos <- eos*8.18
  dock8_uri <- uri*15.5
  dock8_teeth <- teeth*(-4.54)
  dock8_frx <- frx*(-9.09)
  
  dock8_score_components <- c("Lung abnormalities","Retained teeth",
                              "Abnormal fractures","Eosinophils","URIs")
  
  dock8_score_values <- c(dock8_lung,dock8_teeth,dock8_frx,dock8_eos,dock8_uri)
  
  tibble(components = dock8_score_components, values = dock8_score_values)
}