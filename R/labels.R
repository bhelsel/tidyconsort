align_breaks <- function(v, align = "left"){
  sapply(1:length(v), function(x){
    paste0(v[x], sprintf("<br align='%s'/>", align))
  }) %>%
    paste0(., collapse = "")
}