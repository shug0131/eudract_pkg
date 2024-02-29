#' provide standard structured tables to report incidence rates of AEs by arm
#' 
#' @param safety  an object created by \code{\link{safety_summary}}
#' @param type a choice of "non_serious" (default) or "serious" as to which type of AE to report on
#' @param percent_round integer giving the number of decimal places to round the incidence
#' percentage. Default of 0. Maybe you need more if there is a large sample size 
#' and a rare event of interest
#' 
#' @return a data.frame that can be directly printed as a table to a report.  Each arm has its own column
#' and the text contains "p\% (r, o)",  where r is the number of participants with the term, 
#' o is the number of occurrences, and p a percentage of participants with the term.
#' @seealso  \code{\link{safety_summary}}
#' 
#' @export
#' @importFrom dplyr left_join mutate select arrange
#' @importFrom magrittr %>%
#' 
#' @examples 
#' safety_statistics <- safety_summary(safety,
#'            exposed=c("Experimental"=60,"Control"=67))
#' head( incidence_table(safety_statistics, type="serious") )

incidence_table <- function(safety, 
                            type=c("non_serious", "serious"),
                            percent_round=0){
  type=match.arg(type)
  df <- switch(type,
               non_serious=safety$NON_SERIOUS,
               serious= safety$SERIOUS
  )
  denom <- safety$GROUP %>% select(title, subjectsExposed)
  
  output <- df %>% 
    mutate( eutctId=as.double(eutctId)) %>% 
    left_join(soc_code, by="eutctId") %>% 
    left_join(denom, by=c("groupTitle"="title")) %>% 
    mutate( pct=round(as.integer(subjectsAffected)/as.integer(subjectsExposed)*100, digits = percent_round),
            text= paste0(pct,"% (",subjectsAffected,", ", occurrences,")")
    ) %>% select(groupTitle, term, soc_term, text) %>% 
    tidyr::pivot_wider(names_from = groupTitle, values_from = text) %>% 
    dplyr::arrange(soc_term, term) %>% 
    mutate( soc=ifelse(duplicated(soc_term),"",soc_term) )%>% 
    select(soc, term, dplyr::any_of(safety$GROUP$title))
  header <- c("System Organ Class","Preferred Term",
              paste0(  safety$GROUP$title, " (N = ", safety$GROUP$subjectsExposed,")")
  )
  names(output) <- header
  output %>% as.data.frame
}

