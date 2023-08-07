#' Calculate relative risks to be reported or plotted as dot plot
#' 
#' @param safety  an object created by \code{\link{safety_summary}}
#' @param type a choice of "non_serious" (default) or "serious" as to which type of AE to report on
#' @param reference character vector nameing the refenrence arm for teh calculations. 
#' Defualts to the first row of the safety$GROUP.
#' @param size a number between 0-100, giving the size of the confidence interval. 
#' Default is 95.

#' @return a data.frame that has the relative risk estimate and confidence intervals. 
#' No adjustment made to deal with zeroes. 
#' @seealso  \code{\link{safety_summary}} \code{\link{dot_plot}}
#' 
#' @rdname relative_risk_table
#' 
#' @export
#' @importFrom dplyr left_join mutate select arrange
#' @importFrom magrittr %>%
#' 
#' @examples 
#' safety_statistics <- safety_summary(safety,
#'            exposed=c("Experimental"=60,"Control"=67))
#' head( relative_risk(safety_statistics, type="serious") )
#' relative_risk_table(safety_statistics, type="serious")

relative_risk <- function(safety,
                          type=c("non_serious", "serious"),
                          reference=safety$GROUP$title[1],
                          size=95
){
  
  crit = stats::qnorm(1-(1-size/100)/2)
  #safety <- safety_statistics
  
  type=match.arg(type)
  df <- switch(type,
               non_serious=safety$NON_SERIOUS,
               serious= safety$SERIOUS
  )
  denom <- safety$GROUP %>% select(title, subjectsExposed)
  
  df <- df %>% 
    mutate( eutctId=as.double(eutctId)) %>% 
    left_join(soc_code, by="eutctId") %>% 
    left_join(denom, by=c("groupTitle"="title")) %>% 
    rename("group"=groupTitle, 
           "count"=subjectsAffected,
           "n"=subjectsExposed
    ) %>% 
    mutate(n=as.integer(n),
           count=as.integer(count),
           pct=count/n*100
    )
 
  
  ref <- df %>% dplyr::filter(group==reference) %>% 
    select(term,soc_term, count, n) %>% 
    rename("count_ref"=count, "n_ref"=n)
  
 non_ref <-df %>% dplyr::filter(group!=reference)
  
 rr <- left_join(non_ref, ref,by=c("soc_term","term")) %>% 
    mutate( rr= count/n/count_ref*n_ref,
            rr= ifelse(rr==0, NA, rr),
            log_rr=log(rr),
            log_rr_se=sqrt(1/count+1/n+1/count_ref+1/n_ref),
            lower=exp( log_rr-crit*log_rr_se),
            upper=exp( log_rr+crit*log_rr_se)
    )
  
  output <- list("relative_risk"=rr %>% as.data.frame,
       "percentage"=df %>% as.data.frame)
  class(output) <- "relative_risk"
  output
}


# obj <- relative_risk(safety_statistics, type="serious")
# obj$relative_risk %>% dplyr::filter(!is.na(rr) & !is.infinite(rr))
# obj$percentage %>% head


#' @param digits integer giving the number of significant figures to report to. Default of 3.
#' @param  valid_estimates a logical, which determines if only terms with valid estimates of relative risk are included in the table.
#' The alternative is to include terms with zeroes.
#' @return A data frame that is suitable for printing to a report, giving relative risks
#' @export

relative_risk_table <- function(safety,
                                type=c("non_serious", "serious"),
                                reference=safety$GROUP$title[1],
                                size=95,
                                digits=3, valid_estimates=TRUE){
  obj <- relative_risk(safety,type,reference, size)
  
  
  df <- obj$relative_risk
  if( valid_estimates){df <- df %>% dplyr::filter(!is.na(rr)& !is.infinite(rr)) }
 cols <- df$group %>% unique %>% length
 df <- df %>% 
   dplyr::arrange(soc_term, term, group) %>% 
   mutate( 
     text=paste0(signif(rr,digits=digits), " (", signif(lower, digits=digits),", ", signif(upper, digits=digits),")")
   ) %>% 
   select(group,soc_term, term, text)
 if( 1<cols){
   df <- tidyr::pivot_wider(names_from = "group", values_from = "text") 
   header <- paste0(df$group %>% unique, " Relative Risk (C.I.)")
 }else{
   df <- df %>% select(-group) 
   header <- "Relative Risk (C.I.)"
 }
 df <- df %>% mutate(soc_term=ifelse(duplicated(soc_term), "", soc_term))
 names(df) <- c("System Organ Class", "Preferred Term", header)
 as.data.frame(df)
}

# relative_risk_table(safety_statistics, type="serious")
# relative_risk_table(safety_statistics, type="serious", reference="Experimental")
# relative_risk_table(safety_statistics, type="non_serious", reference="Experimental")



if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c("rr","lower","upper","pct","group",".x","text",
      "title","n","count","soc_term","count_ref",
      "log_rr","log_rr_se","n_ref"
    ))
}
