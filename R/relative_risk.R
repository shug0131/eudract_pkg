#' Calculate relative risks to be reported or plotted as dot plot
#' 
#' @param safety  an object created by \code{\link{safety_summary}}
#' @param type a choice of "non_serious" (default) or "serious" as to which type of AE to report on
#' @param reference character vector naming the reference arm for the calculations. 
#' Defaults to the first row of the safety$GROUP.
#' @param size a number between 0-100, giving the size of the confidence interval. 
#' Default is 95.

#' @return \code{relative_risk} returns of list of three items. "relative_risk" a data.frame that has the relative risk estimate and confidence intervals. 
#' "percentage" a data.frame with absolute percentages. "GROUP" a copy from the original \code{safety_summary} object.
#' No adjustment made to deal with zeroes. This is suitable input for the \code{dot_plot} function, and in most cases will not be 
#' used directly, but may potentially be modified with filtration, or editing of terms, see \code{order_filter}.
#' @seealso  \code{\link{safety_summary}} \code{\link{dot_plot}}
#' 
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
#' rr <- relative_risk(safety_statistics)
#' rr2 <- order_filter(rr, threshold=2)
#' dot_plot(rr2)

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
       "percentage"=df %>% as.data.frame,
       "GROUP"=safety$GROUP
       )
  class(output) <- "relative_risk"
  output
}


# obj <- relative_risk(safety_statistics, type="serious")
# obj$relative_risk %>% dplyr::filter(!is.na(rr) & !is.infinite(rr))
# obj$percentage %>% head


#' @param digits integer giving the number of significant figures to report to. Default of 3.
#' @param  valid_estimates a logical, which determines if only terms with valid estimates of relative risk are included in the table.
#' The alternative is to include terms with zeroes.
#' @return \code{relative_risk_table} returns a data frame that is suitable for printing to a report, giving relative risks
#' @export
#' @rdname relative_risk

relative_risk_table <- function(safety,
                                type=c("non_serious", "serious"),
                                reference=safety$GROUP$title[1],
                                size=95,
                                digits=3, valid_estimates=TRUE){
  obj <- relative_risk(safety,type,reference, size)
  
  
  df <- obj$relative_risk
  if( valid_estimates){df <- df %>% dplyr::filter(!is.na(rr)& !is.infinite(rr)) }
 cols <- df$group %>% unique 
 df <- df %>% 
   dplyr::arrange(soc_term, term, group) %>% 
   mutate( 
     text=paste0(signif(rr,digits=digits), " (", signif(lower, digits=digits),", ", signif(upper, digits=digits),")")
   ) %>% 
   select(group,soc_term, term, text)
 if( 1<length(cols)){
   df <- tidyr::pivot_wider(df,names_from = "group", values_from = "text", values_fill = "-") 
   header <- paste0(cols, " - Relative Risk (C.I.)")
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


#' @param rel_risk a relative risk object
#' @param threshold a threshold on the percent scale, the max percentage for a term the incidence rate needs to exceed
#' @export
#' @returns \code{order_filter} returns a revised relative risk object, with the terms concatenated with SOC if there are any duplicates, 
#' then ordered by relative risk, into a factor, and filtered to only those terms with an incidence rate above
#' the threshold.
#' @rdname relative_risk

order_filter <- function(rel_risk,threshold=10){
  if(!inherits(rel_risk,"relative_risk")){stop("need to input a relative_risk object")}
  terms <- rel_risk$relative_risk$term 
  dups <- terms[duplicated(terms)]
  
  rr2 <- rel_risk$relative_risk %>% 
    mutate( term= ifelse( term %in% dups,  paste(term, soc_term, sep="-"), term))
  index <- order(rr2$rr)
  
  rr2$term <- factor(rr2$term,levels = rr2$term[index], ordered = TRUE)
  pct2 <- rel_risk$percentage%>% 
    mutate( term= ifelse( term %in% dups,  paste(term, soc_term, sep="-"), term))
  pct2$term <- factor(pct2$term,levels = rr2$term[index], ordered = TRUE)
  
  rel_risk_ord <- rel_risk
  rel_risk_ord$relative_risk <- rr2
  rel_risk_ord$percentage <- pct2
  
  
  keep <- pct2 %>% group_by(term) %>% 
    summarise( keep= threshold < max(pct)) %>% dplyr::filter(keep) %>% 
    dplyr::pull(term)
  rel_risk_ord <- rel_risk
  rel_risk_ord$relative_risk <- rr2 %>% dplyr::filter( term %in% keep)
  rel_risk_ord$percentage <- pct2 %>% dplyr::filter( term %in% keep)
  rel_risk_ord
}





if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c("rr","lower","upper","pct","group",".x","text",
      "title","n","count","soc_term","count_ref",
      "log_rr","log_rr_se","n_ref"
    ))
}
