#' Calculate relative risks to be reported or plotted as dot plot
#' 
#' @param safety  an object created by \code{\link{safety_summary}}
#' @param type a choice of "non_serious" (default) or "serious" as to which type of AE to report on
#' @param reference character vector nameing the refenrence arm for teh calculations. 
#' Defualts to the first row of the safety$GROUP.
#' @param size a number between 0-100, giving the size of the confidence interval. 
#' Default is 95.
#' 
#' @return a data.frame that has the relative risk estimate and confidence intervals. 
#' No adjustment made to deal with zeroes. 
#' @seealso  \code{\link{safety_summary}}
#' 
#' 
#' @importFrom dplyr left_join mutate select arrange
#' @importFrom magrittr %>%
#' 
#' @examples 
#' safety_statistics <- safety_summary(safety,
#'            exposed=c("Experimental"=60,"Control"=67))
#' relative_risk(safety_statistics, type="serious") %>% head
#' 

relative_risk <- function(safety,
                          type=c("non_serious", "serious"),
                          reference=safety$GROUP$title[1],
                          size=95
){
  
  crit = qnorm(1-(1-size/100)/2)
  #safety <- safety_statistics
  
  type=match.arg(type)
  df <- switch(type,
               non_serious=safety$NON_SERIOUS,
               serious= safety$SERIOUS
  )
  denom <- safety$GROUP %>% select(title, subjectsExposed)
  
  df <- df %>% 
    mutate( eutctId=as.double(eutctId)) %>% 
    left_join(eudract::soc_code, by="eutctId") %>% 
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


obj <- relative_risk(safety_statistics, type="serious")
obj$relative_risk %>% dplyr::filter(!is.na(rr) & !is.infinite(rr))
obj$percentage %>% head




relative_risk_table <- function(safety,
                                type=c("non_serious", "serious"),
                                reference=safety$GROUP$title[1],
                                size=95,
                                digits=3, tidy=TRUE){
  obj <- relative_risk(safety,type,reference, size)
  
  
  df <- obj$relative_risk
  if( tidy){df <- df %>% dplyr::filter(!is.na(rr)& !is.infinite(rr)) }
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

relative_risk_table(safety_statistics, type="serious")
relative_risk_table(safety_statistics, type="serious", reference="Experimental")
relative_risk_table(safety_statistics, type="non_serious", reference="Experimental")





dot_plot <- function(safety,
                     type=c("non_serious", "serious"),
                     reference=safety$GROUP$title[1],
                     size=95,
                     text_width=10,
                     base=2,
                     valid_estimates=TRUE
                     ){
    tidy_text <- function(x){ sapply( strwrap(x, width=text_width, simplify=FALSE),
                                      paste0, collapse="\n")}
  
    obj <- relative_risk(safety,type,reference,size)
    
    obj$relative_risk %<>% 
      mutate(
        term=tidy_text(term),
        rr=ifelse(is.infinite(rr),NA, rr),
        lower=ifelse(is.infinite(lower),NA, lower),
        upper=ifelse(is.infinite(upper),NA, upper),     
      )
    obj$percentage %<>% mutate(term=tidy_text(term))
    if( valid_estimates){
      obj$relative_risk %<>% dplyr::filter(!is.na(rr))
      index <-paste(obj$percentage$soc_term,obj$percentage$term) %in%
        paste(obj$relative_risk$soc_term,obj$relative_risk$term)
      obj$percentage <- obj$percentage[index,]
    }
    
    
    
    if( any(duplicated(obj$relative_risk$term))){warning("The same Preferred Term is used repeatedly with multiple System Organ Classes")}
    
    
    base.p <- ggplot(obj$percentage, aes(x = term, y = NULL)) +
      labs(x=NULL, y=NULL)
    left.panel <- base.p + 
      geom_point(data = obj$percentage, aes(x=term, y=pct,shape = group, color = group), size=3) +
      coord_flip()+
      ylab("Percent (%)") +
      xlab("") +
      scale_y_continuous(breaks = pretty(obj$percentage$pct)) +
      ret()
    
    n_groups=nrow(safety$GROUP)
    
    cols <- scales::hue_pal()(n_groups)[-1]
    # set shapes
    shps <- scales::shape_pal()(n_groups)[-1]
    names(shps) <- names(cols) <- safety$GROUP$title[ safety$GROUP$title!=reference]
    
    
    pd <- position_dodge(0.5)
    right.panel <- ggplot(data=obj$relative_risk,aes(x=term, y=rr, shape = group,colour=group))+
      #geom_pointrange(data = ae_rr, aes(x=pt, y=rr, ymin=rr.LCI, ymax=rr.UCI)) +
      geom_point( position = pd, size = 2.5)+
      geom_errorbar( aes(ymin=lower, ymax=upper), position = pd, linewidth=.2)+
      scale_color_manual(values=cols) +
      scale_shape_manual(values=shps) +
      coord_flip()+
      scale_y_continuous(trans="log",
                         name = paste0("Relative risk (",size,"% CI)"),
                         breaks = scales::trans_breaks("log", function(x) base^x),
                         labels =  scales::math_format(.x) 
      )+
      # scale_y_log10(name = "Relative risk (95% CI)",
      #               breaks = scales::trans_breaks("log10", function(x) 10^x),
      #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      geom_hline(yintercept = 1, linetype="dotted", color = "black", linewidth=0.5) +
      xlab("")  +
      ret(y.blank = T)
    
    #Deal with legend
    lg <- g_legend(left.panel)
    lheight <- sum(lg$height)
    lwidth <- sum(lg$width)
    
    #Combine two plots
    grid.arrange(arrangeGrob(left.panel + theme(legend.position="none",
                                                plot.margin=unit(c(1,0,0,1), "cm")),
                             right.panel + theme(axis.text.y=element_blank(),
                                                 legend.position = "none",
                                                 plot.margin=unit(c(1,1,0,0), "cm")),
                             nrow=1),
                 lg, nrow=2,
                 heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
  
}

ret <- function(y.blank = F){
  th <- theme(rect = element_rect(fill ="#FFFFFF", linetype = 0, colour = NA),
              title = element_text(hjust = 0.5),
              panel.grid.major.y = element_line(colour = "#D8D8D8"),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_line(linewidth  = 0.6, linetype = "solid"),
              axis.line.x = element_line(linewidth = 0.6, linetype = "solid"),
              axis.text = element_text(colour = "#000000"),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key = element_blank())
  if(y.blank){
    th <- th + theme(axis.text.y=element_blank())
  }
  th
}


# Extract legend of the ggplot
g_legend<-function(gplot){
  tmp <- ggplot_gtable(ggplot_build(gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




dot_plot(safety_statistics, type="non_serious", base=4)


### tidy up this into multiple files
## work out how to group by SOC
## work how to combine serious and non-serious.
## add documentation
## add testing. 



