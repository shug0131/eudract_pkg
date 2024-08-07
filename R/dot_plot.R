#' creates a dot-plot of safety data showing the absolute and relative risks
#' 
#' @inheritParams relative_risk
#' 
#' @param safety  an object created by \code{\link{safety_summary}} or by \code{\link{relative_risk}}, in case you want to re-order or filter the choice of rows.
#' @param text_width Integer giving a target width to which the labels are wrapped. Defaults to 10.
#' @param base numeric value to which a log scale uses as tick marks. Suggest powers of 2, or 5.
#' 
#' @return a graphical object that shows the estimates and CI of relative and absolute risk.
#' @seealso  \code{\link{safety_summary}} \code{\link{relative_risk}} [relative_risks()]
#' 
#' @export
#' @importFrom dplyr left_join mutate select arrange
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point coord_flip ylab xlab scale_y_continuous geom_errorbar scale_color_manual scale_shape_manual geom_hline theme element_rect element_text element_line element_blank  ggplot_gtable ggplot_build labs position_dodge unit
#' @importFrom patchwork plot_layout
#' 
#' @details This is essentially a list of two ggplot objects joined together in a list, named
#' as "left.panel" and "right.panel". 
#' They can each be individually edited if needed
#' 
#' @examples 
#' safety_statistics <- safety_summary(safety,
#'            exposed=c("Experimental"=60,"Control"=67))
#' head( relative_risk(safety_statistics, type="serious") )
#' fig <- dot_plot(safety_statistics, type="non_serious", base=4)
#' fig
#' fig$left.panel <- fig$left.panel + ggplot2::labs(title="Absolute Risk")
#' fig
#' temp <- tempfile(fileext=".png")
#' png(filename = temp)
#' print(fig)
#' dev.off()
#' 

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
  if(inherits(safety,"safety_summary")){
    obj <- relative_risk(safety,type,reference,size)
  }
  if( inherits(safety,"relative_risk")){
   obj <- safety 
   if( obj$reference!=reference){
     warning(paste( "The reference group is taken from the relative_risk input: ",obj$reference))
     reference <- obj$reference
    }
  }
  if( !inherits(safety, c("safety_summary","relative_risk"))){
   stop("invalid input: needs to be either safety_summary or relative_risk object") 
  }
  
  obj$relative_risk %<>% 
    mutate(
      #term=tidy_text(term),
      rr=ifelse(is.infinite(rr),NA, rr),
      lower=ifelse(is.infinite(lower),NA, lower),
      upper=ifelse(is.infinite(upper),NA, upper),     
    )
  if( is.factor(obj$relative_risk$term)){
   levels( obj$relative_risk$term) <- tidy_text(levels( obj$relative_risk$term) )
   levels( obj$percentage$term) <- tidy_text(levels( obj$percentage$term) )
  }else{
    obj$relative_risk$term <- tidy_text(obj$relative_risk$term)
    obj$percentage$term <- tidy_text(obj$percentage$term)
  }
  
  n_groups=nrow(obj$GROUP)
  
 # obj$percentage %<>% mutate(term=tidy_text(term))
  if( valid_estimates & 1< n_groups){
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
  
  
  
  if( n_groups==1){output <- left.panel+theme(legend.position = "none")}
  
  if( 1< n_groups){
    title <- sort(obj$GROUP$title)
    ref_index <- which(title==reference)
    
    cols <- scales::hue_pal()(n_groups)[-ref_index]
    # set shapes
    shps <- scales::shape_pal()(n_groups)[-ref_index]
    names(shps) <- names(cols) <- title[ -ref_index]
    
    
    pd <- position_dodge(0.5)
    right.panel <- ggplot(data=obj$relative_risk,aes(x=term, y=rr, shape = group,colour=group))+
      #geom_pointrange(data = ae_rr, aes(x=pt, y=rr, ymin=rr.LCI, ymax=rr.UCI)) +
      geom_point( position = pd, size = 2.5)+
      #geom_errorbar( aes(ymin=lower, ymax=upper), position = pd, linewidth=.2)+
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
    
    if(n_groups==2){
      right.panel <- right.panel+ 
        geom_errorbar( aes(ymin=lower, ymax=upper), position = pd, linewidth=.2,colour="black")
    }else{
      right.panel <- right.panel+ 
        geom_errorbar( aes(ymin=lower, ymax=upper), position = pd, linewidth=.2)  
    }
    
    # Version 1 pre patchwork  
    output <- list(left.panel=left.panel, right.panel=right.panel)
    class(output) <- c( "dot_plot")
    #   # so you can still edit the component ggplots using standard tools
    #   print(output)
    #   invisible(output)
  }
  output
}

#' print methods for dot_plot object
#' 
#' @param x dot_plot object
#' @param ... other arguments for generic methods

#' @export
print.dot_plot <- function(x,...){
  left.panel <- x$left.panel
  right.panel <- x$right.panel
  
  #Deal with legend
  lg <- g_legend(left.panel)
  
  # get number of rows 
  
  n <- dplyr::n_distinct( left.panel$data$term)
  
  print( 
    ((left.panel|right.panel)&theme(legend.position = "none"))/lg +
    plot_layout(heights=c(n+1,1))
  )
  invisible(x)
  
}


#' plot methods for dot_plot object
#' 
#' @param x dot_plot object
#' @param ... other arguments for generic methods

#' @export
plot.dot_plot <- print.dot_plot
  




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


if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c("rr","lower","upper","pct","group",".x"
    ))
}


## work out how to group by SOC
## work how to combine serious and non-serious.
## add documentation
## add testing. 
## test with 3 or more groups

## THIS IS ALSO In the relative_risk  so delete one ---!!
# 
# order_filter <- function(rel_risk,threshold=10){
#   if(!inherits(rel_risk,"relative_risk")){stop("need to input a relative_risk object")}
#   terms <- rel_risk$relative_risk$term 
#   dups <- terms[duplicated(terms)]
# 
#   rr2 <- rel_risk$relative_risk %>% 
#   mutate( term= ifelse( term %in% dups,  paste(term, soc_term, sep="-"), term))
#   index <- order(rr2$rr)
#   
#   rr2$term <- factor(rr2$term,levels = rr2$term[index], ordered = TRUE)
#   pct2 <- rel_risk$percentage%>% 
#     mutate( term= ifelse( term %in% dups,  paste(term, soc_term, sep="-"), term))
#   pct2$term <- factor(pct2$term,levels = rr2$term[index], ordered = TRUE)
#   
#   rel_risk_ord <- rel_risk
#   rel_risk_ord$relative_risk <- rr2
#   rel_risk_ord$percentage <- pct2
#   
#   
#   keep <- pct2 %>% group_by(term) %>% 
#     summarise( keep= threshold < max(pct)) %>% filter(keep) %>% 
#     pull(term)
#   rel_risk_ord <- rel_risk
#   rel_risk_ord$relative_risk <- rr2 %>% filter( term %in% keep)
#   rel_risk_ord$percentage <- pct2 %>% filter( term %in% keep)
#   rel_risk_ord
# }
# 
