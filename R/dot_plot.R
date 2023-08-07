#' creates a dot-plot of safety data showing the absolute and relative risks
#' 
#' @inheritParams relative_risk
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
#' @importFrom gridExtra  grid.arrange arrangeGrob
#' 
#' @examples 
#' safety_statistics <- safety_summary(safety,
#'            exposed=c("Experimental"=60,"Control"=67))
#' head( relative_risk(safety_statistics, type="serious") )
#' dot_plot(safety_statistics, type="non_serious", base=4)

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


if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c("rr","lower","upper","pct","group",".x"
    ))
}


## work out how to group by SOC
## work how to combine serious and non-serious.
## add documentation
## add testing. 



