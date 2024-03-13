library(eudract)
safety_statistics <- safety_summary(safety,
                                    exposed=c("Control"=99, "Experimental"=101))
safety_statistics$GROUP
head( incidence_table(safety_statistics, type="serious")  )
relative_risk_table(safety_statistics, type="serious") 
dot_plot(safety_statistics, type="serious", base=4) 
