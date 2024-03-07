context("dotplot test")
#library(eudract)
#library(grDevices)


if( is_testing()){path <- tempdir()} else{ path <- "tests/testthat"}
if( is_testing()){ref_path <- "."} else{ ref_path <- "tests/testthat"}


safety_statistics <- safety_summary(safety,
                                    exposed=c("Experimental"=60,"Control"=67))
fig <- dot_plot(safety_statistics, type="non_serious", base=4)
fig
print(fig)
plot(fig)


save_ps <- function(fig){
  temp <- tempfile(fileext=".ps")
  postscript(file = temp, onefile = TRUE)
  print(fig)  #  need to have print() as fig on its own doesn't work inside testthat
  on.exit(dev.off())
  temp
}

if (FALSE) {
  local_edition(3)
  expect_snapshot_file(save_ps(fig), "plot.ps")
}
test_that("compare image of dotplot",{
  local_edition(3)
  #announce_snapshot_file("plot.svg")
  #path <- save_svg(fig)
  #expect_snapshot_file(save_ps(fig),"plot.ps")
  vdiffr::expect_doppelganger("dotplot", fig)
})

if(FALSE){
  svg("tests/testthat/reference_plot.svg")
  print(fig)
  dev.off()
  
}

test_that("warnings for repeated terms",
          {
          expect_warning(
          dot_plot(safety_statistics, type="non_serious", base=4, valid_estimates = FALSE),
          "The same Preferred Term is used repeatedly with multiple System Organ Classes"
          )
          expect_error(dot_plot("whatever"),"invalid input: needs to be either safety_summary or relative_risk object")
          }
          )

rr <- relative_risk(safety_statistics)
rr$relative_risk$term <- as.factor(rr$relative_risk$term)
rr$percentage$term <- as.factor(rr$percentage$term)

test_that("relative risk object as input",{
  fig2 <- dot_plot(rr, type="non_serious", base=4)
  fig2
  #fig2 <- ggplot(mapping=aes(x=x,y=y), data=data.frame(x=1,y=1))+geom_point()
  ## This is bending the rules.  I want this to  compare to the reference output
  ## that was create for line 35 above,  by using the same title "dotplot"
  vdiffr::expect_doppelganger("dotplot", fig2)
  }
)





# test_that("dotplot",{
#   temp <- save_svg(fig)
#   #unlink(test_path("dotplot.png"))
#   checksum <- tools::md5sum(c(test_path(temp),test_path("reference_plot.svg")))
#   #expect_true(file.exists(test_path(temp)))
#   expect_equivalent(
#     checksum[1],checksum[2])
# })






tab <- incidence_table(safety_statistics, type="serious")

test_that("incidence table",{
  expect_s3_class(tab,"data.frame")
  expect_equal(tab[1,3],"1% (1, 1)")
} )

# with 3 groups
test_that("three groups",{
other <- safety %>% dplyr::filter(group=="Control") %>% 
  dplyr::mutate(group="Other")
safety3 <- rbind(safety,other)
stats3 <- safety_summary(safety3, exposed=c("Experimental"=60,"Control"=67, "Other"=67))
fig_3groups <- dot_plot(stats3, reference="Control", type = "non_serious")
local_edition(3)
vdiffr::expect_doppelganger("dotplot_3groups", fig_3groups)
})

#


test_that("one_group",{
  other <- safety %>% dplyr::filter(group=="Control") %>% 
    dplyr::mutate(group="Other")
fig_1group <- other %>% safety_summary(exposed=c("Other"=67)) %>% dot_plot
vdiffr::expect_doppelganger("dotplot_1group", fig_1group)
}
)

# other %>% safety_summary(exposed=c("Other"=67)) %>% incidence_table()
# other %>% safety_summary(exposed=c("Other"=67)) %>% relative_risk()
# other %>% safety_summary(exposed=c("Other"=67)) %>% relative_risk_table()


