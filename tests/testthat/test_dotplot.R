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

rr <- relative_risk(safety_statistics)
rr2 <- order_filter(rr, threshold=2)
min_pct <- rr2$percentage %>% group_by(term) %>% 
  summarise( pct=max(pct)) %>% dplyr::pull(pct) %>% min

test_that("relative risk table",{
  expect_equal(length(rr),3)
  expect_s3_class(rr$relative_risk,"data.frame")
  expect_s3_class(rr$percentage,"data.frame")
  expect_s3_class(rr$GROUP,"data.frame")
  expect_gt(min_pct, 2)
} )

