context("dotplot test")
library(eudract)
library(grDevices)


if( is_testing()){path <- tempdir()} else{ path <- "tests/testthat"}
if( is_testing()){ref_path <- "."} else{ ref_path <- "tests/testthat"}


safety_statistics <- safety_summary(safety,
                                    exposed=c("Experimental"=60,"Control"=67))
fig <- dot_plot(safety_statistics, type="non_serious", base=4)
fig
print(fig)
plot(fig)


save_png <- function(fig){
  temp <- tempfile(fileext=".png")
  png(filename = temp)
  print(fig)  #  need to have print() as fig on its own doesn't work inside testthat
  on.exit(dev.off())
  temp
}

if (FALSE) {
  local_edition(3)
  expect_snapshot_file(save_png(fig), "plot.png")
}
test_that("compare image of dotplot",{
  local_edition(3)
  announce_snapshot_file("plot.png")
  path <- save_png(fig)
  expect_snapshot_file(path,"plot.png")
})


# test_that("dotplot",{
#   temp <- withr::local_tempfile(fileext=".png")
#   unlink(test_path("dotplot.png"))
#   png(filename = test_path("dotplot.png"))
#   fig
#   dev.off()
#   checksum <- tools::md5sum(c(test_path("dotplot.png"),test_path("reference_dotplot.png")))
#   expect_true(file.exists(test_path("dotplot.png")))
#   expect_equivalent(
#     checksum[1],checksum[2])
#   unlink(test_path("dotplot.png"))
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

