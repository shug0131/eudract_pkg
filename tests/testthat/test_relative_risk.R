context("relative risk test")

safety_statistics <- safety_summary(safety,
                                    exposed=c("Experimental"=60,"Control"=67))
rr <- relative_risk(safety_statistics)
rr2 <- order_filter(rr, threshold=2)
min_pct <- rr2$percentage %>% group_by(term) %>% 
  summarise( pct=max(pct)) %>% dplyr::pull(pct) %>% min

test_that("relative risk object",{
  expect_equal(length(rr),3)
  expect_s3_class(rr$relative_risk,"data.frame")
  expect_s3_class(rr$percentage,"data.frame")
  expect_s3_class(rr$GROUP,"data.frame")
  expect_gt(min_pct, 2)
} )


rr_tab <- relative_risk_table(safety_statistics, type="non_serious")
rr_tab4 <- relative_risk_table(safety_statistics, type="non_serious", digits=4)
test_that("relative risk table",{
          expect_s3_class(rr_tab, "data.frame")
          expect_equal(names(rr_tab),c("System Organ Class","Preferred Term","Relative Risk (C.I.)"))
          expect_equal(rr_tab[1,3],"1.12 (0.0683, 18.2)")
          expect_equal(rr_tab4[1,3],"1.117 (0.06834, 18.25)")
})

test_that("order filter error",
          expect_error(order_filter(safety_statistics), "need to input a relative_risk object")
)


test_that("three groups",{
  other <- safety %>% dplyr::filter(group=="Control") %>% 
    dplyr::mutate(group="Other")
  safety3 <- rbind(safety,other)
  stats3 <- safety_summary(safety3, exposed=c("Experimental"=60,"Control"=67, "Other"=67))
  relative_risk(stats3)
  rr_tab <- relative_risk_table(stats3, type="non_serious")
  expect_equal(ncol(rr_tab),4)
}
)

