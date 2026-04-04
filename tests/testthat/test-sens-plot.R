library(testthat)
library(mrgsim.sa)

context("test-sens-plot")

s1 <- 
  mrgsolve::house() %>% 
  ev(amt = 100) %>% 
  parseq_cv(CL, VC, KIN, KOUT) %>%
  sens_each(end = 12) 

s2 <- mrgsolve::house() %>% 
  ev(amt = 100) %>% 
  parseq_cv(CL, VC, KOUT) %>%
  sens_grid(end = 12) 

test_that("by default, plot everything", {
  p <- sens_plot(s1)  
  expect_is(p, "list")
  expect_length(p, length(unique(s1$dv_name)))
})

test_that("single plot - default", {
  p <- sens_plot(s1, "CP")
  expect_is(p, "gg")  
})

test_that("single plot - list layout", {
  p <- sens_plot(s1, "CP", layout = "list")
  expect_is(p, "list")
  expect_length(p, 1)
  expect_is(p[[1]], "gg")
})

test_that("single plot - facet_grid layout", {
  p <- sens_plot(s1, "CP", layout = "facet_grid")
  expect_is(p, "gg")
})

test_that("single plot - facet_wrap layout", {
  p <- sens_plot(s1, "CP", layout = "facet_wrap")
  expect_is(p, "gg")
})

test_that("single plot - grid", {
  p <- sens_plot(s1, "CP", grid = TRUE)
  expect_is(p, "gg")
})

test_that("multiple plots - default", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP") 
  expect_is(p, "list")
  expect_length(p, 3)
  expect_is(p[[2]], "gg")
})

test_that("multiple plots - list", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "list") 
  expect_is(p, "list")
  expect_length(p, 3)
  expect_is(p[[2]], "gg")
})

test_that("multiple plots - facet_wrap", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_wrap") 
  expect_is(p, "gg")
})

test_that("multiple plots - facet_grid", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_grid") 
  expect_is(p, "gg")
})

test_that("multiple plots - facet_grid", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_grid") 
  expect_is(p, "gg")
})

test_that("multiple plots - grid", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", layout = "facet_grid", grid = TRUE) 
  expect_is(p, "list")
  expect_length(p, 3)
  expect_is(p[[2]], "gg")
})

test_that("plot a sens_grid object", {
  out <- sens_run(house(), par = "CL,VC", vary = "grid") 
  expect_is(out, "sens_grid")
  expect_is(sens_plot(out, dv_name = "CP"), "gg")
  out2 <- sens_run(house(), par = "CL,VC,KA,IC50", vary = "grid")
  expect_error(sens_plot(out2, "CP"), regexp = "Too many ")
})

test_that("sens_grid - single plot", {
  p <- sens_plot(s2, "CP")
  expect_is(p, "gg")
  expect_error(sens_plot(s2, TRUE), "dv_name is not a character")
})

test_that("sens_grid - multiple plots", {
  p <- sens_plot(s2, "CP,RESP")
  expect_is(p, "list")
  expect_length(p, 2)
  expect_is(p[[1]], "gg")
})

test_that("sens_grid - multiple plots everything", {
  p <- sens_plot(s2)
  expect_is(p, "list")
  outv <- unique(s2$dv_name)
  expect_length(p, length(outv))
  expect_is(p[[1]], "gg")
})

# xlab / ylab - sens_each -----------------------------------------------

test_that("sens_each - xlab is applied", {
  p <- sens_plot(s1, "CP", xlab = "my-xlab")
  expect_equal(p$labels$x, "my-xlab")
})

test_that("sens_each - ylab is applied for single dv", {
  p <- sens_plot(s1, "CP", ylab = "my-ylab")
  expect_equal(p$labels$y, "my-ylab")
})

test_that("sens_each - ylab defaults to dv_name for single dv", {
  p <- sens_plot(s1, "CP")
  expect_equal(p$labels$y, "CP")
})

test_that("sens_each - vectorized ylab applied to each plot in list", {
  p <- sens_plot(s1, dv_name = "GUT,CP,RESP", ylab = c("gut", "conc", "resp"))
  expect_is(p, "list")
  expect_length(p, 3)
  expect_equal(p[[1]]$labels$y, "gut")
  expect_equal(p[[2]]$labels$y, "conc")
  expect_equal(p[[3]]$labels$y, "resp")
})

test_that("sens_each - ylab length mismatch with dv_name errors", {
  expect_error(
    sens_plot(s1, dv_name = "GUT,CP,RESP", ylab = c("a", "b")),
    regexp = "dv_name.*ylab.*different lengths"
  )
})

test_that("sens_each - xlab must be character", {
  expect_error(sens_plot(s1, "CP", xlab = 123), "xlab is not a character")
})

# xlab / ylab - sens_grid -----------------------------------------------

test_that("sens_grid - xlab is applied", {
  p <- sens_plot(s2, "CP", xlab = "my-xlab")
  expect_equal(p$labels$x, "my-xlab")
})

test_that("sens_grid - ylab is applied for single dv", {
  p <- sens_plot(s2, "CP", ylab = "my-ylab")
  expect_equal(p$labels$y, "my-ylab")
})

test_that("sens_grid - ylab defaults to dv_name for single dv", {
  p <- sens_plot(s2, "CP")
  expect_equal(p$labels$y, "CP")
})

test_that("sens_grid - vectorized ylab applied to each plot in list", {
  p <- sens_plot(s2, dv_name = "CP,RESP", ylab = c("conc", "resp"))
  expect_is(p, "list")
  expect_length(p, 2)
  expect_equal(p[[1]]$labels$y, "conc")
  expect_equal(p[[2]]$labels$y, "resp")
})

test_that("sens_grid - ylab length mismatch with dv_name errors", {
  expect_error(
    sens_plot(s2, dv_name = "CP,RESP", ylab = "only-one"),
    regexp = "dv_name.*ylab.*different lengths"
  )
})

# group / facet - sens_grid ---------------------------------------------

test_that("sens_grid - group selects the within-panel color variable", {
  p_default <- sens_plot(s2, "CP")
  p_group   <- sens_plot(s2, "CP", group = "VC")
  expect_equal(p_default$scales$scales[[1]]$name, "CL")
  expect_equal(p_group$scales$scales[[1]]$name,   "VC")
})

test_that("sens_grid - facet selects the faceting variable", {
  # default: pars order is CL, VC, KOUT → group=CL, cols=VC
  # with group="VC", facet="CL" → group=VC, cols=CL
  p_default <- sens_plot(s2, "CP")
  p_facet   <- sens_plot(s2, "CP", group = "VC", facet = "CL")
  expect_true(grepl("VC", names(p_default$facet$params$cols)))
  expect_true(grepl("CL", names(p_facet$facet$params$cols)))
})

test_that("sens_grid - group errors for non-parameter name", {
  expect_error(sens_plot(s2, "CP", group = "FOO"), "is not a sensitivity parameter")
})

test_that("sens_grid - facet errors for non-parameter name", {
  expect_error(sens_plot(s2, "CP", group = "CL", facet = "FOO"), "is not a sensitivity parameter")
})

# palette - sens_each ---------------------------------------------------

test_that("sens_each - custom palette with grid = TRUE produces a valid plot", {
  pal <- ggplot2::scale_color_manual(values = c("red", "blue", "green", "orange", "purple"))
  p <- sens_plot(s1, "CP", grid = TRUE, palette = pal)
  expect_is(p, "gg")
})

test_that("sens_each - custom palette applied per-panel when grid = TRUE", {
  pal <- ggplot2::scale_color_manual(values = c("red", "blue", "green", "orange", "purple"))
  p <- sens_plot(s1, "CP", grid = TRUE, palette = pal)
  # The custom colors should appear in the scale
  scale_vals <- p$scales$scales[[1]]$palette(5)
  expect_true(all(c("red", "blue", "green") %in% scale_vals))
})

test_that("sens_each - palette NULL default still produces a valid plot with grid = TRUE", {
  p <- sens_plot(s1, "CP", grid = TRUE)
  expect_is(p, "gg")
})

test_that("sens_each - palette is ignored for default layout (viridis used instead)", {
  pal <- ggplot2::scale_color_manual(values = c("red", "blue", "green"))
  p_default <- sens_plot(s1, "CP")
  p_pal     <- sens_plot(s1, "CP", palette = pal)
  # Both should still produce a valid gg object
  expect_is(p_default, "gg")
  expect_is(p_pal, "gg")
  # Default uses viridis (continuous), so the scale class should be the same
  # regardless of the palette argument
  expect_equal(class(p_default$scales$scales[[1]]), class(p_pal$scales$scales[[1]]))
})

# palette - sens_grid ---------------------------------------------------

test_that("sens_grid - custom palette is applied", {
  pal <- ggplot2::scale_color_manual(values = c("red", "blue", "green", "orange", "purple"))
  p <- sens_plot(s2, "CP", palette = pal)
  expect_is(p, "gg")
  # The custom colors should appear in the scale
  scale_vals <- p$scales$scales[[1]]$palette(5)
  expect_true(all(c("red", "blue", "green") %in% scale_vals))
})

test_that("sens_grid - palette NULL default produces valid plot with discrete scale", {
  p <- sens_plot(s2, "CP")
  expect_is(p, "gg")
  # Default palette is a scale_color_manual from pick_palette
  expect_is(p$scales$scales[[1]], "ScaleDiscrete")
})

test_that("sens_grid - custom palette forwarded across multiple dv_names", {
  pal <- ggplot2::scale_color_manual(values = c("red", "blue", "green", "orange", "purple"))
  p <- sens_plot(s2, "CP,RESP", palette = pal)
  expect_is(p, "list")
  expect_length(p, 2)
  scale_vals1 <- p[[1]]$scales$scales[[1]]$palette(5)
  scale_vals2 <- p[[2]]$scales$scales[[1]]$palette(5)
  expect_true(all(c("red", "blue", "green") %in% scale_vals1))
  expect_true(all(c("red", "blue", "green") %in% scale_vals2))
})

