# mrgsim.sa (development version)

- `sens_plot()` will now plot the all items in `dv_name` by default 
  (#7). 

- Add `layout` argument to `sens_plot()` to control how plots appear
  when multiple dependent variables are getting plotted (#7). 

- `NEWS.md` is now retained in the built package (#7).

## Bugs Fixed

- Fixed bug that allowed non-integer values of `.n` getting 
  passed into `parseq_` friends (#8).

- Fixed warnings that were issued because extra arguments were 
  getting passed through to `mrgsim()` (#7). 

## Internal

- Now using `rlang::abort()` globally (#8).

# mrgsim.sa 0.1.0 

- Initial version
