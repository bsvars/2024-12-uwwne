
library(bsvars)
set.seed(123)



# Download data using the readrba package
############################################################

# Gross domestic product (GDP); Chain volume
rgdp_dwnld      = readrba::read_rba(series_id = "GGDPCVGDP")
rgdp_tmp        = xts::xts(rgdp_dwnld$value, rgdp_dwnld$date, tclass = 'yearqtr')
drgdp           = na.omit(400 * diff(log(rgdp_tmp)))
drgdp           = xts::to.quarterly(drgdp, OHLC = FALSE)

# Consumer price index; All groups; Quarterly change (in per cent)
picpi_dwnld     = readrba::read_rba(series_id = "GCPIAGSAQP")
pi              = 4 * xts::xts(picpi_dwnld$value, picpi_dwnld$date, tclass = 'yearqtr')
pi              = xts::to.quarterly(pi, OHLC = FALSE)

# Interbank Overnight Cash Rate
cr_dwnld        = readrba::read_rba(series_id = "FIRMMCRID")   # Cash Rate Target
cr_tmp          = xts::xts(cr_dwnld$value, cr_dwnld$date)
cr              = xts::to.quarterly(cr_tmp, OHLC = FALSE)

# Real Trade-Weighted Index
rtwi_dwnld      = readrba::read_rba(series_id = "FRERTWI")
rtwi_tmp        = xts::xts(rtwi_dwnld$value, rtwi_dwnld$date, tclass = 'yearqtr')
rtwi            = 100 * na.omit(diff(log(rtwi_tmp)))
drtwi            = xts::to.quarterly(rtwi, OHLC = FALSE)

y               = na.omit(merge(drgdp, pi, cr, drtwi))
plot(y, main = "Australian monetary system",
     legend.loc = "bottomleft", col = c("#FF00FF","#990099","#77001b","#330033"))




# Estimation setup
############################################################
N       = ncol(y)
p       = 4
S_burn  = 1e3
S       = 5e3


# estimation - lower-triangular model
############################################################
# specify a model
spec = specify_bsvar$new(
  as.matrix(y), 
  p = p, 
  stationary = rep(TRUE, N)
)

# estimate a model
spec |>
  estimate(S = S_burn) |>
  estimate(S = S) -> post

# compute and plot impulse responses
post |> 
  compute_impulse_responses(horizon = 20) |> 
  plot()

# compute and plot forecast error variance decompositions
post |> 
  compute_variance_decompositions(horizon = 20) |> 
  plot()

# compute and plot structural shocks
post |> 
  compute_structural_shocks() |> 
  plot()

# compute and plot fitted values
post |> 
  compute_fitted_values() |> 
  plot()

# compute and plot forecasts
post |> 
  forecast(horizon = 8) |> 
  plot(data_in_plot = 0.3)

