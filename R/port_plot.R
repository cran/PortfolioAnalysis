#' @title Plot Efficient frontier
#' @description Plots the efficient frontier for the security (mean-variance portfolios)
#' @param asset.names Vector of ticker of securities
#' @param increment Number of portfolio to be generated, Default: 100
#' @param rf Risk-free rate of return, Default: 0
#' @param period Period for which the returns are calculated, Default: c("months", "weeks", "quarters", "years")
#' @return Interactive plot of the effiecnt frontier for given security/funds
#' @details Give and interactive effient frontier for given securities
#' @examples
#' EfficientFrontier(c('FXAIX', 'TIBFX'), period = 'days')
#' @rdname EfficientFrontier
#' @import ggplot2
#' @import quantmod
#' @importFrom purrr map
#' @import PerformanceAnalytics
#' @importFrom stats cov
#' @importFrom xts to.period merge.xts
#' @import quadprog
#' @importFrom lubridate years
#' @export
EfficientFrontier = function(asset.names, increment = 100, rf = 0, period = c('months', 'weeks', 'quarters', 'years')){
  tgt.ret = 0
  tgt.sd = 0
  port_summary = optim.portfolio(asset.names = asset.names, increment = increment, rf = rf, period = period)
p1 = ggplot(data = port_summary)+
  geom_line(mapping = aes(x = tgt.sd, y = tgt.ret), col = 'red')+
  labs(x = 'Risk', y = 'Return', title = 'Efficient Frontier')
p1
}
