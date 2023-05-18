#' Title
#'
#' @param df Objeto resultado da funcao hmsis
#'
#' @return Um Plot da curva caracteristica do sistema
#' @export
#'
#' @examples
#' df=curva_sistema <- hmsis(
#'   hr = 20, hs = 1, dr_com = 60 / 1000, q = 10 / 3600, lr = 50, per = 148,
#'   ds_com = 75 / 1000, ls = 2, pes = 305
#' )
#' plot_sys(curva_sistema)
plot_sys <- function(df) {
  # plot(hm ~ q,
  #   data = df[[1]],
  #   type = "b",
  #   lwd = 2,
  #   xlab = "Vazao - Q(m3/h)",
  #   ylab = "Altura manometrica - Hm (mca)",
  #   main = "Curva Caracteristica do Sistema"
  # )
  
  df[[1]] %>%
    ggplot(aes(x=q*3600, y=hm))+
    #geom_line(color="blue", size=1)+
    stat_smooth(method = "lm",formula = y ~ poly(x, 2),se = FALSE, color="blue")+
    labs(
      x = "Flow rate (m3/h)",
      y = "Manometric height (mca)",
      
    )
}

