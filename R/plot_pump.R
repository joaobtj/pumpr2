#' Title
#'
#' @param df Objeto resultado da funcao pumpsis
#'
#' @return Um Plot da curva caracteristica da motombomba
#' @export
#'
#' @examples
#' Q_BC21_15 <- c(19.2, 18.2, 17.2, 16.0, 13.3, 9.9)/3600
#' Hm_BC21_15 <- c(9, 10, 11, 12, 14, 16)
#' df=curva_bomba <- hmpump(Q_BC21_15, Hm_BC21_15)
#' plot_pump(curva_bomba)
plot_pump <- function(df) {

  
  df[[1]] %>%
  ggplot(aes(x=q_bomba*3600, y=hm_bomba))+
    #geom_line( color="red", size=1)+
    stat_smooth(method = "lm",formula = y ~ poly(x, 2),se = FALSE, color="red")+
    labs(
      x = "Flow rate (m3/h)",
      y = "Manometric height (mca)",
      
    )
  
  
  
}

