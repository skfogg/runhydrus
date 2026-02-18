# soil catalog

if(model %in% c(0, 3)){
  # VGM
  if(is.null(hysteresis)){
    sand <- data.frame(theta_r = 0.045,
                       theta_s = 0.43,
                       alpha = 0.145,
                       n = 2.68,
                       K_s = 712.8,
                       l = 0.5)
  }else{
    sand <- data.frame(theta_r = 0.045,
                       theta_s = 0.43,
                       alpha = 0.145,
                       n = 2.68,
                       K_s = 712.8,
                       l = 0.5,
                       theta_m = 0.43,
                       theta_sW = 0.43,
                       alphaW = 0.29, # alfaW
                       K_sW = 712.8)
  }

}if(model == 1){
  # modified VGM
  sand <- data.frame(theta_r = 0.045,
                     theta_s = 0.43,
                     alpha = 0.145,
                     n = 2.68,
                     K_s = 712.8,
                     l = 0.5,
                     theta_m = 0.43,
                     theta_a = 0.045,
                     theta_k = 0.43,
                     K_k = 712.8)
}if(model == 2){
  # Brooks-Corey
  sand <- data.frame(theta_r = 0.02,
                     theta_s = 0.417,
                     alpha = 0.138,
                     n = 0.592,
                     K_s = 504,
                     l = 1)
}if(model == 4){
  # Kosugi
  sand <- data.frame(theta_r = 0.045,
                     theta_s = 0.43,
                     alpha = 303.693,
                     n = 0.3827,
                     K_s = 712.8,
                     l = 0.5)
}if(model == -4){
  # Kosugi + Brunswick modification
  sand <- data.frame(theta_r = 0.056245,
                     theta_s = 0.372635,
                     alpha = 303.693,
                     n = 0.3827,
                     K_s = 1380.85,
                     l = 0.5,
                     K_s_ncap = 0.0190546,
                     aFilm = 1.5,
                     f0 = 6.8,
                     hr = 1)
}


