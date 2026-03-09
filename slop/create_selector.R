#' Title
#'
#' @param met
#' @param vg Van Genchten parameters
#' @param time.int starting interval for numerical solutions
#' @param model.number integer indicating the model type: 0 = single porosity, 5 = dual porosity overlapping domains, 6 = dual porosity (saturation transfer), 7 = dual porosity (pressure head transfer), 9 = dual permeability
#' @param material.number
#' @param tv.info
#' @returns
#' @export
#'
#' @examples
create_selector <- function(met, vg, time.int, model.number, material.number, tv.info){

  # read in model based template file
  if(model.number == 0){
    selector.file = file(paste0("./SELECTOR_NoMacropores.IN"),"r") # r makes a sequential read start where the previous ended
  }

  if(model.number == 7){
    selector.file = file(paste0("./SELECTOR_Macropores.IN"),"r") # r makes a sequential read start where the previous ended
  }

  if(model.number == 9){
    selector.file = file(paste0("./SELECTOR_DualPerm.IN"),"r") # r makes a sequential read start where the previous ended
  }


  #break selector to allow for multiple materials
  end.27 <- 27

  selector.temp.a <- readLines(selector.file, n=end.27)

  if(model.number != 9){
    selector.temp.b <- readLines(selector.file)
  }

  if(model.number == 9){
    selector.temp.notimportant <- readLines(selector.file,4)
    selector.temp.b <- readLines(selector.file)
  }

  close(selector.file)

  #Tell selector how many soil materials there are
  selector.temp.a[14] <- sprintf(paste0("  ", material.number+1, "       1       1"))

  #tell selector what model to use
  selector.temp.a[25] <- sprintf(paste0("    ", model.number, "     0"))

  #fill van genuchten parameters based on model and material number


  if(material.number == 1 & model.number == 0){

    selector.temp.a[27] <- sprintf(
      "%f %f %f %f %f %f",
      vg$thetar[1], #theta r
      vg$thetas[1], #theta s
      vg$alpha[1], #alpha
      vg$n[1], #n
      vg$ks[1], #k sat
      vg$l[1] #l

    )


  }

  if(material.number >= 2 & model.number == 0){

    selector.temp.a[27] <- sprintf(
      "%f %f %f %f %f %f",
      vg$thetar[1], #theta r
      vg$thetas[1], #theta s
      vg$alpha[1], #alpha
      vg$n[1], #n
      vg$ks[1], #k sat
      vg$l[1] #l
    )

    vg.file <- file("./VGLine.IN", "r")
    vg.temp <- readLines(vg.file)
    vg.temp2 <- rep(vg.temp, times= material.number-1)
    close(vg.file)

    for(i in 2:material.number){

      vg.temp2[i-1] <- sprintf(
        "%f %f %f %f %f %f",
        vg$thetar[i], #theta r
        vg$thetas[i], #theta s
        vg$alpha[i], #alpha
        vg$n[i], #n
        vg$ks[i], #k sat
        vg$l[i] #l
      )
    }

  }

  # if(model.number == 5){
  #   selector.temp[26] <- sprintf("   thr     ths    Alfa      n         Ks       l        w2    Alfa2       n2")
  #
  #   selector.temp[27] <- sprintf(
  #     "%f %f %f %f %f %f %f %f %f",
  #     vg$thetar, #theta r
  #     vg$thetas, #theta s
  #     vg$alpha, #alpha
  #     vg$n, #n
  #     vg$ks, #k sat
  #     vg$l, #l
  #     vg$w2, #weight for second set of parameters
  #     vg$alpha2, #alpha for macropores
  #     vg$n2 #n for macropores
  #   )
  # }

  # if(model.number == 6){
  #   selector.temp[26] <- sprintf("   thr     ths    Alfa      n         Ks       l        thrIm   thsIm  Omega")
  #
  #   selector.temp[27] <- sprintf(
  #     "%f %f %f %f %f %f %f %f %f",
  #     vg$thetar, #theta r
  #     vg$thetas, #theta s
  #     vg$alpha, #alpha
  #     vg$n, #n
  #     vg$ks, #k sat
  #     vg$l, #l
  #     vg$thetar2, #wilting point for macropores
  #     vg$thetas2, #saturation for macropores
  #     vg$omega #omega for water transfer
  #   )
  # }

  if(material.number == 1 & model.number == 7){

    selector.temp.a[27] <- sprintf(
      "%f %f %f %f %f %f %f %f %f %f %f",
      vg$thetar2[1], #theta r
      vg$thetas2[1], #theta s
      vg$alpha2[1], #alpha
      vg$n2[1], #n
      vg$ks[1], #k sat
      vg$l[1], #l
      vg$thetar[1], #wilting point for macropores
      vg$thetas[1], #saturation for macropores
      vg$alpha[1], #alpha for macropores
      vg$n[1], #n for macropores
      vg$omega[1] #omega for water transfer
    )


  }

  if(material.number >= 2 & model.number == 7){

    selector.temp.a[27] <- sprintf(
      "%f %f %f %f %f %f %f %f %f %f %f",
      vg$thetar2[1], #theta r
      vg$thetas2[1], #theta s
      vg$alpha2[1], #alpha
      vg$n2[1], #n
      vg$ks[1], #k sat
      vg$l[1], #l
      vg$thetar[1], #wilting point for macropores
      vg$thetas[1], #saturation for macropores
      vg$alpha[1], #alpha for macropores
      vg$n[1], #n fo macropores
      vg$omega[1] #omega for water transfer
    )

    vg.file <- file("./VGLine.IN", "r")
    vg.temp <- readLines(vg.file)
    vg.temp2 <- rep(vg.temp, times= material.number-1)
    close(vg.file)

    for(i in 2:material.number){

      vg.temp2[i-1] <- sprintf(
        "%f %f %f %f %f %f %f %f %f %f %f",
        vg$thetar2[i], #theta r
        vg$thetas2[i], #theta s
        vg$alpha2[i], #alpha
        vg$n2[i], #n
        vg$ks[i], #k sat
        vg$l[i], #l
        vg$thetar[i], #wilting point for macropores
        vg$thetas[i], #saturation for macropores
        vg$alpha[i], #alpha for macropores
        vg$n[i], #n fo macropores
        vg$omega[i] #omega for water transfer
      )
    }

  }


  if(model.number == 9 & material.number == 1){
    selector.temp.a[26] <- sprintf("   thr     ths    Alfa      n         Ks       l")

    selector.temp.a[27] <- sprintf(
      "%.2f %.2f %.4f %.3f %.2f %.1f",
      vg$thetar, #wilting point
      vg$thetas, #saturation
      vg$alpha, #alpha
      vg$n, #n
      vg$ks, #k sat
      vg$l #l

    )

    selector.temp.a[29] <- sprintf("  thrFr   thsFr  AlfaFr     nFr       KsFr     lFr       W    beta   gamma       a        Ka")

    selector.temp.a[30] <- sprintf(
      "%.2f %.2f %.2f %.2f %.2f %.1f %.2f %.2f %.3f %.2f %.4f",
      vg$thetar2, #theta r macropores
      vg$thetas2, #theta s macropores
      vg$alpha2, #alpha macropores
      vg$n2, #n macropores
      vg$ks2, #k sat macropores
      vg$l2, #l macropores
      vg$w, #weight of macropores?
      vg$beta, #macropore geometry
      vg$gamma, #macropore scaling factor
      vg$a, #macropore diffusion length
      vg$ka #interface water??



    )

    selector.temp.a[31] <- sprintf(
      "%.2f %.2f %.2f %.2f %.2f %.1f %.2f %.2f %.3f %.2f %.4f",
      vg$thetar2, #theta r macropores
      vg$thetas2, #theta s macropores
      vg$alpha2, #alpha macropores
      vg$n2, #n macropores
      vg$ks2, #k sat macropores
      vg$l2, #l macropores
      vg$w, #weight of macropores?
      vg$beta, #macropore geometry
      vg$gamma, #macropore scaling factor
      vg$a, #macropore diffusion length
      vg$ka #interface water??

    )

  }

  if(model.number == 9 & material.number >= 2){
    selector.temp.a[26] <- sprintf("   thr     ths    Alfa      n         Ks       l")

    selector.temp.a[27] <- sprintf(
      "%.2f %.2f %.4f %.3f %.2f %.1f",
      vg$thetar[1], #wilting point
      vg$thetas[1], #saturation
      vg$alpha[1], #alpha
      vg$n[1], #n
      vg$ks[1], #k sat
      vg$l[1] #l

    )

    vg.file1 <- file("./VGLine.IN", "r")
    vg.temp <- readLines(vg.file1)
    vg.temp2 <- rep(vg.temp, times= material.number-1)
    close(vg.file1)

    for(i in 2:material.number){

      vg.temp2[i-1] <- sprintf(
        "%.2f %.2f %.4f %.3f %.2f %.1f",
        vg$thetar[i], #wilting point
        vg$thetas[i], #saturation
        vg$alpha[i], #alpha
        vg$n[i], #n
        vg$ks[i], #k sat
        vg$l[i] #l
      )
    }

    selector.temp.gravelm <- sprintf(
      "%.2f %.2f %.4f %.3f %.2f %.1f",
      0.053, #wilting point
      0.3747, #saturation
      0.0353, #alpha
      3.1798, #n
      642.98, #k sat
      0.5 #l

    )

    selector.temp.dp1 <- sprintf("  thrFr   thsFr  AlfaFr     nFr       KsFr     lFr       W    beta   gamma       a        Ka")

    selector.temp.dp2 <- sprintf(
      "%.2f %.2f %.2f %.2f %.2f %.1f %.2f %.2f %.3f %.2f %.4f",
      vg$thetar2[1], #theta r macropores
      vg$thetas2[1], #theta s macropores
      vg$alpha2[1], #alpha macropores
      vg$n2[1], #n macropores
      vg$ks2[1], #k sat macropores
      vg$l2[1], #l macropores
      vg$w[1], #weight of macropores?
      vg$beta[1], #macropore geometry
      vg$gamma[1], #macropore scaling factor
      vg$a[1], #macropore diffusion length
      vg$ka[1] #interface water??



    )

    vg.file2 <- file("./VGLine.IN", "r")
    vg.temp3 <- readLines(vg.file2)
    vg.temp3 <- rep(vg.temp3, times= material.number-1)
    close(vg.file2)

    for(i in 2:material.number){

      vg.temp3[i-1] <- sprintf(
        "%.2f %.2f %.2f %.2f %.2f %.1f %.2f %.2f %.3f %.2f %.4f",
        vg$thetar2[i], #theta r macropores
        vg$thetas2[i], #theta s macropores
        vg$alpha2[i], #alpha macropores
        vg$n2[i], #n macropores
        vg$ks2[i], #k sat macropores
        vg$l2[i], #l macropores
        vg$w[i], #weight of macropores?
        vg$beta[i], #macropore geometry
        vg$gamma[i], #macropore scaling factor
        vg$a[i], #macropore diffusion length
        vg$ka[i] #interface water??
      )
    }

    selector.temp.gravel <- sprintf(
      "%.2f %.2f %.2f %.2f %.2f %.1f %.2f %.2f %.3f %.2f %.4f",
      vg$thetar2[1], #theta r macropores
      vg$thetas2[1], #theta s macropores
      vg$alpha2[1], #alpha macropores
      vg$n2[1], #n macropores
      vg$ks2[1], #k sat macropores
      vg$l2[1], #l macropores
      vg$w[1], #weight of macropores?
      vg$beta[1], #macropore geometry
      vg$gamma[1], #macropore scaling factor
      vg$a[1], #macropore diffusion length
      vg$ka[1] #interface water??

    )


  }

  #add water flow in for model 9
  if(model.number == 9){

    selector.temp.b[2] <- paste("    ", vg$water.flow[1])

  }

  # non-dual permeability models

  if(model.number != 9){

    #time interval
    substr(selector.temp.b[4],4,11) <- time.int

    #model day count
    model.day.count <- length(met$Date)
    selector.temp.b[6] <-
      sprintf(
        "%s %.0f",
        "0", model.day.count);
    selector.temp.b[10] <- model.day.count

    #roots
    each.root <- "      -1"
    all.root <- rep(each.root, times= material.number+1)

    selector.temp.b[20] <- sprintf(paste0(all.root, collapse = "   "))

  }

  #dual permeability models
  if(model.number == 9){

    #time interval
    substr(selector.temp.b[5],4,11) <- time.int

    #model day count
    model.day.count <- length(met$Date)
    selector.temp.b[7] <-
      sprintf(
        "%s %.0f",
        "0", model.day.count);
    selector.temp.b[11] <- model.day.count

    #roots
    substr(selector.temp.b[16],18,20) <- str_pad(tv.info$RootStartDate,
                                                 width= 3)

    substr(selector.temp.b[16],38,40) <- str_pad(tv.info$RootEndDate,
                                                 width= 3)

    substr(selector.temp.b[16],46,51) <- str_pad(tv.info$RootMinDepth,
                                                 width= 5)

    substr(selector.temp.b[16],69,71) <- str_pad(tv.info$RootMaxDepth,
                                                 width= 3)
  }

  if(material.number == 1 | model.number != 9){

    selector.new.file = file("SELECTOR.IN", "w")
    writeLines(c(selector.temp.a,
                 vg.temp2,
                 selector.temp.b),
               selector.new.file)
    close(selector.new.file)
  }

  if(material.number >= 2 & model.number == 9){

    selector.new.file = file("SELECTOR.IN", "w")
    writeLines(c(selector.temp.a,
                 vg.temp2,
                 selector.temp.gravelm,
                 selector.temp.dp1,
                 selector.temp.dp2,
                 vg.temp3,
                 selector.temp.gravel,
                 selector.temp.b),
               selector.new.file)
    close(selector.new.file)

  }

}
