hydrus_sci_format <- function(df){

  df <- format(df, scientific = TRUE)

  for(i in 1:nrow(df)){
    cut <- stringr::str_split(df[i,], "e", simplify = T)
    for(j in 1:ncol(df)){
      if(stringr::str_detect(df[i,j], "\\.")){
        df[i,j] <- stringr::str_flatten(c(cut[j,1],
                                        rep("0", times = 6 - stringr::str_count(stringr::str_split(cut[j,1], "\\.", simplify = T)[,2])),
                                        "e",
                                        cut[j,2]))
      }else{
        df[i,j] <- paste0(cut[j,1], ".000000e", cut[j,2])
      }
    }
  }

  return(df)
}
