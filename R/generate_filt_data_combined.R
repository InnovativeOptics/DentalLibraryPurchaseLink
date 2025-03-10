
#' Generate Filt Data Combined
#'
#' @param filt_data_laser
#'
#' @return A list
#' @export
#'
#' @examples
#' filt_data_laser <- list(structure(list(Wavelength = NA, Lens = "Pi1", Summary = "High VLT, lightweight and relatively inexpensive",
#'OD = "800-830nm (OD 5), 9000-11000nm (OD 7)", VLT = "70%",
#'Shape = "curved", Material = "Polycarbonate", Website = "https://innovativeoptics.com/pi1-laser-glasses-frames/",
#'Image = "https://innovativeoptics.com/wp-content/uploads/2018/04/pi1.jpg",
#'Graph = "graphs/Pi1_plot.png", `Price(from)` = "$175"), class = c("tbl_df",
#'                                                                  "tbl", "data.frame"), row.names = c(NA, -1L)))
#' filt_data_loupe <- structure(list(`Selected Device` = "Clarion (800-820nm OD 5)",
#'`Selected Loupes` = "Andau Blues LG (One size) ", `Compatible Loupe Insert` = "IVL.R"), row.names = c(NA,
#'                                                                                                      -1L), class = c("tbl_df", "tbl", "data.frame"))
#' generate_filt_data_combined(filt_data_laser, filt_data_loupe)

generate_filt_data_combined <- function(filt_data_laser, filt_data_loupe){

  if(all(sapply(filt_data_laser, "[[", 'Lens') == "Pi1") & nrow(filt_data_loupe) != 0){

    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Pi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi1-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi1-Lens.jpg"
        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Pi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi1-Lens.jpg"
        x
      })

    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Pi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-r-pi1-laser-insert-for-loupes/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi1-Lens.jpg"
        x
      })

    }
    #    else if(filt_data_loupe$`Compatible Loupe Insert` == "IVR.R"){
    #      print("IVR.R Pi1")
    #      filt_data_laser <- lapply(filt_data_laser, function(x) {
    #        x[['Website']] <- "https://innovativeoptics.com/product/ivr-r-pi1-laser-insert-for-loupes/"
    #        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi1-Lens.jpg"
    #        x
    #      })

    #    }
    else if(filt_data_loupe$`Compatible Loupe Insert` == "Primo"){
      print("Primo Pi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/primo-pi1-laser-inserts/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2020/11/primo-GiT7.jpg"
        x
      })

    }
  }
  #################################################################################################
  ### Test for Pi10
  #################################################################################################

  if (all(sapply(filt_data_laser, "[[", 'Lens') == "Pi10") & nrow(filt_data_loupe) != 0){
    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Pi10")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi10-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi10-Lens.jpg"
        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Pi10")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi10-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi10-Lens.jpg"
        x
      })

    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Pi10")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-r-pi10-laser-insert-for-loupes/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi10-Lens.jpg"
        x
      })

    }
    #    else if(filt_data_loupe$`Compatible Loupe Insert` == "IVR.R"){
    #      print("IVR.R Pi10")
    #      filt_data_laser <- lapply(filt_data_laser, function(x) {
    #        x[['Website']] <- "https://innovativeoptics.com/product/ivr-r-pi10-laser-insert-for-loupes/"
    #        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi10-Lens.jpg"
    #        x
    #      })

    #    }
  }
  #################################################################################################
  ### Test for Pi17
  #################################################################################################
  if (all(sapply(filt_data_laser, "[[", 'Lens') == "Pi17") & nrow(filt_data_loupe) != 0){
    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Pi17")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi17-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi17-Lens.jpg"

        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Pi17")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi17-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi17-Lens.jpg"
        x
      })

    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Pi17")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-r-pi17-laser-insert-for-loupes/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi17-Lens.jpg"
        x
      })

    } #else if(filt_data_loupe$`Compatible Loupe Insert` == "IVR.R"){
    #      print("IVR.R Pi17")
    #      filt_data_laser <- lapply(filt_data_laser, function(x) {
    #        x[['Website']] <- "https://innovativeoptics.com/product/ivr-r-pi17-laser-insert-for-loupes/"
    #        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi17-Lens.jpg"
    #        x
    #      })

    #    }
    else if(filt_data_loupe$`Compatible Loupe Insert` == "Primo"){
      print("Primo Pi17")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/primo-pi17-laser-inserts/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/primo.pi17-front-NEW.jpg"
        x
      })

    }
  }

  #################################################################################################
  ### Test for Pi19, change image and landing page
  #################################################################################################

  if (all(sapply(filt_data_laser, "[[", 'Lens') == "Pi19") & nrow(filt_data_loupe) != 0){
    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Pi19")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi19-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi19-Lens-1.jpg"
        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Pi19")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi19-Lens-1.jpg"
        x
      })

    }  else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Pi19")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi19-Lens-1.jpg"
        x
      })

    }
    #    else if(filt_data_loupe$`Compatible Loupe Insert` == "IVR.R"){
    #      print("IVR.R Pi19")
    #      filt_data_laser <- lapply(filt_data_laser, function(x) {
    #        x[['Website']] <- "https://innovativeoptics.com/product/ivr-r-pi19-laser-insert-for-loupes/"
    #        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi19-Lens-1.jpg"
    #        x
    #      })

    #    }
    else if(filt_data_loupe$`Compatible Loupe Insert` == "Primo"){
      print("Primo Pi19")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/primo-pi19-laser-inserts/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/02/primo.pi19-front-NEW.jpg"
        x
      })

    }
  }
  #################################################################################################
  ### Test for Pi23, change image and landing page
  #################################################################################################

  if(all(sapply(filt_data_laser, "[[", 'Lens') == "Pi23") & nrow(filt_data_loupe) != 0){
    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Pi23")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi23-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi19-Lens-1.jpg"
        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Pi23")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi19-Lens-1.jpg"
        x
      })

    }  else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Pi23")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-r-pi23-laser-insert-for-loupes/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi19-Lens-1.jpg"
        x
      })

    }
    #    else if(filt_data_loupe$`Compatible Loupe Insert` == "IVR.R"){
    #      print("IVR.R Pi23")
    #      filt_data_laser <- lapply(filt_data_laser, function(x) {
    #        x[['Website']] <- "https://innovativeoptics.com/product/ivr-r-pi23-laser-insert-for-loupes/"
    #        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi19-Lens-1.jpg"
    #        x
    #      })

    #    }
    else if(filt_data_loupe$`Compatible Loupe Insert` == "Primo"){
      print("Primo Pi23")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/primo-pi23-laser-inserts/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/02/primo.pi19-front-NEW.jpg"
        x
      })

    }
  }
  #################################################################################################
  ### Test for Pi32
  #################################################################################################
  if (all(sapply(filt_data_laser, "[[", 'Lens') == "Pi32") & nrow(filt_data_loupe) != 0){
    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Pi32")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/pi32-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Pi17-Lens.jpg"

        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Pi32")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-pi32-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi17-Lens.jpg"
        x
      })

    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Pi32")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-r-pi32-laser-insert-for-loupes/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi17-Lens.jpg"
        x
      })

    }

    else if(filt_data_loupe$`Compatible Loupe Insert` == "Primo"){
      print("Primo Pi32")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/primo-pi32-laser-inserts/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/primo.pi17-front-NEW.jpg"
        x
      })

    }
  }


  #################################################################################################
  ### Test for Gi1, change image and landing page
  #################################################################################################
  if (all(sapply(filt_data_laser, "[[", 'Lens') == "Gi1") & nrow(filt_data_loupe) != 0){
    if(filt_data_loupe$`Compatible Loupe Insert` == "IVR"){
      print("IVR Gi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/gi1-inview-regular-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Gi1-Lens.jpg"
        x
      })
    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL"){
      print("IVL Gi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/gi1-inview-large-laser-clip-in/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Pi19-Lens.jpg"
        x
      })

    } else if(filt_data_loupe$`Compatible Loupe Insert` == "IVL.R"){
      print("IVL.R Gi1")
      filt_data_laser <- lapply(filt_data_laser, function(x) {
        x[['Website']] <- "https://innovativeoptics.com/product/ivl-r-gi1-laser-insert-for-loupes/"
        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVL-Clip-In-Gi1-Lens.jpg"
        x
      })

    }
    #    else if(filt_data_loupe$`Compatible Loupe Insert` == "IVR.R"){
    #      print("IVR.R Gi1")
    #      filt_data_laser <- lapply(filt_data_laser, function(x) {
    #        x[['Website']] <- "https://innovativeoptics.com/product/ivr-r-gi1-laser-insert-for-loupes/"
    #        x[['Image']] <- "https://innovativeoptics.com/wp-content/uploads/2022/05/IVR-Clip-In-Gi1-Lens.jpg"
    #        x
    #      })

    #    }

  }

  list(filt_data_laser = filt_data_laser, filt_data_loupe = filt_data_loupe)

}
