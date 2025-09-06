
#' Generate Lens Link
#'
#' @param dental_data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' dental_data <- tibble::tibble( `Laser Mfg` = "AMD", `Laser Model` = "Clarion",
#' Wavelengths = "800-820nm OD 5",`Laser Type` = "Diode Laser", `Eyewear Lens Compatible` = "Pi1",
#' Website = "https://innovativeoptics.com/pi1-laser-glasses-frames/")
#' loupe_insert <- tibble::tibble(Mfg = "Lumadent", `Lumadent Frame` = "Argon", Style = "One size",
#' `Innovative Optics Insert` = "IVL")
#' generate_lens_link(dental_data, loupe_insert)
generate_lens_link <- function(dental_data, loupe_insert){
  result <- dental_data %>%
    dplyr::mutate(`INVO Part Number Raw` = dplyr::if_else(`Eyewear Lens Compatible` == "Gi1",
                                                          glue::glue_safe(loupe_insert$`Innovative Optics Insert`,"." , `Eyewear Lens Compatible`),
                                                          glue::glue_safe(loupe_insert$`Innovative Optics Insert`,"." , `Eyewear Lens Compatible`, ".2B")),
                  `Website`= dplyr::case_when(loupe_insert$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/ivl-r-pi1-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R.2") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/ivl-r-2-pi1-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/",

                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/pi17-inview-large-laser-clip-in/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/ivl-r-pi17-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R.2") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/ivl-r-2-pi17-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/pi17-inview-large-laser-clip-in/",

                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/ivl-r-pi19-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R.2") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/ivl-r-2-pi19-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/",

                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivl-r-pi23-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R.2") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivl-r-2-pi23-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/",

                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi32" ~ "https://innovativeoptics.com/product/pi32-inview-large-laser-clip-in/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi32" ~ "https://innovativeoptics.com/product/ivl-r-pi32-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R.2") & `Eyewear Lens Compatible` == "Pi32" ~ "https://innovativeoptics.com/product/ivl-r-2-pi32-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi32" ~ "https://innovativeoptics.com/product/pi32-inview-large-laser-clip-in/",

                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-large-laser-clip-in/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/ivl-r-gi1-laser-insert-for-loupes/",
                                              loupe_insert$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-regular-laser-clip-in/",
                                              .default = Website)
    ) %>%
    dplyr::mutate(`INVO Part Number` = glue::glue_safe("<a href='{Website}' target ='_blank'> 'Click here to order' </a> "))

  result
}
