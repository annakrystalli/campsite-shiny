# ---- Plotting color palettes:

# Palettes
colours <- harrypotter::hp(n = 6, option = "Ravenclaw")

# Plot settings
ref_beak_lwd <- 2
beak_lwd <- 4
campsite_font_size <- 13

# Theme colours
primary <- colours[1]
secondary <- colours[6]

campsite_theme <- bslib::bs_theme(version = 5,
                                     primary = primary,
                                     secondary = secondary,
                                     warning = primary,
                                     spacer = "1rem",
                                     "nav-tabs-link-active-border-color" = paste(rep(primary, 4), collapse = " "),
                                     "nav-tabs-border-color" = paste(primary, "!default"),
                                     "border-color" = primary,
                                     "nav-tabs-border-color" = paste(rep(primary, 3), collapse = " "),
                                     "nav-tabs-link-active-color" = "white",
                                     "nav-link-font-weight" = "bold",
                                     "nav-tabs-link-active-bg"= primary,
                                     "nav-tabs-border-width" = "3px",
                                     "nav-link-font-size" = campsite_font_size * 4,
                                     "nav-link-padding-x" = "1.3rem",
                                     "nav-link-padding-y" = "0rem",
                                     "card-group-margin" = "1rem")
