cat("
# ──╔╦═══╦═╗─╔╦═══╦════╦╗─╔╦═══╦═╗─╔╗
# ──║║╔═╗║║╚╗║║╔═╗║╔╗╔╗║║─║║╔═╗║║╚╗║║
# ──║║║─║║╔╗╚╝║║─║╠╝║║╚╣╚═╝║║─║║╔╗╚╝║
# ╔╗║║║─║║║╚╗║║╚═╝║─║║─║╔═╗║╚═╝║║╚╗║║
# ║╚╝║╚═╝║║─║║║╔═╗║─║║─║║─║║╔═╗║║─║║║
# ╚══╩═══╩╝─╚═╩╝─╚╝─╚╝─╚╝─╚╩╝─╚╩╝─╚═╝
#         ╔═══╦═══╦═══╦╗──╔╦═══╦═══╗
#         ║╔═╗║╔══╣╔═╗║╚╗╔╝║╔═╗║╔═╗║
#         ║║─╚╣╚══╣╚═╝╠╗║║╔╣║─║║╚══╗
#         ║║─╔╣╔══╣╔╗╔╝║╚╝║║╚═╝╠══╗║
#         ║╚═╝║╚══╣║║╚╗╚╗╔╝║╔═╗║╚═╝║
#         ╚═══╩═══╩╝╚═╝─╚╝─╚╝─╚╩═══╝
### Code to Replicate \"Tools for Identifying Partisan Gerrymandering\"
# 🅙🅞🅝🅐🅣🅗🅐🅝 🅡. 🅒🅔🅡🅥🅐🅢, University of California Irvine
# 🅑🅔🅡🅝🅐🅡🅓 🅖🅡🅞🅕🅜🅐🅝, University of California Irvine
#
•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞\n
	")

sink("/dev/null")
        toInstall <- c("maptools", "rgdal", "ggplot2", "spatstat", "RColorBrewer", "maps", "stargazer")
        if(doInstall){install.packages(toInstall, repos = "http=//cran.r-project.org")}

    lapply(toInstall, library, character.only = TRUE)
sink()