########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Wed Jul 21 18:25:44 2010
########################################################
#This program does this and that
library(ggplot2)
library(maptools)
library(gpclib)
library(RColorBrewer)

barPlot <- function(df, mean, colors = brewer.pal(9, "YlOrBr"),
                    percent = FALSE, xlab = "") {
    if(min(df$rates) == 0 && !percent) df[df$rates==0,]$rates <- .005
    if(percent) {
        df$barlabels <- paste(round(df$rates * 100), "%",
                           sep = "")
    } else {
        df$barlabels <- round(df$rates, digits = 1)
    }
    meanannotation <- paste("Mean =",
                        ifelse(percent, paste(round(mean*100, 1), "%",
                                              sep = ""),
                               round(mean, 1)))
    df$State <- reorder(df$State, df$rates)
    p <- ggplot(df, aes(State, rates, fill = rates)) +
               geom_bar(stat = "identity") +
               ylab(xlab) +
               coord_flip() +
               geom_text(aes(label = barlabels),
                         hjust = -.05, size = 4,
                         color = "gray50") +
               annotate("text", x = "Campeche", y = mean * 1.1,
                        label = meanannotation,
                        size = 4, hjust = 0) +
               geom_hline(yintercept = mean, alpha=.4,
                          linetype=2) +
               scale_fill_gradientn("Homicide\nRate",
                                    colour = colors,
                                    trans = "log") +
               opts(legend.position = "none")
    if(!percent) {
      p + scale_fill_gradientn("Homicide\nRate",
                              colour = colors,
                              trans = "log") +
          scale_y_continuous(limits = c(0, max(df$rates) * 1.1))
    } else {
      p + scale_fill_gradientn("Homicide\nRate",
                              colour = colors) +
          scale_y_continuous(limits = c(0, max(df$rates) * 1.1),
                             formatter = "percent")
    }
}

loadMXMap <- function() {
    if(!file.exists("gadm-mexico.RData")) {
        con <- url("http://gadm.org/data/rda/MEX_adm1.RData")
        load(con)
        save(gadm, file = "map/gadm-mexico.RData")
     } else {
        load("map/gadm-mexico.RData")
    }
    #load("map/map_mx.RData")
    gpclibPermit()
    #mx.map <- fortify(mexico.shp, region = "NAME_1")
    mx.map <- fortify(gadm, region = "NAME_1")
    mx.map
}


plotChoro <- function(df, mean, colors = brewer.pal(9, "YlOrRd"),
                      title = "", percent = FALSE) {
  if(min(df$rates) == 0 && !percent) df[df$rates==0,]$rates <- .005
  mx.map <- merge(mx.map, df, by.x = "id", by.y = "State")
  p <- ggplot(mx.map, aes(long,lat)) +
         geom_polygon(aes(group = group, fill = rates),
                   color = I("black")) +
         theme_bw() +
         coord_map(projection = "gilbert") +
         opts(legend.position = "none") +
         opts(title = title) +
         scale_y_continuous(breaks = NA) +
         scale_x_continuous(breaks = NA) + xlab("") + ylab("") +
         opts(panel.grid.minor = theme_line(colour = NA),
                   panel.grid.major = theme_line(colour = NA),
                   panel.border = theme_rect(fill = NA, colour = NA),
                   panel.background = theme_rect(fill = NA,
                                                 colour = NA),
                   plot.background = theme_rect(fill = NA,
                                                colour = NA),
                   legend.position = "none")
  if(!percent) {
      p + scale_fill_gradientn("Homicide\nRate",
                              colour = colors,
                              trans = "log")
  } else {
      p + scale_fill_gradientn("Homicide\nRate", colour = colors)
  }
}

saveBarChoro <- function(df, mean, filename,
                     colors = brewer.pal(9, "YlOrRd"),
                     title = "Homicide rates in Mexico - 2009",
                     percent = FALSE, xlab = ""){
    grid.newpage()
    pushViewport(viewport(layout =  grid.layout(nrow = 1, ncol = 2)))

    subplot <- function(x, y) viewport(layout.pos.row = x,
                                       layout.pos.col = y)

    barplot <- barPlot(df, mean, colors = colors, percent = percent,
                       xlab = xlab)
    choropleth <- plotChoro(df, colors = colors, title = title,
                            percent = percent)
    print(barplot, vp = subplot(1, 1))
    print(choropleth, vp = subplot(1, 2))
    filename <- paste("charts/", filename, sep = "")
    dev.print(png, filename, width = 960, height = 400)
}


cieisp <- read.csv("data/cieisp2009.csv")

#regressions to predict the homicide data October-December
lms <- apply(cieisp[2:13], 1, function(x) lm(unlist(x[1:9]) ~ c(1:9)))
res <- data.frame(lapply(lms, function(x) coef(x)[1] + 10:12 * coef(x)[2]))
cieisp.pre <- cbind(cieisp[1:10], t(res))

#Mexican Population at the State Level
mexico.pop <- 107550697
state.pops <- c(1141946, 3165776, 565400, 795982, 2628942, 600924,
4507177, 3391617, 8841916, 1550417, 5044735, 3140529,
2421606, 7016595, 14837208, 3964009, 1674795, 969540,
4448068, 3550788, 5651371, 1720556, 1314062, 2484949,
2652451, 2510562, 2050514, 3193017, 1134844, 7278690,
1921959, 1379752)
sum(state.pops) == mexico.pop

sum(apply(cieisp.pre[2:13], 1, sum)) + 400
icesi <- c(66, 749, 31, 41, 240, 52, 457, 2523, 747,
930, 414, 1431, 137, 570, 1345, 728, 317, 148,
267, 752, 414, 90, 177, 158, 1251, 498, 117,
288, 48, 347, 33, 76)
hom.com <- data.frame(SNSP = icesi,
                      estimates = apply(cieisp.pre[2:13], 1, sum),
                      cieisp[1])
hom.com$State <- with(hom.com, reorder(State,
                                       abs(SNSP - estimates)))
print(ggplot(melt(hom.com, id = "State"), aes(State, value,
               color = variable)) +
    geom_point() +
    coord_flip() +
    ylab("number of homicides") +
    opts(title = "Comparison of the SNSP homicide data with missing homicides\nto estimates obtained by linear regression"))
dev.print(png, "charts/snsp-vs-est.png", width = 480, height = 640)

#rate for 2009 = 15.0
icesi[8] <- sum(cieisp.pre[8,2:13])
hom.mean <- sum(icesi) / mexico.pop * 100000

#Load the map of Mexico
mx.map <- loadMXMap()


sum(icesi)  / mexico.pop * 100000
########################################################
#barplot and choropleth of the homicide rates
########################################################
state.rates <- data.frame(rates = icesi /
                                   state.pops * 100000,
                          population = state.pops,
                          cieisp[1])

saveBarChoro(state.rates, hom.mean, "homicide-rate-2009.png",
             xlab ="homicides per 100,000")

########################################################
#barplot and choropleth of the execution rates
########################################################
exe <- read.csv("data/executions-bystate.csv")
exe$mean <- apply(exe[2:4], 1, mean, na.rm = TRUE)
exe.mean <- exe$mean[33] / mexico.pop * 100000

exe <- merge(exe, state.rates[,2:3], by = "State")
exe$rates <- exe$mean / exe$population * 100000
saveBarChoro(exe, exe.mean, "execution-rate-2009.png",
         colors = brewer.pal(9, "PuBuGn"),
         title = "Narco-Execution rates in Mexico - 2009",
         xlab = "narco-executions per 100,000")


########################################################
#Percentage of homicides that are executions
########################################################

exe.perc <- merge(exe, state.rates, by = "State")
exe.perc <- transform(exe.perc, rates = rates.x / rates.y)
exe.perc$State <- with(exe.perc, reorder(State, rates))

saveBarChoro(exe.perc, exe.mean / hom.mean,
             "homicide-vs-execution.png",
             colors = brewer.pal(9, "Greens"),
             title = "Percentage of homicides that were narco-executions in Mexico - 2009",
             percent = TRUE,
             xlab = "narco-executions as a percentage of homicides")


