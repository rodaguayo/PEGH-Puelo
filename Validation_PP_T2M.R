Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("exactextractr")
library("hydroGOF")
library("terra")
library("readxl")
library("plotly")
library("RColorBrewer")

area  <-ext(c(-74,-69,-51,-40))
period<-seq(from = as.POSIXct("1980-01-01", tz="UTC"), to = as.POSIXct('2019-12-31', tz="UTC"), by = "day")

#Observations
pp_shape<-vect("C:/Users/rooda/Dropbox/Patagonia/GIS South/Precipitation_v10.shp")
pp_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/precipitation/Data_precipitation_v10.xlsx", sheet = "data_daily", guess_max = 30000))
pp_obs$Date<-as.POSIXct(pp_obs$Date)
pp_obs <- subset(pp_obs, Date >= min(period) & Date <= max(period))
pp_obs <- aggregate(pp_obs[,-1], FUN = sum, by = list(strftime(period,format="%Y-%V")))
pp_obs$Group.1<- NULL

t2m_shape<-vect("C:/Users/rooda/Dropbox/Patagonia/GIS South/Temperature_v10.shp")
t2m_obs<-as.data.frame(read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/temperature/Data_temperature_v10.xlsx", sheet = "data_daily", guess_max = 30000))
t2m_obs$Date<-as.POSIXct(t2m_obs$Date)
t2m_obs <- subset(t2m_obs, Date >= min(period) & Date <= max(period))
t2m_obs <- aggregate(t2m_obs[,-1], FUN = mean, by = list(strftime(period,format="%Y-%V")))
t2m_obs$Group.1<- NULL

#CR2MET data
pp_cr2met <- rast("E:/Datasets/CR2MET/CR2MET_pr_v2.0_day_1979_2020_005deg.nc",  subds="pr")
pp_cr2met <- subset(pp_cr2met, which(time(pp_cr2met) >= min(period) & (time(pp_cr2met) <= max(period))))
pp_cr2met <- t(extract(pp_cr2met, pp_shape, method='simple'))
pp_cr2met <- pp_cr2met[-1, ]
colnames(pp_cr2met) <- pp_shape$ID
pp_cr2met <- aggregate(pp_cr2met, FUN = sum, by = list(strftime(period,format="%Y-%V")))
pp_cr2met$Group.1<- NULL

t2m_cr2met <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc", subds="t2m")
t2m_cr2met <- subset(t2m_cr2met, which(time(t2m_cr2met) >= min(period) & (time(t2m_cr2met) <= max(period))))
t2m_cr2met <- t(extract(t2m_cr2met, t2m_shape, method='simple'))
t2m_cr2met <- t2m_cr2met[-1, ]
colnames(t2m_cr2met) <- t2m_shape$ID
t2m_cr2met <- aggregate(t2m_cr2met, FUN = mean, by = list(strftime(period,format="%Y-%V")))
t2m_cr2met$Group.1<- NULL

KGE_pp <-KGE(sim=pp_cr2met, obs=pp_obs, method="2012", out.type="full",na.rm=TRUE)
KGE_pp <- cbind(t(KGE_pp$KGE.elements),KGE_pp$KGE.value)
colnames(KGE_pp)<- c("r_wk", "Beta_wk", "Gamma_wk", "KGE_wk")
KGE_pp<-cbind(as.data.frame(pp_shape), KGE_pp)

KGE_t2m<-cbind(me(sim=t2m_cr2met, obs=t2m_obs, na.rm=TRUE), t(KGE(sim=t2m_cr2met, obs=t2m_obs, method="2009", out.type="full", na.rm=TRUE)[["KGE.elements"]]))
colnames(KGE_t2m)<- c("ME_wk", "r_wk", "Beta_wk", "Gamma_wk")
KGE_t2m<-cbind(as.data.frame(t2m_shape), KGE_t2m)

KGE_pp$Zone <- factor(KGE_pp$Zone, levels = c("Northern", "Center", "Southern"))
KGE_t2m$Zone <- factor(KGE_t2m$Zone, levels = c("Northern", "Center", "Southern"))

f <- list(family = "Verdana", size = 22)
f2 <- list(family = "Verdana", size = 18)
marker <- list(color = ~Zone, size = 5)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation PP (r)", titlefont = f, 
          tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.2, 1))

fig1 <- plot_ly(KGE_pp, y = ~r_wk, x = ~Zone, type = "box", color = ~Zone, boxpoints = "all",  marker = marker, pointpos = 0, colors = brewer.pal(3, 'Set2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)
fig1

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias PP (β)", titlefont = f, range = c(0, 2),
           tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(KGE_pp, y = ~Beta_wk, x = ~Zone, type = "box", color = ~Zone, boxpoints = "all",  marker = marker, pointpos = 0, colors = brewer.pal(3, 'Set2'))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)
fig2

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability PP(γ)", titlefont = f, range = c(0.4, 1.2),
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(KGE_pp, y = ~Gamma_wk, x = ~Zone, type = "box", color = ~Zone, boxpoints = "all",  marker = marker, pointpos = 0, colors = brewer.pal(3, 'Set2'))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)
fig3

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE PP ", titlefont = f, 
           tickfont = f2, dtick = 0.3, ticks = "outside", zeroline = FALSE, range = c(0, 1.2))

fig4 <- plot_ly(KGE_pp, y = ~KGE_wk, x = ~Zone, type = "box", color = ~Zone, boxpoints = "all",  marker = marker, pointpos = 0, colors = brewer.pal(3, 'Set2'))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4)
fig4

title5 <-list(text = "e)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.91)
y5 <- list(title = "Mean error T2M (ºC)", titlefont = f, 
           tickfont = f2, dtick = 1.5, ticks = "outside", zeroline = FALSE, range = c(-3, 3))

fig5 <- plot_ly(KGE_t2m, y = ~ME_wk, x = ~Zone, type = "box", color = ~Zone, boxpoints = "all",  marker = marker, pointpos = 0, colors = brewer.pal(3, 'Set2'))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig5 <- fig5 %>% layout(annotations = title5)

title6 <-list(text = "f)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.91)
y6 <- list(title = "Variability T2M", titlefont = f, 
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.6, 1.4))

fig6 <- plot_ly(KGE_t2m, y = ~Gamma_wk, x = ~Zone, type = "box", color = ~Zone, boxpoints = "all",  marker = marker, pointpos = 0, colors = brewer.pal(3, 'Set2'))
fig6 <- fig6 %>% layout(xaxis = x, yaxis = y6, showlegend = FALSE)
fig6 <- fig6 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig6 <- fig6 %>% layout(annotations = title6)

fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, 
               nrows = 3, shareX = T, titleY = T, 
               margin = c(0.04, 0.04, 0.01, 0.01))
fig

server <- orca_serve()
server$export(fig, file = "Figure5_Validation.pdf", width = 1200, height = 1000, scale = 4)
server$export(fig, file = "Figure5_Validation.png", width = 1200, height = 1000, scale = 4)
server$close()

htmlwidgets::saveWidget(fig, "fig.html")
