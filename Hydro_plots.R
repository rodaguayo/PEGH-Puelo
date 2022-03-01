Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("hydroGOF")
library("plotly")
library("hydroTSM")


q_sim<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/Caudales_Simulados.csv")
q_obs<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/Caudales_Observados.csv")
names_gauge<-c("Río Azul", "Río Foyel","Río Manso en los Moscos","Rio Manso en Lago Steffen","Río Manso en Confluencia",
               "Río Manso antes Río Puelo", "Puelo en Frontera","Río Puelo antes Río Manso","Río Puelo en Tagua-Tagua","Río Puelo en Carrera Basilio")

q_sim$X<-as.Date(q_sim$X, "%b %d %Y")
q_obs$X<-as.Date(q_obs$X, "%b %d %Y")

q_sim<-zoo(q_sim[,2:11], order.by = q_sim$X)
q_obs<-zoo(q_obs[,2:11], order.by = q_obs$X)
period<-as.yearmon(time(q_sim))
q_sim<-as.data.frame(aggregate(q_sim, as.yearmon(time(q_sim)), mean))
q_obs<-as.data.frame(aggregate(q_obs, as.yearmon(time(q_obs)), mean))
period<-as.Date(paste0(rownames(q_sim),1), "%b %Y%d")

q_sim_nc<-q_sim
q_sim_nc[is.na(q_obs)]<-NA
data_sim_mean<-as.data.frame(t(monthlyfunction(cbind(period,q_sim), FUN = mean, na.rm = T)))
data_sim_mean$month<- factor(rownames(data_sim_mean), levels = c(rownames(data_sim_mean)))
data_obs_mean<-as.data.frame(t(monthlyfunction(cbind(period,q_obs), FUN = mean, na.rm = T)))
data_obs_mean$month<- factor(rownames(data_obs_mean), levels = c(rownames(data_obs_mean)))

data_sim_annual<-monthly2annual(cbind(period,q_sim), FUN = mean, na.rm = T, out.type = "data.frame")
data_obs_annual<-monthly2annual(cbind(period,q_obs), FUN = mean, na.rm = F, out.type = "data.frame")

q_sim_nc_prob<-as.data.frame(hydroTSM::fdc(q_sim_nc, plot =FALSE))
q_obs_prob<-as.data.frame(hydroTSM::fdc(q_obs, plot =FALSE))

f <- list(family = "Verdana", size = 16)
f2 <- list(family = "Verdana", size = 14)

for(i in 1:10) {
  
  x1 <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date', tickformat = "%Y")
  y1 <- list(titlefont = f, title = "Caudal (m3/s)", tickfont = f2, ticks = "outside", zeroline = FALSE)
  title1 <-list(text = names_gauge[i], font = f, showarrow = F, xref = "paper", yref = "paper", x = 1, y = 1)
  title11 <-list(text = "a) Caudal medio anual", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.05, y = 0.05)
  fig1 <- plot_ly(showlegend = F, y = data_sim_annual[,i], x = seq(1980,2020), type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
  fig1 <- fig1 %>% add_trace(y = data_obs_annual[,i], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
  fig1 <- fig1 %>% layout(xaxis = x1, yaxis = y1, annotations = title1)
  fig1 <- fig1 %>% layout(annotations = title11)
  
  x2 <- list(titlefont = f, tickfont = f2, ticks = "outside")
  title2 <-list(text = "b) Caudal medio mensual", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.25, y = 0.1)
  fig2 <- plot_ly(data_sim_mean, y = data_sim_mean[,i], showlegend = FALSE, x = ~month, type = 'scatter', mode = 'lines+markers', name = "GR4J",  marker = list(size = 3, color = "grey"), line = list(color = "grey", width = 1))
  fig2 <- fig2 %>% add_trace(data_obs_mean, y = data_obs_mean[,i], x = ~month, mode = 'lines+markers', name = "GR5J", marker = list(size = 3, color = "black"), line = list(color = "black", width = 1))
  fig2 <- fig2 %>% layout(xaxis = x2, annotations = title2, showlegend = FALSE)
  
  x3 <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date', tickformat = "%Y")
  y3 <- list(titlefont = f, title = "Caudal (m3/s)", tickfont = f2, ticks = "outside", zeroline = FALSE)
  title3 <-list(text = "c) Caudal mensual", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.05, y = 0.9)
  fig3 <- plot_ly(showlegend = F, y = q_sim[,i], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
  fig3 <- fig3 %>% add_trace(y = q_obs[,i], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
  fig3 <- fig3 %>% layout(xaxis = x3, yaxis = y3, annotations = title3)
  fig3
  
  x4 <- list(title = "Probabilidad (%)", titlefont = f, tickfont = f2, ticks = "outside")
  title4 <-list(text = "d) Curva de duración", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.25, y = 0.9)
  fig4 <- plot_ly(showlegend = FALSE, y = sort(q_sim_nc[,i]), x =  sort(q_sim_nc_prob[,i])*100, type = 'scatter', mode = 'lines', line = list(size = 3, color = "grey"))
  fig4 <- fig4 %>% add_trace(y = sort(q_obs[,i]), x =  sort(q_obs_prob[,i])*100, mode = 'lines', line = list(size = 3, color = "black"))
  fig4 <- fig4 %>% layout(xaxis = x4, yaxis = y4, annotations = title4)
  
  s1  <- subplot(fig1, fig2, nrows = 1, shareX = T, shareY= T, titleY = T, titleX = T, margin = c(0.02, 0.02, 0.02, 0.02))
  s2  <- subplot(fig3, fig4, nrows = 1, shareX = F, shareY= T, titleY = T, titleX = T, margin = c(0.02, 0.02, 0.02, 0.02))
  fig <- subplot(s1, s2, nrows = 2,  titleY = T, titleX = T, margin = c(0.02, 0.02, 0.02, 0.02))
  fig
  
  server <- orca_serve()
  server$export(fig, file = paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/Hydro_plot_",i,".png"), width = 1200, height = 1200, scale = 3)
  server$close()
  
  }

