Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))
rm(list=ls())
cat("\014")  

library("hydroGOF")
library("plotly")
library("hydroTSM")

q_sim<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/Caudales_Simulados.csv")
q_obs<-read.csv("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/Caudales_Observados.csv")

q_sim$X<-as.Date(q_sim$X, "%b %d %Y")
q_obs$X<-as.Date(q_obs$X, "%b %d %Y")

q_sim<-zoo(q_sim[,2:11], order.by = q_sim$X)
q_obs<-zoo(q_obs[,2:11], order.by = q_obs$X)
period<-as.yearmon(time(q_sim))
q_sim<-as.data.frame(aggregate(q_sim, as.yearmon(time(q_sim)), mean))
q_obs<-as.data.frame(aggregate(q_obs, as.yearmon(time(q_obs)), mean))
period<-as.Date(paste0(rownames(q_sim),1), "%b %Y%d")

names_gauge<-c("Río Azul", "Río Foyel","Río Manso en los Moscos","Rio Manso en Lago Steffen","Río Manso en Confluencia",
               "Río Manso antes Río Puelo", "Puelo en Frontera","Río Puelo antes Río Manso","Río Puelo en Tagua-Tagua","Río Puelo en Carrera Basilio")

q_sim_c<-q_sim[as.numeric(substr(rownames(q_sim), 7, 10)) %% 2 != 1,]
q_obs_c<-q_obs[as.numeric(substr(rownames(q_obs), 5, 8)) %% 2 != 1,]
q_sim_v<-q_sim[as.numeric(substr(rownames(q_sim), 5, 8)) %% 2 == 1,]
q_obs_v<-q_obs[as.numeric(substr(rownames(q_obs), 5, 8)) %% 2 == 1,]

KGE_c<-KGE(sim=q_sim_c, obs=q_obs_c, method="2012", out.type="full", na.rm=TRUE)
KGE_c<-rbind(KGE_c$KGE.value, KGE_c$KGE.elements)
KGE_v<-KGE(sim=q_sim_v, obs=q_obs_v, method="2012", out.type="full", na.rm=TRUE)
KGE_v<-rbind(KGE_v$KGE.value, KGE_v$KGE.elements)

write.csv(rbind(KGE_c,KGE_v), "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/KGE_Q.csv")

f <- list(family = "Verdana", size = 12)
f2 <- list(family = "Verdana", size = 10)

x <- list(titlefont = f, tickfont = f2, ticks = "outside", type = 'date', tickformat = "%Y-%m")
y <- list(titlefont = f, title = "Q (m3/month)", tickfont = f2, ticks = "outside", zeroline = FALSE)

title1 <-list(text = "a) Azul en Puesto 2", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
fig1 <- plot_ly(showlegend = F, y = q_sim[,1], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig1 <- fig1 %>% add_trace(y = q_obs[,1], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, annotations = title1)

title2 <-list(text = "b) Foyel antes Manso", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
fig2 <- plot_ly(showlegend = F, y = q_sim[,2], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig2 <- fig2 %>% add_trace(y = q_obs[,2], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y, annotations = title2)

title3 <-list(text = "c) Manso en Los Moscos", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
fig3 <- plot_ly(showlegend = F, y = q_sim[,3], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig3 <- fig3 %>% add_trace(y = q_obs[,3], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y, annotations = title3)

title4 <-list(text = "d) Manso en Lago Steffen", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
fig4 <- plot_ly(showlegend = F, y = q_sim[,4], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig4 <- fig4 %>% add_trace(y = q_obs[,4], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y, annotations = title4)
fig4

title5 <-list(text = "e) Manso en Confluencia", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
fig5 <- plot_ly(showlegend = F, y = q_sim[,5], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig5 <- fig5 %>% add_trace(y = q_obs[,5], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y, annotations = title5)

title6 <-list(text = "f) Manso antes de Puelo", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.15, y = 0.99)
fig6 <- plot_ly(showlegend = F, y = q_sim[,6], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig6 <- fig6 %>% add_trace(y = q_obs[,6], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig6 <- fig6 %>% layout(xaxis = x, yaxis = y, annotations = title6)

title7 <-list(text = "g) Puelo en Frontera", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.15, y = 0.99)
fig7 <- plot_ly(showlegend = F, y = q_sim[,7], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig7 <- fig7 %>% add_trace(y = q_obs[,7], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig7 <- fig7 %>% layout(xaxis = x, yaxis = y, annotations = title7)

title8 <-list(text = "h) Puelo antes Manso", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.15, y = 0.99)
fig8 <- plot_ly(showlegend = F, y = q_sim[,8], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig8 <- fig8 %>% add_trace(y = q_obs[,8], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig8 <- fig8 %>% layout(xaxis = x, yaxis = y, annotations = title8)

title9 <-list(text = "i) Puelo en Tagua-Tagua", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.15, y = 0.99)
fig9 <- plot_ly(showlegend = F, y = q_sim[,9], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig9 <- fig9 %>% add_trace(y = q_obs[,9], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig9 <- fig9 %>% layout(xaxis = x, yaxis = y, annotations = title9)

title10 <-list(text = "j) Puelo en Carrera Basilio", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.15, y = 0.99)
fig10 <- plot_ly(showlegend = F, y = q_sim[,10], x = period, type = 'scatter', mode = 'lines', name = "Sim",  opacity  = 1, line = list(color = "grey", width = 1))
fig10 <- fig10 %>% add_trace(y = q_obs[,10], mode = 'lines', name = "Obs",  opacity  = 1, line = list(color = "black", width = 1))
fig10 <- fig10 %>% layout(xaxis = x, yaxis = y, annotations = title10)

s1  <- subplot(fig1, fig2, fig3, fig4, fig5, nrows = 5, shareX = T, shareY= F, titleY = T, titleX = T, margin = c(0.02, 0.02, 0.02, 0.02))
s2  <- subplot(fig6, fig7, fig8, fig9, fig10, nrows = 5, shareX = T, shareY= F, titleY = F, titleX = T, margin = c(0.02, 0.02, 0.02, 0.02))
s3  <-  subplot(s1, s2, shareX = F, shareY= F, titleY = T, titleX = T, margin = c(0.04, 0.04, 0.04, 0.04))
s3

server <- orca_serve()
server$export(s3, file = "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Results/Hydro_plots_all.png", width = 1300, height = 750, scale = 3)
server$close()