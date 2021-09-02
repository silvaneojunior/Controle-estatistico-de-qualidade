library(ggplot2)
library(reticulate)
library(latex2exp)
setwd(rstudioapi::getActiveProject())
source_python("helpers/python.py")

d2=c(NA,1.128,1.693,2.059,2.326,2.534,2.704,2.847,
    2.970,3.078,3.173,3.258,3.336,3.407,3.472)
d3=c(NA,0.853,0.888,0.880,0.864,0.848,0.833,0.820,
    0.808,0.797,0.787,0.778,0.770,0.763,0.756)

c4=c(NA,0.798,0.886,0.921,0.940,0.952,0.959,0.965,
    0.969,0.973,0.975,0.978,0.979,0.981,0.982)

create_sheward_X_barra=function(x_data,mu_0=NULL,sigma_0=NULL,k=3){
  n=dim(x_data)[2]
  current_d2=ifelse(n>=15,d2[15],d2[n])
  
  X_barra=get_mean(x_data,axis=2)
  if(is.null(mu_0)){
    mu_0=sum(x_data)/(prod(dim(x_data)))
  }
  if(is.null(sigma_0)){
    R_i=get_max(x_data,axis=2)-get_min(x_data,axis=2)
    R_barra=mean(R_i)
    sigma_0=R_barra/current_d2
  }
  
  LSC=mu_0+k*sigma_0/sqrt(n)
  LM=mu_0
  LIC=mu_0-k*sigma_0/sqrt(n)
  
  plt_x=ggplot()+
          geom_line(aes(x=c(1:length(X_barra)),y=X_barra))+
          geom_point(aes(x=c(1:length(X_barra)),y=X_barra))+
          geom_hline(yintercept=LM)+
          geom_hline(yintercept=LSC,linetype='dashed',color='red')+
          geom_hline(yintercept=LIC,linetype='dashed',color='red')+
          scale_x_continuous('Tempo')+
          scale_y_continuous(expression(bar('X')))+
          theme_bw()
  cat('\nLimites de controle de X_barra: ')
  cat(paste0('\nSuperior: ',LSC,'\n'))
  cat(paste0(  'Média:    ',LM,'\n'))
  cat(paste0(  'Inferior: ',LIC,'\n'))
  
  plt_x
}

create_sheward_R=function(x_data,sigma_0=NULL,k=3){
  n=dim(x_data)[2]
  current_d2=ifelse(n>=15,d2[15],d2[n])
  current_d3=ifelse(n>=15,d3[15],d3[n])
  
  R_i=get_max(x_data,axis=2)-get_min(x_data,axis=2)
  if(is.null(sigma_0)){
    R_barra=mean(R_i)
    sigma_0=R_barra/current_d2
  }
  
  
  LSC=current_d2*sigma_0+k*current_d3*sigma_0
  LM=current_d2*sigma_0
  LIC=max(current_d2*sigma_0-k*current_d3*sigma_0,0)
  
  plt_R=ggplot()+
    geom_line(aes(x=c(1:length(R_i)),y=R_i))+
    geom_point(aes(x=c(1:length(R_i)),y=R_i))+
    geom_hline(yintercept=LM)+
    geom_hline(yintercept=LSC,linetype='dashed',color='red')+
    geom_hline(yintercept=LIC,linetype='dashed',color='red')+
    scale_x_continuous('Tempo')+
    scale_y_continuous('R')+
    theme_bw()
  cat('\nLimites de controle de R: ')
  cat(paste0('\nSuperior: ',LSC,'\n'))
  cat(paste0(  'Média:    ',LM,'\n'))
  cat(paste0(  'Inferior: ',LIC,'\n'))
  plt_R
}

create_sheward_S=function(x_data,sigma_0=NULL,k=3){
  n=dim(x_data)[2]
  current_c4=ifelse(n>=15,c4[15],c4[n])
  
  S_i=get_std(x_data,axis=2)
  
  if(is.null(sigma_0)){
    S_barra=mean(S_i)
    sigma_0=S_barra/current_c4
  }
  
  LSC=current_c4*sigma_0+k*sqrt(1-current_c4**2)*sigma_0
  LM=current_c4*sigma_0
  LIC=max(current_c4*sigma_0-k*sqrt(1-current_c4**2)*sigma_0,0)
  
  plt_R=ggplot()+
    geom_line(aes(x=c(1:length(S_i)),y=S_i))+
    geom_point(aes(x=c(1:length(S_i)),y=S_i))+
    geom_hline(yintercept=LM)+
    geom_hline(yintercept=LSC,linetype='dashed',color='red')+
    geom_hline(yintercept=LIC,linetype='dashed',color='red')+
    scale_x_continuous('Tempo')+
    scale_y_continuous('R')+
    theme_bw()
  cat('\nLimites de controle de S: ')
  cat(paste0('\nSuperior: ',LSC,'\n'))
  cat(paste0(  'Média:    ',LM,'\n'))
  cat(paste0(  'Inferior: ',LIC,'\n'))
  plt_R
}

create_sheward_X_Pd=function(k,delta,n){
  return(1-pnorm(k-delta*sqrt(n))+pnorm(-k-delta*sqrt(n)))
}