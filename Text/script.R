library(magrittr)
library(tidyverse)
library(here)
library(lubridate)
# library(BGPhazard)
library(Brobdingnag)

db <- read_csv(here("DB/200704COVID19MEXICO.csv"),na = "98")
db %<>% select(ID_REGISTRO,SEXO,FECHA_ACTUALIZACION, TIPO_PACIENTE, FECHA_INGRESO:FECHA_DEF,INTUBADO:HIPERTENSION,CARDIOVASCULAR:TABAQUISMO,RESULTADO)
db %<>% filter(FECHA_SINTOMAS > ymd("2020-02-25"))
db %<>% filter(RESULTADO == 1) 
# db %>% filter(time > 75, !is.na(FECHA_DEF))
db %<>% filter(! ID_REGISTRO %in% c("0977d7","0d2268","0b8bbc"))


db %<>% 
  mutate(activos = case_when((FECHA_ACTUALIZACION - FECHA_SINTOMAS)< days(14) ~"activos",
                             T~"no activos"),
         recuperados = case_when(is.na(FECHA_DEF) & (FECHA_ACTUALIZACION - FECHA_SINTOMAS) >= days(14) &
                                   TIPO_PACIENTE == 1~ "recuperados",
                                 T~"no recuperados"),
         time = case_when(!is.na(FECHA_DEF)~as.numeric(FECHA_DEF-FECHA_SINTOMAS),
                          T~as.numeric(FECHA_ACTUALIZACION-FECHA_SINTOMAS)),
         delta = case_when(!is.na(FECHA_DEF)~1,
                           T~0))

db %<>% filter(time > 0)
db %>% na.omit %>% nrow
# Explorar ----------------------------------------------------------------

db %>% filter(activos=="no activos") %>% ggplot(aes(x=time,group=delta,fill=delta)) + geom_histogram(bins = 100)
db %>% ggplot(aes(x=time2,group=delta,fill=delta)) + geom_histogram(bins = 60)
db %>% ggplot(aes(x=time,group=DIABETES,fill=as.factor(DIABETES))) + geom_histogram(bins = 100)
db %>% filter(INTUBADO == 1) %>% ggplot(aes(x=time,group=delta,fill=as.factor(delta))) + 
  geom_histogram(bins = 50)

db %>% ggplot(aes(x=time,group=OBESIDAD,fill=as.factor(OBESIDAD))) + geom_histogram(bins = 100)
db %>% filter(FECHA_SINTOMAS < ymd("2020-03-18")) %>% select(FECHA_SINTOMAS,FECHA_DEF,time,delta) %>% 
  filter(delta == 1) %>% arrange(desc(time))


library(survival)
library(ggthemes)
db2 <- db %>% select(time,delta,EDAD,SEXO,HIPERTENSION,DIABETES,OBESIDAD,TABAQUISMO)
db2 %<>% na.omit
db2 %<>% mutate(cuantil = cut(EDAD,breaks = quantile(EDAD,c(0,.25,.5,.75,1)),
                              include.lowest = T,right = F))
t<-Surv(db2$time,event = db2$delta)

(a <- db2 %>% select(-time,-delta,-cuantil,-EDAD,-SEXO,
                     HYPERTENSION=HIPERTENSION,OBESITY = OBESIDAD,
                     SMOKING = TABAQUISMO) %>% map2(.y = names(.),~{
  aux <- survfit(t~.x)
  tibble(a= aux$time,
         b = aux$surv,
         c = c(rep("Yes", aux$strata[[1]]),
               rep("No", aux$strata[[2]])),
         d = .y
         )
  }) %>% do.call(rbind, .) %>% ggplot(aes(x = a,y = b, color = c)) + geom_step(size = 1) + 
    geom_vline(xintercept = 40,color = "gray50", linetype = "dotted")+
  # xlim(c(0,50))+ 
  theme_tufte() +
  scale_color_manual(values = c("#F3B700","#68B0AB")) + 
  theme(axis.line = element_line(colour = "black")) + 
  labs(color = "Comorbidity") +
  xlab("Time to death (days)") + ylab("Survival probability") + 
  facet_wrap(~d,nrow = 1))

library(ggthemes)
xfit<-survfit(t~db2$SEXO)

(s1 <- ggplot(tibble(a=c(xfit$time),
                     b=c(xfit$surv),
                     c=c(rep("Women",xfit$strata[[1]]),rep("Men",xfit$strata[[2]])))) + 
    geom_step(aes(x=a,y=b,color=as.factor(c)), size = 1) + 
    geom_vline(xintercept = 30, linetype = "dotted", color = "gray50") +
    ylim(c(0.5,1)) +
    # xlim(c(0,50)) +
    labs(color = "Gender") + theme_tufte() +
    scale_color_manual(values = c("#2b3a67","#ff1b1c"))+
    theme(axis.line = element_line(colour = "black")) + 
    xlab("Time to death (days)") + ylab("Survival probability")
)

xfit<-survfit(t~db2$cuantil)

(s2 <- ggplot(tibble(a=c(xfit$time),
                     b=c(xfit$surv),
                     c=c(rep("0-32",xfit$strata[[1]]),
                         rep("33-43",xfit$strata[[2]]),
                         rep("44-55",xfit$strata[[3]]),
                         rep("56-120",xfit$strata[[4]])
                         )
                     )) + 
    geom_step(aes(x=a,y=b,color=as.factor(c)), size = 1) + 
    geom_vline(xintercept = 30, linetype = "dotted", color = "gray50") +
    ylim(c(.5,1)) + 
    # xlim(c(0,50))+
    labs(color = "Age") + theme_tufte() +
    # scale_color_manual(values = c("#68b0ab","#f06543","#cb9cf2","#e85d75"))+
    scale_color_viridis(discrete = T, direction = -1)+
    theme(axis.line = element_line(colour = "black")) + 
    xlab("Time to death (days)") + ylab("Survival probability")
)
library(viridis)

xfit<-survfit(t~db2$EDAD)

(s3 <- ggplot(tibble(a=c(xfit$time),
                     b=c(xfit$surv),
                     c = xfit$strata %>% data.frame %>% rownames_to_column("id") %>% separate(id,into = c("basura","basura2","edad")) %>% select(3,4) %>% set_names(c("edad","n")) %>% pmap(function(edad,n){rep(edad,n)}) %>% do.call(c,.)
)) + 
    geom_step(aes(x=a,y=b, group = c,color=as.numeric(c)), size = 1) + 
    geom_vline(xintercept = 30, linetype = "dotted", color = "gray50") +
    ylim(c(.5,1)) + 
    # xlim(c(0,50))+
    labs(color = "Age") + theme_tufte() +
    scale_color_viridis(option = "D",alpha = .6,direction = -1)+
    # scale_color_manual(values = c("#68b0ab","#f06543","#cb9cf2","#e85d75"))+
    theme(axis.line = element_line(colour = "black")) + 
    xlab("Time to death (days)") + ylab("Survival probability")
)

save(a,s1,s2,s3,file = "exploratory.Rdata")
# db2 %<>% mutate(sexoEdad = str_c(SEXO, cuantil,sep = ", "))
gridExtra::grid.arrange(s1,s2,nrow=1)
xfit<-survfit(t~db2$sexoEdad)
plot(xfit)

db3 <- db %>% select(time, delta, INTUBADO) %>% filter(INTUBADO %in% c(1,2))
t<-Surv(db3$time,event = db3$delta)
xfit<-survfit(t~db3$INTUBADO)
plot(xfit)
# Modelo ------------------------------------------------------------------
#Todos
list.files("~/Documents/Git/BGPhazard/R") %>% walk(~source(sprintf("~/Documents/Git/BGPhazard/R/%s",.x)))
aux <- db
# aux <- db %>% filter(recuperados == "no recuperados")
res <- CuMRes(times = muestra$time,delta = aux$delta,type.t = 2,K = 7, length =1,iterations = 2000,burn.in = 200)

CuPloth(res)
CuPlotDiag(res,variable = "Z")

#Intubados
db2 <- db %>% filter(INTUBADO== 1)
res2 <- CuMRes(times = db2$time,delta = db2$delta,type.t = 2,K = 7, length =1,iterations = 2000,burn.in = 200)
CuPloth(res2)
CuPlotDiag(res2,variable = "Z")

# No intubados
db3 <- db %>% filter(INTUBADO== 2)
res3 <- CuMRes(times = db3$time,delta = db3$delta,type.t = 2,K = 7, length =1,iterations = 2000,burn.in = 200)
CuPloth(res3)
CuPlotDiag(res3,variable = "Z")



# Modelo con covariables --------------------------------------------------
load("uno.RData")
# muestra<- db %>%
#   group_by(HIPERTENSION, OBESIDAD, DIABETES, INTUBADO, SEXO) %>%
#   mutate(num_rows=n()) %>%
#   sample_frac(0.01, weight=num_rows) %>%
#   ungroup %>% select(time,delta,EDAD,DIABETES,OBESIDAD,HIPERTENSION,INTUBADO)
# 
# 
# res <- CCuMRes(data = muestra,type.t = 2,length = 1,type.c = 4,
#                iterations = 10000,burn.in = 1000,thinning = 5, c.nu = .5,var.delta.str = .05)
# save(res,muestra,file = "uno.Rdata")
CCuPloth(res)
CCuPlotDiag(res,variable="Delta",pos = 1)
CCuPlotDiag(res,variable="Theta",pos = 5)
CCuPlotDiag(res,"Z.m",pos = 1)

CuPlotDiag(res,"Lambda",pos = 45)
CuPlotDiag(res,"Mu")
CuPlotDiag(res,"Pi",pos = 2)
CuPlotDiag(res,"C",pos = 2)
CuPlotDiag(res,"Z",pos = 2)

PRED <- CCuPloth(res, new_obs = tibble(EDAD = 45,DIABETES = 1, OBESIDAD = 2, 
                                       HIPERTENSION = 2, INTUBADO = 97))

res.m <- CuMRes(muestra$time,delta = muestra$delta,type.t = 2,length = 1,type.c = 4,
               iterations = 10000,burn.in = 1000,thinning = 5, c.nu = .5)

CuPloth(res.m)
CuPlotDiag(res.m,variable = "Z")
