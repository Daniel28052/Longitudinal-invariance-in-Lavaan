library(haven)
library(semPlot)
library(lavaan)
library(semTools)
library(tidyverse)
library(SBSDiff)


data <- read_sav("Your dataset")

##################################################################################
############### Testing GOF for base model at Time point 1 #######################
##################################################################################
SDQ_long_1 <- 'pp1 =~ s6_1 + s11_1R + s14_1R + s19_1 + s23_1 
             cp1 =~ s5_1 + s7_1R + s12_1 + s18_1 + s22_1
             es1 =~ s3_1 + s8_1 + s13_1 + s16_1 + s24_1
             hy1 =~ s2_1 + s10_1 + s15_1 + s21_1R +s25_1R
             ps1 =~ s1_1 + s4_1 + s9_1 + s17_1 + s20_1'

SDQ.fit.1 <- cfa(SDQ_long_1, data, estimator="WLSMV", meanstructure=F)
summary(SDQ.fit.1, standardized = T, fit.measures = T, rsq = T)
## only run if plotting 
#semPaths(SDQ.fit.1, whatLabels = "std", layout = "tree2", rotation = 2, sizeLat = 12,
         #sizeInt = 5, sizeMan2 = 5, shapeMan = "square", shapeLat = "circle",
         #node.width = 0.6, asize = 3, edge.label.cex = 0.75, fade = F)

##################################################################################
############### Testing GOF for base model at Time point 2 #######################
##################################################################################
SDQ_long_2 <- 'pp2 =~ s6_2 + s11_2R + s14_2R + s19_2 + s23_2 
             cp2 =~ s5_2 + s7_2R + s12_2 + s18_2 + s22_2
             es2 =~ s3_2 + s8_2 + s13_2 + s16_2 + s24_2
             hy2 =~ s2_2 + s10_2 + s15_2 + s21_2R +s25_2R
             ps2 =~ s1_2 + s4_2 + s9_2 + s17_2 + s20_2'

SDQ.fit.2 <- cfa(SDQ_long_2, data, estimator="WLSMV", meanstructure=F)
summary(SDQ.fit.2, standardized = T, fit.measures = T, rsq = T)
## only run if plotting
#semPaths(SDQ.fit.2, whatLabels = "std", layout = "tree2", rotation = 2, sizeLat = 12,
         #sizeInt = 5, sizeMan2 = 5, shapeMan = "square", shapeLat = "circle",
         #node.width = 0.6, asize = 3, edge.label.cex = 0.75, fade = F)

##################################################################################
############### Testing GOF for base model at Time point 3 #######################
##################################################################################
SDQ_long_3 <- 'pp3 =~ s6_3 + s11_3R + s14_3R + s19_3 + s23_3 
             cp3 =~ s5_3 + s7_3R + s12_3 + s18_3 + s22_3
             es3 =~ s3_3 + s8_3 + s13_3 + s16_3 + s24_3
             hy3 =~ s2_3 + s10_3 + s15_3 + s21_3R +s25_3R
             ps3 =~ s1_3 + s4_3 + s9_3 + s17_3 + s20_3'

SDQ.fit.3 <- cfa(SDQ_long_3, data, estimator="WLSMV", meanstructure=F)
summary(SDQ.fit.3, standardized = T, fit.measures = T, rsq = T)
## only run if plotting
#semPaths(SDQ.fit.3, whatLabels = "std", layout = "tree2", rotation = 2, sizeLat = 12,
         #sizeInt = 5, sizeMan2 = 5, shapeMan = "square", shapeLat = "circle",
         #node.width = 0.6, asize = 3, edge.label.cex = 0.75, fade = F)


##################################################################################
################## Internal consistency at T1 T2 and T3 ##########################
##################################################################################
## method using semTools package
semTools::reliability(SDQ.fit.1)
semTools::reliability(SDQ.fit.2)
semTools::reliability(SDQ.fit.3)

## method using pysch package
names(data)

t1 <- data%>%
  select(2:26)
t2 <- data%>%
  select(27:51)
t3 <- data%>%
  select(52:91)

omega(t1)
omega(t2)
omega(t3)

##################################################################################
##################### Testing configural invariance ##############################
##################################################################################
## Acceptable GOF is sufficient proof of configural invariance

SDQ_config <- 'pp1 =~ s6_1 + s11_1R + s14_1R + s19_1 + s23_1 
               cp1 =~ s5_1 + s7_1R + s12_1 + s18_1 + s22_1
               es1 =~ s3_1 + s8_1 + s13_1 + s16_1 + s24_1
               hy1 =~ s2_1 + s10_1 + s15_1 + s21_1R +s25_1R
               ps1 =~ s1_1 + s4_1 + s9_1 + s17_1 + s20_1

               pp2 =~ s6_2 + s11_2R + s14_2R + s19_2 + s23_2 
               cp2 =~ s5_2 + s7_2R + s12_2 + s18_2 + s22_2
               es2 =~ s3_2 + s8_2 + s13_2 + s16_2 + s24_2
               hy2 =~ s2_2 + s10_2 + s15_2 + s21_2R +s25_2R
               ps2 =~ s1_2 + s4_2 + s9_2 + s17_2 + s20_2

               pp3 =~ s6_3 + s11_3R + s14_3R + s19_3 + s23_3 
               cp3 =~ s5_3 + s7_3R + s12_3 + s18_3 + s22_3
               es3 =~ s3_3 + s8_3 + s13_3 + s16_3 + s24_3
               hy3 =~ s2_3 + s10_3 + s15_3 + s21_3R +s25_3R
               ps3 =~ s1_3 + s4_3 + s9_3 + s17_3 + s20_3'

fit_config <- cfa(SDQ_config, data, estimator="WLSMV", meanstructure=F)
summary(fit_config, standardized = T, fit.measures = T, rsq = T)
## only run if plotting
#semPaths(fit_config, whatLabels = "std", layout = "tree2", rotation = 2, sizeLat = 10,
         #sizeInt = 3, sizeMan2 = 4, shapeMan = "square", shapeLat = "circle",
         #node.width = 1, asize = 3, edge.label.cex = 0.75, fade = F)

##################################################################################
##################### Testing metric invariance ##################################
##################################################################################
SDQ_metric <- 'pp1 =~ V1*s6_1 + V2*s11_1R + V3*s14_1R + V4*s19_1 + V5*s23_1 
               cp1 =~ V6*s5_1 + V7*s7_1R + V8*s12_1 + V9*s18_1 + V10*s22_1
               es1 =~ V11*s3_1 + V12*s8_1 + V13*s13_1 + V14*s16_1 + V15*s24_1
               hy1 =~ V16*s2_1 + V17*s10_1 + V18*s15_1 + V19*s21_1R + V20*s25_1R
               ps1 =~ V21*s1_1 + V22*s4_1 + V23*s9_1 + V24*s17_1 + V25*s20_1

               pp2 =~ V1*s6_2 + V2*s11_2R + V3*s14_2R + V4*s19_2 + V5*s23_2 
               cp2 =~ V6*s5_2 + V7*s7_2R + V8*s12_2 + V9*s18_2 + V10*s22_2
               es2 =~ V11*s3_2 + V12*s8_2 + V13*s13_2 + V14*s16_2 + V15*s24_2
               hy2 =~ V16*s2_2 + V17*s10_2 + V18*s15_2 + V19*s21_2R + V20*s25_2R
               ps2 =~ V21*s1_2 + V22*s4_2 + V23*s9_2 + V24*s17_2 + V25*s20_2

               pp3 =~ V1*s6_3 + V2*s11_3R + V3*s14_3R + V4*s19_3 + V5*s23_3 
               cp3 =~ V6*s5_3 + V7*s7_3R + V8*s12_3 + V9*s18_3 + V10*s22_3
               es3 =~ V11*s3_3 + V12*s8_3 + V13*s13_3 + V14*s16_3 + V15*s24_3
               hy3 =~ V16*s2_3 + V17*s10_3 + V18*s15_3 + V19*s21_3R + V20*s25_3R
               ps3 =~ V21*s1_3 + V22*s4_3 + V23*s9_3 + V24*s17_3 + V25*s20_3'


fit_metric <- cfa(SDQ_metric, data, estimator="WLSMV", meanstructure=F)
summary(fit_metric, standardized = T, fit.measures = T, rsq = T)
## only run if plotting
#semPaths(fit_metric, whatLabels = "std", layout = "tree2", rotation = 2, sizeLat = 10,
         #sizeInt = 3, sizeMan2 = 4, shapeMan = "square", shapeLat = "circle",
         #node.width = 1, asize = 3, edge.label.cex = 0.75, fade = F)

################################################################################
####### Compare GOF indices between configural and metric models ###############
################################################################################

c0 <- fit_config@Fit@test[["scaled.shifted"]][["scaling.factor"]]
chi0 <- fit_config@Fit@test[["standard"]][["stat"]]
df0 <- fit_config@Fit@test[["standard"]][["df"]]

c1 <- fit_metric@Fit@test[["scaled.shifted"]][["scaling.factor"]]
chi1 <- fit_metric@Fit@test[["standard"]][["stat"]]
df1 <- fit_metric@Fit@test[["standard"]][["df"]]

## Scaled Chi square difference test - p<.05 indicates non-invariance
sbs.chi(chi1, chi0, df1, df0, c1, c0)

## Delta CFI, TLI and RMSEA
`configural invariance` <- fitmeasures(fit_config, c("chisq", "df", "pvalue", "cfi","tli","rmsea"))
`metric invariance` <- fitmeasures(fit_metric, c("chisq", "df", "pvalue", "cfi","tli","rmsea"))
difference <- `metric invariance`-`configural invariance`
rbind(`configural invariance`,`metric invariance`,difference)

##################################################################################
##################### Testing scalar invariance ##################################
##################################################################################
SDQ_scalar <- 'pp1 =~ V1*s6_1 + V2*s11_1R + V3*s14_1R + V4*s19_1 + V5*s23_1 
               cp1 =~ V6*s5_1 + V7*s7_1R + V8*s12_1 + V9*s18_1 + V10*s22_1
               es1 =~ V11*s3_1 + V12*s8_1 + V13*s13_1 + V14*s16_1 + V15*s24_1
               hy1 =~ V16*s2_1 + V17*s10_1 + V18*s15_1 + V19*s21_1R + V20*s25_1R
               ps1 =~ V21*s1_1 + V22*s4_1 + V23*s9_1 + V24*s17_1 + V25*s20_1

               pp2 =~ V1*s6_2 + V2*s11_2R + V3*s14_2R + V4*s19_2 + V5*s23_2 
               cp2 =~ V6*s5_2 + V7*s7_2R + V8*s12_2 + V9*s18_2 + V10*s22_2
               es2 =~ V11*s3_2 + V12*s8_2 + V13*s13_2 + V14*s16_2 + V15*s24_2
               hy2 =~ V16*s2_2 + V17*s10_2 + V18*s15_2 + V19*s21_2R + V20*s25_2R
               ps2 =~ V21*s1_2 + V22*s4_2 + V23*s9_2 + V24*s17_2 + V25*s20_2

               pp3 =~ V1*s6_3 + V2*s11_3R + V3*s14_3R + V4*s19_3 + V5*s23_3 
               cp3 =~ V6*s5_3 + V7*s7_3R + V8*s12_3 + V9*s18_3 + V10*s22_3
               es3 =~ V11*s3_3 + V12*s8_3 + V13*s13_3 + V14*s16_3 + V15*s24_3
               hy3 =~ V16*s2_3 + V17*s10_3 + V18*s15_3 + V19*s21_3R + V20*s25_3R
               ps3 =~ V21*s1_3 + V22*s4_3 + V23*s9_3 + V24*s17_3 + V25*s20_3

                s1_1~ i1*1
                s1_2~ i1*1
                s1_3~ i1*1
                s2_1~ i2*1
                s2_2~ i2*1
                s2_3~ i2*1
                s3_1~ i3*1
                s3_2~ i3*1
                s3_3~ i3*1
                s4_1~ i4*1
                s4_2~ i4*1
                s4_3~ i4*1
                s5_1~ i5*1
                s5_2~ i5*1
                s5_3~ i5*1
                s6_1~ i6*1
                s6_2~ i6*1
                s6_3~ i6*1 
                s7_1R~ i7*1
                s7_2R~ i7*1
                s7_3R~ i7*1
                s8_1~ i8*1
                s8_2~ i8*1
                s8_3~ i8*1
                s9_1~ i9*1
                s9_2~ i9*1
                s9_3~ i9*1
                s10_1~ i10*1
                s10_2~ i10*1
                s10_3~ i10*1
                s11_1R~ i11*1
                s11_2R~ i11*1
                s11_3R~ i11*1 
                s12_1~ i12*1
                s12_2~ i12*1
                s12_3~ i12*1
                s13_1~ i13*1
                s13_2~ i13*1
                s13_3~ i13*1 
                s14_1R~ i14*1
                s14_2R~ i14*1
                s14_3R~ i14*1
                s15_1~ i15*1
                s15_2~ i15*1
                s15_3~ i15*1
                s16_1~ i16*1
                s16_2~ i16*1
                s16_3~ i16*1
                s17_1~ i17*1
                s17_2~ i17*1
                s17_3~ i17*1
                s18_1~ i18*1
                s18_2~ i18*1
                s18_3~ i18*1
                s19_1~ i19*1
                s19_2~ i19*1
                s19_3~ i19*1
                s20_1~ i20*1
                s20_2~ i20*1
                s20_3~ i20*1
                s21_1R~ i21*1
                s21_2R~ i21*1
                s21_3R~ i21*1
                s22_1~ i22*1
                s22_2~ i22*1
                s22_3~ i22*1
                s23_1~ i23*1
                s23_2~ i23*1
                s23_3~ i23*1
                s24_1~ i24*1
                s24_2~ i24*1
                s24_3~ i24*1 
                s25_1R~ i25*1
                s25_2R~ i25*1
                s25_3R~ i25*1

                pp1 ~~ 0*pp2
                pp1 ~~ 0*pp3
                pp2 ~~ 0*pp3
                cp1 ~~ 0*cp2
                cp1 ~~ 0*cp3
                cp2 ~~ 0*cp3
                es1 ~~ 0*es2
                es1 ~~ 0*es3
                es2 ~~ 0*es3
                hy1 ~~ 0*hy2
                hy1 ~~ 0*hy3
                hy2 ~~ 0*hy3
                ps1 ~~ 0*ps2
                ps1 ~~ 0*ps3
                ps2 ~~ 0*ps3'


fit_scalar <- cfa(SDQ_scalar, data, estimator="WLSMV", meanstructure=F)
summary(fit_scalar, standardized = T, fit.measures = T, rsq = T)
#Only run if plotting
#semPaths(fit_scalar, whatLabels = "std", layout = "tree2", rotation = 2, sizeLat = 10,
         #sizeInt = 3, sizeMan2 = 4, shapeMan = "square", shapeLat = "circle",
         #node.width = 1, asize = 3, edge.label.cex = 0.75, fade = F)

      
################################################################################
########### Compare GOF indices between metric and scalar models ###############
################################################################################

c0 <- fit_metric@Fit@test[["scaled.shifted"]][["scaling.factor"]]
chi0 <- fit_metric@Fit@test[["standard"]][["stat"]]
df0 <- fit_metric@Fit@test[["standard"]][["df"]]

c1 <- fit_scalar@Fit@test[["scaled.shifted"]][["scaling.factor"]]
chi1 <- fit_scalar@Fit@test[["standard"]][["stat"]]
df1 <- fit_scalar@Fit@test[["standard"]][["df"]]

sbs.chi(chi1, chi0, df1, df0, c1, c0)


`configural invariance` <- fitmeasures(fit_config, c("chisq", "df", "pvalue", "cfi","tli","rmsea"))
`metric invariance` <- fitmeasures(fit_metric, c("chisq", "df", "pvalue", "cfi","tli","rmsea"))
`conf-met diff` <- `metric invariance`-`configural invariance`
`scalar invariance` <- fitmeasures(fit_scalar, c("chisq", "df", "pvalue", "cfi","tli","rmsea"))
`met-scal diff` <- `scalar invariance`-`metric invariance`

rbind(`configural invariance`,`metric invariance`,`scalar invariance`,
      `conf-met diff`, `met-scal diff`)

 
         
                 

