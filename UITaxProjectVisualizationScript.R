### UI Trust Fund, Funding, and Payment Visualization

## X (Dependent Variable) - Taxable Wage Base (TWB), Trust Fund (TF), Maximum
## Weekly Benefit Amount (MWBA)
## Y (Independent variable) - State, Political party affiliation (Republican 
## Governor(RG) v Democratic Governor (DG), Republican President (RP) v 
## Democratic President (DP) 
## Controlling for: Geographic Area, and Gross Domestic Product (GDP))

## Hypothesis 1: States with higher trust fund amounts will have higher taxable 
## wage base.
## H2: States with lower MWBA will have lower TWB, higher TFs.
## H3: In states that see a change from Republican to Democratic administrations,
## there will be higher TWBs, lower TFs, higher MWBAs.

## Let's focus in on one region of the US. Midwest for starters.
MWonly <- subset(TWBState, MW == 1)
summary(MWonly)

modelMWonly1 <- lm(TWB21 ~ TF20, data=MWonly)
display(modelMWonly1)

MW1 <- ggplot(MWonly, aes(y=TF20, x=TWB21)) +
  xlab("Taxable Wage Base 2021 by State") +
  ylab("Trust Fund 2020 by State") +
  geom_text(label = MWonly$State, size = 4)
MW2 <- ggplot(MWonly, aes(y=TF19, x=TWB20)) +
  xlab("Taxable Wage Base 2020 by State") +
  ylab("Trust Fund 2019 by State") +
  geom_text(label = MWonly$State, size = 4)
MW3 <- ggplot(MWonly, aes(y=TF18, x=TWB19)) +
  xlab("Taxable Wage Base 2019 by State") +
  ylab("Trust Fund 2018 by State") +
  geom_text(label = MWonly$State, size = 4)
MW4 <- ggplot(MWonly, aes(y=TF17, x=TWB18)) +
  xlab("Taxable Wage Base 2018 by State") +
  ylab("Trust Fund 2017 by State") +
  geom_text(label = MWonly$State, size = 4)
MW5 <- ggplot(MWonly, aes(y=TF16, x=TWB17)) +
  xlab("Taxable Wage Base 2017 by State") +
  ylab("Trust Fund 2016 by State") +
  geom_text(label = MWonly$State, size = 4)
MW6 <- ggplot(MWonly, aes(y=TF15, x=TWB16)) +
  xlab("Taxable Wage Base 2016 by State") +
  ylab("Trust Fund 2015 by State") +
  geom_text(label = MWonly$State, size = 4)

multiplot(MW1,MW2, MW3, MW4, MW5, MW6, cols=3)
MWTF20 <- lm(TF20 ~ TWB21+TWB20+TWB19+TWB18+TWB17+TWB16, data = MWonly)
summary (MWTF20)
MWTF19 <- lm(TF19 ~ TWB21+TWB20+TWB19+TWB18+TWB17+TWB16, data = MWonly)
summary (MWTF19)
MWTF18 <- lm(TF18 ~ TWB21+TWB20+TWB19+TWB18+TWB17+TWB16, data = MWonly)
summary (MWTF18)
MWTF17 <- lm(TF17 ~ TWB21+TWB20+TWB19+TWB18+TWB17+TWB16, data = MWonly)
summary (MWTF17)
MWTF16 <- lm(TF16 ~ TWB21+TWB20+TWB19+TWB18+TWB17+TWB16, data = MWonly)
summary (MWTF16)
MWTF15 <- lm(TF15 ~ TWB21+TWB20+TWB19+TWB18+TWB17+TWB16, data = MWonly)
summary (MWTF15)

## ggplot(subset(df, dose %in% c("D0.5", "D1")), aes(x = dose, y = len))+
## geom_col(aes(fill = supp), width = 0.7) +
##  scale_fill_viridis_d()


## Scatterplot of Taxable Wage Base by Trust Fund in Previous Year
p1 <- ggplot(TWBState, aes(y=TF20, x=TWB21)) +
  xlab("Taxable Wage Base 2021 by State") +
  ylab("Trust Fund 2020 by State") +
  geom_text(label = TWBState$State, size = 2)
## abline()?
p2 <- ggplot(TWBState, aes(y=TF19, x=TWB20)) +
  xlab("Taxable Wage Base 2020 by State") +
  ylab("Trust Fund 2019 by State") +
  geom_text(label = TWBState$State, size = 2)
p3 <- ggplot(TWBState, aes(y=TF18, x=TWB19)) +
  xlab("Taxable Wage Base 2019 by State") +
  ylab("Trust Fund 2018 by State") +
  geom_text(label = TWBState$State, size = 2)
p4 <- ggplot(TWBState, aes(y=TF17, x=TWB18)) +
  xlab("Taxable Wage Base 2018 by State") +
  ylab("Trust Fund 2017 by State") +
  geom_text(label = TWBState$State, size = 2)
p5 <- ggplot(TWBState, aes(y=TF16, x=TWB17)) +
  xlab("Taxable Wage Base 2017 by State") +
  ylab("Trust Fund 2016 by State") +
  geom_text(label = TWBState$State, size = 2)
p6 <- ggplot(TWBState, aes(y=TF15, x=TWB16)) +
  xlab("Taxable Wage Base 2016 by State") +
  ylab("Trust Fund 2015 by State") +
  geom_text(label = TWBState$State, size = 2)

multiplot(p1, p2, p3, p4, p5, p6, cols=3)

## Need to control for outliers, namely OR and WA.
noORWA16 <- subset(TWBState, State != "TWB2016" > 45000)
## EX: noSwiss <- subset(powell, country != "Switzerland")
## Model2t <- lm(turnout ~ linkage, data=noSwiss)
## display(Model2t)

noWA <- subset(TWBState, State != "WA")
modelnoWAp1 <- lm(TWB21 ~ TF20, data=noWA)
display(modelnoWAp1)
modelnoWAp2 <- lm(TWB20 ~ TF19, data=noWA)
display(modelnoWAp2)
modelnoWAp3 <- lm(TWB19 ~ TF18, data=noWA)
display(modelnoWAp3)
modelnoWAp4 <- lm(TWB18 ~ TF17, data=noWA)
display(modelnoWAp4)
modelnoWAp5 <- lm(TWB17 ~ TF16, data=noWA)
display(modelnoWAp5)
modelnoWAp6 <- lm(TWB16 ~ TF15, data=noWA)
display(modelnoWAp6)

## Make a separate fix for OR, then ggplot them.
## Going about it another way...
## Boxplots with outliers, multiplotted.

b1 <- ggplot(TWBState, aes(y=TF20, x=TWB21)) +
  geom_boxplot()
b2 <- ggplot(TWBState, aes(y=TF19, x=TWB20)) +
  geom_boxplot()
b3 <- ggplot(TWBState, aes(y=TF18, x=TWB19)) +
  geom_boxplot()
b4 <- ggplot(TWBState, aes(y=TF17, x=TWB18)) +
  geom_boxplot()
b5 <- ggplot(TWBState, aes(y=TF16, x=TWB17)) +
  geom_boxplot()
b6 <- ggplot(TWBState, aes(y=TF15, x=TWB16)) +
  geom_boxplot()

multiplot(b1, b2, b3, b4, b5, b6, cols=3)

## Boxplots without outliers
Q20 <- quantile(TWBState$TF20, probs=c(.25, .75), na.rm = FALSE)
iqr20 <- IQR(TWBState$TF20)
up20 <-  Q20[2]+1.5*iqr 
low20<- Q20[1]-1.5*iqr
Q19 <- quantile(TWBState$TF19, probs=c(.25, .75), na.rm = FALSE)
iqr19 <- IQR(TWBState$TF19)
up19 <-  Q19[2]+1.5*iqr 
low19<- Q19[1]-1.5*iqr
Q18 <- quantile(TWBState$TF18, probs=c(.25, .75), na.rm = FALSE)
iqr18 <- IQR(TWBState$TF18)
up18 <-  Q18[2]+1.5*iqr 
low18<- Q18[1]-1.5*iqr
Q17 <- quantile(TWBState$TF17, probs=c(.25, .75), na.rm = FALSE)
iqr17 <- IQR(TWBState$TF17)
up17 <-  Q17[2]+1.5*iqr 
low17<- Q17[1]-1.5*iqr
Q16 <- quantile(TWBState$TF16, probs=c(.25, .75), na.rm = FALSE)
iqr16 <- IQR(TWBState$TF16)
up16 <-  Q16[2]+1.5*iqr 
low16<- Q16[1]-1.5*iqr
Q15 <- quantile(TWBState$TF15, probs=c(.25, .75), na.rm = FALSE)
iqr15 <- IQR(TWBState$TF15)
up15 <-  Q15[2]+1.5*iqr 
low15<- Q15[1]-1.5*iqr

nooutTF20 <- subset(TWBState, TWBState$TF20 > (Q20[1] - 1.5*iqr20) & 
                      TWBState$TF20 < (Q20[2]+1.5*iqr20))
nooutTF19 <- subset(TWBState, TWBState$TF19 > (Q19[1] - 1.5*iqr19) & 
                      TWBState$TF19 < (Q19[2]+1.5*iqr19))
nooutTF18 <- subset(TWBState, TWBState$TF18 > (Q18[1] - 1.5*iqr18) & 
                      TWBState$TF18 < (Q18[2]+1.5*iqr18))
nooutTF17 <- subset(TWBState, TWBState$TF17> (Q17[1] - 1.5*iqr17) & 
                      TWBState$TF17 < (Q17[2]+1.5*iqr17))
nooutTF16 <- subset(TWBState, TWBState$TF16 > (Q16[1] - 1.5*iqr16) & 
                      TWBState$TF16 < (Q16[2]+1.5*iqr16))
nooutTF15 <- subset(TWBState, TWBState$TF15 > (Q15[1] - 1.5*iqr15) & 
                      TWBState$TF15 < (Q15[2]+1.5*iqr15))

bno1 <- ggplot(nooutTF20, aes(y=TF20, x=TWB21)) +
  geom_boxplot()
bno2 <- ggplot(nooutTF19, aes(y=TF19, x=TWB20)) +
  geom_boxplot()
bno3 <- ggplot(nooutTF18, aes(y=TF18, x=TWB19)) +
  geom_boxplot()
bno4 <- ggplot(nooutTF17, aes(y=TF17, x=TWB18)) +
  geom_boxplot()
bno5 <- ggplot(nooutTF16, aes(y=TF16, x=TWB17)) +
  geom_boxplot()
bno6 <- ggplot(nooutTF15, aes(y=TF15, x=TWB16)) +
  geom_boxplot()

multiplot(bno1, bno2, bno3, bno4, bno5, bno6, cols=3)

## Do some more visualizing on this, along with showing figures/regression
## analysis. Then play around with WBA/how much is paid out. And regional
## differences (done 08-08-21), partisan differences.

b1 <- ggplot(TWBState, aes(y=TF17, x=DG17)) +
  geom_bar(stat = "identity") +
  xlab("Gubenatorial Administration Democratic") +
  ylab("Trust Fund 2017 Amount")
b2 <- ggplot(TWBState, aes(y=TF20, x=DG20)) +
  geom_bar(stat = "identity") +
  xlab("Gubenatorial Administration Democratic") +
  ylab("Trust Fund 2020 Amount")
multiplot(b1, b2, cols = 2)
## Clean this up a bit, make a scatterplot mapping out gubenatorial party
## instead of regions (see "AC Highlight 2015" below.)


## WBA Comparison Visualized
WBABasep20 <- ggplot(TWBState, aes(y=MWBA20, x=TWB20)) +
  xlab("Taxable Wage Base 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3) 
WBABasep19 <- ggplot(TWBState, aes(y=MWBA19, x=TWB19)) +
  xlab("Taxable Wage Base 2019 by State") +
  ylab("Maximum Weekly Benefit Amount 2019 by State") +
  geom_text(label = TWBState$State, size = 3)
WBABasep18 <- ggplot(TWBState, aes(y=MWBA18, x=TWB18)) +
  xlab("Taxable Wage Base 2018 by State") +
  ylab("Maximum Weekly Benefit Amount 2018 by State") +
  geom_text(label = TWBState$State, size = 3)
WBABasep17 <- ggplot(TWBState, aes(y=MWBA17, x=TWB17)) +
  xlab("Taxable Wage Base 2017 by State") +
  ylab("Maximum Weekly Benefit Amount 2017 by State") +
  geom_text(label = TWBState$State, size = 3)
WBABasep16 <- ggplot(TWBState, aes(y=MWBA16, x=TWB16)) +
  xlab("Taxable Wage Base 2016 by State") +
  ylab("Maximum Weekly Benefit Amount 2016 by State") +
  geom_text(label = TWBState$State, size = 3)
WBABasep15 <- ggplot(TWBState, aes(y=MWBA15, x=TWB15)) +
  xlab("Taxable Wage Base 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3) 
multiplot(WBABasep20, WBABasep19, WBABasep18, WBABasep17, WBABasep16, WBABasep15, 
          cols=3)

WBAFundp20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20)) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3) 
WBAFundp19 <- ggplot(TWBState, aes(y=MWBA19, x=TF19)) +
  xlab("Trust Fund 2019 by State") +
  ylab("Maximum Weekly Benefit Amount 2019 by State") +
  geom_text(label = TWBState$State, size = 3)
WBAFundp18 <- ggplot(TWBState, aes(y=MWBA18, x=TF18)) +
  xlab("Trust Fund 2018 by State") +
  ylab("Maximum Weekly Benefit Amount 2018 by State") +
  geom_text(label = TWBState$State, size = 3)
WBAFundp17 <- ggplot(TWBState, aes(y=MWBA17, x=TF17)) +
  xlab("Trust Fund 2017 by State") +
  ylab("Maximum Weekly Benefit Amount 2017 by State") +
  geom_text(label = TWBState$State, size = 3)
WBAFundp16 <- ggplot(TWBState, aes(y=MWBA16, x=TF16)) +
  xlab("Trust Fund 2016 by State") +
  ylab("Maximum Weekly Benefit Amount 2016 by State") +
  geom_text(label = TWBState$State, size = 3)
WBAFundp15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15)) +
  xlab("Trust Fund 2016 by State") +
  ylab("Maximum Weekly Benefit Amount 2016 by State") +
  geom_text(label = TWBState$State, size = 3)
multiplot(WBAFundp20, WBAFundp19, WBAFundp18, WBAFundp17, WBAFundp16, WBAFundp15, 
          cols=3)

## AC Highlight 20
WBAFundAC20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20, color=REG)) +
  scale_color_manual(values = c("red", "gray40", "gray40", "gray40", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3)
## MW Highlight 20
WBAFundMW20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20, color=REG)) +
  scale_color_manual(values = c("gray40", "red", "gray40", "gray40", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3)
## NE Highlight 20
WBAFundNE20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "red", "gray40", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3)
## S Highlight 20
WBAFundS20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "gray40", "red", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3)
# W Highlight 20
WBAFundW20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "gray40", "gray40", 
                                "red","gray40")) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3)
# WC Highlight 20
WBAFundWC20 <- ggplot(TWBState, aes(y=MWBA20, x=TF20, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "gray40", "gray40", 
                                "gray40", "red")) +
  xlab("Trust Fund 2020 by State") +
  ylab("Maximum Weekly Benefit Amount 2020 by State") +
  geom_text(label = TWBState$State, size = 3)

## AC Highlight 15
WBAFundAC15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15, color=REG)) +
  scale_color_manual(values = c("red", "gray40", "gray40", "gray40", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3)
## MW Highlight 15
WBAFundMW15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15, color=REG)) +
  scale_color_manual(values = c("gray40", "red", "gray40", "gray40", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3)
## NE Highlight 15
WBAFundNE15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "red", "gray40", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3)
## S Highlight 15
WBAFundS15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "gray40", "red", 
                                "gray40","gray40")) +
  xlab("Trust Fund 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3)
# W Highlight 15
WBAFundW15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "gray40", "gray40", 
                                "red","gray40")) +
  xlab("Trust Fund 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3)
# WC Highlight 15
WBAFundWC15 <- ggplot(TWBState, aes(y=MWBA15, x=TF15, color=REG)) +
  scale_color_manual(values = c("gray40", "gray40", "gray40", "gray40", 
                                "gray40", "red")) +
  xlab("Trust Fund 2015 by State") +
  ylab("Maximum Weekly Benefit Amount 2015 by State") +
  geom_text(label = TWBState$State, size = 3)
multiplot(WBAFundAC15,WBAFundAC20, cols=2)
multiplot(WBAFundMW15,WBAFundMW20, cols=2)
multiplot(WBAFundNE15,WBAFundNE20, cols=2)
multiplot(WBAFundS15,WBAFundS20, cols=2)
multiplot(WBAFundW15,WBAFundW20, cols=2)
multiplot(WBAFundWC15,WBAFundWC20, cols=2)

## Bar color filled.
## LEFT OFF HERE. NEED TO UNIFORM THE MEASUREMENTS (SCALES DIFFER BETWEEN YEARS
## AND THAT CODE IS AROUND SOMEWHERE) AND CARRY THIS OVER TO Y OF TWB AND MWBA.
## ALSO, DO A DEEPER DIVE FOR STATISTICAL CONTROLS AS WELL, NOT JUST VISUALIZATIONS.
TFbf20 <- ggplot(TWBState, aes(y=TF20, x=REG, fill = REG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                               "darkslateblue", "cyan4")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2020 Amount")
TFbf19 <- ggplot(TWBState, aes(y=TF19, x=REG, fill = REG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                               "darkslateblue", "cyan4")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2019 Amount")
TFbf18 <- ggplot(TWBState, aes(y=TF18, x=REG, fill = REG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                               "darkslateblue", "cyan4")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2018 Amount")
TFbf17 <- ggplot(TWBState, aes(y=TF17, x=REG, fill = REG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                               "darkslateblue", "cyan4")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2017 Amount")
TFbf16 <- ggplot(TWBState, aes(y=TF16, x=REG, fill = REG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                               "darkslateblue", "cyan4")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2016 Amount")
TFbf15 <- ggplot(TWBState, aes(y=TF15, x=REG, fill = REG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                               "darkslateblue", "cyan4")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2015 Amount")
multiplot(TFbf15,TFbf16, TFbf17, TFbf18, TFbf19, TFbf20, cols=3)

## Bar with stripes to show where different states hit.
TFbs15 <- ggplot(TWBState, aes(y=TF15, x=REGcolor = REG)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                                "darkslateblue", "navy")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2015 Amount")
TFbs16 <- ggplot(TWBState, aes(y=TF16, x=REGcolor = REG)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                                "darkslateblue", "navy")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2016 Amount")
TFbs17 <- ggplot(TWBState, aes(y=TF17, x=REG, color = REG)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                                "darkslateblue", "navy")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2017 Amount")
TFbs18 <- ggplot(TWBState, aes(y=TF18, x=REGcolor = REG)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                                "darkslateblue", "navy")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2018 Amount")
TFbs19 <- ggplot(TWBState, aes(y=TF19, x=REGcolor = REG)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                                "darkslateblue", "navy")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2019 Amount")
TFbs20 <- ggplot(TWBState, aes(y=TF20, x=REGcolor = REG)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = c("blue4", "royalblue2", "turquoise3", "steelblue1", 
                                "darkslateblue", "navy")) +
  xlab("Geographic Regions") +
  ylab("Trust Fund 2020 Amount")
multiplot(TFbs15,TFbs16, TFbs17, TFbs18, TFbs19, TFbs20, cols=3)
