### UI Trust Fund, Funding, and Payment Comparison Analysis

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


### Regression analysis TWB by trust fund amount (current year)
  ## This isn't regression analysis, per se. I think it's daisy chain or some such?
TFC20 <- lm(TWB20 ~ TF20, data = TWBState)
summary(TFC20)
TFC19 <- lm(TWB19 ~ TF19, data = TWBState)
summary(TFC19)
TFC18 <- lm(TWB18 ~ TF18, data = TWBState)
summary(TFC18)
TFC17 <- lm(TWB17 ~ TF17, data = TWBState)
summary(TFC17)
TFC16 <- lm(TWB16 ~ TF16, data = TWBState)
summary(TFC16)
TFC15 <- lm(TWB15 ~ TF15, data = TWBState)
summary(TFC15)

## States by trust fund amount (prior year)
TFP21 <- lm(TWB21 ~ TF20, data = TWBState)
summary(TFP21)
TFP20 <- lm(TWB20 ~ TF19, data = TWBState)
summary(TFP20)
TFP19 <- lm(TWB19 ~ TF18, data = TWBState)
summary(TFP19)
TFP18 <- lm(TWB18 ~ TF17, data = TWBState)
summary(TFP18)
TFP17 <- lm(TWB17 ~ TF16, data = TWBState)
summary(TFP17)
TFP16 <- lm(TWB16 ~ TF15, data = TWBState)
summary(TFP16)

### Regression analysis TWB by MWBA
WBAT21<- lm(TWB21 ~ MWBA20, data = TWBState)
summary(WBAP21)
WBAT20 <- lm(TWB20 ~ MWBA19, data = TWBState)
summary(WBAP20)
WBAT19 <- lm(TWB19 ~ MWBA18, data = TWBState)
summary(WBAP19)
WBAT18 <- lm(TWB18 ~ MWBA17, data = TWBState)
summary(WBAP18)
WBAT17 <- lm(TWB17 ~ MWBA16, data = TWBState)
summary(WBAP17)
WBAT16 <- lm(TWB16 ~ MWBA15, data = TWBState)
summary(WBAP16)

## And TF by MWBA
WBATF20 <- lm(TF20 ~ MWBA19, data = TWBState)
summary(WBATF20)
WBATF19 <- lm(TF19 ~ MWBA18, data = TWBState)
summary(WBATF19)
WBATF18 <- lm(TF18 ~ MWBA17, data = TWBState)
summary(WBATF18)
WBATF17 <- lm(TF17 ~ MWBA16, data = TWBState)
summary(WBATF17)
WBATF16 <- lm(TF16 ~ MWBA15, data = TWBState)
summary(WBATF16)

### Multiple regression TWB/TF (controlling for Governor, Presidential Party 
## preference)

## 2020 Presidential Run
TWB21RMult <- lm(TWB21 ~ RG21 + RP20, data = TWBState)
summary(TWB21RMult)
TWB21DMult <- lm(TWB21 ~ DG21 + DP20, data = TWBState)
summary(TWB21DMult)
TF20RMult <- lm(TF20 ~ RG20 + RP20, data = TWBState)
summary(TF20RMult)
TF20DMult <- lm(TF20 ~ DG20 + DP20, data = TWBState)
summary(TF20DMult)
## 2016 Presidential Run
TWB17RMult <- lm(TWB17 ~ RG17 + RP16, data = TWBState)
summary(TWB17RMult)
TWB17DMult <- lm(TWB17 ~ DG17 + DP16, data = TWBState)
summary(TWB17DMult)
TF17RMult <- lm(TF17 ~ RG17 + RP16, data = TWBState)
summary(TF17RMult)
TF17DMult <- lm(TF17 ~ DG17 + DP16, data = TWBState)
summary(TF17DMult)

### Multiple regression controlling for GDP, DP/RP, and DG/RG
TWB15Mult <- lm(TWB15 ~ DG15 + RG15 + GDP15, data = TWBState)
summary(TWB15Mult)
## Getting NAs on RG15 and GDP15?

## Improved regression (heteroskedasticity)
ImprTWB21R <- lm(TWB21 ~ RG21 + RP20 + TF20, data = TWBState, 
                  na.action = na.exclude)
summary(ImprTWB21R)
bptest(ImprTWB21R, studentize = FALSE)

## Calculate confidence intervals for the regression parameters
confint((TWB21RMult), level = 0.90)

## Checking for multicollinearity in variables
vif(TWB21RMult)


### States by geographic location

S21 <- lm(TWB21 ~ S, data = TWBState) 
summary(S21)
NE21 <- lm(TWB21 ~ NE, data = TWBState) 
summary(NE21)
WC21 <- lm(TWB21 ~ WC, data = TWBState) 
summary(WC21)
W21 <- lm(TWB21 ~ W, data = TWBState) 
summary(W21)
MW21 <- lm(TWB21 ~ MW, data = TWBState) 
summary(MW21)
AC21 <- lm(TWB21 ~ AC, data = TWBState) 
summary(AC21)

S20 <- lm(TWB20 ~ S, data = TWBState) 
summary(S20)
NE20 <- lm(TWB20 ~ NE, data = TWBState) 
summary(NE20)
WC20 <- lm(TWB20 ~ WC, data = TWBState) 
summary(WC20)
W20 <- lm(TWB20 ~ W, data = TWBState) 
summary(W20)
MW20 <- lm(TWB20 ~ MW, data = TWBState) 
summary(MW20)
AC20 <- lm(TWB20 ~ AC, data = TWBState) 
summary(AC20)

S19 <- lm(TWB19 ~ S, data = TWBState) 
summary(S19)
NE19 <- lm(TWB19 ~ NE, data = TWBState) 
summary(NE19)
WC19 <- lm(TWB19 ~ WC, data = TWBState) 
summary(WC19)
W19 <- lm(TWB19 ~ W, data = TWBState) 
summary(W19)
MW19 <- lm(TWB19 ~ MW, data = TWBState) 
summary(MW19)
AC19 <- lm(TWB19 ~ AC, data = TWBState) 
summary(AC19)

S18 <- lm(TWB18 ~ S, data = TWBState) 
summary(S18)
NE18 <- lm(TWB18 ~ NE, data = TWBState) 
summary(NE18)
WC18 <- lm(TWB18 ~ WC, data = TWBState) 
summary(WC18)
W18 <- lm(TWB18 ~ W, data = TWBState) 
summary(W18)
MW18 <- lm(TWB18 ~ MW, data = TWBState) 
summary(MW18)
AC18 <- lm(TWB18 ~ AC, data = TWBState) 
summary(AC18)

S17 <- lm(TWB17 ~ S, data = TWBState) 
summary(S17)
NE17 <- lm(TWB17 ~ NE, data = TWBState) 
summary(NE17)
WC17 <- lm(TWB17 ~ WC, data = TWBState) 
summary(WC17)
W17 <- lm(TWB17 ~ W, data = TWBState) 
summary(W17)
MW17 <- lm(TWB17 ~ MW, data = TWBState) 
summary(MW17)
AC17 <- lm(TWB17 ~ AC, data = TWBState) 
summary(AC17)

S16 <- lm(TWB16 ~ S, data = TWBState) 
summary(S16)
NE16 <- lm(TWB16 ~ NE, data = TWBState) 
summary(NE16)
WC16 <- lm(TWB16 ~ WC, data = TWBState) 
summary(WC16)
W16 <- lm(TWB16 ~ W, data = TWBState) 
summary(W16)
MW16 <- lm(TWB16 ~ MW, data = TWBState) 
summary(MW16)
AC16 <- lm(TWB16 ~ AC, data = TWBState) 
summary(AC16)

S15 <- lm(TWB15 ~ S, data = TWBState) 
summary(S15)
NE15 <- lm(TWB15 ~ NE, data = TWBState) 
summary(NE15)
WC15 <- lm(TWB15 ~ WC, data = TWBState) 
summary(WC15)
W15 <- lm(TWB15 ~ W, data = TWBState) 
summary(W15)
MW15 <- lm(TWB15 ~ MW, data = TWBState) 
summary(MW15)
AC15 <- lm(TWB15 ~ AC, data = TWBState) 
summary(AC15)

### Mult regression analysis TWB by region
## AC is the control group.

RTWB21 <- lm(TWB21 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB21)
RTWB20 <- lm(TWB20 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB20)
RTWB19 <- lm(TWB19 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB19)
RTWB18 <- lm(TWB18 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB18)
RTWB17 <- lm(TWB17 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB17)
RTWB16 <- lm(TWB16 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB16)
RTWB15 <- lm(TWB15 ~ S + NE + WC + W + MW, data = TWBState)
summary(RTWB15)
