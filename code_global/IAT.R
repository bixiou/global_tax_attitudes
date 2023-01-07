# TODO: read Carpenter et al. (19) before use
# Takes 5-6 min and requires a computer (with keyboard)
# Specify beforehand which policies are national and which ones are global
writeIATfull(IATname="global_policy",
             posname="Good", # Fair
             negname="Bad", # Unfair
             Aname="Global",
             Bname="National",
             catType="words",
             poswords = c("Good", "Equitable", "Appropriate", "Fair", "Positive", "Useful"), # 15 syllabs in each c("Fair", "Equitable", "Honest", "Just", "Positive", "Valid") from Carbon tax acceptance in a polarized society: Bridging the partisan divide over climate policy in the US (22), see also https://osf.io/mjxv9/?view_only=27d2c3fc8b4c4b3fb1bc672cb630b5da for details
             negwords = c("Bad", "Unsound", "Inappropriate", "Unfair", "Negative", "Harmful"), # c("Unfair", "Unbalanced", "Costly", "Improper", "Negative", "Bad")
             tgtType="words", 
             Awords = c("Global wealth tax", "Foreign aid", "Overseas Development Assistance", "Global climate program", "Global basic income", "Paris agreement"), # 33 syllabs in each # Carbon tax, climate tax, ecological tax, energy tax, environmental tax, pollution tax
             Bwords = c("Federal wealth tax", "Food stamps", # now renamed Supplemental Nutrition Assistance Program 
                        "Medicare / Medicaid", "National carbon fee and divdend", "Earned Income Tax Credit", "Clean Power Plan"), # Consumption tax, sales tax, gross receipt tax, value added tax, use tax, excise tax
             
             #advanced options with recommended IAT settings
             n=c(20, 20, 20, 40, 40, 20, 40), # duration of each block
             qsf=TRUE, # creates .qsf
             note=TRUE, # displays keys shortcuts
             correct.error=TRUE, # waits for correct answer
             pause=250, # duration between trials in milliseconds
             errorpause=300, #not used if correct.error=TRUE
             tgtCol="black",
             catCol="green", 
             norepeat=FALSE # not so important
)

# In the example code below we use correct.error=FALSE hence algorithm D600
# dat <- read.csv("../data/US2_pilot.csv", header=T)
### Collapse  IAT data down ####
# e$compatible.crit <- combineIATfourblocks(e$Q4.RP4, e$Q18.LP4, e$Q14.RN7, e$Q28.LN7)
# e$incompatible.crit <- combineIATfourblocks(e$Q7.RP7, e$Q21.LP7, e$Q11.RN4, e$Q25.LN4)
# ### Collapse  IAT practice blocks ####
# e$compatible.prac<- combineIATfourblocks(e$Q3.RP3, e$Q17.LP3, e$Q13.RN6, e$Q27.LN6)
# e$incompatible.prac <- combineIATfourblocks(e$Q6.RP6, e$Q20.LP6, e$Q10.RN3, e$Q24.LN3)

e <- us2p

e$compatible.crit <- combineIATfourblocks(e$iat_rp4, e$iat_lp4, e$iat_rn7, e$iat_ln7)
e$incompatible.crit <- combineIATfourblocks(e$iat_rp7, e$iat_lp7, e$iat_rn4, e$iat_ln4)
### Collapse  IAT practice blocks ####
e$compatible.prac<- combineIATfourblocks(e$iat_rp3, e$iat_lp3, e$iat_rn6, e$iat_ln6)
e$incompatible.prac <- combineIATfourblocks(e$iat_rp6, e$iat_lp6, e$iat_rn3, e$iat_ln3)

### Clean the IAT ### 
clean <- cleanIAT(prac1=e$compatible.prac, 
                  crit1=e$compatible.crit, 
                  prac2=e$incompatible.prac, 
                  crit2=e$incompatible.crit, 
                  
                  timeout.drop=TRUE, 
                  timeout.ms=10000, 
                  
                  fasttrial.drop=FALSE, 
                  
                  fastprt.drop=TRUE, # Fast participants (instead of fast trials) are dropped as they make many mistakes
                  fastprt.percent=.10, 
                  fastprt.ms=300, 
                  
                  error.penalty=TRUE, # This should be FALSE in case correct.error=TRUE (we'd use the D-built.in.error.penalty algorithm)
                  error.penalty.ms=600 # An alternative is "2SD", recommended by Greenwald et al. (2003)
                  )

### NUMBER OF PARTICIPANTS WHO COMPLETED THE IAT ###
sum(!clean$skipped) # 37
decrit(e$branch_iat) # 51

### TIMEOUT DROP RATE (% of TRIALS dropped because they exceed 10s) ###
clean$timeout.rate

### FAST PARTICIPANT 'BUTTON MASHER' DROP COUNT AND RATE (% of SAMPLE dropped as they were too fast) ###
clean$fastprt.count
clean$fastprt.rate
# clean$drop.participant # gives the T/F vector of whether each respondent is dropped due to rushing

### ERROR RATE (typically < 10%) ###
clean$error.rate # 16%
# clean$error.rate.prt # error rate for each respondent
# Error rates for each block:
clean$error.rate.prac1
clean$error.rate.crit1
clean$error.rate.prac2
clean$error.rate.crit2

### RELIABILITY ANALYSIS (cf. De Houwer and De Bruycker, 2007) ###
IATreliability(clean)$reliability
IATalpha(clean)$alpha.total # more crude estimate using Cronbach alpha, cf. https://github.com/iatgen/iatgen # I guess we can just ignore the warning, it's what they do here: https://github.com/iatgen/iatgen/blob/master/README.md

# place back into dat
e$D <- clean$D

#### test for IAT effect ####
# D > 0 <=> respondent was faster in the compatible block i.e. A with pos i.e. global with good
mean(e$D, na.rm=T) # -.44***
sd(e$D, na.rm=T)
t.test(e$D)
mean(e$D, na.rm=T) / sd(e$D, na.rm=T) #cohen d: -.94
datasummary(D ~ Mean * branch_iat, data = e) # LN: -.27 / RN: .-40 / RP: -.61 / LP: bug (due to typo in Qualtrics)
mean(e$D[e$left_right < 3], na.rm=T) # -.41
mean(e$D[e$gcs_support > 0], na.rm=T) # -.30
mean(e$D[e$debt_cancellation_support > 0], na.rm=T) # -.52
mean(e$D[e$foreign_aid_more_less == "More"], na.rm=T) # -.31

# Density plot of the D score
ggplot(e, aes(x=D)) + geom_density(color="black", fill="light blue") + theme_light() # mostly negative
# write.csv(clean$D, "iat_results.csv") # these D-scores can be correlated with other measures

### RT DESCRIPTIVES BY BLOCK
mean(clean$clean.means.crit1, na.rm=T) 
mean(clean$clean.means.crit2, na.rm=T)
mean(clean$clean.means.prac1, na.rm=T)
mean(clean$clean.means.prac2, na.rm=T)
sd(clean$clean.means.crit1, na.rm=T)
sd(clean$clean.means.crit2, na.rm=T)
sd(clean$clean.means.prac1, na.rm=T)
sd(clean$clean.means.prac2, na.rm=T)

# RP/LP: Right/Left + Positive = Compatible first (i.e. global+fair) / RN/LN: ...Negative = Incompatible first