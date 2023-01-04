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
dat <- read.csv("../data/IAT_test.csv", header=T)
### Collapse  IAT data down ####
dat$compatible.crit <- combineIATfourblocks(dat$Q4.RP4, dat$Q18.LP4, dat$Q14.RN7, dat$Q28.LN7)
dat$incompatible.crit <- combineIATfourblocks(dat$Q7.RP7, dat$Q21.LP7, dat$Q11.RN4, dat$Q25.LN4)

### Collapse  IAT practice blocks ####
dat$compatible.prac<- combineIATfourblocks(dat$Q3.RP3, dat$Q17.LP3, dat$Q13.RN6, dat$Q27.LN6)
dat$incompatible.prac <- combineIATfourblocks(dat$Q6.RP6, dat$Q20.LP6, dat$Q10.RN3, dat$Q24.LN3)

### Clean the IAT ### 
clean <- cleanIAT(prac1=dat$compatible.prac, 
                  crit1=dat$compatible.crit, 
                  prac2=dat$incompatible.prac, 
                  crit2=dat$incompatible.crit, 
                  
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
sum(!clean$skipped)

### TIMEOUT DROP RATE (% of TRIALS dropped because they exceed 10s) ###
clean$timeout.rate

### FAST PARTICIPANT 'BUTTON MASHER' DROP COUNT AND RATE (% of SAMPLE dropped as they were too fast) ###
clean$fastprt.count
clean$fastprt.rate
# clean$drop.participant # gives the T/F vector of whether each respondent is dropped due to rushing

### ERROR RATE (typically < 10%) ###
clean$error.rate 
# clean$error.rate.prt # error rate for each respondent
# Error rates for each block:
clean$error.rate.prac1
clean$error.rate.crit1
clean$error.rate.prac2
clean$error.rate.crit2

### RELIABILITY ANALYSIS (cf. De Houwer and De Bruycker, 2007) ###
IATreliability(clean)$reliability
IATalpha(clean)$alpha.total # more crude estimate using Cronbach alpha, cf. https://github.com/iatgen/iatgen

# place back into dat
dat$D <- clean$D

#### test for IAT effect ####
mean(clean$D, na.rm=T)
sd(clean$D, na.rm=T)
t.test(clean$D)
mean(clean$D, na.rm=T) / sd(clean$D, na.rm=T) #cohen d

# Density plot of the D score
ggplot(dat, aes(x=D)) + geom_density(color="black", fill="light blue") + theme_light()
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