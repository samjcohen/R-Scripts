##FOR 131 Lab Week 7 
##Introduction to R Coding
##
##R is free programming software for statistical computing and graphics
##
##Using RStudio script is beneficial to write code without running
##or to run bit by bit; also make notes by using "#"
#
##click the left hand side of the code in the script
##then press 'shift'+'down' to select the whole line of code
##'control'+'enter' to run the code

##Step One: Import and Basic Stats
##OPTION 1: Click 'import dataset' on right under 'Environment'
##You can also write code in order to upload a dataset, this is more professional
##because code allows you to modify how you upload your data
##for smaller datsets, such as this class, it works fine to prepare our data in Excel
##OPTION 2: Run code below; it's the same thing
install.packages("tidyverse")
#SAY NO TO COMPILATION
library (readxl)
malel <- read_excel(file.choose())
View(malel)

#Note: we create objects by using "=" or "<-"
#shortcut: 'alt' + '-' creates "<-"
two <- 2
two*2
series <- c(1,2,3)
series*2
#objects allow us to easily maniupulate large/complex sets of data
#let's try something using our data, 'data$column' selects one column
aspect <-na.omit(malel$ASPE)
mean(aspect)
sd(aspect)
median(aspect)
#notice the diff between median and mean; is this data skewed left or right?
#check out your guess by running the below code
boxplot(aspect)
summary(aspect)

################################MAKING FIGURES################################

##ONE: Boxplots
##boxplot(y~x, data=)
boxplot(DIST~HERB, data=malel)
##We see boxplots for ALL herb species
attach(malel)
boxplot(DIST~HERB)
##attach() makes it so we do not need to specify our dataset each time now
##if we want boxplots for only a few species, manipulate data in Excel
##this is acceptable for now since the dataset is relatively small

##TWO: Scatterplots
#plot(y~x)
plot(SOIL~LITT)
#lm() means linear model, (y~x)
model <- lm(SOIL~LITT)
#abline ("A" "B" Line) fits the lm onto the plot
abline(model)
#What if want to separate these values by SOIL texture?
s_SOIL <- split(SOIL,SOTX)
s_LITT <- split(LITT,SOTX)
#Check out what this did to our data...
s_SOIL
#Now we can call each texture individually by
s_SOIL[[1]]
s_SOIL[["SALO"]]
#Now we're ready to replot
plot(SOIL~LITT, type="n")
#points are (x,y, col=color, pch=character)
points(s_SOIL[[1]],s_LITT[[1]],col="red",pch=16)
points(s_SOIL[[2]],s_LITT[[2]],col="blue",pch=16)
points(s_SOIL[[3]],s_LITT[[3]],col="green",pch=16)
#Do they look different? Let's fine out
model1 <- lm(s_LITT[[1]]~s_SOIL[[1]])
model2 <- lm(s_LITT[[2]]~s_SOIL[[2]])
model3 <- lm(s_LITT[[3]]~s_SOIL[[3]])
abline(model1,col="red")
abline(model2,col="blue")
abline(model3,col="green")
#seems like maybe they are!

##THREE: Making Figures Pretty
##Axis titles
plot(SOIL~LITT, type="n", xlab="Litter Depth (cm)", ylab = "Soil Depth (cm)")
##Four ways to edit how our graphics look
#default
points(s_SOIL[[1]],s_LITT[[1]])
#color can be "name" or number
points(s_SOIL[[2]],s_LITT[[2]],col="red")
points(s_SOIL[[3]],s_LITT[[3]],col=12)
#pch is character; 16 is a solid dot which is usually ideal
points(s_SOIL[[2]],s_LITT[[2]],pch=2)
points(s_SOIL[[3]],s_LITT[[3]],pch=16)
#cex is size
points(s_SOIL[[2]],s_LITT[[2]],cex=2)
points(s_SOIL[[3]],s_LITT[[3]],cex=3)
#lty is line type
abline(model2,lty=1)
abline(model3,lty=2)
##For all of the above, standard is always 1
##You can easily look up all of these online to more customization
##Last but not least, Legend. Rerun our practice scatterplot.
legend(locator(1),c("LOSA","SALO","SAND"),pch=16,col=c("red","blue","green"))
#locator(1) places the top left corner of the legend where you click
#c() lists the groups
#make sure col=c() lists colors in same order as groups

##############################Statistical Tests###############################

##ONE: Linear Regression
#fit a linear model
lm(SOIL~LITT)
summary(model)

##TWO: T-test (between two categories)
#IN EXCEL...
#We need two columns, name of the column is the treatment (Category)
#each value in the column would be the distribution of values (SOIL/LTT)
#IN R... We already split them 
#but let's make the names nicer
LOSA <- s_SOIL[[1]]
SALO <- s_SOIL[[2]]
#now we can test the difference in soil depth between these two textures
boxplot(LOSA,SALO)
#are the variances equal?
t.test(LOSA,SALO)
t.test(LOSA,SALO, var.equal=T)
#FOR (ALMOST) ANY FUNCTION IN R YOU CAN SEE INFO BY DOING ?FXN()
?t.test()
t.test(LOSA,SALO, alternative=c("greater"), var.equal=T)
#What is our conclusion?

##THREE: ANOVA (>2 categories)
#aov(y~x) 
anova <- aov(SOIL~SOTX)
summary(anova)
#pr(>F) is our p-value, are these distributions significantly different?

##FOUR: Chi Squared Test
chisq.test(HELA,CALA)