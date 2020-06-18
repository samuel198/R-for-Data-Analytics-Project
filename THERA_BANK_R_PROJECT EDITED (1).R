getwd()
setwd("C:\\Users\\User\\Desktop\\SAMMR")
install.packages("readxl")
library(readxl)
data_bank=read_excel("r_bankproject.xlsx")
View(data_bank)
str(data_bank)
head(data_bank)#library(plyr)
as.factor(data_bank$PERSONAL_LOAN)
as.factor(data_bank$SECURITIES_ACCT)
as.factor(data_bank$ONLINE)
as.factor(data_bank$CREDIT_CARD)
as.factor(data_bank$`CERT_OF_DEP_ ACCT`)
summary(data_bank)
dim(data_bank)
names(data_bank)
#1.distribution of personal loans to customers (1)means have p.loan,(0)means no p.loan 
table(data_bank$ PERSONAL_LOAN)
counts <- table(data_bank$PERSONAL_LOAN)
barplot(counts, main="PERSONAL LOAN DISTRIBUTION BY CUSTOMERS",
        xlab="PERSONAL LOAN",col=c("darkblue","red"),
        legend = rownames(counts))
#The total numberCustomers with personal loans were 479 out of 4999 (1) while those without were 4520 of the total 4999 (0.


#2.distribution of sequrities account to customers (1)means have s.account,(0)means no s.account
table(data_bank$SECURITIES_ACCT)
counts <- table(data_bank$SECURITIES_ACCT)
barplot(counts, main="CUSTOMER DISTRIBUTION BY SECURITIES_ACCT",
        xlab="SECURITIES ACCOUNT",col=c("YELLOW","GREEN"),
        legend = rownames(counts))
#The total number of customers with Securities account were 522 of 4999 while without were 4477 of 4999

#3.distribution of CREDIT CARD to customers (1)means have CC.account,(0)means no CC.account.
table(data_bank$CREDIT_CARD)
counts <- table(data_bank$CREDIT_CARD)
barplot(counts, main="ACCESS TO CREDIT CARD",
        xlab="CREDIT_CARD",col=c("YELLOW","GREEN"),
        legend = rownames(counts))

#4.customers with online accounts (1)means have online account,(0)means no online account.
table(data_bank$ONLINE)
counts <- table(data_bank$ONLINE)
barplot(counts, main="ACCESS TO ONLINE ACCOUNT",
        xlab="ONLINE",col=c("BLUE","BLACK"),
        legend = rownames(counts))

#5.Personal loan distribution by family ,where 1 = one family member,2=two family members,3=three fam members,4 =four fam members) 
addmargins(xtabs(~PERSONAL_LOAN+FAMILY,data=data_bank))
densityplot(~ PERSONAL_LOAN, groups = FAMILY, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)

#6.income distribution by education,where(1=undergraduate,2=graduate,3=post grad/proffessional.
densityplot(~ INCOME, groups = EDUCATION, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)

#7.Income distribution and online(0=no online account,1=have online account)
densityplot(~ INCOME, groups =  ONLINE, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)

#8.Age and online  banking
densityplot(~ AGE, groups =  ONLINE, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)

#9.Distribution of customers by experience
plot(prop.table(table(data_bank$EXPERIENCE)))

#10.income vs credit use
densityplot(~ INCOME, groups =  CREDIT_CARD, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)

#income vs family size.
densityplot(~ INCOME, groups = FAMILY, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)

densityplot(~ AGE, groups =  CREDIT_CARD, data = data_bank,
            plot.points = FALSE, auto.key = TRUE)
plot(prop.table(table(data_bank$CREDIT_CARD)))








