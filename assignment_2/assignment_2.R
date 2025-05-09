#We first generate the data as specified in the instructions.

sd <- sqrt(20)
sd

L <- rnorm(1000, mean = 50, sd = sd)
L

summary(L)

sd_two <- sqrt(15)
eps_1 <- rnorm(1000, mean = 0, sd = sd_two)
eps_1

Y_1 <- L + eps_1
Y_1

summary(Y_1)

eps_2 <- rnorm(1000, mean = 0, sd = sd_two)
eps_2

Y_2 <- L + eps_2
Y_2

summary(Y_2)

#We will separately consider the following three treatments:

#Treatment 1: Half the students are assigned randomly to take the course
#independently of Y_1

#Treatment 2: Students are assigned to take the course if their grade on Y_1
#is below a certain cutoff. We use the median of Y_1 as a cutoff to ensure that
#half the students take the course and half do not.

#Treatment 3: Students choose to take the course if their value of L is below a
#certain cutoff. Like in the previous treatment, we use the median of L as a
#cutoff to ensure that half the students take the course and half do not.


