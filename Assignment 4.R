#Q1
airpollt <- read.table(
  file = "https://people.stat.sc.edu/Hitchcock/airpoll.txt",
  header = TRUE
)
airpoll <- data.frame(airpollt[,-1],row.names = airpollt[,1])
head(airpoll)
#Q2
symbols(airpoll$SO2,airpoll$Mortality,circles = airpoll$Popden,inches=0.1,
        bg="blue",xlab = "SO2", ylab = "Mortality rate", 
        main = "Plotting mortality rate against different amt of SO2 in air in different population density regions")
'''The objective for this plot is to see how mortality rate is affected
by SO2 in air and how they differ in different population density regions. So took
SO2 as the independent variable and population density as bubble size, larger the
area,larger the density. We can conclude from the graph that as amt of SO2 increases,
 mortality also increases. We can also see that population density increases
 with SO2 till a certain level and then decreases indicating large levels of SO2
do not make regions habitable.We can account density increasing with SO2 to the 
increaed usage of vehicles in cities and towns where density is more and
also SO2 level is more due to car emissions. Mortality rate also increases
with pop density and then again decreases indicating people dont live in such regions.'''
#Q3
x<-(-4:4)
df1<-as.data.frame(cbind(x,(-1*x + 9)))
ggparcoord(df1, columns = c(1:2))
df2<-as.data.frame(cbind(x,(2*x + pi)))
ggparcoord(df2, columns = c(1:2))
'''If they are strongly correlated positively, all the lines are parallel. if correlation
is negative then, the lines would be concurrent.'''
#Q4
'''To understand the strong correlation between the two variables, the lines
between the two successive points should be parallel or concurrent. If we dont keep those
variables successively, it wont be possible to figure out the parallel
nature of those lines or concurrency. So to make it least suitable, put the 3rd variable in between
the two highly correlated variables'''
#Q5
cor(iris[,-5])
'''Seeing the correlation between different variables, I have kept variables with
high (absolute) correlation together.'''
ggparcoord(iris, columns = c(2,4,3,1), groupColumn = 5)
'''We can see for setosa, the sepal widths are greater than the petal width,
vice-versa for others. Setosa has shorter petal lengths and widths than the other
two by a large margin while versicolor and virginica are close to each other. However
the sepal lengths show a continuous graduation the order being viriginca, versicolor
and setosa. Overall we can conclude versicolor and virginica are similar in features
while setosa stands out from them'''
#Q6

#Q7
plot(iris$Petal.Width,iris$Sepal.Length,type="n")
points(iris$Petal.Width[iris$Species=="setosa"],iris$Sepal.Length[iris$Species=="setosa"],col="pink")
points(iris$Petal.Width[iris$Species=="versicolor"],iris$Sepal.Length[iris$Species=="versicolor"],col="green")
points(iris$Petal.Width[iris$Species=="virginica"],iris$Sepal.Length[iris$Species=="virginica"],col="blue")
#Q8
plot(iris$Petal.Width,iris$Sepal.Length,type="n")
points(iris$Petal.Width[iris$Petal.Length>4.55],iris$Sepal.Length[iris$Petal.Length>4.55],col="green")
points(iris$Petal.Width[iris$Petal.Length<4.55],iris$Sepal.Length[iris$Petal.Length<4.55],col="red")
#Q9
boxplot(iris$Sepal.Width~iris$Species)
#Q10
centered_sepal_width<-(iris$Sepal.Width-mean(iris$Sepal.Width))
#Q11
sepal_width_var<-mean(centered_sepal_width^2)
#Q12
irc <- as.matrix(t(iris[,-5]) - colMeans(iris[,-5])); diag(irc%*%t(irc))/dim(irc[2])
'''The fist command transposes the iris matrix and then subtracts the mean of each
variable from the individual datapoints. This ensures all the row means of irc are
set to 0. As for the 2nd command, diag command computes the square of data values
for each variable and sums them. However dim(irc[2]) returns NULL so not sure
what it does'''