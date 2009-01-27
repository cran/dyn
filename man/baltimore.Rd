\name{baltimore}
\alias{baltimore}
\docType{data}
\title{Baltimore energy data}
\description{
  Heating costs for a home in Baltimore.
}
\usage{data(baltimore)}
\format{
  The data set is a zoo series with a "Date" class time index
  and 6 numeric numeric columns.
\tabular{ll}{
start\tab date of start of billing period\cr
therms\tab integer number of therms used in this billing period\cr
gas\tab Total cost of gas (including delivery and commodity charges) for
natural gas\cr
KWHs\tab integer number of KWH used in this billing period\cr
elect\tab Total cost of electricity (including delivery and commodity
charges)\cr
temp\tab average daily outdoor temperature in degrees Fahrenheit, as
printed on the bill\cr
days\tab number of days in billing period.
}



}
\details{

Heating system is a 10-15 year old natural gas steam boiler supplying
iron radiators. Hot water heater, clothes dryer and stove and oven are
also natural gas. Air conditioning is by various numbers of window
units. If surface area of house is desired, I can add this at a later
time.

Some interesting points in time:

\tabular{ll}{
22-Apr-04\tab Date when house was upgraded 2 failed, older storm windows to more
modern ones.\cr
1-Sep-04\tab Date when house was upgraded 4 failed, older storm windows to more
modern ones. Interesting question: Did upgrading the windows
significantly change the heat loss?\cr
last week of July 1999\tab Spouse moved in; both adults absent during the
work day, setback thermostat used. Interesting question: Is there a
discernable difference in the energy costs for heating between a single
person and a couple? What's the heating "cost" of adding a spouse or
roommate?\cr
18-Dec-2005\tab Brought home son; spouse and son home during the day,
setback thermostat no longer used. Interesting question: What's the
"cost" of adding a child?\cr
}


}
\source{
Zembower, Kevin, 
\url{https://www.stat.math.ethz.ch/pipermail/r-help/2006-October/115499.html
} and correction of one documentation item in \url{https://www.stat.math.ethz.ch/pipermail/r-help/2006-October/115632.html}.
}
\examples{
library(lattice)
data(baltimore)
xyplot(baltimore)
cor(baltimore)
xyplot(elect + gas ~ temp, 
	data = as.data.frame(baltimore), pch = 20, auto.key = TRUE)
}
\keyword{datasets}
