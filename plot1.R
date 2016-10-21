setwd("~/CursoDS/curso5/week2/RepData_PeerAssessment1")

# install.packages("reshape2")
library(reshape2)

    data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")


data2 <- data[,c("date", "interval", "steps")]
names(data2) <- c("date", "variable", "value")

d <- dcast(data2, date~variable)
d2 <- d[,c(-1)]

coln <- names(d2)

meanAll <- sapply(d2, mean, na.rm=TRUE)

# meanAll2 <- meanAll[c(-1)]

nulos <- sapply(d2,is.na)


for (i in 1:nrow(data)){ 
    if(is.na(data[i,"steps"])){ 
        res <- agFive[agFive$interval == data[i,"interval"],"steps"]
        print(res)
    }
}


generate_index <- function() {
    hr <- c()
    for(i in seq(0,23)) { 
        for (j in seq(0,55, by = 5)) { 
            if (i == 0){ 
                hr <- c(hr,  as.character(j))
            } else { 
                if(j == 5 | j == 0){
                    v <- paste(i,j, sep="0")
                    hr <- c(hr, v)
                } else {
                    v <- paste(i,j, sep="")
                    hr <- c(hr, v)
                }
            }
        }  
    }
    return(hr)
}



minuts_to_hour <- function(v) {
    v <- sprintf("%04d", v)
    v <- gsub("(..)(..)", "\\1:\\2", v)

    return(v)
}


idx <- generate_index()

for (i in idx) {
    col <- d2[,i]
    val <- meanAll[i]
    nulos <- is.na(col)
    col[nulos] <- c(val)
    d[,i] <- col
}

# hasta aqui ya no hay nulos


calc_sum_By_day <- function(){
    fecha <- c()
    suma <- c()
    for(i in 1 : nrow(d)){
        fecha <- c(fecha, as.character(d[i, "date"]))
        s <- sum(d[i,idx])
        suma <- c(suma, s)
    }
    df <- data.frame(fecha=fecha, sum=suma)
}

df <- calc_sum_By_day()

hist(df$sum)

calc_mean_By_day <- function(){
    fecha <- c()
    suma <- c()
    for(i in 1 : nrow(d)){
        fecha <- c(fecha, as.character(d[i, "date"]))
        s <- mean(d[i,idx])
        suma <- c(suma, s)
    }
    df <- data.frame(fecha=fecha, mean=suma)
}

df2 <- calc_mean_By_day()


m <- melt(d, id=c("date"))
aggregate(m$value, by = list(m$date), FUN=mean  )


## ##
## ##

proc_element <- function (x) {
    g <- regexpr("^S.*", x)
    if (g[1] == 1) { 
        hr <- TRUE
    } else { 
        hr <- FALSE
    }
    return(hr)
}

apply(c(dfw$dias), 1, function(x){proc_element(x)})


weekday_type <- function(v) {
    if (v == "Sunday" | v == "Saturday") {
        return("weekend")
    } else {
        return("weekday")
    }
}

r <- unlist(lapply(data2$weekday, weekday_type))


