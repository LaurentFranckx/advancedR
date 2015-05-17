j <- function(x){
  y <- 2
  function(){
    c(x,y)
  }
  
}

j <- function(){
  if(!exists("a")){
    a <- 1
  } else{
    a <- a + 1
  }
  print(a)
}

f <- function(x){
  force(x)
  10
}

add <- function(x){
  function(y) x + y
}

adders <- lapply(1:10, add)

x <- NULL

if(!is.null(x) && x > 0){
  cat("zever")
}


f1 <- function(x = {y <- 1; 2}, y = 0){
  x + y
}

f2 <- function(x=z){
  z <- 100
  x  
}

base_list <- ls(search()[10])
#base_list <- ls(search()["package:base"])
base_generic <- grep("default", base_list, value = TRUE)
base_generic_names <- sapply(base_generic, gsub, pattern= "\\.default", replacement = "" )
base_generic_names2 <- paste("^",base_generic_names,"\\.",sep="")
base_method_names <- lapply(base_generic_names2, grep, x = base_list, value = TRUE)
base_method_count <- sapply(base_method_names, length)
base_method_max <- (base_method_count == max(base_method_count))
base_method_names[base_method_max]


y <- 1
g <- function(x){
  y <- 2
  UseMethod("g")
}
g.numeric <- function(x) y
g(10)

h <- function(x){
  x <- 10
  UseMethod("h")
}
h.character <- function(x) paste("char", x)
h.numeric <- function(x) paste("num",x)
h("a")
h(5)


f <- function() 1
g <- function() 2

class(g) <- "function"

length.function <- function(x) "function"
length(f)
length(g)


listofS4methods <- lapply(getGenerics(),showMethods)

my_env <- new.env(parent = emptyenv())
my_env$a <- 1

get_a <- function(){
  my_env$a
  
}

set_a <- function(value){
  old <- my_env$a
  my_env$a <- value
  invisible(old) 
}


whatisminusone <- function(){
  as.environment(-1)  
}

searchlist <- c(".GlobalEnv")


search_lf <- function(env = as.environment(1)){ 
  if (identical(env,emptyenv())){
    searchlist <- c(searchlist, attr(env,"name"))
    return(searchlist)
  } else { 
    print(searchlist)
    searchlist <- c(searchlist, attr(env,"name"))
    print(searchlist)
    search_lf(parent.env(env))
  }    
}

#search_lf2 <- function(env = parent.env(as.environment(1)), searchlist = parent.frame()){  
search_lf2 <- function(env = parent.env(as.environment(1)), searchlist = parent.frame()){  
#search_lf2 <- function(env = parent.env(as.environment(1)), searchlist = ".Globalenv"){  
  if (identical(parent.env(env),emptyenv())){    
    searchlist <-  c(searchlist, "package:base")
    return(searchlist)
  } else { 
#    if()
    searchlist <-  c(searchlist, attr(env,"name"))
    search_lf2(parent.env(env), searchlist = searchlist)
  }    
}

search_lf3 <- function(env = environment(), searchlist = NULL){  
  #search_lf2 <- function(env = parent.env(as.environment(1)), searchlist = ".Globalenv"){  
  if (identical(env,baseenv())){    
    searchlist <-  c(searchlist, "package:base")
    if(!identical(parent.env(env),emptyenv())) stop ("The parent of the base environment is not the empty environment. ")
    return(searchlist)
  } else { 
    if(identical(globalenv(),parent.frame(1))) {
      searchlist <- ".GlobalEnv"} else {
        searchlist <-  c(searchlist, attr(env,"name"))
      }  
    search_lf3(parent.env(env), searchlist = searchlist)
  }    
}


get_lfr <- function(name, env = parent.frame(), inheritval = TRUE){
  if(identical(env,emptyenv())){
    stop("Cannot find" ,name)
  } else if (exists(name, envir = env, inherits = inheritval)){
    browser() 
    print(name)
 #     return(name)
    } else {
      get_lfr(name, parent.env(env))
    }
  
}


get_lfr2 <- function(name, env = parent.frame(), inheritval = TRUE){
  if(identical(env,emptyenv())){
    stop("Cannot find" ,name)
  } else if (exists(paste(name), envir = env, inherits = inheritval)){
    browser() 
    print(name)
    #     return(name)
  } else {
    browser()
    get_lfr2(name, parent.env(env))
  }
  
}

x <- c(1,2,"a")


fget <- function(name, env = parent.frame(), inherits= TRUE, foundfunction = FALSE){
  if(identical(env,emptyenv())){
   if(!foundfunction)   stop("Cannot find function. \n")
  } else if(exists(name, envir = env, inherits = FALSE) & class(get(name)) == "function"){
    if (identical(env,baseenv())) {
      cat("Function", name, "found in base environment.\n")
    } else {
      cat("Function", name, "found in environment", attr(env,"name") , ".\n")
    }
    
   # foundfunction <- TRUE
    if(inherits) fget(name, env = parent.env(env), foundfunction = TRUE)
  } else {
    #browser()
    fget(name, env = parent.env(env), foundfunction = foundfunction)
  } 
}



fexists <- function(name, env = parent.frame(), inherits= TRUE, foundobject = FALSE){
  if(identical(env,emptyenv())){
    if(!foundobject)   stop("Cannot find object.  \n")
  } else  if(name %in% ls(env) ){
      if (identical(env,baseenv())) {
        cat("Object", name, "found in base environment.\n")
      } else if (identical(env,globalenv())){
          cat("Object", name, "found in the global environment.\n")
      } else {
          cat("Object", name , "found in environment", attr(env,"name") , ".\n")
      }
            
      # foundfunction <- TRUE
      if(inherits) fexists(name, env = parent.env(env),inherits= TRUE, foundobject = TRUE)
    } else {
      #browser()
      fexists(name, env = parent.env(env), inherits= TRUE, foundobject = foundobject)
    } 
     
}

fexists("data.frame",globalenv())

fexists("data.frame",as.environment("package:stats"))

fexists("plot",as.environment("package:stats"))

strf <- function(x) UseMethod("strf") 

strf.default <- str(x)

strf.function <- function(fun){
  a <- str(fun)
  b <- environment(fun)
  c <- parent.env(environment())
  d <- parent.frame()
  return(list(a,b,c,d ))
}

strf(mean)
  

str(mean)


f_assign <- function(name, value, env = parent.frame()){
  if(identical(env,emptyenv())){
    assign(name,value, globalenv())
  } else  if(name %in% ls(env) ){
    if (identical(env,baseenv())) {
      stop("Object ", name, " already exists in base environment.\n")
    } else if (identical(env,globalenv())){
      stop("Object ", name, " already exists in the global environment.\n")
    } else {
      stop("Object ", name , " already exists in environment", attr(env,"name") , ".\n")
    }
    
    # foundfunction <- TRUE
     } else {
    #browser()
       f_assign(name, value, env = parent.env(env))
  } 
  
}



f_assign("x",15)

f_assign("matrix",15)

f_assign("plot",15)


show_condition <- function(code){
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
           )
 
}

show_condition("zever")
show_condition("zeverwarning")
show_condition("warningzeber")
show_condition(warning("xsddf"))


try2 <- function(code, silent = FALSE){
  tryCatch(code, error = function(c){
    msg <- conditionMessage(c)
    if (!silent) message(c)
    invisible(structure(msg, class = "try-error"))
  
  })
  
}

try2(log(-10))

try2(stop("Hi"))

try2(stop("Hi"), silent = TRUE)


col_means <- function(df){
  if(!is.data.frame(df)) df <- as.data.frame(df)
  if(ncol(df) == 0) stop("No columns supplied. ")
  if(nrow(df) == 0) stop("No rows supplied. ")
  numeric <- sapply(df,is.numeric)
  if(sum(numeric)== 0) stop("No numeric columns. ")
  if(sum(numeric)== 1) return(mean(df[, numeric]))
  mumeric_cols <- df[, numeric]
  data.frame(lapply(mumeric_cols, mean))
}



col_means(mtcars[, 0])
col_means(mtcars[0, ])
col_means(mtcars[, "mpg" , drop = F])
col_means(1:10)
col_means(as.matrix(mtcars))
col_means(as.list(mtcars))


mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means(mtcars2)

lag <- function(x, n = 1L){
  if(!is.vector(x)) stop("input is not a  vector")
  if(n < 0) stop("n must be at least one")
  if(n > length(x)) stop("n cannot exceed the vector's length")
  xlen <- length(x)
  c(rep(NA,n), x[seq_len(xlen-n )])
    
}

lapply(mtcars, function(x) sd(x)/mean(x))

integrate(function(x) x^2 -x, 0 ,10)

integrate(function(x) sin(x) + cos(x), -pi , pi)


moment <- function(x){
  return(function(y) 1/length(y) * sum((y - mean(y))^x))
}

m1 <- moment(1)

x <- runif(100)
all.equal(m1(x),0)

m2 <- moment(2)
all.equal(m2(x), var(x) * 99/100)


pick <- function(i){
  return(function(x) x[[i]])  
}


sapply(mtcars, pick(5))
sapply(mtcars, function(x) x[[5]])


simple_tag <- function(tag){
  force(tag)
  function(...){
    paste0("<", tag, ">", paste0(...), "</", tag, ">")
  }
  
}

tags <- c("p","b","i")
html <- lapply(setNames(tags,tags), simple_tag)

with(html, p("This is ", b("bold"), "text."))


list2env(html,environment(ls))



newton_cotes <- function(coef, open = FALSE){
  n <- length(coef) + open
  function(f, a, b){
   pos <- function(i) a + i * (b-a)/n
   points <- pos(seq.int(0,length(coef)-1))
   (b - a) /sum(coef) * sum(f(points) * coef)
    
  }
}


boole <- newton_cotes(c(7,32,12,32,7))


composite <- function(f1, a, b, n , rule){
  points <- seq(a,b, length = n + 1)
  area <- 0
  for(i in seq_len(n)){
    area <- area + rule(f1,   points[i], points[i + 1])  
  }
  area 
}


midpoint <- function(f1, a, b){
 (b-a) * f1((a+b)/2)
  
}

trapezoid <- function(f, a, b){
  (b-a) /2 * (f(a) + f(b))
}

sin_sq_inv <- function(x) sin(1/x^2)

composite(sin,  0.001, pi, 10, midpoint)

composite(sin,  0, pi, 10, midpoint)


n <- 1

nval <- seq(499900,500000)
intval <- numeric(length(nval))

for(n in seq_along(nval)){  
  intval[n] <- composite(sin_sq_inv,  0.00001, pi, n, midpoint)
}


plot(nval, intval)

x <-  seq(0, pi, length.out = 2000)

plot(x , y = sin_sq_inv(x))


while(abs(composite(sin_sq_inv,  0.00001, pi, n, midpoint) - composite(sin_sq_inv,  0.00001, pi, n, trapezoid)/composite(sin_sq_inv,  0.001, pi, n, midpoint)) > 0.0001)  {
  cat("No convergence for n = ", n , ".\n")    
  cat("Relative difference is ", abs(composite(sin_sq_inv,  0.00001, pi, n, midpoint) - composite(sin_sq_inv,  0.00001, pi, n, trapezoid)/composite(sin_sq_inv,  0.00001, pi, n, midpoint)) , ".\n")    
  n <- n + 1
} 


while(abs(composite(sin_sq_inv,  0.05, pi, n, midpoint) - composite(sin_sq_inv,  0.05, pi, n, trapezoid)/composite(sin_sq_inv,  0.001, pi, n, midpoint)) > 0.0001)  {
  if(n %% 100 == 0){
    cat("No convergence for n = ", n , ".\n")    
    cat("Relative difference is ", abs(composite(sin_sq_inv,  0.00001, pi, n, midpoint) - composite(sin_sq_inv,  0.00001, pi, n, trapezoid)/composite(sin_sq_inv,  0.00001, pi, n, midpoint)) , ".\n")    
  }
  n <- n + 1
} 


l <- replicate(20, runif(sample(1:10,1)), simplify = FALSE)

formulas <- list(
  mpg  ~ disp,
  mpg  ~ I(1/disp),
  mpg  ~ disp + wt,
  mpg  ~ I(1/disp) + wt 
  )


resulst <- lapply(formulas, lm, data = mtcars)
lapply(resulst, summary)


bootstraps <- lapply(1:10, function(i){
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

boot_results <- lapply(bootstraps, function(x) lm( mpg  ~ disp, data = x  ))


boot_r2 <- lapply(boot_results, function(x) summary(x)$r.squared)

num_mt_cars <- vapply(mtcars,is.numeric,logical(1))
sd_mtcars <- vapply(mtcars[, num_mt_cars],sd,numeric(1))

trials <- replicate(
100,
t.test(rpois(10,10),rpois(7,10)), 
simplify = FALSE
)

#trials_summary <- lapply(trials, summary)

trials_rsq <- vapply(trials, "[[", numeric(1), "p.value")


a <- matrix(1:20,nrow = 5)
a1 <- apply(a,1,identity)


x <- matrix(rnorm(20,0,10), nrow =4)

apply(x,1,min)

x1 <- sweep(x, 1, apply(x,1,min), '-')
x2 <- sweep(x1, 1, apply(x,1,max), '/')



pulse <- round(rnorm(22,70,10/3) ) + rep(c(0,5), c(10,12))

group <- rep(c("A","B"),c(10,12))
tapply(pulse,group, length)


tapply_LFR <- function(x, group, f, type, arg_length, usenames = TRUE, ...){
  pieces <- split(x, group)
  vapply(X= pieces, FUN= f, FUN.VALUE = vector(type,arg_length), USE.NAMES = usenames)
  
}

tapply_LFR(x = pulse,group, f = length, type = "numeric", arg_length = 1)


tapply_LFR <- function(x, group, f, type, usenames = TRUE, ...){
  pieces <- split(x, group)
  vapply(X= pieces, FUN= f, FUN.VALUE = type, USE.NAMES = usenames)
  
}

tapply_LFR(x = pulse,group, f = length, numeric(1))


split_LFR <- function (y, group){
  group_args <- unique(group)
  y <- data.frame(y,group)
  y2 <- lapply(group_args, function(x) y[y[,2] == x, 1] )
  names(y2) <- group_args
  return(y2)
} 

split_LFR(pulse,group)

df <- data.frame(x= 1:3, y = c("a","b","c"), z = c("d","e","f"))
Filter(is.factor,df)
Find(is.factor,df)


Any_lfr <- function(in_list, pred_fun){
  results <- Find(pred_fun, in_list)
  if(is.null(results)) FALSE else TRUE
}


Any_lfr(df, is.factor)
Any_lfr(mtcars, is.factor)
Any_lfr(mtcars, is.numeric)
Any_lfr(strf(mean), is.function)
Any_lfr(strf(mean), is.environment)


All_lfr <- function(in_list, pred_fun){
  results <- Filter(pred_fun, in_list)
  if(length(results) == length(in_list)) TRUE else FALSE
}


All_lfr(df, is.factor)
All_lfr(mtcars, is.factor)
All_lfr(mtcars, is.numeric)
All_lfr(strf(mean), is.function)
All_lfr(strf(mean), is.environment)


span <- function(in_list, pred_fun){
  pred_fun_true <- vapply(in_list, pred_fun, logical(1) )
  pred_fun_true_l <- rle(pred_fun_true)$lengths
  pred_fun_true_v <- rle(pred_fun_true)$values
  pred_fun_true_v_true <- pred_fun_true_v[pred_fun_true_v == TRUE]
  pred_fun_true_l_true <- pred_fun_true_l[pred_fun_true_v == TRUE]
  pred_fun_true_v_false <- pred_fun_true_v[pred_fun_true_v == FALSE]
  pred_fun_true_l_false <- pred_fun_true_l[pred_fun_true_v == FALSE]
  
  if(length(pred_fun_true_l_true) > 0){
    pred_fun_true_l_max <- pred_fun_true_l[pred_fun_true_l == max(pred_fun_true_l_true)]
    if(length(pred_fun_true_v_false) > 0) {
      pred_fun_false_l_max <- pred_fun_true_l[pred_fun_true_l == max(pred_fun_true_l_false)]
    }  else
      {
        pred_fun_false_l_max <- 0
    }
    if((length(pred_fun_true_v_false) == 0) | (pred_fun_true_l_max != pred_fun_false_l_max)) {
      pred_fun_true_l_pos <- match(pred_fun_true_l_max,pred_fun_true_l)
      } else {
      stop()
      }
    #pred_fun_true_l_pos <- match_true(pred_fun_true_l_max,pred_fun_true_l)
    
    end_index <- sum(pred_fun_true_l[1:(pred_fun_true_l_pos)])
    #pred_fun_true_v_max <- pred_fun_true_v[pred_fun_true_l == max(pred_fun_true_l)]
    #if (pred_fun_true_v_max) begin_index <- Position(pred_fun, in_list) else begin_index <- Position(Negate(pred_fun), in_list)
    #end_index <- match(names(pred_fun_true_v_max), names(in_list) )
    begin_index <- end_index - max(pred_fun_true_l_true) + 1
    return(c(begin_index,end_index))  
    
  } else{
    cat("No true values found.")
   # return()
  }
}


match_true <- function(x, table){
  position <- match(x,table)
  if( rle(table)$values[position] == TRUE) return(position) else match_true(x,table[(position +1):length(table)])
}



span(mtcars,is.numeric)
span(df, is.factor)
span(mtcars, is.factor)
span(mtcars, is.numeric)
span(strf(mean), is.function)
span(strf(mean), is.environment)


arg_max <- function(f, x){
  y <- sapply(x, f)
  result <- x[y == max(y)]  
  return(result)
}

arg_max(function(x) x^2, -10:5)

arg_max(function(x) x^2, -5:5)

smaller <- function(x, y, na.rm = FALSE){
  if(na.rm && (is.na(x) || is.na(y))) rm_na(x,y,Inf) else {
    stopifnot(length(x)==1,length(y)==1 , is.numeric(x), is.numeric(y))
    min(x,y)
  }
}

rm_na <- function(x,y,identity){
  if(is.na(x) && is.na(y)) {
    identity
  } else if (is.na(x)) {
    y
  } else {
    x
  }
  

}


smaller(-3,8)
smaller(-3,8, na.rm = TRUE)
smaller(NA,8, na.rm = TRUE)
smaller(-3,NA, na.rm = TRUE)
smaller(NA,NA, na.rm = TRUE)


smaller(8, smaller(NA, NA, na.rm = TRUE), na.rm = TRUE)



smaller_p <- function(x, y, na.rm = FALSE){
    stopifnot(length(x)==length(y), is.numeric(x), is.numeric(y))
    if(length(x) == 0) return(numeric())
    simplify2array(
      Map(function(x,y)  smaller(x,y, na.rm = na.rm)  , x, y)
      )
}

x <- c(1:10)
y <- sample(c(1:10,NA),10, replace = TRUE)

smaller_p(x,y)

smaller_p(x,y, na.rm = TRUE)

smaller_v <- function(xs, na.rm = TRUE){
  Reduce(function(x,y) smaller_p(x,y, na.rm  = na.rm), xs)  
}


smaller_v(x)

row_min <- function(x, na.rm = FALSE){
  apply(x, 1, smaller_v, na.rm = na.rm)
  
}



z <- matrix(sample(1:100,20,replace = TRUE), nrow =4)

row_min(z)


library(memoise)

write_time <- function(f, ...){
  now <- format(Sys.time(), "%a %b %d %X % Y")
  write(now, "D:\\advancedR\\last_exu.txt",append= TRUE)  
  f(...)
}

write_time(mean, c(1:10))

write_time(sample, c(1:10), 20, replace=TRUE)


readLines("last_exu.txt")

delay_by_since <- function(delay,f){
  last_exu <- NULL
  function(...){
    if(!is.null(last_exu)) {
      cat("Last execution at: ", last_exu, ".\n") 
      while(as.numeric(format(Sys.time(),"%s")) < as.numeric(last_exu) + delay ){
       # cat("Sys.time at:" , Sys.time(), ".\n")
      }
    }
    #Sys.sleep(delay)
    result <- f(...)
    last_exu <<- format(Sys.time(),"%s")
    return(result)
    
  }
  
}


delay_10_mean <- delay_by_since(10, mean)
as.list(environment(delay_10_mean))

delay_10_mean(c(1:30))
delay_10_mean(c(100:300))
delay_10_mean(c(1500:30000))
delay_10_mean(c(10000000:300000000))


f <- function(a,b) {
  force(c(a,b))
  function(x) a * x + b
}

lin_0_1 <- f(1,0)

lin_0_1(-5)

lin_0_1(-5:5)


lin_1_1 <- f(1,1)

lin_1_1(-5)

lin_1_1(-5:5)

fs <- Map(f, a = c(0,1), b = c(0,1))


fs[[1]](3)

fs[[2]](3)


negative <- function(f){
  force(f)
  function(...) -f(...)
}

negative(exp)(seq(-1, 1))

setwd("D:\\advancedR")  

file_changes <- function(f){
  val <- function(...) {
    init_dir <- dir()
    force(f)
    f(...)
    new_dir <- dir()
    combdir <- union(init_dir,new_dir)
    newfiles <- setdiff(combdir,init_dir)
    cat("The following new files have been created:" , newfiles , ".\n")
    deltedfiles <- setdiff(combdir,new_dir)
    cat("The following new files have been deleted:" , deltedfiles , ".\n") 
  }
  return(val)
}

create_text_files <- function(lenght_text, number_of_files){
  for(i  in seq_along(1:number_of_files)){
    random_text <- sample(union(letters,LETTERS),lenght_text, replace = TRUE)
    write(random_text, paste("example",i, ".txt", sep= "") )  
  }
  invisible()
}

file_changes_text <- file_changes(create_text_files)
file_changes_text(60,20)

rm_text_files <- function(begin_index, end_index){
  for(i  in begin_index:end_index ){
    file.remove(paste("example",i, ".txt", sep= "") )  
  }
  invisible()
}

file_changes_rm <- file_changes(rm_text_files)
file_changes_rm(11,15)


and <- function(functionlist){
  lapply(functionlist, force)
  function(...){
#    browser()
    resultval <- lapply(functionlist, function(x) x(...))
    globres <- Reduce('&&', resultval)
   # Reduce('&&',functionlist,...)    
  }

}


or <- function(functionlist){
  lapply(functionlist, force)
  function(...){
    #    browser()
    resultval <- lapply(functionlist, function(x) x(...))
    globres <- Reduce('||', resultval)
    # Reduce('&&',functionlist,...)    
  }
  
}


Filter(or(list(is.numeric,is.complex)), iris)

xor1 <- function(functionlist){
  lapply(functionlist, force)
  function(...){
    #    browser()
    resultval <- lapply(functionlist, function(x) x(...))
    globres <- Reduce('xor', resultval)
    # Reduce('&&',functionlist,...)    
  }
  
}

head(Filter(xor1(list(is.numeric,is.complex)), iris))

head(Filter(xor1(list(is.numeric,is.factor,is.character)), iris))

head(Filter(xor1(list(is.function,is.factor,is.character)), iris))

xor2 <- function(functionlist){
  lapply(functionlist, force)
  function(...){
    #    browser()
    resultval <- lapply(functionlist, function(x) x(...))
    resultrue <- resultval[resultval == TRUE]
    if (length(resultrue)==1) result <- TRUE else result <- FALSE
    return(result)
    #globres <- Reduce('||', resultval)
    # Reduce('&&',functionlist,...)    
  }
  
}


head(Filter(xor2(list(is.numeric,is.complex)), iris))

head(Filter(xor2(list(is.numeric,is.factor,is.character)), iris))

head(Filter(xor2(list(is.function,is.factor,is.character)), iris))

head(Filter(xor2(list(is.numeric,is.character)), iris))




deparse_complete <- function(x){
  string_vec <- deparse(substitute(x))
  string <- Reduce(paste0, string_vec)
  string <- gsub("  ", "", string)
  return(string)
}

# paste_plus <- quote(paste(x, sep = "+") )
# letter_string <- Reduce(eval(paste_plus), letters)
# letter_string <- do.call(eval(paste_plus), as.list(letters))

deparse_complete(g(a + B+C+D+E+F+G+H+J+K+LM+N+O+P+Q+R+S+T+U+V+X+Y+Z))

deparse(substitute((g(a + B+C+D+E+F+G+H+J+K+LM+N+O+P+Q+R+S+T+U+V+X+Y+Z))))
        

z <- data.frame(x = c(1:52), "aBCDEFGHJKLMNOPQRSTUVXYZaBCDEFGHJKLMNOPQRSTUVXYZaBCDEFGHJKLMNOPQRSTUVXYZ aBCDEFGHJKLMNOPQRSTUVXYZ" = c(letters, letters))
pairwise.t.test(z$x,z[ , "aBCDEFGHJKLMNOPQRSTUVXYZaBCDEFGHJKLMNOPQRSTUVXYZaBCDEFGHJKLMNOPQRSTUVXYZ aBCDEFGHJKLMNOPQRSTUVXYZ"])


f <- function(x){
  substitute(x)
}

ex1 <-  f("x <- 10")

f(x <- 10)

g <- function(x) deparse(f(x))
g(x <- 10)

g2 <- function(y) deparse(f(y))
g2(x <- 10)

deparse(x <- 10)
is.expression(x <- 10)
deparse(substitute(x <- 10))
is.expression(substitute(x <- 10))
is.call(substitute(x <- 10))

f2 <- function(x){
  substitute(x, list(y = 5))
}

f2(z <- y)
f2(expression(z <- y))
substitute(z <- y, list(y = 5))
f(z <- y)

# When a function is called, a new environment (called the evaluation environment) is created, whose 
# enclosure (see Environment objects) is the environment from the function closure. This new environment 
# is initially populated with the unevaluated arguments to the function; as evaluation proceeds, local variables 
# are created within it. 


debug(g)


zeer <- function(x) return(f(x) )

correct <- function(x) deparse(substitute(x))
correct(( x + y^2 /z + exp(a + sin(b))))

zever <- function(x){
  y <- x 
  browser()
  return(f(y) )
} 

eval(quote(eval(quote(eval(quote(2+2))))))
eval(eval(quote(eval(quote(eval(quote(2+2)))))))
testval <- quote(eval(quote(eval(quote(eval(quote(2+2)))))))

subset2 <- function(x, condition){
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  r <- ifelse(is.na(r), FALSE, r)
  x <- x[r, , drop = FALSE]
  return(x)
}

sample_df2 <- data.frame(x = c(NA, 1:5, NA, 7:10, NA))
subset2(sample_df2, x > 8)

subset3 <- function(x, condition){
  condition_call <- quote(condition)
  r <- eval(condition_call, x)
  r <- ifelse(is.na(r), FALSE, r)
  x <- x[r, , drop = FALSE]
  return(x)
}

subset3(sample_df2, x > 8)

#to test difference between substitute and quote, look at value of condition just after function call
# of subset2 and subset3 for

x <- c(8,5,125)

subs_order <- function(...){
  substitute(order(...))
}

rand_array <- sample(1:1000,15)

subs_order(rand_array)

order_array <- eval(subs_order(rand_array))

rand_array[order_array]

sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5,3,1,4,1))

subset4 <- function(x, condition){
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  r <- ifelse(is.na(r), FALSE, r)
  x <- x[r, , drop = FALSE]
  return(x)
}

subset5 <- function(x, condition){
  r <- eval(substitute(condition), x, parent.frame())
  r <- ifelse(is.na(r), FALSE, r)
  x <- x[r, , drop = FALSE]
  return(x)
}



scramble <- function(x) x[sample(nrow(x)), ]

subscramble <- function(x, condition){
  scramble(subset5(x, condition))
}

subscramble(sample_df, a >= 4)


subset2_q <- function(x, condition){
  r <- eval(condition, x, parent.frame())
  r <- ifelse(is.na(r), FALSE, r)
  x <- x[r, , drop = FALSE]
  return(x)
}

subset2 <- function(x, condition){
  subset2_q(x, substitute(condition))
}


subscramble <- function(x, condition){
  condition <- substitute(condition)
  scramble(subset2_q(x, condition))
}

library(lattice)

xyplot3 <- function(x,y, ...){
  substitute(xyplot(x   ~ y, ...))
}


eval(xyplot3(mpg, disp, data = mtcars, col = "red"))


substitute(a + b + c, list("+" = quote(`*`)))

substitute(f(g(a , b), c), list("g" = quote(`+`),"f" = quote(`*`) ))

substitute(f(a < b, c, d), list("f" = quote(`if`) ))


nl <- function(...){
  dots <- pryr::named_dots(...)
  lapply(dots, eval, parent.frame()) 
}

nl(3 < 4, 5 + 7)

nl2 <- function(...) lapply(substitute(alist(...)), eval, parent.frame())

nl2(3 < 4, 5 + 7)

nl3 <- function(...) lapply(eval(substitute(alist(...))), eval, parent.frame())
nl3(3 < 4, 5 + 7)


ast(if(x > 3) y <- 2 else if (x > 0) y <- 1 else y <- 0)


ast(x + y %+% z)

ast(x ^ y %+% z)

ast("a")

ast(TRUE)

ast(c("a","b"))

is.expression(c("a","b"))

as.expression(c("a","b"))


ast("a","b")


is.elem <- function(x){
  testers <- c(is.call, is.name, is.pairlist, is.atomic)
  tested <- vapply(testers, function(f) f(x), FUN.VALUE = TRUE)
  if(is.atomic(x) & length(x) > 1) stop("Atomic vectors cannot have length larger than one in an expression.")
  any(tested)
}

is.elem(1:2)
is.elem(c("a","b"))

f <- function(x) 10

formals(f)
is.name(formals(f)$x)
formals(f)$x

g <- function(x = 20, y){
  x + y
}

formals(g)

class(formals(g))

formals(g) <- alist(x = , y = 10)

get_lfr <- function(x, env_arg){
  eval(as.name(x), env_arg)
  
}

assign("my_name", 10)

get_lfr("my_name", environment())

assign_lfr <- function(y, value, env){
  eval(substitute(as.name(y <- value)), env)
}

assign_lfr("lfr_val", {x <- 10; x^2}, .GlobalEnv)


y <- quote(read.csv(("important.csv"), row.names = FALSE))

y$row.names <- TRUE

y$sep <- ","

z <- call("sd",quote(1:50))
eval(z)













ast(complex(r = 3, i = 2))

is.expression(TRUE)


(a <- call("mean", 1:10))

c <- eval(a)

b <- call("mean", quote(1:10))

d <- eval(b)

identical(c,d)
identical(a,b)

class(a)

class(b)

attributes(a)

df1 <- data.frame(a = c(1:3), b = c("a","b","c"))
df2 <- data.frame(c = c(4:6), d = c("A","B","C"))
df3 <- data.frame(e = c(7:9), f = c("A","D","C"))

test_list <- list(df1,df2,df3)

do_call <- function(what, args, quote = FALSE, envir = parent.frame){
  if (!is.list(args)) stop("second arguments must be a list")
  if (quote) args <- lapply(args, enquote)
  new_res <- args[[1]]
  for(i in 2:length(args)){
    new_res <- eval(call(what, quote(new_res), quote(args[[i]])))
  }
  return(new_res)
}

do_call("cbind", test_list)


concat <- function(what, a, b){
 # res <- as.call(list(what, as.name("a ="), substitute(a), as.name("b="), substitute(b)))
  #res <- as.call(list(what, as.name("a ="), substitute(a), as.name("b="), eval(b)))
  #res <- as.call(list(what,  as.name("a ="), substitute(a), as.name("b="), b))
  #res <- as.call(list(what, quote( as.name("a ="), substitute(a)), as.name("b="), b))
  #res <- as.call(list(what, quote( as.name("a = substitute(a)" )), as.name("b="), b))
  #res <- as.call(list(what, quote("a =") , substitute(a) , as.name("b="), b))
  #res <- as.call(list(what, substitute(resa = a, list(resa = quote(a))) , as.name("b="), b))
  #as.call(list(what, substitute(as.expression(a = a_arg), list(a_arg = a)) ,  b))
  #res
  
  #as.call(list(what, substitute(expression(a = a_arg), list(a_arg = a)) ,  b))
  #as.call(list(what, eval(substitute(quote(a = a_arg), list(a_arg = a))) ,  b))
  #as.call(list(what, substitute({a = a_arg}, list(a_arg = a)) ,  b))
  x<- substitute({a = a_arg}, list(a_arg = a))
  y <- substitute({b = b_arg}, list(b_arg = b))
  as.call(list(what, x,  y))
  
  
 # eval(res)
}


concat <- function(what, a, b, x = substitute({a = a_arg}, list(a_arg = a)), y = substitute({b = b_arg}, list(b_arg = b))){
  as.call(list(what, x[[2]],  y[[2]])) 
}

concat <- function(what, a, b){
  as.call(list(what, substitute({a = a_arg}, list(a_arg = a))[[2]],  substitute({b = b_arg}, list(b_arg = b))[[2]])) 
}

concat(quote(f), a = 1, b = quote(mean(a)))



make_call <- function(functionname, ...){ 
  inputlist <- c(...)
  as.call(c(functionname, inputlist)  )
}

make_call(quote(mean),list(quote(x), na.rm = TRUE))
make_call(quote(mean),quote(x), na.rm = TRUE)


standardise_call(quote(mean(1:10,na.rm = TRUE)))

standardise_call(quote(mean(n = T, 1:10)))

standardise_call(quote(mean(1:10, , TRUE)))


f <- function(abc =1, def =2, ghi = 3){
  list(sys = sys.call(), match = match.call())
  
}

f(d=2,2)

mod <- lm(mpg  ~ wt, data = mtcars)

mod$call
mod$call[[1]]
mod$call[[2]]
mod$call[[3]]

names(mod$call)
formula(mod)

f <- function(){
  n <- 3
  lm(mpg ~poly(wt, n), data = mtcars)
}

mod <- f()

update_call <- function(object, formula., ...){
  call <- object$call
  if(!missing(formula.)) {
    call$formula <- update.formula(formula(object), formula.)
  }
  modify_call(call, dots(...))
}

update_model <- function(object, formula., ...){
  call <- update_call(object, formula., ...)
  eval(call, environment(object))
}


update_model(mod, ~ .+ cyl, data = mtcars)

update_model(mod, ~ .- cyl, data = mtcars)


first_level <- function(...){
  ndarg <- "info2"
  newarg <- paste(...,ndarg, sep = "()")
  secondlevel(..., newarg)  
}


secondlevel <- function(...){
  sartg <- paste(..., sep = "_")
  ndarge <- "info3"
  thirdlevel(sartg, ndarge) 
}

thirdlevel <- function(...){
  start <- "info4"
  return(list(paste(..., start, sep = " "), sys.call(-1), sys.call(-2), sys.call(-3)))
}
  
first_level("info1","info2")

g <- function(a,b){
  3 * a + 2*b  
}

e <- quote(`foo bar`+1)

firste <- deparse(e)
parse(text = firste)


seconde <- deparse(e, control = "all")
parse(text = seconde)





find_assign <- function(x){
  if( is.atomic(x) || is.name (x)){
    character()
  } else if (is.call(x)){
    if(identical(x[[1]], quote(`<-`)) && is.name(x[[2]])) {
      lhs <- as.character(x[[2]])
    } else {
      lhs <- character()
    } 
    unique(c(lhs,unlist(lapply(x,find_assign)))) 
  }  else if (is.pairlist(x)){
    unique(unlist(lapply(x,find_assign)))
  } else {
    stop("Don't know how to handle type", typeof(x), call. = FALSE)
  }  
  
}

find_assign(quote({
  a <- 1
  b <- 2
  a <- 3
}))

find_assign(quote({
  system.time(x <- print(y <- 5))
}))


find_assign(quote({
  l <- list()
  l$a <- 5
  names(l)<- "b"
}))


logical_abbr <- function(x){
  if (is.atomic(x)){
    FALSE
  } else if (is.name(x)){
    identical(x,quote(T)) || identical(x,quote(F))
  } else if (is.call(x) || is.pairlist(x)){
    for(i in seq_along(x)){
      if(logical_abbr(x[[i]])) return(TRUE)
    }
    FALSE
  } else {
    stop("Don't know how to handle type", typeof(x), call. = FALSE)    
  }
  
}

logical_abbr(quote(function(x, na.rm = T) FALSE))

f <- function(x = T){
  g(x + T)
}


logical_abbr(f)

logical_abbr2 <- function(x){
  if(is.function(x)) x <- formals(f)
  if (is.atomic(x)){
    FALSE
  } else if (is.name(x)){
    identical(x,quote(T)) || identical(x,quote(F))
  } else if (is.call(x) || is.pairlist(x)){
    for(i in seq_along(x)){
      if(logical_abbr(x[[i]])) return(TRUE)
    }
    FALSE
  } else {
    stop("Don't know how to handle type", typeof(x), call. = FALSE)    
  }
  
}

logical_abbr2(f)

ast_type <- function(x){
  if (is.atomic(x)){
    return("constant")
  } else if (is.name(x)){
    return("name")
  } else if (is.call(x) ){
    return("call")
    
    } else if (is.pairlist(x)){
    return("pairlist")
  } else {
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)    
  }
  
}

ast_type("x")

x <- 3

ast_type(formals(f))

ast_type( `mean`)

cl <- call("round", 10.5)
ast_type(cl)

ast_type(as.name("aarg"))


logical_abbr3 <- function(x){
  if(is.function(x)) x <- formals(f)
  y <- ast_type(x)
  callorpair <- function(x) {
    for(i in seq_along(x)){
      if(logical_abbr3(x[[i]])) {
        
        return(TRUE)
      }
    }
    FALSE
  }
  nameval <- function(x) {
    return(identical(x,quote(T)) || identical(x,quote(F)))
    browser()
  }
  switch(y,
         constant = FALSE,
         name = nameval(x)         ,
         call = callorpair(x),
         pairlist = callorpair(x)
    )
  
}


logical_abbr3(f)

logical_abbr3(quote(TRUE))

logical_abbr3(quote(T))

logical_abbr3(quote(mean(x, na.rm = TRUE)))

logical_abbr3(quote(mean(x, na.rm = T)))



library(lineprof)
f <- function(){
  pause(0.1)
  g()
  h()
}
g <- function(){
  pause(0.1)
  h()
}
h <- function(){
  pause(0.1)
}

