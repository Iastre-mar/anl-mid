kach = c()
kol = c()
for (x in colnames(tit)){
  vec <- filter(select(tit, x), (select(tit, x)[1] != "" & select(tit, x)[1] != " "))
  uniq <- unique(vec)
  nunique <- nrow(uniq)
  if (nunique < 10){
    kach = c(kach, x)
  } else {
    kol = c(kol, x)
  }
}
kach
length(kach)
