
# Single parents ==== 

# Counter$go must be applied on a group of people with the same parent ids
# It then adds a phantom parent, if one parent is missing.
Counter <- setRefClass(
  "Counter", 
  fields = list(c = "numeric", m = "numeric", make_parents =
                  "function"),
  methods = list(
    initialize = function(m, make_parents) {
      c <<- 0
      m <<- m
      make_parents <<- make_parents
    },
    # inserts a "phantom parent" with an appropriate UPN
    go = function(sibs) { # input is a data.frame of siblings
      if(nrow(sibs) == 0) return(sibs)
      
      if (sibs[1,]$Father.ID == 0 & sibs[1,]$Mother.ID != 0 ) {
        c <<- c + 1
        newUPN <- c + m
        
        father <- make_parents(newUPN,'male',sibs[1,]$Pedigree.name)
        sibs$Father.ID <- newUPN                
        sibs <- rbind(father,sibs)
      } else if(sibs[1,]$Mother.ID == 0 & sibs[1,]$Father.ID != 0 ) {
        c <<- c + 1
        newUPN <- c + m
        
        mother <- make_parents(newUPN,'female',sibs[1,]$Pedigree.name)
        sibs$Mother.ID <- newUPN
        sibs <- rbind(sibs,mother)
      }
      return (sibs)  
    }
  )
)

# Get the people with only on parent
GetProblems <- function(data) {
  return (data[(data$Father.ID == 0 & data$Mother.ID != 0) |
                 data$Father.ID != 0 & data$Mother.ID == 0,])
}

# Take a sub of data, that's split according to pedigree
# make_parents(upn,sex,pedigree)
pedLevel <- function(pedigree, make_parents) {
  m <- max(pedigree$UPN)
  c <- Counter$new(m, make_parents)
  phant <- ddply(pedigree,c("Mother.ID","Father.ID"), function (x) c$go(x))
  return(phant)
}

