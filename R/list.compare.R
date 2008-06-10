`list.compare` <-
function(list1,list2){

result<-rep(FALSE,length(list2))


for(i in 1:length(list1)){

result[list2==list1[i]]<-TRUE


}
result

}

