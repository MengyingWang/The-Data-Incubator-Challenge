#read in data
path<- vector(mode="character", length=5)
for (i in 1:5){
	path[i]<-paste('/Users/Mengying/Desktop/DataIncubator_challenge/Calls_for_Service_201',i,'.csv',sep='')
}

d<-lapply(path,read.csv)

#Using RHadoop calculate number of events of every event, key=Type_, val=count number
pd.map<-function(k,v){
	type<-v$Type_
	keyval(type,1)
}

pd.reduce<-function(k,v){
	keyval(k,sum(v))
}
input_nopd<-to.dfs(pd)
count<-mapreduce(input_nopd,map=pd.map,reduce=pd.reduce)
results=from.dfs(count)
results.df=as.data.frame(results,stringsAsFactors=F)
Tcount<-results.df  

#Prepare RSQL
install.packages("RSQLite")
install.packages("DBI")
install.packages("sqldf")
library(DBI)
library(RSQLite)
library(sqldf)

#response time
nopd$TimeArrive<-as.numeric(nopd$TimeArrive)
nopd$TimeDispatch<-as.numeric(nopd$TimeDispatch)
dispatch<-sqldf("select PoliceDistrict, TimeArrive-TimeDispatch as Response from nopd where TimeDispatch>0 ")
dispatched<-sqldf("select * from dispatch where Response>0")
mean_district<-sqldf("select PoliceDistrict , avg(Response) as meanResp from dispatched group by PoliceDistrict order by meanResp")

#type count group by year
count_1<-sqldf("select Type_, count(*) as count from pd1 group by Type_ order by Type_")

count_2<-sqldf("select Type_, count(*) as count from pd2 group by Type_ order by Type_")
count_3<-sqldf("select Type_, count(*) as count from pd3 group by Type_ order by Type_")
count_4<-sqldf("select Type_, count(*) as count from pd4 group by Type_ order by Type_")
count_5<-sqldf("select Type_, count(*) as count from pd5 group by Type_ order by Type_")
diff<-sqldf("select count_1.Type_ ,count_1.count-count_5.count as diff from count_1 ,count_5 where count_1.Type_=count_5.Type_ order by diff")


#Proprity
#number of event in (type, priority)pair group
pri<-sqldf("select Type_ ,Priority ,count(*) as count 
			from nopd
			group by Type_,Priority
			order by Type_ ,count")

# left join, adding total number of event in (tpye) group
priority<-sqldf("select * 
			from pri
			left join
			Tcount
			on pri.Type_=Tcount.key")
#calculate fraction
priority$percent<-priority$count/priority$val
#find most common priortiy of every type, and get the samllest one
small_pri<-sqldf("select Type_, max(percent) as max from priority
				group by Type_
				order by max  ")

#ratio of conditional probability vs unconditional probability
dis_type<-sqldf("select PoliceDistrict, Type_,count(*) as count
				from nopd
				where  PoliceDistrict>0
				group by PoliceDistrict, Type_
				order by PoliceDistrict, count")
dis_total<-sqldf("select PoliceDistrict as dist, count(*) as total
				from nopd
				where PoliceDistrict>0
				group by PoliceDistrict")

dis_type<-sqldf("select PoliceDistrict, Type_, count, total
				from dis_type
				left join
				dis_total
				on dis_type.PoliceDistrict=dis_total.dist")

dis_type$percent<-dis_type$count/dis_type$total

dis_cond<-sqldf("select * from dis_type
		left join
		Tcount
		on dis_type.Type_=Tcount.key")

total<-sum(Tcount$val)
dis_cond$PerT<-dis_cond$val/total
dis_cond$ratio<-dis_cond$percent/dis_cond$PerT

last<-sqldf("select ratio from dis_cond where count>100 order by ratio")
