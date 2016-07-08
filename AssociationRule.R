library (Matrix)  
library (arules)

setwd("E:\\learning material\\Data mining\\AssociationRuleM\\")

###################################################
###a1	Temperature of patient { 35C-42C } 35.0-36.9 normal,37.0-37.9,low,38.0-39.9 middle,40.0-42.0 high	
###a2	Occurrence of nausea { yes, no }	
###a3	Lumbar pain { yes, no }	
###a4	Urine pushing (continuous need for urination) { yes, no }	
###a5	Micturition pains { yes, no }	
###a6	Burning of urethra, itch, swelling of urethra outlet { yes, no }	
###d1	decision: Inflammation of urinary bladder { yes, no }	
###d2	decision: Nephritis of renal pelvis origin { yes, no }
###################################################

###################################################
###Read Data
###################################################
diagnosis <- read.table('diagnosis.data',
                         header=F,
                         dec = ",",
                         sep='\t',
                         fileEncoding="UTF-16LE",
                         as.is=TRUE,
                         col.names=c('temperature','nausea','lumbar','urine','micturition','urethra','bladder','nephritis')
                        )
colum <- list('temperature','nausea','lumbar','urine','micturition','urethra','bladder','nephritis')
###################################################
###1 Preprocess,converted  dataset into forms that are suitable for Mining Association rules
###################################################
str_all=""
for(i in 1:nrow(diagnosis))
{
  if(as.numeric(diagnosis[i,1])<=36.9){str='normal'}
  else if(as.numeric(diagnosis[i,1])<=37.9){str='low'}
  else if(as.numeric(diagnosis[i,1])<=39.9){str='midde'}
  else if(as.numeric(diagnosis[i,1])<=42.0){str='high'}
 
  for(j in 2:ncol(diagnosis))
  {
    if(diagnosis[i,j]=="yes"){str <- paste(str,colum[[j]],sep=',')}
  }
  if(str_all==''){str_all=str}
  else{str_all <- paste(str_all,str,sep='\r')}
}
write(str_all,'data_preprocess.csv')
###################################################
###2 Find the frequent items ,default supp=0.1,conf=0.8
###################################################
a <- read.transactions('data_preprocess.csv',format = 'basket',sep=',')
sink("summary.txt")
summary(a)
sink()
sink("frequent_items.txt")
###frequentsets=eclat(a,parameter=list(support=0.1,maxlen=8))  
###inspect(frequentsets)
itemsets_apr = apriori (a, 
                        parameter=list (support=0.1,confidence=0.5,maxlen=8,target="frequent itemsets"),
                        control=list(sort=-1)) 
inspect(itemsets_apr) 
sink()
###################################################
###3 Export rules, calculate its support and confidence
###################################################
sink("rules.txt")
rules = apriori (a, parameter=list (support=0.1,confidence=0.5,minlen=3,maxlen=8,targe="rules"), 
                 appearance=list(rhs=c("bladder","nephritis"),
                 default="lhs"),
                 control=list(sort=-1))    #若minlen=2则无法画出平行坐标图，并出现警告信息？？？？ 
inspect(rules) 
sink()
###################################################
###4 Delete redundant rules X->Y ,Y==bladder or nephritis
###################################################
subset.matrix<-is.subset(rules,rules)    #生成一个所有规则的子集矩阵,行和列分别是每条rules，其中的值是TRUE和FALSE，当rules2是rules1的子集时，rules2在rules1的值为TRUE
#subset.matrix[row(subset.matrix)==col(subset.matrix)]<-NA
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA    #将矩阵对角线以下的元素置为空，只保留上三角
redundant<-colSums(subset.matrix,na.rm=T)>=1    #R会将矩阵中的TRUE当做1，统计每列的和（忽略缺失值），如果该列的和大于等于1，也就是表示该列（规则）是别的规则的子集，应该删除。
rules.pruned<-rules[!redundant]    #去掉冗余的规则

sink("rules_delete_redundant.txt")
inspect(rules.pruned) 
sink()
###################################################
###5 Evaluated rules, use the Lift
###################################################
sink("rules_delete_redundant_sorted_lift.txt")
rules.pruned.sorted_lift = sort(rules.pruned,by='lift')
inspect(rules.pruned.sorted_lift) 
sink()
###################################################
###6 Visualization, display rules
###################################################
library (grid)
library (arulesViz)

#Scatter chart
plot(rules.pruned.sorted_lift) 
#Parallel coordinates
plot(rules.pruned.sorted_lift,method="paracoord")
#Bubble chart
plot(rules.pruned.sorted_lift,method="grouped") 



















