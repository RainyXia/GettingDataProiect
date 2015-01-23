#step1原始数据集的生成(dt)：
#将x_train,subject_train,y_train用cbind连接，生成train
#将x_test,subject_test,y_test用cbind连接，生成test
#将train和test用rbind连接，生成dt

#step2中measurements的抽取（dt3)：
#利用grep函数提取出变量名中含有mean和std的变量的列号，grep1和grep2
#先从dt中提出含有mean和std的变量，生成dt2，但是发现angle变量与mean和std无关，要剔除
#用grep3提取了dt2中含有angle的变量，用-号表示剔除，生成dt3

#step3对activity进行描述：
#"activity_labels.txt"中是对activity变量的描述
#利用factor函数将"activity_labels.txt"中的描述性词作为因子level的标签赋给activity

#step4对数据集变量进行标注
#这个步骤在生成dt的过程中已经进行，"features.txt"即为数据集大部分变量的标签
#通过对merge之前的每一部分数据集命名再merge的方式实现

#step5生成整洁数据
#先利用split加sapply的方式生成含有每个activity的各个变量的均值的数据集mean_act
#再利用reshape2将mean_act变为整洁数据集melt_act。其中”mea"代表除“subject"和”activity“
#之外的变量名，”a_s_value“代表activity的值
#同理生成melt_sub的整洁数据集
#为了将melt_act和melt_sub合二为一，均加一列"a_s"表明是activity还是activity
#最终用rbind生成整洁数据集dt_melt
#用write.table生成tidydata.txt
