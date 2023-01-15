from pyspark.sql.functions import *
import pyspark.ml.stat

FILE_PATH1 = "/home/edu3/projekt/question_tags_mini.csv"
FILE_PATH2 = "/home/edu3/projekt/questions_mini.csv"

question_tags_mini = spark.read.option("delimiter", ",").option('header', True).csv(FILE_PATH1)
questions_mini = spark.read.option("delimiter", ",").option('header', True).csv(FILE_PATH2)


#task 1

filtered_questions_mini = questions_mini.filter(col("ClosedDate") == 'NA') #not closed have 'ClosedDate' NA
open_questions = filtered_questions_mini.count()
print(f"{open_questions=}")

closed_questions = questions.filter(col("ClosedDate") != 'NA')

tab1 = closed_questions.select((datediff(col("ClosedDate"),col("CreationDate"))*24).alias("Open Time")) #compute time opened
tab2 = tab1.select(round(avg("Open Time"), 2).alias("Avg open hours")) #compute average
tab2.show()


#task2

count_tags = question_tags.groupBy("Tag").count() #count tags
most_popular = count_tags.orderBy(col("count").desc()) #sort them in desc order
most_popular.show()

#to find pairs, we need to join the table with itself
question_tags2 = question_tags.select(col("Id"), col("Tag").alias("Second Tag")) #we only take Id and Tag, we rename Tag to Second Tag so that we don't have 2 columns with the same name
joined = question_tags.join(question_tags2, question_tags.Id == question_tags2.Id) #join the tables
filtered = joined.filter(col("Tag") < col("Second Tag")) #to take only the unique pairs, otherwise it takes (tag1, tag2) and (tag2, tag1), so it's a duplicit record
most_common_pairs= filtered.groupBy(col("Tag"), col("Second Tag")).count().orderBy((col("count")).desc()) #count pairs and sort them by count
most_common_pairs.show()

#task3

joined_tab = questions_mini.join(question_tags_mini, "Id") #join tables on Id
retyped = joined_tab.select(col("Tag"), col("Score").cast("int"), col("AnswerCount").cast("int")) #Score and AnswerCount were strings, so we retyped them
res = retyped.groupBy("Tag").agg(avg("Score"), avg("AnswerCount"))
res.show()

#task4

only_csharp = joined_tab.filter(col("Tag") == "c#") #c# is the most common tag
retyped2 = only_csharp.select(col("Tag"), col("Score").cast("int"), col("AnswerCount").cast("int"))
cor = retyped2.stat.corr("Score", "AnswerCount")
print(f"{cor=}")
