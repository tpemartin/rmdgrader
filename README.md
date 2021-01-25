v 0.3.5

A major upgrade in step3_grade and step4_return.
新的Return模組，有幾點要注意：

1. 成績結果不再是寫在標題右側，而是另外在Rmd的開頭插入一個grade codechunk，學生要去執行它才會看到成績。

2. return Rmd會以 -ans.Rmd長怎麼樣去自動生成，其中： 

2.1 -ans.Rmd要有一標題列寫「## 成績」，Return模組會去偵測它，有此列才會去插入grade codechunk. 沒有，則不會有grade codechunk

3. Grade codechunk 長得像：
```{r grade}

df_grade <- 
jsonlite::fromJSON('[{"grade":1,"comment":"","_row":"ans12.1"},{"grade":1,"comment":"","_row":"ans12.2"},{"grade":1,"comment":"","_row":"ans12.3"},{"grade":0,"comment":"","_row":"ans13.1s"},{"grade":0,"comment":"","_row":"ans13.2s"}]')
totalGrade <- list()
within(
  totalGrade,
  {
    rawGrade = sum(df_grade$grade)
    finalGrade = rawGrade/nrow(df_grade)*7+3
  }
) -> totalGrade
print(df_grade)
print(totalGrade)
        ans <- new.env(parent=.GlobalEnv) #對答案用

```

學生除了看成績，也看得到comment，唯step3也要依循新的step3(見HW8)架構去跑才會有正確grade code。

grade code當中的：

```{r}
totalGrade <- list()
within(
  totalGrade,
  {
    rawGrade = sum(df_grade$grade)
    finalGrade = rawGrade/nrow(df_grade)*7+3
  }
) -> totalGrade
```

可在Return步驟時換掉，只要在step4_return時，執行：
```{r}
newCaclulationText <- 
"
totalGrade <- 'You fail'
"
# 一口氣改所有並return
re$inBatch$return(newCaclulationText)
# 只改單一學生並return
re$studentRmds$HW8_410672033.Rmd$returnRmd$return(newCaclulationText)
```

這時學生收到的returnRmd grade codeChunk會變成：
```{r grade}

df_grade <- 
jsonlite::fromJSON('[{"grade":1,"comment":"","_row":"ans12.1"},{"grade":1,"comment":"","_row":"ans12.2"},{"grade":1,"comment":"","_row":"ans12.3"},{"grade":0,"comment":"","_row":"ans13.1s"},{"grade":0,"comment":"","_row":"ans13.2s"}]')

totalGrade <- 'You fail'

print(df_grade)
print(totalGrade)
        ans <- new.env(parent=.GlobalEnv) #對答案用

```

這個設計是為了因應各種算分方式而設計，如Bonus, 你只要把totalGrade的計分方式換成你的演算coding即可。


v 0.3.0

new 

for step2_process, it is streamlined into a clear and more efficient approach: using Process and Evaluate instance initiators.

v 0.2.7

new

* subcategorise_byMsgRmdPairs function allows manually defined list of msg and rmd vectors (both equal length) to automtically create subcat element in ae 

* accommodate_grades method attached to ae\$subcat as to revise ae\$extract_grades (see final grading_flow step3 ans2)

* getMaxGrade function can get the max grade out of multiple level grades

* merge_gradeVectors can merge all rmd named grade vectors into a tibble with rmd names column supplied.

v 0.2.6

new
* gradeMix function 

* functional module

revised
* ifelsethen: now return errorMsg when logical(0) is the outcome

v 0.2.5
