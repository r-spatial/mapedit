app <- ShinyDriver$new("../../")
app$snapshotInit("test2")

app$snapshot()
app$setInputs(id = "2")
app$setInputs(comments = "working")
app$setInputs(row_add = "click")
