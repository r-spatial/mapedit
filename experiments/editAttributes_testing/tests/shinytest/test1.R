app <- ShinyDriver$new("../../")
app$snapshotInit("test1")

app$setInputs(donebtn = "click")
app$snapshot()
