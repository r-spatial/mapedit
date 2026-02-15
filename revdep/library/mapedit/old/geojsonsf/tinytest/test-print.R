## "print method called",{

	geo <- '{"type":"Point","coordinates":[0,0]}'
	attr(geo, "class") <- "geojson"

	expect_stdout(print(geo))
