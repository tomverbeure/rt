
sim:
	sbt "testOnly rt.PanoTester"

syn:
	sbt "runMain rt.TopRT"

waves:
	gtkwave -o simWorkspace/PanoCoreDut/test.vcd &
    
