
sim:
	sbt "test-only rt.PanoTester"

syn:
	sbt "run-main rt.TopRT"

waves:
	gtkwave -o simWorkspace/PanoCoreDut/test.vcd &
    
