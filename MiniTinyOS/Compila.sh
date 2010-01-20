make clean
rm *.out
rm *~
java net.tinyos.sim.LinkLayerModel topoConfig.txt && javac Ausiliario.java && java Ausiliario && rm *.class && make micaz sim && ./run.py