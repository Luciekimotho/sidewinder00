clear
rm *~ 
rm -r CosEventComm
rm -r orb.db
rm *.class
if [ $# -eq 0 ]
then
    idlj -fall CosEventComm.idl &&
    javac PushConsumerImpl.java &&
    javac PushSupplierImpl.java && 
    javac PushConsumerServer.java && 
    javac PushSupplierClient.java && 
    #konsole --workdir "/home/stefan/Scrivania/Mini Corba/Consumer-Supplier" --title "ORB" --noclose -e orbd -ORBInitialPort 1050 && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Consumer-Supplier" --title "PushConsumerServer" --noclose -e java PushConsumerServer -ORBInitialPort 1050 -ORBInitialHost localhost && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Consumer-Supplier" --title "PushSupplierClient" --noclose -e java PushSupplierClient -ORBInitialPort 1050 -ORBInitialHost localhost && 
    read -p "Premi per terminare" && 
    killall konsole
fi