clear
rm *~ 
rm -r CosEventComm
rm -r CosTypedEventComm
rm -r Type
rm -r orb.db
rm *.class
if [ $# -eq 0 ]
then
    idlj -fall CosEventComm.idl &&
    idlj -fall CosTypedEventComm.idl &&
    idlj -fall Type.idl &&
    javac TypedPushConsumerServer.java && 
    javac PushSupplierClient.java &&
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed Consumer-Supplier" --title "ORB" --noclose -e orbd -ORBInitialPort 1050 && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed Consumer-Supplier" --title "TypedPushConsumerServer" --noclose -e java TypedPushConsumerServer -ORBInitialPort 1050 -ORBInitialHost localhost && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed Consumer-Supplier" --title "PushSupplierClient" --noclose -e java PushSupplierClient -ORBInitialPort 1050 -ORBInitialHost localhost && 
    read -p "Premi per terminare" && 
    killall konsole
fi