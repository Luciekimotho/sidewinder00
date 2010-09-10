clear
rm *~ 
rm -r CosEventComm
rm -r orb.db
rm *.class
if [ $# -eq 0 ]
then
    idlj -fall CosEventComm.idl &&
    javac PushConsumerServer.java && 
    javac PushSupplierClient.java && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Consumer-Supplier" --title "ORB" --noclose -e orbd -ORBInitialPort 1050 && 
    read -p "Premi per avviare il resto" &&
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Consumer-Supplier" --title "PushConsumerImplServer" --noclose -e java PushConsumerServer -ORBInitialPort 1050 -ORBInitialHost localhost && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Consumer-Supplier" --title "PushSupplierImplClient" --noclose -e java PushSupplierClient -ORBInitialPort 1050 -ORBInitialHost localhost && 
    read -p "Premi per terminare" && 
    killall konsole
fi