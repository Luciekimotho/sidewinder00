clear
rm *~ 
rm -r CosEventComm
rm -r CosTypedEventComm
rm -r CosEventChannelAdmin
rm -r CosTypedEventChannelAdmin
rm -r orb.db
rm *.class
if [ $# -eq 0 ]
then
    idlj -fall CosEventComm.idl && 
    idlj -fall CosEventChannelAdmin.idl && 
    idlj -fall CosTypedEventComm.idl &&
    idlj -fall CosTypedEventChannelAdmin.idl &&  
    javac TypedEventChannelServer.java && 
    javac Admin.java && 
    #javac Client.java && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "ORB" --noclose -e orbd -ORBInitialPort 1050 && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "TypedEventChannelServer" --noclose -e java TypedEventChannelServer -ORBInitialPort 1050 -ORBInitialHost localhost && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "Admin" --noclose -e java Admin -ORBInitialPort 1050 -ORBInitialHost localhost && 
    #konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "Client1" --noclose -e java Client -ORBInitialPort 1050 -ORBInitialHost localhost &&
    #konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "Client2" --noclose -e java Client -ORBInitialPort 1050 -ORBInitialHost localhost && 
    #konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "Client3" --noclose -e java Client -ORBInitialPort 1050 -ORBInitialHost localhost && 
    #konsole --workdir "/home/stefan/Scrivania/Mini Corba/Typed EventChannel" --title "Client4" --noclose -e java Client -ORBInitialPort 1050 -ORBInitialHost localhost &&  
    read -p "Premi per terminare" && 
    killall konsole
fi