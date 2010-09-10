clear
rm *~ 
rm -r CosEventComm
rm -r CosEventChannelAdmin
rm -r orb.db
rm *.class
if [ $# -eq 0 ]
then
    idlj -fall CosEventComm.idl && 
    idlj -fall CosEventChannelAdmin.idl && 
    javac EventChannelServer.java && 
    javac Admin.java && 
    javac Client.java && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/EventChannel" --title "ORB" --noclose -e orbd -ORBInitialPort 1050 && 
    read -p "Premi per avviare il resto" && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/EventChannel" --title "EventChannelServer" --noclose -e java EventChannelServer -ORBInitialPort 1050 -ORBInitialHost localhost && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/EventChannel" --title "Admin" --noclose -e java Admin -ORBInitialPort 1050 -ORBInitialHost localhost && 
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/EventChannel" --title "Client1" --noclose -e java Client -ORBInitialPort 1050 -ORBInitialHost localhost &&
    konsole --workdir "/home/stefan/Scrivania/Mini Corba/EventChannel" --title "Client2" --noclose -e java Client -ORBInitialPort 1050 -ORBInitialHost localhost && 
    read -p "Premi per terminare" && 
    killall konsole
fi