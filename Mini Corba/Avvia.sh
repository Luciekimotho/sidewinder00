if [ $# -eq 1 ]
then
    java $1 -ORBInitialPort 1050 -ORBInitialHost localhost
fi