#/bin/bash

rm *.beam
rm *.dump
rm *~
if [ $# -eq 0 ]
then
    erlc pubsub.erl &&
    konsole --workdir /home/stefan/Scrivania/DemoErlang --noclose --title "node1" -e erl -sname node1@localhost &&
    konsole --workdir /home/stefan/Scrivania/DemoErlang --noclose --title "node2" -e erl -sname node2@localhost &&
    konsole --workdir /home/stefan/Scrivania/DemoErlang --noclose --title "node3" -e erl -sname node3@localhost &&
    konsole --workdir /home/stefan/Scrivania/DemoErlang --noclose --title "node4" -e erl -sname node4@localhost &&
    konsole --workdir /home/stefan/Scrivania/DemoErlang --noclose --title "node5" -e erl -sname node5@localhost &&
    konsole --workdir /home/stefan/Scrivania/DemoErlang --noclose --title "node6" -e erl -sname node6@localhost &&
    read -p "Premi x terminare"
    killall konsole;
fi