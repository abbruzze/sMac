#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=
OPT="-server -Xms64M -Xmx64M --add-opens java.desktop/com.sun.media.sound=ALL-UNNAMED"
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi
$JAVA $OPT -cp $CP -DsmacHome=$HOME ucesoft.mac.ui.MacGUI "$@"
