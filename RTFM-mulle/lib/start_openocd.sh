#!/bin/bash
cp mulle.cfg /tmp/mulle_ocd.cfg
if [ $# -gt 2 ]
then
	echo "usage: $basename <?fdti serial> <?gdb port>"
	exit 1
fi

if [ $# -gt 0 ]
then
	sed -i -e 's/#FTDI_SERIAL/ftdi_serial \"'"$1"'\"/' /tmp/mulle_ocd.cfg
fi
if [ $# -eq 2 ]
then
	sed -i -e 's/#GDB_PORT/gdb_port \"'"$2"'\"/' /tmp/mulle_ocd.cfg
fi
sudo openocd -s /usr/local/share/openocd/scripts/ -f /tmp/mulle_ocd.cfg
