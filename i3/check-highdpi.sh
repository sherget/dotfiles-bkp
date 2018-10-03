#!/bin/sh

#check if eDP1 is connected
if (xrandr | grep "eDP1 connected" > /dev/null)
then
 nitrogen --random --set-scaled /home/shin/Pictures/desktop-bgs &&
 sed -i 's/notebook-bgs/desktop-bgs/g' ~/.i3/config

 	LOCK_DIR="/home/shin/Pictures/desktop-bgs/locks-bg"
	for f in ${LOCK_DIR}/*.png 
	do
			i=$(( ( RANDOM % 100 )  + 1 ))
			file=`basename "$f"`
			mv $LOCK_DIR/$file $LOCK_DIR/lock$i.png
	done
	IMG=`find "$LOCK_DIR"/ -name "*.png" | shuf -n1`
	mv $LOCK_DIR/`basename "$IMG"` $LOCK_DIR/lock.png
 exit
else
 xrandr --output eDP1 --mode 1920x1080 &&
 nitrogen --random --set-scaled /home/shin/Pictures/notebook-bgs &&
 sed -i 's/desktop-bgs/notebook-bgs/g' ~/.i3/config

	LOCK_DIR="/home/shin/Pictures/notebook-bgs/locks-bg"
 		for f in ${LOCK_DIR}/*.png 
		do
			i=$(( ( RANDOM % 100 )  + 1 ))
			file=`basename "$f"`
			mv $LOCK_DIR/$file $LOCK_DIR/lock$i.png
 		done
	IMG=`find "$LOCK_DIR"/ -name "*.png" | shuf -n1`
	mv $LOCK_DIR/`basename "$IMG"` $LOCK_DIR/lock.png
 exit
fi
