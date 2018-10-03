#!/bin/sh
# create custom dmenu setup

# setup Applications path
GET_USER=`whoami`
HOME_PATH="/home/${GET_USER}/Applications/"

# create custom buttons
ACTION=`printf "dmenu
FirefoxDev
ATOM
IntelliJ
PhpStorm
OllyDBG
VMware
" | dmenu -i $*`

# generate action for each entry
if [ "$ACTION" == 'dmenu' ]; then
	exec dmenu_run
fi

if [ "$ACTION" == 'ATOM' ]; then
	exec atom
fi

if [ "$ACTION" == 'FirefoxDev' ]; then
	exec "${HOME_PATH}/${ACTION}/firefox"
fi

if [ "$ACTION" == 'PhpStorm' ]; then
	exec "${HOME_PATH}/${ACTION}/bin/phpstorm.sh"
fi

if [ "$ACTION" == 'IntelliJ' ]; then
	exec "${HOME_PATH}/${ACTION}/bin/idea.sh"
fi

if [ "$ACTION" == 'OllyDBG' ]; then
	`wine "${HOME_PATH}/${ACTION}/OLLYDBG.exe"`
fi

if [ "$ACTION" == 'VMware' ]; then
	vmware
fi
