OS="`uname`"
if [[ "$OS" == "Linux" ]]; then
	ppcx64 @linuxopts.cfg -B opendsscmd.lpr
elif [[ "$OS" == "Darwin" ]]; then
	ppcx64 @fpcopts.cfg -B opendsscmd.lpr
fi
