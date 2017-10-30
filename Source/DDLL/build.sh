OS="`uname`"
if [[ "$OS" == "Linux" ]]; then
	ppcx64 @linuxopts.cfg -B OpenDSSDirect.lpr
elif [[ "$OS" == "Darwin" ]]; then
	ppcx64 @fpcopts.cfg -B OpenDSSDirect.lpr
fi
