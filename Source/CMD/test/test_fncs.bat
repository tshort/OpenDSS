start /b cmd /c fncs_broker 3 ^>broker.log 2^>^&1
start /b cmd /c fncs_player 6h opendss.player ^>player.log 2^>^&1
set FNCS_CONFIG_FILE=tracer.yaml
start /b cmd /c fncs_tracer 6h tracer.out ^>tracer.log 2^>^&1
set FNCS_CONFIG_FILE=opendss.yaml
start /b cmd /c opendsscmd -f ^>opendss.log 2^>^&1


