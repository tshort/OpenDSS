(exec fncs_broker 3 &> broker.log &)
(exec fncs_player 6h opendss.player &> player.log &)
(export FNCS_CONFIG_FILE=tracer.yaml && exec fncs_tracer 6h tracer.out &> tracer.log &)
(export FNCS_CONFIG_FILE=opendss.yaml && exec ./opendsscmd -f &> opendss.log &)


