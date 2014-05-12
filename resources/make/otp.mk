lfedev:
	@echo "Running OTP app in the foreground ..."
	@ERL_LIBS=$(ERL_LIBS) $(LFE) -eval "application:start('lfecljapp')" \
	-noshell

lferun: dev

lfeprod:
	@echo "Running OTP app in the background ..."
	@ERL_LIBS=$(ERL_LIBS) $(LFE) -eval "application:start('lfecljapp')" \
	-name lfecljapp@$${HOSTNAME} -setcookie `cat ~/.erlang.cookie` \
	-noshell -detached

lfedaemon: prod

lfestop:
	@ERL_LIBS=$(ERL_LIBS) $(LFE) \
	-eval "rpc:call('lfecljapp@$${HOSTNAME}', init, stop, [])" \
	-name controller@$${HOSTNAME} -setcookie `cat ~/.erlang.cookie` \
	-noshell -s erlang halt
