ERL=erl
APP_NAME=gravity
NODE_NAME=$(APP_NAME)

all: app

app:
	( $(ERL) -make && \
	if [ ! -e ebin/$(APP_NAME).app ]; then cp -f src/$(APP_NAME)/$(APP_NAME).app.src ebin/$(APP_NAME).app; fi )

docs:   
	erl -pa `pwd`/ebin \
	-noinput \
	-run edoc_run application "'vecterl'" '"."' '[no_packages]'

run: app
	$(ERL) -pa `pwd`/ebin \
	-s $(APP_NAME) \
	-sname $(NODE_NAME)

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-docs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
	rm -fv ebin/$(APP_NAME).app
