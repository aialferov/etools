
all:
	erlc -o ebin/ src/*.erl

clean:
	rm -f ebin/emake.beam

install:
	mkdir -p /usr/local/lib/emake/ebin
	install -p ebin/emake.app /usr/local/lib/emake/ebin
	install -p ebin/emake.beam /usr/local/lib/emake/ebin

uninstall:
	rm -f /usr/local/lib/emake/ebin/emake.app
	rm -f /usr/local/lib/emake/ebin/emake.beam
	rmdir --ignore-fail-on-non-empty -p /usr/local/lib/emake/ebin
