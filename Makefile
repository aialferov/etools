
all:
	erlc -o ebin/ src/*.erl

clean:
	rm ebin/emake.beam

install:
	mkdir -p /usr/local/lib/emake/lib/emake/ebin
	install ebin/emake.beam /usr/local/emake/lib/emake/ebin

uninstall:
	rm /usr/local/lib/emake/lib/emake/ebin/emake.beam
	rmdir --ignore-fail-on-non-empty -p /usr/local/emake/lib/emake/ebin
