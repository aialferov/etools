
all:
	mkdir -p ebin
	erlc -o ebin/ src/*.erl

clean:
	rm -f ebin/emake.beam
	rmdir --ignore-fail-on-non-empty -p ebin

install:
	mkdir -p /usr/local/lib/emake/ebin
	install -p ebin/emake.beam /usr/local/lib/emake/ebin
	install -p priv/emake /usr/local/bin
	install -p priv/etmpl /usr/local/bin

uninstall:
	rm -f /usr/local/lib/emake/ebin/emake.beam
	rm -f /usr/local/bin/emake
	rm -f /usr/local/bin/etmpl
	rmdir --ignore-fail-on-non-empty -p /usr/local/lib/emake/ebin
