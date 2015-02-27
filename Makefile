
all:
	mkdir -p ebin
	erlc -o ebin/ src/*.erl

clean:
	rm -f ebin/*.beam
	rmdir --ignore-fail-on-non-empty -p ebin

install:
	mkdir -p /usr/local/lib/emake/ebin
	install -p ebin/emake.beam /usr/local/lib/emake/ebin
	install -p priv/eapp /usr/local/bin
	install -p priv/emake /usr/local/bin
	install -p priv/etmpl /usr/local/bin
	install -p -m 644 priv/emake_bash_completion /etc/bash_completion.d/emake
	install -p -m 644 priv/etmpl_bash_completion /etc/bash_completion.d/etmpl

uninstall:
	rm -f /usr/local/lib/emake/ebin/emake.beam
	rm -f /usr/local/bin/eapp
	rm -f /usr/local/bin/emake
	rm -f /usr/local/bin/etmpl
	rm -f /etc/bash_completion.d/emake
	rm -f /etc/bash_completion.d/etmpl
	rmdir --ignore-fail-on-non-empty -p /usr/local/lib/emake/ebin

run:
	erl -pa /usr/local/lib/emake/ebin ebin
