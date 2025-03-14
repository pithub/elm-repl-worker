
# Variables

DIST_IO_CLIENT=dist/io.js
DIST_TEA_CLIENT=dist/tea.js
DIST_WORKER=dist/worker.js


# Targets

.PHONY: worker clean

all: worker

worker: src
	elm make src/Repl/Worker.elm --output $(DIST_WORKER)
	elm make src/Repl/Client/IO.elm --output $(DIST_IO_CLIENT)
	elm make src/Repl/Client/TEA.elm --output $(DIST_TEA_CLIENT)

clean:
	rm -f $(DIST_TEA_CLIENT) $(DIST_IO_CLIENT) $(DIST_WORKER)
