.PHONY: clean

ELM_FILES = $(shell find src -type f -name '*.elm')
COMPILE_TARGETS = build/app.js \
									build/index.html

all: $(COMPILE_TARGETS)

build/app.js: $(ELM_FILES)
	elm make $(ELM_FILES) --yes --warn --output $@

build/index.html: src/index.html
	cp $? $@

clean:
	rm build/*
