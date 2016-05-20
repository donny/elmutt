.PHONY: clean

ELM_FILES = $(shell find src -type f -name '*.elm')
COMPILE_TARGETS = static/app.js \
									templates/index.html

all: $(COMPILE_TARGETS)

static/app.js: $(ELM_FILES)
	elm make $(ELM_FILES) --yes --warn --output $@

templates/index.html: src/index.html
	cp $? $@

clean:
	rm build/*
