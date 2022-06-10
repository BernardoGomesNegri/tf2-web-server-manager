FRONTEND-STATIC = $(wildcard frontend/static/*)
ELM-SRC = $(wildcard frontend/src/*.elm)
HS-SRC = $(wildcard *.cabal) $(wildcard app/*.hs)
BUILD = out
BUILD-FRONTEND = $(BUILD)/frontend/
ELM-DIR = frontend
ELM = elm make
CABAL = cabal
ELM-OPTS =
CABAL-OPTS =

ifeq ($(OS),Windows_NT)
	EXE-NAME=tf2-server-manager.exe
else
	EXE-NAME=tf2-server-manager
endif

# For every file on the ELM-SRC list (with its suffix replaced by .js), remove everything but
# the filename, and join it with $(BUILD-FRONTEND)
ELM-OUT := $(foreach file,$(ELM-SRC:.elm=.js),$(join $(BUILD-FRONTEND),$(notdir $(file))))
STATIC-OUT = $(foreach file,$(FRONTEND-STATIC),$(join $(BUILD-FRONTEND),$(notdir $(file))))

FRONTEND= $(ELM-OUT) $(STATIC-OUT)

.SUFFIXES: 

all: $(FRONTEND) $(BUILD)/$(EXE-NAME)

final: ELM-OPTS += --optimize
final: CABAL-OPTS += -O2 --enable-executable-strip
final: clean all
	strip $(BUILD)/$(EXE-NAME)

dist: final
	zip -r out/tf2-web-server-manager.zip out

clean:
	rm -rf $(BUILD)
	rm -rf dist-newstyle

$(BUILD):
	mkdir -p $(BUILD)
	mkdir -p $(BUILD-FRONTEND)

$(BUILD)/$(EXE-NAME): $(HS-SRC) $(BUILD)
	$(CABAL) build $(CABAL-OPTS)
	$(CABAL) install $(CABAL-OPTS) --installdir=$(BUILD) --install-method=copy --overwrite-policy=always

# This is the only way I found to maintain shell variables between
# command execution. GNU Make actually runs each command in a separate
# shell process
$(BUILD-FRONTEND)%.js: $(ELM-DIR)/src/%.elm $(BUILD)
	ELM_SRC_REL=$$(realpath --relative-to $(ELM-DIR) $<) &&\
	ELM_OUT_REL=$$(realpath --relative-to $(ELM-DIR) $@) &&\
	ELM_RETURN=$$(realpath --relative-to $(ELM-DIR) .) &&\
	cd $(ELM-DIR) &&\
	$(ELM) $$ELM_SRC_REL --output=$$ELM_OUT_REL $(ELM-OPTS) &&\
	cd $(ELM-RETURN)

$(BUILD-FRONTEND)%: frontend/static/% $(BUILD)
	cp $< $@
