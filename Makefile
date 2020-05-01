# There are a couple of conventions we use so make works a little better.
#
# We sometimes want to build an entire directory of files based on one file.
# We do this for dependencies. E.g.: package.json -> node_modules.
# For these cases, we track an empty `.stamp` file in the directory.
# This allows us to keep up with make's dependency model.
#
# We also want some place to store all the excess build artifacts.
# This might be test outputs, or it could be some intermediate artifacts.
# For this, we use the `$(BUILD)` directory.
# Assuming the different tools allow us to put their artifacts in here,
# we can clean up builds really easily: delete this directory.
#
# We use some make syntax that might be unfamiliar, a quick refresher:
# make is based on a set of rules
#
# <targets>: <prerequisites> | <order-only-prerequisites>
# 	<recipe>
#
# `<targets>` are the things we want to make. This is usually a single file,
# but it can be multiple things separated by spaces.
#
# `<prerequisites>` are the things that decide when `<targets>` is out of date.
# These are also usually files. They are separated by spaces.
# If any of the `<prerequisites>` are newer than the `<targets>`,
# the recipe is run to bring the `<targets>` up to date.
#
# `<recipe>` are the commands to run to bring the `<targets>` up to date.
# These are commands like we write on a terminal.
#
# See: https://www.gnu.org/software/make/manual/make.html#Rule-Syntax
#
# `<order-only-prerequisites>` are similar to normal `<prerequisites>`
# but they don't cause a target to be rebuilt if they're out of date.
# This is mostly useful for creating directories and whatnot.
#
# See: https://www.gnu.org/software/make/manual/make.html#Prerequisite-Types
#
# And a quick refresher on some make variables:
#
# $@ - Expands to the target we're building.
# $< - Expands to the first prerequisite of the recipe.
#
# See: https://www.gnu.org/software/make/manual/make.html#Automatic-Variables
#
# `.DEFAULT_GOAL` is the goal to use if no other goals are specified.
# Normally, the first goal in the file is used if no other goals are specified.
# Setting this allows us to override that behavior.
#
# See: https://www.gnu.org/software/make/manual/make.html#index-_002eDEFAULT_005fGOAL-_0028define-default-goal_0029
#
# `.PHONY` forces a recipe to always run. This is useful for things that are
# more like commands than targets. For instance, we might want to clean up
# all artifacts. Since there's no useful target, we can mark `clean` with
# `.PHONY` and make will run the task every time we ask it to.
#
# See: https://www.gnu.org/software/make/manual/make.html#Phony-Targets
# See: https://www.gnu.org/software/make/manual/make.html#index-_002ePHONY-1

BOWER_COMPONENTS := bower_components/.stamp
BUILD := .build
DEPS := 'bower_components/purescript-*/src/**/*.purs'
NODE_MODULES := node_modules/.stamp
OUTPUT := output
PSA_ARGS := --censor-lib --stash=$(BUILD)/.psa_stash --strict
SRC := src
SRCS := $(shell find $(SRC) -name '*.purs' -type f)
TEST := test
TESTS := $(shell find $(TEST) -name '*.purs' -type f)

# Allow RTS args to be passed in to override the default behavior.
# We can invoke make like: `RTS_ARGS='+RTS -N16 -RTS' make`.
RTS_ARGS ?=
BOWER_ARGS ?=

# Colors for printing
RED := \033[0;31m

.DEFAULT_GOAL := $(BUILD)/main.js

$(BOWER_COMPONENTS): bower.json $(NODE_MODULES)
	npx bower install $(BOWER_ARGS)
	touch $@

$(BUILD):
	mkdir -p $@

$(BUILD)/main.js: $(OUTPUT)/Data.Fuzzy/index.js | $(BUILD)
	npx purs bundle \
	  $(RTS_ARGS) \
	  '$(OUTPUT)/*/index.js' \
	  '$(OUTPUT)/*/foreign.js' \
	  --main Data.Fuzzy \
	  --module Data.Fuzzy \
	  --output $@

$(BUILD)/test.js: $(OUTPUT)/Test.Main/index.js | $(BUILD)
	npx purs bundle \
	  $(RTS_ARGS) \
	  '$(OUTPUT)/*/index.js' \
	  '$(OUTPUT)/*/foreign.js' \
	  --main Test.Main \
	  --module Test.Main \
	  --output $@

$(BUILD)/test.out: $(BUILD)/test.js | $(BUILD)
	node $< | tee $@.tmp # Store output in a temp file in case of a failure.
	mv $@.tmp $@ # Move the output where it belongs.

$(NODE_MODULES): package.json
	npm install
	touch $@

$(OUTPUT)/Data.Fuzzy/index.js: $(BOWER_COMPONENTS) $(NODE_MODULES) | $(BUILD)
	npx psa $(PSA_ARGS) $(RTS_ARGS) $(DEPS) $(SRCS)

$(OUTPUT)/Test.Main/index.js: $(BOWER_COMPONENTS) $(NODE_MODULES) | $(BUILD)
	npx psa $(PSA_ARGS) $(RTS_ARGS) $(DEPS) $(SRCS) $(TESTS)

.PHONY: clean
clean:
	rm -fr \
	  $(BUILD) \
	  $(OUTPUT) \
	  bower_components \
	  node_modules

test: $(BUILD)/main.js $(BUILD)/test.out
