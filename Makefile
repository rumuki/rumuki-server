include ../Helpers.mk

# Use git commit hashes to version docker images
GLOBAL_STACK_ROOT					:= $(shell stack path --stack-root)
LOCAL_BIN_PATH						:= $(shell stack path --local-bin)
ROOT_DIR									:= $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
RUN_CMD										:= docker run -d \
															--memory=512M \
															-v $(GLOBAL_STACK_ROOT):/home/builder/.stack \
															-v $(ROOT_DIR)/.stack-work:/opt/server/.stack-work \
															rumuki-build:latest \
															tail -f /dev/null

SRC_FOLDER								:= src
CONFIG_FOLDER							:= config
SRC_FILES									:= $(shell find $(SRC_FOLDER) -type f) $(shell find $(CONFIG_FOLDER) -type f)

dist: dist/x86_64-linux/server

.PHONY: image
image: DOCKER_REPO_NAME			:= rumuki-server
image: DOCKER_IMAGE_VERSION	:= git-$(shell git rev-parse HEAD | cut -c1-9)
image: UPGRADE_COMMAND			:= sed -i '' 's/rumuki-server:.*/rumuki-server:$(DOCKER_IMAGE_VERSION)/g' ../$(SERVER_CONFIG_FILE)
image:
	@$(UPLOAD_IMAGE_GCR_AND_UPGRADE)

dist/x86_64-linux/server: server.cabal stack.yaml $(SRC_FILES)
	$(call LOG, Building, binary)
	@mkdir -p $(@D)
	stack --docker install
	cp `stack path --docker --local-bin`/server $@

.PHONY: setup
setup:
	@stack setup
	@stack install hlint

.PHONY: test
test:
	@stack --docker test

.PHONY: lint
lint:
	@stack exec -- hlint src
