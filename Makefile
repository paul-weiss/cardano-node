help: ## Print documentation
	@{ grep -hE '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST); echo -e '$(EXTRA_HELP)'; } | sed 's/^ //' | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-33s\033[0m %s\n", $$1, $$2}'

include lib.mk
include nix.mk

PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)

## One of:  shey alra mary alzo bage
ERA     ?= bage

PROFILE ?= default-${ERA}
BACKEND ?= supervisor
REV     ?= master
ITER    ?=
BATCH   ?=
ARGS    ?=
CMD     ?=
RUN     ?=

lint hlint: ## Run the CI version of hlint
	nix build --no-link '.#checks/hlint' --cores 0
haddock-hoogle haddocks hoogle:
	if test -z "$$IN_NIX_SHELL"; then nix-shell --run 'cabal update && cabal haddock all --haddock-hoogle'; else cabal update && cabal haddock all --haddock-hoogle; fi
host-hlint: ## Run the system (not Nix) version of hlint
	hlint bench cardano-{api,cli,node,node-capi,node-chairman,submit-api,testnet,tracer}

stylish-haskell: ## Apply stylish-haskell on all *.hs files
	@find . -type f -name "*.hs" -not -path '.git' -print0 | xargs -0 stylish-haskell -i

cabal-hashes:
	nix run .#checkCabalProject

cli node:
	cabal --ghc-options="+RTS -qn8 -A32M -RTS" build cardano-$@

trace-documentation:
	cabal run -- exe:cardano-node trace-documentation --config 'configuration/cardano/mainnet-config-new-tracing.yaml' --output-file 'doc/new-tracing/tracers_doc_generated.md'

###
### Workbench
###
workbench-ci: workbench-ci-test ci-test-auto ci-test-autonix ci-test-autonomadpodman
CI_TARGETS := hlint workbench-ci haddock-hoogle
ci:  ci-report ci-targets
ci-report:
	@echo -e "\033[34mGoals under test\033[0m:  \033[33m$(CI_TARGETS)\033[0m"
ci-targets:  $(CI_TARGETS)

workbench-internals-walkthrough:
	emn nix/workbench/doc.org

##
## Base targets:
##
shell:                                           ## Nix shell, (workbench from /nix/store), vars: PROFILE, CMD, RUN
	nix-shell -A 'workbench-shell' --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE} --argstr backendName ${BACKEND} ${ARGS} ${if ${CMD},--command "${CMD}"} ${if ${RUN},--run "${RUN}"}
shell-dev shell-prof shell-nix: shell
shell-nix: ARGS += --arg 'useCabalRun' false ## Nix shell, (workbench from Nix store), vars: PROFILE, CMD, RUN
shell-prof: ARGS += --arg 'profiling' '"space"'  ## Nix shell, everything Haskell built profiled

analyse: RUN := wb analyse std ${TAG}
analyse: shell

list-profiles:                                   ## List workbench profiles
	nix build .#all-profiles-json && cat result
show-profile:                                    ## NAME=profile-name
	@test -n "${NAME}" || { echo 'HELP:  to specify profile to show, add NAME=profle-name' && exit 1; }
	nix build .#all-profiles-json --json --option substitute false | jq '.[0].outputs.out' -r | xargs jq ".\"${NAME}\" | if . == null then error(\"\n###\n### Error:  unknown profile: ${NAME}  Please consult:  make list-profiles\n###\") else . end"
ps:                                              ## Plain-text list of profiles
	@nix build .#workbench.profile-names-json --json | jq '.[0].outputs.out' -r | xargs jq '.[]' --raw-output

##
## Profile-based cluster shells (autogenerated targets)
##
PROFILES_BASE             := default default-p2p plutus plutus-secp-ecdsa plutus-secp-schnorr oldtracing idle tracer-only
PROFILES_FAST             := fast fast-p2p fast-plutus fast-notracer fast-oldtracing
PROFILES_CI_TEST          := ci-test ci-test-p2p ci-test-plutus ci-test-notracer ci-test-rtview ci-test-dense10
PROFILES_CI_BENCH         := ci-bench ci-bench-p2p ci-bench-plutus ci-bench-plutus-secp-ecdsa ci-bench-plutus-secp-schnorr ci-bench-notracer ci-bench-rtview
PROFILES_TRACE_BENCH      := trace-bench trace-bench-notracer trace-bench-oldtracing trace-bench-rtview
PROFILES_TRACE_FULL       := trace-full trace-full-rtview
PROFILES_EPOCHTRANS       := epoch-transition
PROFILES_PLUTUSCALL       := plutuscall-loop-plain plutuscall-secp-ecdsa-plain plutuscall-secp-schnorr-plain
PROFILES_PLUTUSCALL       += plutuscall-loop-half plutuscall-secp-ecdsa-half plutuscall-secp-schnorr-half
PROFILES_PLUTUSCALL       += plutuscall-loop-double plutuscall-secp-ecdsa-double plutuscall-secp-schnorr-double
PROFILES_MODEL		        := model-value model-secp-ecdsa-plain model-secp-ecdsa-half model-secp-ecdsa-double
PROFILES_MODEL		       	+= model-value-test
PROFILES_10               := 10 10-p2p 10-plutus 10-notracer
PROFILES_FORGE_STRESS     := forge-stress forge-stress-p2p forge-stress-plutus forge-stress-plutus-solo forge-stress-notracer forge-stress-large forge-stress-solo forge-stress-solo-xs
PROFILES_FORGE_STRESS_PRE := forge-stress-pre forge-stress-pre-plutus forge-stress-pre-notracer forge-stress-pre-solo forge-stress-pre-solo-xl forge-stress-pre-solo-xs
PROFILES_FORGE_STRESS_RTS := forge-stress-pre-rtsA4m forge-stress-pre-rtsA64m forge-stress-pre-rtsN3 forge-stress-pre-rtsA4mN3 forge-stress-pre-rtsA64mN3 forge-stress-pre-rtsxn
PROFILES_CHAINSYNC        := chainsync-early-byron  chainsync-early-byron-notracer  chainsync-early-byron-oldtracing
PROFILES_CHAINSYNC        += chainsync-early-alonzo chainsync-early-alonzo-notracer chainsync-early-alonzo-oldtracing chainsync-early-alonzo-p2p
PROFILES_VENDOR           := dish dish-plutus dish-10M dish-10M-plutus
# The dedicated P&T Nomad cluster on AWS
# Cloud version of "default", "ci-test" and "ci-bench" plus value (52+explorer)
# Not all local profiles are compatible or tested (yet) with a cloud runs
PROFILES_NOMAD_PERF       := default-nomadperf ci-test-nomadperf ci-bench-nomadperf value-nomadperf oldtracing-nomadperf ci-test-oldtracing-nomadperf ci-bench-oldtracing-nomadperf value-oldtracing-nomadperf
PROFILES_NOMAD_PERF       += plutus-nomadperf fast-nomadperf latency-nomadperf
PROFILES_NOMAD_PERF_NOP2P := default-nomadperf-nop2p oldtracing-nomadperf-nop2p ci-test-nomadperf-nop2p ci-bench-nomadperf-nop2p
PROFILES_NOMAD_PERF_NOP2P += value-nomadperf-nop2p value-oldtracing-nomadperf-nop2p plutus-nomadperf-nop2p fast-nomadperf-nop2p

LOCAL_PROFILES += $(PROFILES_BASE)
LOCAL_PROFILES += $(PROFILES_FAST)
LOCAL_PROFILES += $(PROFILES_CI_TEST)
LOCAL_PROFILES += $(PROFILES_CI_BENCH)
LOCAL_PROFILES += $(PROFILES_TRACE_BENCH)
LOCAL_PROFILES += $(PROFILES_TRACE_FULL)
LOCAL_PROFILES += $(PROFILES_EPOCHTRANS)
LOCAL_PROFILES += $(PROFILES_PLUTUSCALL)
LOCAL_PROFILES += $(PROFILES_MODEL)
LOCAL_PROFILES += $(PROFILES_10)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS_PRE)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS_RTS)
LOCAL_PROFILES += $(PROFILES_CHAINSYNC)
LOCAL_PROFILES += $(PROFILES_VENDOR)
CLOUD_PROFILES += $(PROFILES_NOMAD_PERF)
CLOUD_PROFILES += $(PROFILES_NOMAD_PERF_NOP2P)


## Note:  to enable a shell for a profile, just add its name (one of names from 'make ps') to SHELL_PROFILES

$(eval $(call define_profile_targets,           $(LOCAL_PROFILES)))
$(eval $(call define_profile_targets_nomadcloud,$(CLOUD_PROFILES)))

###
### Misc
###
clean-profile proclean:
	rm -f *.html *.prof *.hp *.stats *.eventlog

clean: clean-profile
	rm -rf logs/ socket/ cluster.*

full-clean: clean
	rm -rf db dist-newstyle $(shell find . -name '*~' -or -name '*.swp')

cls:
	echo -en "\ec"

.PHONY: cabal-hashes clean cli cls cluster-profiles help node run-test shell shell-dev stylish-haskell $(LOCAL_PROFILES) workbench-ci-test
