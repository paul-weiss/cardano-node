#!/usr/bin/env bash

set -e
# set -x

# This script will initiate the transition to protocol version 9 (Conway).

# It will also set up a working stake pool (including delegating to it).

# You need to provide the current epoch as a positional argument (the Shelley
# update system requires this to be included in the update proposal).


# In order for this to be successful, you need to already be in protocol version
# 8.
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

ROOT=example

case "$1" in
  "")
    # Handle the case where $1 is empty
    cardano-cli query tip --testnet-magic 42 --socket-path example/node-spo1/node.sock --out-file $ROOT/tip.json
    CURREPOCH=$(jq '.epoch' $ROOT/tip.json)
    EPOCH=$((CURREPOCH + 1))
    ;;
  *)
    # Handle other cases
    EPOCH="$1"
    ;;
esac

echo "Hardfork target epoch number: $EPOCH"

cardano-cli query protocol-parameters --socket-path $ROOT/node-spo1/node.sock --testnet-magic 42 --out-file $ROOT/protocol-parameters.json



CURRENTVERSION=$(jq '.protocolVersion.major' $ROOT/protocol-parameters.json)
VERSION=$((CURRENTVERSION + 1))
echo "Version: $VERSION"

# Get current epoch



COINS_IN_INPUT=1000000000
mkdir -p ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

updateproposalfp="update-proposal-conway"
cardano-cli babbage governance action create-protocol-parameters-update \
            --out-file $updateproposalfp \
            --epoch ${EPOCH} \
            --genesis-verification-key-file $ROOT/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file $ROOT/genesis-keys/genesis2.vkey \
            --protocol-major-version ${VERSION} \
            --protocol-minor-version 0


# Now we'll construct one whopper of a transaction that does everything
# just to show off that we can, and to make the script shorter

# We'll transfer all the funds to the user1, which delegates to pool1
# We'll register certs to:
#  1. register the pool-owner1 stake address
#  2. register the stake pool 1
#  3. register the user1 stake address
#  4. delegate from the user1 stake address to the stake pool
# We'll include the update proposal

utxoaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file "example/utxo-keys/utxo1.vkey")

utxo1=$ROOT/utxo1.json

cardano-cli query utxo --testnet-magic 42 --socket-path $ROOT/node-spo1/node.sock --address $utxoaddr --out-file $utxo1

txin1=$(jq -r 'keys[0]' $utxo1)

cardano-cli transaction build \
            --babbage-era \
            --tx-in ${txin1}\
            --testnet-magic 42 \
            --tx-out $utxoaddr+1000000 \
            --socket-path $ROOT/node-spo1/node.sock \
            --change-address $utxoaddr \
            --update-proposal-file $updateproposalfp \
            --out-file $ROOT/tx2.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

cardano-cli transaction sign \
            --signing-key-file $ROOT/utxo-keys/utxo1.skey \
            --signing-key-file $ROOT/delegate-keys/delegate1.skey \
            --signing-key-file $ROOT/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  $ROOT/tx2.txbody \
            --out-file      $ROOT/tx2.tx


cardano-cli transaction submit --socket-path $ROOT/node-spo1/node.sock --tx-file $ROOT/tx2.tx --testnet-magic 42

sed -i $ROOT/configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 8/LastKnownBlockVersion-Major: 9/' \



echo "Restart the nodes now to endorse the update."
# Look for:
# Event: LedgerUpdate (HardForkUpdateInEra S (S (S (S (S (Z (WrapLedgerUpdate {unwrapLedgerUpdate = ShelleyUpdatedProtocolUpdates [ProtocolUpdate {protocolUpdateProposal = UpdateProposal {proposalParams = PParamsUpdate (BabbagePParams {bppMinFeeA = SNothing, bppMinFeeB = SNothing, bppMaxBBSize = SNothing, bppMaxTxSize = SNothing, bppMaxBHSize = SNothing, bppKeyDeposit = SNothing, bppPoolDeposit = SNothing, bppEMax = SNothing, bppNOpt = SNothing, bppA0 = SNothing, bppRho = SNothing, bppTau = SNothing, bppProtocolVersion = SJust (ProtVer {pvMajor = Version 9, pvMinor = 0}), bppMinPoolCost = SNothing, bppCoinsPerUTxOByte = SNothing, bppCostModels = SNothing, bppPrices = SNothing, bppMaxTxExUnits = SNothing, bppMaxBlockExUnits = SNothing, bppMaxValSize = SNothing, bppCollateralPercentage = SNothing, bppMaxCollateralInputs = SNothing}), proposalVersion = Just (ProtVer {pvMajor = Version 9, pvMinor = 0}), proposalEpoch = EpochNo 2}, protocolUpdateState = UpdateState {proposalVotes = [KeyHash "aebfb5c4429678ed5df8658864b04810e41287ae09c1296bfde0491b",KeyHash "2c72612ebc44b8ea4d2439d6d2e910ab0938056f5c47350aef7fb42e"], proposalReachedQuorum = True}}]})))))))
# Event: LedgerUpdate (HardForkUpdateTransitionConfirmed <EraIndex Babbage> <EraIndex Conway> (EpochNo 2))
# Your problem is here:
# [31m[system76:cardano.node.IpSubscription:Error:63][0m [2024-05-14 20:18:45.90 UTC] IPs: 0.0.0.0:0 [::]:0 [127.0.0.1:3002,127.0.0.1:3003] Application Exception: 127.0.0.1:3002 HeaderError (At (Block {blockPointSlot = SlotNo 1069, blockPointHash = 6b86ac834df09dcdeeead0b69243473d630acec0526708af08488e410e7d537f})) (HeaderEnvelopeError (OtherHeaderEnvelopeError (HardForkEnvelopeErrFromEra S (S (S (S (S (S (Z (WrapEnvelopeErr {unwrapEnvelopeErr = ObsoleteNode (Version 9) (Version 8)})))))))))) (Tip (SlotNo 971) 1941177c6bc4aba3240c1f6580d7022c5a6381f8bf535317cc0dc116ae30e4ef (BlockNo 86)) (Tip (SlotNo 971) 1941177c6bc4aba3240c1f6580d7022c5a6381f8bf535317cc0dc116ae30e4ef (BlockNo 86))
          