#!/usr/bin/env bash

# Use this script to submit a governance action that adds 3 new committe members and change
# quorum to 2/3.

# This scripts uses set -x to show in terminal the commands executed by the script.
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -x
set -euo pipefail

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

sprocket() {
  if [ "$UNAME" == "Windows_NT" ]; then
    # Named pipes names on Windows must have the structure: "\\.\pipe\PipeName"
    # See https://docs.microsoft.com/en-us/windows/win32/ipc/pipe-names
    echo -n '\\.\pipe\'
    echo "$1" | sed 's|/|\\|g'
  else
    echo "$1"
  fi
}

CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
NETWORK_MAGIC=42
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$CC_DIR"

# Generate keys for new cc member

for i in {4..4}; do
  $CARDANO_CLI conway governance committee key-gen-cold \
    --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
    --cold-signing-key-file "${CC_DIR}/cold${i}-cc.skey"
done


# Get key-hashes

for i in {4..4}; do
   $CARDANO_CLI conway governance committee key-hash \
   --verification-key-file "${CC_DIR}/cold${i}-cc.vkey" > "${CC_DIR}/cold${i}-vkey.hash"
done
echo "ADD"
cat "${CC_DIR}/cold4-vkey.hash"
echo "REMOVE"
cat "${CC_DIR}/cold2-vkey.hash"
# ----------------------

# DOWNLOAD THE DUMMY PROPOSAL FILE
wget https://shorturl.at/asIJ6  -O "${TRANSACTIONS_DIR}/cc_proposal.txt"

# CREATE THE PROPOSAL

epoch=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq -r .epoch)
govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq .enactState.curPParams.govActionDeposit)
proposalHash="$($CARDANO_CLI conway governance hash --file-text ${TRANSACTIONS_DIR}/cc_proposal.txt)"

$CARDANO_CLI conway governance action update-committee \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url https://shorturl.at/asIJ6 \
  --anchor-data-hash "$proposalHash" \
  --remove-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold2-vkey.hash")" \
  --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold4-vkey.hash")" \
  --epoch $((epoch + 100)) \
  --quorum 1/2 \
  --governance-action-tx-id "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .enactState.prevGovActionIds.pgaCommittee.txId)" \
  --governance-action-index "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .enactState.prevGovActionIds.pgaCommittee.govActionIx)" \
  --out-file "${TRANSACTIONS_DIR}/new-committee.action"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/new-committee.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/new-committee-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/new-committee-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/new-committee-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/new-committee-tx.signed"

$CARDANO_CLI query tip --testnet-magic 42

sleep 5

# LETS FIND THE ACTION ID

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.txId')"
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.govActionIx')"

# LETS VOTE AS DREPS AND AS SPOS

### ----------––––––––
# DREP VOTES
### ----------––––––––

for i in {1..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-drep${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/committee-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${DREP_DIR}/drep1.skey" \
    --signing-key-file "${DREP_DIR}/drep2.skey" \
    --signing-key-file "${DREP_DIR}/drep3.skey" \
    --out-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.signed"

  $CARDANO_CLI query tip --testnet-magic 42

  sleep 5

### ----------––––––––
# SPO VOTES
### ----------––––––––

for i in {1..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-spo${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-spo${i}.vote"
done
$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/committee-spo1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-spo2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-spo3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${POOL_DIR}/cold1.skey" \
  --signing-key-file "${POOL_DIR}/cold2.skey" \
  --signing-key-file "${POOL_DIR}/cold3.skey" \
  --out-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.signed"

sleep 5
# Query gov state, looking for the votes1

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

expiresAfter=$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].expiresAfter')

echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE THE NEW CC MEMBER RATIFIED ON THE GOVERNANCE STATE"

tip=$(cardano-cli query tip --testnet-magic 42 | jq .)
current_epoch=$(echo $tip | jq .epoch)
slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

sleep $((60 * (expiresAfter - current_epoch) + slots_to_epoch_end / 10))

$CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.enactState.committee'
