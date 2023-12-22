#! /usr/bin/env bash

set -euo pipefail

WORKFLOW_FILE=$1
echo "Workflow file is $WORKFLOW_FILE"

REF=$2
echo "Ref is $REF"

TOKEN=$GITHUB_TOKEN
GH_USER="cardano-foundation"
REPO="cardano-wallet"

WORKFLOW_ID=$(tr -dc '0-9' </dev/urandom | head -c 16 ; echo)
echo "Workflow id is $WORKFLOW_ID"

NOW=$(date +"%Y-%m-%dT%H:%M")
LATER=$(date -d "-5 minutes" +"%Y-%m-%dT%H:%M")
DATE_FILTER=$(echo "$NOW-$LATER")

JSON=""
printf -v JSON '{"ref": "%s", "inputs": {"id": "%s"}}' "$REF" "$WORKFLOW_ID"

echo "Triggering workflow $WORKFLOW_FILE with id $WORKFLOW_ID"
curl -s \
    -X POST \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer $TOKEN" \
    "https://api.github.com/repos/$GH_USER/$REPO/actions/workflows/$WORKFLOW_FILE/dispatches" \
    -d "$JSON"

INFO="null"
COUNT=1
ATTEMPTS=10
CHECK="0"

echo "Waiting for workflow to start to retrieve run id..."
until [ $CHECK -eq "$WORKFLOW_ID" ] || [ $COUNT -eq "$ATTEMPTS" ]

do

    echo -e "$(( COUNT++ ))..."
    sleep 5s

    # get latest workflows run ids
    INFO=$(curl -s \
        -X GET \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $TOKEN" \
        "https://api.github.com/repos/$GH_USER/$REPO/actions/runs?created:<$DATE_FILTER" \
            | jq -r '.workflow_runs[].id' \
            | grep -m1 ""\
        )

    # get the run id that matches the workflow identifier
    CHECK=$(curl -s \
        -X GET \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $TOKEN" \
        "https://api.github.com/repos/$GH_USER/$REPO/actions/runs/$INFO/jobs" \
            | jq -r '.jobs[].steps[].name' \
            | grep -m1 "$WORKFLOW_ID" || echo "0"
        )
done

echo "Run id is $INFO"

status="null"

echo "Waiting for workflow run to complete..."
until [ $status == "completed" ] || [ $status == "failure" ] || [ $status == "success" ]

do
    status=$(curl -s \
        -X GET \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $TOKEN" \
        "https://api.github.com/repos/$GH_USER/$REPO/actions/runs/$INFO" \
            | jq -r '.status'\
        )
    sleep 10s
done

case $status in
    "completed")
        echo "Workflow completed"
        ;;
    "failure")
        echo "Workflow failed"
        exit 1
        ;;
    "success")
        echo "Workflow succeeded"
        ;;
    *)
        echo "Workflow status unknown: $status"
        exit 1
        ;;
esac