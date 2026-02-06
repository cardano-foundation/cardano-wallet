---
name: buildkite
description: Buildkite CI: bk CLI, job logs, build triggers, failure analysis
---

# Buildkite CLI (bk) Skill

Use this skill when working with Buildkite CI for cardano-wallet.

## Setup

Set organization before first use:
```bash
bk use cardano-foundation
```

Check current config:
```bash
bk whoami
```

## Common Pipelines

- `cardano-wallet` - Main wallet pipeline

## List Builds

```bash
# List recent builds for a branch
bk build list --pipeline cardano-wallet --branch bump-cardano-addresses --limit 5

# List builds for current branch
bk build list --pipeline cardano-wallet --branch "$(git branch --show-current)" --limit 5

# List my builds
bk build list --pipeline cardano-wallet --mine --limit 5
```

## View Build Details

```bash
# List jobs in a build (shows pass/fail status)
bk job list BUILD_NUMBER --pipeline cardano-wallet

# Example: bk job list 10282 --pipeline cardano-wallet
```

Note: `bk build view` may crash on some builds due to CLI bugs.

## Get Job Logs

The CLI doesn't have a direct logs command. Use the API and **always save to a file first** - logs are large and contain special formatting that causes issues if processed inline.

**Important:** The API path should NOT include `/organizations/ORG_NAME` - the organization is already set via `bk use`. Use `/pipelines/...` directly.

### Step 1: Save raw JSON

```bash
bk api "/pipelines/cardano-wallet/builds/BUILD_NUMBER/jobs/JOB_UUID/log" > /tmp/bk-log.json
```

### Step 2: Extract and clean the content

Run these commands in sequence (piping fails due to special characters):

```bash
# Extract content field
jq -r '.content' /tmp/bk-log.json > /tmp/bk-raw.log

# Remove ANSI codes and Buildkite OSC timestamps
sed 's/\x1b\[[0-9;]*m//g; s/\x1b_[^\x07]*\x07/\n/g' /tmp/bk-raw.log > /tmp/bk-step1.log

# Convert carriage returns to newlines (progress indicators overwrite lines)
tr '\r' '\n' < /tmp/bk-step1.log > /tmp/bk-step2.log

# Remove nix progress lines and blank lines, dedupe
grep -v '^\[.*built.*\[K$' /tmp/bk-step2.log | grep -v '^[[:space:]]*$' | uniq > /tmp/bk-job.log
```

### Step 3: Find test failures

For Haskell hspec test output:
```bash
# Find failed tests (marked with ✘)
grep '✘' /tmp/bk-job.log

# Find the Failures section with details
grep -A 100 "Failures:" /tmp/bk-job.log | head -150

# Find test summary
grep -E "examples|failures|pending" /tmp/bk-job.log | tail -5
```

## Haskell Log Parser

For more robust parsing of Buildkite JSON logs, use the Haskell parser at `~/.claude/scripts/ParseBuildkiteLog.hs`.

```bash
# Parse a saved JSON log file
nix-shell -p "haskellPackages.ghcWithPackages (p: [p.aeson p.text])" \
  --run "runhaskell ~/.claude/scripts/ParseBuildkiteLog.hs /tmp/bk-log.json"

# Or within a Haskell project's nix develop
nix develop --command runhaskell -O0 ~/.claude/scripts/ParseBuildkiteLog.hs /tmp/bk-log.json

# Save raw JSON first
bk api "/pipelines/cardano-wallet/builds/BUILD_NUMBER/jobs/JOB_UUID/log" > /tmp/bk-log.json
```

The parser:
- Extracts `content`, `url`, `size` from JSON structure
- Removes Buildkite OSC timestamp markers (`\ESC_bk;t=...\BEL`)
- Removes ANSI color codes
- Handles terminal cursor overwrites (carriage returns)
- Normalizes line endings

## Artifacts

```bash
# List artifacts for a build
bk artifacts list BUILD_NUMBER --pipeline cardano-wallet

# Download artifact by UUID
bk artifacts download ARTIFACT_UUID
```

## Trigger New Build

```bash
# Trigger build on a branch (-y skips confirmation prompt)
bk build new --pipeline cardano-wallet --branch bump-cardano-addresses --commit HEAD -m "Rebuild after fixes" -y
```

## Job Management

```bash
# Retry a failed job
bk job retry JOB_UUID

# Cancel a job
bk job cancel JOB_UUID

# Unblock a manual job
bk job unblock JOB_UUID
```

## Watch Build Progress

```bash
# Watch build in real-time
bk build watch BUILD_NUMBER --pipeline cardano-wallet
```

## Typical Workflow

1. Check build status:
   ```bash
   bk build list --pipeline cardano-wallet --branch YOUR_BRANCH --limit 3
   ```

2. Find failed jobs:
   ```bash
   bk job list BUILD_NUMBER --pipeline cardano-wallet | grep -E "failed|broken"
   ```

3. Get logs for failed job (save and clean):
   ```bash
   bk api "/pipelines/cardano-wallet/builds/BUILD_NUMBER/jobs/JOB_UUID/log" > /tmp/bk-log.json
   jq -r '.content' /tmp/bk-log.json > /tmp/bk-raw.log
   sed 's/\x1b\[[0-9;]*m//g; s/\x1b_[^\x07]*\x07/\n/g' /tmp/bk-raw.log > /tmp/bk-step1.log
   tr '\r' '\n' < /tmp/bk-step1.log > /tmp/bk-step2.log
   grep -v '^\[.*built.*\[K$' /tmp/bk-step2.log | grep -v '^$' | uniq > /tmp/bk-job.log
   ```

4. Find test failures:
   ```bash
   grep '✘' /tmp/bk-job.log                      # Failed tests
   grep -A 100 "Failures:" /tmp/bk-job.log       # Failure details
   ```

5. After fixing, trigger rebuild:
   ```bash
   bk build new --pipeline cardano-wallet --branch YOUR_BRANCH --commit HEAD -m "Fix: description" -y
   ```
