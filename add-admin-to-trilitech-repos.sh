#!/bin/bash
set -euo pipefail

# Script to add mathiasbourgoin as admin to trilitech repos corresponding to ecadlabs repos
# Usage: ./add-admin-to-trilitech-repos.sh [--dry-run]

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
	DRY_RUN=true
	echo "🔍 DRY RUN MODE - No changes will be made"
	echo ""
fi

TARGET_USER="mathiasbourgoin"
SOURCE_ORG="ecadlabs"
TARGET_ORG="trilitech"

echo "Fetching all repositories from $SOURCE_ORG..."
ECADLABS_REPOS=$(gh repo list "$SOURCE_ORG" --limit 1000 --json name --jq '.[].name')

if [[ -z "$ECADLABS_REPOS" ]]; then
	echo "❌ No repositories found in $SOURCE_ORG"
	exit 1
fi

REPO_COUNT=$(echo "$ECADLABS_REPOS" | wc -l)
echo "Found $REPO_COUNT repositories in $SOURCE_ORG"
echo ""

SUCCESS_COUNT=0
SKIP_COUNT=0
ERROR_COUNT=0

for repo in $ECADLABS_REPOS; do
	TRILITECH_REPO="$TARGET_ORG/$repo"

	echo "Processing: $repo"

	# Skip taquito repo
	if [[ "$repo" == "taquito" ]]; then
		echo "  ⏭️  Skip: Excluding taquito repo (hardcoded skip)"
		SKIP_COUNT=$((SKIP_COUNT + 1))
		echo ""
		continue
	fi

	# Check if the repo exists in trilitech
	if ! gh repo view "$TRILITECH_REPO" --json name &>/dev/null; then
		echo "  ⏭️  Skip: Repository $TRILITECH_REPO does not exist"
		SKIP_COUNT=$((SKIP_COUNT + 1))
		echo ""
		continue
	fi

	# Try to add user as admin (API is idempotent - succeeds if already added)
	if [[ "$DRY_RUN" == true ]]; then
		echo "  🔍 Would add: $TARGET_USER as admin to $TRILITECH_REPO"
		SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
	else
		# Capture the output to check for permission errors
		OUTPUT=$(gh api "repos/$TRILITECH_REPO/collaborators/$TARGET_USER" -X PUT -f permission=admin 2>&1 || true)

		if [[ "$OUTPUT" == *"Must have admin rights"* ]]; then
			echo "  ⏭️  Skip: You don't have admin rights to $TRILITECH_REPO"
			SKIP_COUNT=$((SKIP_COUNT + 1))
		elif [[ "$OUTPUT" == *"Not Found"* ]] || [[ "$OUTPUT" == *"404"* ]]; then
			echo "  ❌ Failed: User $TARGET_USER not found or repo $TRILITECH_REPO not accessible"
			ERROR_COUNT=$((ERROR_COUNT + 1))
		elif [[ -z "$OUTPUT" ]] || [[ "$OUTPUT" == "{}"* ]]; then
			echo "  ✅ Success: $TARGET_USER added/updated as admin to $TRILITECH_REPO"
			SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
		else
			echo "  ⚠️  Uncertain: Got unexpected response for $TRILITECH_REPO"
			echo "     Response: ${OUTPUT:0:100}"
			ERROR_COUNT=$((ERROR_COUNT + 1))
		fi
	fi

	echo ""
done

echo "========================================"
echo "Summary:"
echo "  Total repos in $SOURCE_ORG: $REPO_COUNT"
echo "  Successfully processed: $SUCCESS_COUNT"
echo "  Skipped (not in $TARGET_ORG): $SKIP_COUNT"
echo "  Errors: $ERROR_COUNT"
echo "========================================"

if [[ "$DRY_RUN" == true ]]; then
	echo ""
	echo "This was a dry run. Run without --dry-run to make actual changes."
fi
