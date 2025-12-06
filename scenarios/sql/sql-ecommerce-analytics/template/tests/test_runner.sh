#!/bin/bash
rm -f test.db

# Init and Run
sqlite3 test.db < tests/schema.sql
# Run student code
sqlite3 test.db < src/queries.sql || exit 1

# Verify
output=$(sqlite3 test.db "SELECT user_id, name, total_spent FROM HighSpenders ORDER BY total_spent DESC;" -csv)

# Expected:
# 3,Charlie,200
# 1,Alice,150
# Bob should not be there (80 < 100)

lines=$(echo "$output" | wc -l)
if [ "$lines" -ne 2 ]; then
  echo "FAILURE: Expected 2 rows, got $lines"
  echo "$output"
  exit 1
fi

top=$(echo "$output" | head -n 1)
# 3|Charlie|200 (sqlite default sep is | but -csv is comma)
# 3,Charlie,200 (or 200.0)
if [[ "$top" != *"Charlie"* ]]; then
  echo "FAILURE: Top spender should be Charlie"
  echo "$top"
  exit 1
fi

echo "All tests passed"
rm -f test.db
