#!/bin/bash

# Setup
TEST_DIR=$(mktemp -d)
cp src/rotate.sh $TEST_DIR/rotate.sh
chmod +x $TEST_DIR/rotate.sh
cd $TEST_DIR

# Create 10 files with different timestamps
for i in {01..10}; do
  touch -t 2023010100$i "file_$i.log"
  # sleep 0.1 # wait to ensure mtime diff if filesystem is granular? 
  # touch -t is reliable.
done

# Run
./rotate.sh . 3

# Verify
count=$(ls *.log 2>/dev/null | wc -l)
if [ "$count" -ne "3" ]; then
  echo "FAILURE: Expected 3 files, got $count"
  ls -l
  exit 1
fi

# Check correct ones remain (08, 09, 10 are newest)
if [ ! -f "file_10.log" ] || [ ! -f "file_09.log" ] || [ ! -f "file_08.log" ]; then
  echo "FAILURE: Wrong files removed"
  ls
  exit 1
fi

echo "All tests passed"
rm -rf $TEST_DIR
