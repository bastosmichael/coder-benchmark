#!/bin/bash
set -e

# Configuration
LIMIT=10
CONCURRENCY=10
NUM_GPU=999
MAIN_GPU=0
PARALLEL_SLOTS=10

echo "Stopping any existing Ollama instance..."
pkill ollama || true
sleep 2

echo "Starting dedicated Ollama instance with OLLAMA_NUM_PARALLEL=$PARALLEL_SLOTS..."
OLLAMA_NUM_PARALLEL=$PARALLEL_SLOTS OLLAMA_MAX_LOADED_MODELS=1 ollama serve > ollama_bench.log 2>&1 &
OLLAMA_PID=$!

echo "Waiting for Ollama to start..."
sleep 5

echo "Running benchmark..."
npm run bench -- --limit $LIMIT --sequential-models --concurrency $CONCURRENCY --main-gpu $MAIN_GPU --num-gpu $NUM_GPU

echo "Benchmark complete. Cleaning up..."
kill $OLLAMA_PID
