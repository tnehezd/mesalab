#!/bin/bash

set -e

echo "Stopping python processes (manual step recommended!)"

echo "Removing caches..."
find . -type d -name "__pycache__" -exec rm -rf {} +
find . -name "*.pyc" -delete

echo "Removing build artifacts..."
rm -rf build dist *.egg-info

echo "Uninstalling package..."
pip uninstall -y mesalab || true

echo "Clearing pip cache..."
pip cache purge

echo "Reinstalling editable package..."
pip install -e . --no-cache-dir

echo "DONE. NOW RESTART PYTHON/JUPYTER!"