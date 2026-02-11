#!/usr/bin/env bash
# Test selective display, outline mode, and code folding rendering
# Usage: ./test/neomacs/run-selective-display-test.sh

set -e

cd "$(dirname "$0")/../.."

LOG=/tmp/selective-display-test.log
SCREENSHOT=/tmp/selective-display-screenshot.png

echo "=== Selective Display / Folding Test ==="
echo "Starting Emacs..."

RUST_LOG=neomacs_display=debug DISPLAY=:0 ./src/emacs -Q \
    -l test/neomacs/selective-display-test.el 2>"$LOG" &
EMACS_PID=$!

echo "Emacs PID: $EMACS_PID"
echo "Waiting for window to appear..."
sleep 5

# Find emacs window
WIN_ID=$(DISPLAY=:0 xdotool search --name "emacs" 2>/dev/null | head -1)
if [ -z "$WIN_ID" ]; then
    echo "ERROR: Could not find Emacs window"
    kill $EMACS_PID 2>/dev/null || true
    exit 1
fi

echo "Found window: $WIN_ID"

# Activate window
DISPLAY=:0 xdotool windowactivate --sync "$WIN_ID"
sleep 1

# Run all tests
echo "Running all selective-display tests (pressing 'a')..."
DISPLAY=:0 xdotool key --window "$WIN_ID" a
sleep 35

# Take screenshot
if command -v import &>/dev/null; then
    echo "Taking screenshot..."
    DISPLAY=:0 import -window "$WIN_ID" "$SCREENSHOT" 2>/dev/null && \
        echo "Screenshot: $SCREENSHOT" || echo "Screenshot failed"
else
    echo "ImageMagick not available, skipping screenshot"
fi

# Check logs
echo ""
echo "=== Checking logs ==="
PANIC_COUNT=$(grep -ci "PANIC" "$LOG" 2>/dev/null || echo "0")
ERROR_COUNT=$(grep -ci "ERROR" "$LOG" 2>/dev/null || echo "0")
echo "PANIC entries: $PANIC_COUNT"
echo "ERROR entries: $ERROR_COUNT"

if [ "$PANIC_COUNT" -gt 0 ]; then
    echo "--- PANIC log entries ---"
    grep -i "PANIC" "$LOG" | tail -5
fi

# Cleanup
echo ""
echo "Stopping Emacs..."
kill $EMACS_PID 2>/dev/null || true
wait $EMACS_PID 2>/dev/null || true

echo ""
if [ "$PANIC_COUNT" -gt 0 ]; then
    echo "RESULT: FAIL (panics detected)"
    exit 1
else
    echo "RESULT: PASS"
fi
echo "Full log: $LOG"
