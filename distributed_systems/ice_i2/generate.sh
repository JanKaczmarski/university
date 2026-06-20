#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SLICE_DIR="${SCRIPT_DIR}/slice"
PY_OUT="${SCRIPT_DIR}/generated/python"
CPP_OUT="${SCRIPT_DIR}/generated/cpp"

mkdir -p "${PY_OUT}" "${CPP_OUT}"

echo "[generate] slice2py -> ${PY_OUT}"
slice2py --output-dir "${PY_OUT}" -I"${SLICE_DIR}" "${SLICE_DIR}/Counter.ice"

echo "[generate] slice2cpp -> ${CPP_OUT}"
slice2cpp --output-dir "${CPP_OUT}" -I"${SLICE_DIR}" "${SLICE_DIR}/Counter.ice"

echo "[generate] OK"
