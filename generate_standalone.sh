#!/usr/bin/env bash
set -e

ACE_SRC="/c/soft/GitHub/ACE/src"
TARGET_SRC="/c/soft/GitHub/src"
TARGET_DIR="/c/soft/GitHub"

echo "Copying src files..."
rm -rf "$TARGET_SRC"
mkdir -p "$TARGET_SRC"
cp "$ACE_SRC"/* "$TARGET_SRC/"

echo "Removing standalone files from target src..."
rm -f "$TARGET_SRC/z_ace_standalone.prog.abap"
rm -f "$TARGET_SRC/z_ace_standalone.prog.xml"

echo "Removing extra programs from target src (keep only entrypoint z_ace)..."
find "$TARGET_SRC" -maxdepth 1 -name '*.prog.abap' ! -name 'z_ace.prog.abap' -delete
find "$TARGET_SRC" -maxdepth 1 -name '*.prog.xml'  ! -name 'z_ace.prog.xml'  -delete

echo "Running abapmerge..."
cd "$TARGET_DIR"
abapmerge -f src/z_ace.prog.abap -o z_ace_standalone.prog.abap

echo "Copying result back to ACE/src..."
cp "$TARGET_DIR/z_ace_standalone.prog.abap" "$ACE_SRC/z_ace_standalone.prog.abap"

echo "Restoring header comments..."
# Extract comment/blank lines from z_ace.prog.abap after line 1, stop at first code line
header=$(awk 'NR==1{next} /^[[:space:]]*($|")/{print; next} {exit}' "$ACE_SRC/z_ace.prog.abap")
# Insert header between line 1 and the rest of standalone
{ head -1 "$ACE_SRC/z_ace_standalone.prog.abap"; echo "$header"; tail -n +2 "$ACE_SRC/z_ace_standalone.prog.abap"; } \
  > /tmp/z_ace_standalone_fixed.abap
cp /tmp/z_ace_standalone_fixed.abap "$ACE_SRC/z_ace_standalone.prog.abap"

echo "Rewriting += / -= to 7.50-compatible form (standalone only)..."
# x += y.  -> x = x + y.   |   x -= y. -> x = x - y.
# lhs may contain field/deref/field-symbol chars: A-Za-z0-9_ < > ~ -
sed -E -i \
  -e 's/([A-Za-z0-9_<>~-]+)[[:space:]]*\+=[[:space:]]*([^.]*)\./\1 = \1 + \2./g' \
  -e 's/([A-Za-z0-9_<>~-]+)[[:space:]]*-=[[:space:]]*([^.]*)\./\1 = \1 - \2./g' \
  "$ACE_SRC/z_ace_standalone.prog.abap"

# Fail loudly if any compound assignment slipped through (would break on 7.50)
if grep -nE "[[:alnum:]_>)]+[[:space:]]*[+-]=[[:space:]]" "$ACE_SRC/z_ace_standalone.prog.abap"; then
  echo "ERROR: += / -= still present in standalone (7.50 will not compile)."
  exit 1
fi

echo "Done."
