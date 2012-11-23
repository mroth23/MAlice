find -iname "*.alice" | while read f ; do
  echo "File: $f"
  dist/build/MAlice/MAlice $f
  echo "---------------------------------------------------------"
done
