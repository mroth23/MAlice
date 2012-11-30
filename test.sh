find -iname "*.alice" | while read f ; do
  echo "File: $f"
  dist/build/compile/compile $f
  echo -e "---------------------------------------------------------\n"
  cat $f
  echo -e "\n#########################################################\n"
done
