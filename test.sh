find -iname "*.alice" | while read f ; do
  echo "File: $f"
  dist/build/MAlice/MAlice $f
  echo -e "---------------------------------------------------------\n"
  cat $f
  echo -e "\n#########################################################\n"
done
