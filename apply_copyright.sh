for i in ./R/*.R # or whatever other pattern...
do
  echo $i
  if ! /usr/bin/grep -q Copyright $i
  then
    /usr/bin/cat copyright.txt $i >$i.new && /usr/bin/mv $i.new $i
  fi
done