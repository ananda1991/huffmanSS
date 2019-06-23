#!/bin/bash

#benchnames[0]='NONAME'
#benchnames[1]='NAME'
#counter=0;

for file in /home/biswas/gem5/simout/*.txt 
do 
#counter=$counter + 1
b=$(basename $file) 
#echo $b
b=${b::-6}
echo $b
input="$file"

while read line
do
	
	if [[ $line = *"system.cpu.dcache.tags.l1d_mono_lat"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.l1d_reads"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.l1d_trail_lat"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.l1d_writes"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t0_hits"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t0_reads"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t0_writes"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t1_hits"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t1_reads"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t1_writes"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t2_hits"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t2_reads"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t2_writes"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t3_hits"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t3_reads"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi
	if [[ $line = *"system.cpu.dcache.tags.t3_writes"* ]]; then
		echo "$line"
		echo "$line" >> $file.csv
	fi

done < $file
tr -s '   *' ',' <$file.csv >$file.out.csv		
done

for file in /home/biswas/gem5/simout/*.out.csv
do
	out=$file
	out=${out::-12}
	echo $out
	cat < $file | cut -c 24- | sed 's/#.*$//' | tail -n 5 > $out.csv
	rm $file
done
#sed -n '72p;73p;74p;75p;83,94p;97p' $file >> $b.csv

