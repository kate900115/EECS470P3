#!/bin/bash

# Setup your configuration variables


# Build a fresh simv
make_out=$(make simv 2>&1)
# Redirect stdout->stderr and capture
# both in the variable make_out
if [ $? -ne 0 ]; then
# if make fails...
echo "Building simv failed!"
echo "$make_out"
# These quotes are important to
# preserve whitespace in the output
fi

# to generate the ground truth
mkdir original_output
for file in `ls p3_original/test_progs`; do
	file=$(echo $file | cut -d'.' -f1)
	cd p3_original
        echo "Assemble original $file"
	./vs-asm < test_progs/$file.s >program.mem
	#store the assembly language into memory 
	echo "Running original $file"
	#simulate modules
        make > make_result.out
        cd ..
        echo "saving ${file} output"; 
	#save the writeback.out and program.out into another file
	cp p3_original/writeback.out original_output/$file.writeback.out
	cp p3_original/pipeline.out original_output/$file.pipeline.out
        grep '@@@' p3_original/program.out > original_output/$file.program.out
	echo;  
done

mkdir output_data
# Run all the test cases
for file in `ls test_progs`; do
	file=$(echo $file | cut -d'.' -f1)
        echo "Assemble $file"
	./vs-asm < test_progs/$file.s >program.mem
	#store the assembly language into memory 
	echo "Running $file"
 	#simulate modules
	make > make_result.out
# Validate their output
        echo "saving ${file} output"; 
	#save the writeback.out and program.out into another file
	cp writeback.out output_data/$file.writeback.out
	cp pipeline.out output_data/$file.pipeline.out
        grep '@@@' program.out > output_data/$file.program.out
	echo;
done



# Print the results!
mkdir check_result
for file in `ls output_data`; do
	file=$(echo $file | cut -d'.' -f1)
        echo "Check $file.writeback.out with the ground truth version."
	echo "Check $file.program.out with the ground truth version."
	
	diff  -a original_output/$file.writeback.out output_data/$file.writeback.out >  stderr>&stdout 
	#Check whether the writeback.out files match exactly with the ground truth version
	if [ $? -eq 0 ]
	then
		diff -a original_output/$file.program.out output_data/$file.program.out > stderr>&stdout
	#Check whether program.out files match exactly with the ground truth version
		if [ $? -eq 0 ] 
		then
    			echo "testcase passed!";
        	else
			echo "testcase failed!";
		fi
	else
		echo "testcase failed!"
	fi
	echo;  
done

#diff original_output/$file.writeback.out output_data/$file.writeback.out > D
	#if [ "$D" == "" ]

#	diff original_output/$file.writeback.out output_data/$file.writeback.out > check_result/$file.writeback
#	diff original_output/$file.program.out output_data/$file.program.out > check_result/$file.program
#	if diff original_output/$file.writeback.out output_data/$file.writeback.out &> /dev/null ; then
#		if diff original_output/$file.program.out output_data/$file.program.out &> /dev/null ; then  		
#			echo "testcase passed.";
#		else
#               	echo "testcase failed.";
#		fi
#	else
#    		echo "testcase failed."
#	fi

