echo "/****** MINIFIED SOLUTION ******/" > SolutionMinified.scala
for file in *.scala
do
 if [ "$file" != SolutionMinified.scala ]
 then
     echo "$file"
     echo $'\n\n\n\n'"/****** $file ******/"$'\n\n' >> SolutionMinified.scala
     sed -n "/package/!p" "$file" >> SolutionMinified.scala
 fi
done
