#!/opt/local/bin/fish

for file in *.svg
    cairosvg -o (basename $file .svg).png -s 3 $file
end

