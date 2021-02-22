#!/opt/local/bin/fish

for file in *.svg
    cairosvg -o (basename $file .svg).png -s 2 $file
end

for file in *.png
    convert base.png $file -geometry +8+8 -composite $file
end

