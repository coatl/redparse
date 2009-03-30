p <<end
#{compile_body}#{outvar}
end

p <<end
#{compile_body}
#{outvar}
end

p <<end
#{compile_body}\
#{outvar}
end

p <<end
#{compile_body}\
\
\
\
\
\
#{outvar}
end

p "\
"

p "\
#{x}
"

p "
#{x}a\
"

p "
#{x}\
b"

p "\
#{x}\
"

p "
#{x}\
"

p "
#{x}"



p "
#$x\
"

p "
#@x\
"

p "
#@@x\
"

p "
#$x"

p "
#@x"

p "
#@@x"

p " #$a #@b #@@c "
p "#$a#@b#@@c"
