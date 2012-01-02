require 'stacktrace'

def foo
  puts "foo: enter"

  the_fn = lambda do
    puts "foo: count=#{$count}"
    show_stack
    $count -= 1
    bar
  end

  res = callcc do |k|
    puts "pre"
    $k1 = k
    puts "post"
  end

  the_fn.call
end

def bar
  puts "bar: enter"
  if $count > 1
    val = $k1.call
  end
  puts "bar: val='#{val}'"
end


$k1 = nil
$count = 4

#foo
