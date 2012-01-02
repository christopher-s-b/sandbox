def main
  i = 0
  puts "before callcc statement"
  callcc do |k|
    puts "in callcc, before k.call: i=#{i}"
    k.call
    i = 1
    puts "in callcc, after k.call: i=#{i}"
  end
  puts "after callcc: i=#{i}"
end

main
