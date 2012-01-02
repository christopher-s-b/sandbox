def one_two_three
  puts "1"
  puts "2"
  puts "3"
end

def one_two
  puts "1"
  puts "2"
  callcc { |k| return }
  puts "3"
end

def one_two_capture_three
  puts "1"
  puts "2"
  callcc { |k| return k }
  puts "3"
end
