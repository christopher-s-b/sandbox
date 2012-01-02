def show_stack
  begin
    raise "showme"
  rescue => e
    puts e.inspect
    puts e.backtrace
    puts "\n"
  end
end
