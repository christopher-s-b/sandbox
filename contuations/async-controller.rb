require 'io/wait'

$delay = 5


def query_blocking query
  p = IO.popen("sleep #{$delay}; echo #{query}")
  p.readlines
end

def main_blocking
  puts query_blocking "foo"
  puts query_blocking "bar"
end


$pipes = []
def query_async(query, &callback)
  pipe = IO.popen("sleep #{$delay}; echo #{query}")
  $pipes.push({:pipe => pipe, :callback => callback})
end

def poll_pipes
  $pipes.reject! do |item|
    pipe = item[:pipe]
    if pipe.ready? && pipe.eof?  #??
      data = pipe.read #??
      callback = item[:callback]
      $pipes.delete_at(i)
      callback.call data
      return true #prune this item
    end
    false #keep this item
  end
end

def poll_keyboard
  if STDIN.ready?
    char = STDIN.getc
    puts "got char `#{char}`"
    query_async("hello world") { |item| puts item }
  end
end

def reactor
  system("stty raw -echo")
  while true
    #poll_pipes
    poll_keyboard
  end
end

