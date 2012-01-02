require 'io/wait'

$delay = 1

$pipes = {}
def query_async(query, &callback)
  pipe = IO.popen("sleep #{$delay}; echo #{query}")
  $pipes[pipe]=callback
  # $pipes.push({:pipe => pipe, :callback => callback})
end

def poll_pipes
  $pipes.reject! do |pipe, callback|
    #pipe = item[:pipe]
    if pipe.ready? #&& pipe.eof?  #??
      data = pipe.read #??
      #callback = item[:callback]
      callback.call data
      return true #prune this item
    end
    false #keep this item
  end
end

$key_handlers = {}
def poll_keyboard
  if STDIN.ready?
    char = STDIN.getc
    #puts "got char `#{char}`"
    exit if char == 113 #q
    #query_async("#{char}") { |item| puts item }
    $key_handlers[char].call if $key_handlers.has_key? char
  end
end

def reactor
  system("stty raw -echo")
  while true
    poll_pipes
    poll_keyboard
  end
end

