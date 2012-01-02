require 'io/wait'

$delay = 1

$pipes = {}
def query_async(query, &callback)
  pipe = IO.popen("sleep #{$delay}; echo #{query}")
  $pipes[pipe]=callback
end

$results = {}
def query_async_imperative(query)
  # save off current process execution so we can
  # resume it when our async io is finished
  callcc do |k|
    callback = Proc.new do |response|
      # when async io is finished, resume the original
      # process execution context, using global to
      # communicate the results. How can I do this without
      # a global?
      $results[query]=response
      k.call
    end
    query_async(query, &callback)
    # jump out of application logic and into the reactor.
    # we will return to application logic when the data is ready
    $reactor_continuation.call
  end
  # resume here when async io is ready 
  result = $results.delete(query)
end

def poll_pipes
  $pipes.reject! do |pipe, callback|
    if pipe.ready? #&& pipe.eof?  #??
      data = pipe.read #??
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
    $key_handlers[char].call if $key_handlers.has_key? char
  end
end

def reactor
  # the reactor is listening for keypress events,
  # so turn off buffered console IO. TODO: How to
  # guarantee this happens on process terminiation?
  system("stty raw -echo")

  # save a continuation to the reactor so we can jump
  # back into the reactor while blocking application logic.
  callcc do |k|
    $reactor_continuation = k
    # reactor is now bootstrapped, start it up!
    $reactor_continuation.call 
  end

  # reactor resumes here
  while true
    poll_pipes
    poll_keyboard
  end
end

# 113 is keycode for  Q
$key_handlers[113] = Proc.new do 
  system("stty -raw echo")
  exit
end

# 97 is keycode for A
$key_handlers[97] = Proc.new do 
  query_async("hello world") { |response| puts response }
end

# 98 = keycode for B
$key_handlers[98] = Proc.new do
  response = query_async_imperative("hello world")
  puts response
end
