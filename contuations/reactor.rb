require 'io/wait'


def reactor
  # save a continuation to the reactor so we can jump
  # back into the reactor while blocking application logic.
  callcc do |k|
    $reactor_continuation = k
    $reactor_continuation.call 
  end

  # reactor resumes here
  while true
    poll_pipes
    poll_keyboard
  end
end


$pipes = {}
def poll_pipes
  $pipes.reject! do |pipe, callback|
    has_data = pipe.ready? #&& pipe.eof?  #??
    if has_data
      data = pipe.read
      callback.call data
    end
    has_data
  end
end

def poll_keyboard
  if STDIN.ready?
    char = STDIN.getc
    if char == 113 # Q
      system("stty -raw echo")
      exit
    end
    handle_keyboard char
  end
end



def handle_keyboard(char)
  case char

  when 97 # A
    query_async("A") do |response|
      query_async(response) do |response|
        query_async(response) do |response|
          puts response
        end
      end
    end

  when 98 # B
    response = query_blocking("B")
    response = query_blocking(response)
    response = query_blocking(response)
    puts response

  when 99 # C
    response = query_async_imperative("C")
    response = query_async_imperative(response)
    response = query_async_imperative(response)
    puts response
  end
end



def query_async(query, &callback)
  pipe = IO.popen("sleep .3; echo #{query}")
  $pipes[pipe]=callback
end

def query_blocking(query)
  pipe = IO.popen("sleep .3; echo #{query}")
  pipe.read
end

def query_async_imperative(query)
  # save off current process execution so we can
  # resume it when our async io is finished
  callcc do |k|
    callback = Proc.new do |response|
      # when async io is finished, resume the original
      # process execution context, using global to
      # communicate the results. How can I do this without
      # a global?
      $result = response
      k.call
    end
    query_async(query, &callback)
    # jump out of application logic and into the reactor.
    # we will return to application logic when the data is ready
    $reactor_continuation.call
  end

  # resume here when async io is ready 
  $result
end



system("stty raw -echo") # disable line buffering
reactor
