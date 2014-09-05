require 'socket'

TCPSocket.open 'localhost', 5678 do |s|
  s.send "temperature:f:23.6|brightness:i:978", 0
end
