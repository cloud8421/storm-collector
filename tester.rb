require 'socket'

TCPSocket.open 'localhost', 5678 do |s|
  s.send "temp:f:23.6|bright:i:978", 0
end
