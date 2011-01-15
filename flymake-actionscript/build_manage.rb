#!ruby
require 'webrick'
include WEBrick
require 'net/http'
require 'fileutils'

COMPILE_COMMAND = "mxmlc +configname=flex -compiler.warn-no-type-decl=false -compiler.source-path C:/sdfddf/trunk/src C:/boostworthy_animation_v2_1/src/classes  C:/corelib/src -file-specs=main.mxml"

SWF_TO_RUN = "main.swf"
PORT = 2001
HOST = "localhost"

############################################
# If a parameter was provided, take action #
############################################

begin
  case ARGV[0]
  when "compile"
    http = Net::HTTP.new(HOST, PORT)
    resp, date = http.get('/compile')
    puts resp.body
    exit
  when "compile_and_show"
    http = Net::HTTP.new(HOST, PORT)
    resp, date = http.get('/compile_and_show')
    puts resp.body
    exit
  when "exit"
    http = Net::HTTP.new(HOST, PORT)
    resp, date = http.get('/exit')
    puts resp.body
    exit
  end
rescue => e
  puts "Command failed: #{e}"
  exit(1)
end       


#################################################################
# Otherwise, if there are no parameters, start the build server #
#################################################################

def read_to_prompt(f)
  f.flush
  output = ""
  while chunk = f.read(1)
    STDOUT.write chunk
    output << chunk
    if output =~ /^\(fcsh\)/
      break
    end
  end
  STDOUT.write ">"
  output
end

fcsh = IO.popen("fcsh.exe  2>&1", "w+")
read_to_prompt(fcsh)
fcsh.puts COMPILE_COMMAND
read_to_prompt(fcsh)


#####################################################
# Now expose the shell through a small http server  #
#####################################################

s = HTTPServer.new(
  :Port => PORT,
  :Logger => Log.new(nil, BasicLog::WARN),
  :AccessLog => []
)

s.mount_proc("/compile"){|req, res|
  fcsh.puts "compile 1"
  output = read_to_prompt(fcsh)
  res.body = output
  res['Content-Type'] = "text/html"
}

s.mount_proc("/compile_and_show"){|req, res|
  fcsh.puts "compile 1"
  output = read_to_prompt(fcsh)
  res.body = output
  res['Content-Type'] = "text/html"
  if output =~ /#{SWF_TO_RUN} \([0-9]/
      system "SAFlashPlayer.exe #{SWF_TO_RUN}"
  end
}

s.mount_proc("/exit"){|req, res|
  s.shutdown
  fcsh.close
  exit
}

trap("INT"){
  s.shutdown 
  fcsh.close
}

s.start
