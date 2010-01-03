require 'tempfile'
#require 'yaml'
#require 'marshal'

module ParseTreeComm
  SERIALIZE=Marshal
  def put o
    o=SERIALIZE.dump o
    msg= o.size.to_s+"\n"+o+"\n"
    begin
      @out.write msg
      @out.flush
    rescue Exception
      @out=@in=nil
      raise
    end
  end
  def get
    begin
      len=@in.gets.to_i
      msg=@in.read(len)
      @in.getc  #read trailing \n
    rescue Exception
      @in=@out=nil
      raise
    end
    result=SERIALIZE.load msg
    return result
  end
end

class ParseTreeServer
  include ParseTreeComm

  def self.path_to_server_command
    File.expand_path __FILE__
  end

  def ensure_parse_tree_and_1_8
    if ::RUBY_VERSION[/^\d+\.\d+/].to_f>1.8
      ruby18=ENV['RUBY1_8']||fail("you must use ruby <= 1.8 (with parse_tree) or set RUBY1_8 env to a 1.8 interpreter")
      exec ruby18, $0
    else
      begin require 'rubygems'; rescue LoadError; end

      if File.exist? find_home+"/.redparse/parse_tree_server.rc"
        $:.concat File.readlines(find_home+"/.redparse/parse_tree_server.rc").map{|l| l.chop }
      end

      require 'parse_tree'
    end
  rescue Exception=>e
    put e
    put nil
    put nil
    raise
  end

  def main
    si=STDIN
    so=STDOUT
    @out=so; @in=si
    ensure_parse_tree_and_1_8
      begin
      warnstash=Tempfile.new "warnstash"
      STDERR.reopen warnstash
      instance=ParseTree.new
      while true
        str=get
        exit! if str==:exit!
        if str==:version
          put ::RUBY_VERSION
          next
        end

        pos=STDERR.pos

        tree=
        begin
          instance.parse_tree_for_string(str) #tree
        rescue Exception=>e; 
          tree=e
        end
        put tree

        open(STDERR.path){|f| 
          f.pos=pos
          put warnings=f.read.split #warnings
        }
      end
      rescue Exception=>e; put e; raise
      ensure exit!
      end
  end

    ##
    # Finds the user's home directory.
    #--
    # Some comments from the ruby-talk list regarding finding the home
    # directory:
    #
    #   I have HOME, USERPROFILE and HOMEDRIVE + HOMEPATH. Ruby seems
    #   to be depending on HOME in those code samples. I propose that
    #   it should fallback to USERPROFILE and HOMEDRIVE + HOMEPATH (at
    #   least on Win32).
    #(originally stolen from rubygems)
    def find_home
      ['HOME', 'USERPROFILE'].each do |homekey|
        return ENV[homekey] if ENV[homekey]
      end

      if ENV['HOMEDRIVE'] && ENV['HOMEPATH'] then
        return "#{ENV['HOMEDRIVE']}#{ENV['HOMEPATH']}"
      end

      begin
        File.expand_path("~")
      rescue
        if File::ALT_SEPARATOR then
            "C:/"
        else
            "/"
        end
      end
    end

end

ParseTreeServer.new.main if $0==__FILE__
