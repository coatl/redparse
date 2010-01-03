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
      ruby18=ENV['RUBY1_8']||fail("ruby > 1.8 used and no RUBY1_8 with parse_tree to chain to")
      exec ruby18, $0
    else
      require 'rubygems'
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
end

ParseTreeServer.new.main if $0==__FILE__
