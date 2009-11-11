require 'rubygems'
require 'parse_tree'
require 'yaml'
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
      msg=@in.read(@in.gets.to_i)
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
  def main
    si=STDIN
    so=STDOUT
      begin
      @out=so; @in=si
      warnstash=Tempfile.new "warnstash"
      STDERR.reopen warnstash
      instance=ParseTree.new
      while 1
        str=get
        exit! if str==:exit!

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
          put f.read.split #warnings
        }
      end
      rescue Exception=>e; p e; raise
      ensure exit!
      end
  end
end

ParseTreeServer.new.main if $0==__FILE__
