require 'digest/sha2'
require 'fileutils'
begin
  require 'etc'
rescue LoadError #ignore it
end
class RedParse
  class Cache
    def initialize(is_file,name,*params)
      divider=params.index(:/)
      @input_id=params[0...divider]
      @output_id=params[divider+1..-1]
      @rubyversion=@input_id.pop
      @name=name
      @is_file=is_file
    end

    def old_initialize *params
      @callersfile=Digest::SHA2.hexdigest params.join(',')
      @homedir=find_home+"/.redparse/"
      Dir.mkdir @homedir unless File.exist? @homedir
      Dir.mkdir cachedir unless File.exist? cachedir
      saved_digest= File.open(@homedir+"/parserdigest","rb"){|fd| fd.read.chomp } if File.exist?(@homedir+"/parserdigest")
      actual_digest= @@saved_parser_digest ||= redparse_rb_hexdigest
      if saved_digest!=actual_digest
        File.unlink(*all_entry_files)        #flush cache
        File.open(@homedir+"/parserdigest","wb"){|fd| fd.puts actual_digest } #update saved digest
      end
      retire_old_entries
    rescue Errno::EACCES
      #do nothing
    end

    def cachedir
      @homedir+@callersfile+"/"
    end
  
    def entry_files
      Dir[cachedir+"*"]
    end

    def all_entry_files
      Dir[@homedir+"*"].select{|fn| 
        File.directory? fn 
      }.map{|dirname|
        Dir[dirname+"/*"]
      }.flatten
    end

    def retire_old_entries
      size=max_size||10_000_000
      files=entry_files
      total=files.inject(0){|sum,fn| sum+File.size(fn) }
      if total>size
        files=files.sort_by{|fn| File::mtime(fn)}
        while total>size
          f=files.shift
          total-=File.size(f)
          File.unlink(f)
        end
      end
    end

    def redparse_rb_hexdigest
      full_name=nil
      $:.find{|dir| File.exist? full_name=dir+"/redparse.rb"}
      File.open(full_name,"rb"){|fd| hexdigest_of_file fd }
    end

    def hexdigest_of_file fd
      sha2=Digest::SHA2.new
      fd.rewind
      while chunk=fd.read(4096)
        sha2.update chunk
      end
      fd.rewind
      return sha2.hexdigest
    end

    def max_size
       File.open(@homedir+"/size"){|fd| fd.read.chomp.to_i } rescue nil
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
    #before trying env variables, try getpwuid, since the environment
    #might have been cleansed (eg by a cgi server) or altered (eg by 
    #rubygems tests).
    def Cache.find_home
      begin
        return Etc.getpwuid.dir
      rescue Exception
        #do nothing
      end

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
    def find_home; Cache.find_home end
    private :find_home, :entry_files, :redparse_rb_hexdigest, :retire_old_entries, :max_size, :hexdigest_of_file

    def hash_of_input input
      if IO===input
        hexdigest_of_file input
      else
        Digest::SHA2.hexdigest input
      end
    end

    def old_get input
      hash=hash_of_input input
      cachefile=cachedir+hash
      if File.exist? cachefile
        result=File.open(cachefile,"rb"){|fd| 
          line=fd.readline
          fd.rewind
          if /#encoded with Ron\n/i===line
            begin
              require 'ron'
              Ron.load fd.read
            rescue Exception
              return nil
            end
          else
            begin
              Marshal.load fd
            rescue Exception=>e              
              warn "#{e.class}: #{e}"
              warn "cache read failed for:\n#{input}"
              return nil
            end
          end
        }

        begin
          t=Time.now
          File.utime(t,t,cachefile)
        rescue Exception
          File.open(cachefile,"a"){|fd| } #touch cache date
        end
        return result
      end
    rescue EOFError,Errno::EACCES
      return nil
    end

    def old_put input,result
      hash=hash_of_input input
      File.open(cachedir+hash, "wb"){|fd|
        begin
          Thread.current["Marshal.ignore_sclass"]=true
          Marshal.dump(result,fd)
        rescue TypeError=>e #dump failed
          File.unlink cachedir+hash
          begin
            require 'ron'
            File.open(cachedir+hash, "wb"){|fd2|
              fd2.write "#encoded with Ron\n"
              fd2.write Ron.dump(result)
            }
          rescue Exception
            return
          end
        ensure
          Thread.current["Marshal.ignore_sclass"]=nil
        end
      }
    rescue Exception=>e #dump failed
      warn "#{e.class}: #{e}"
      warn "cache write failed for:\n#{result.inspect}"
      File.unlink cachedir+hash
    end

    def get input,output_type="parsed"
      output_type=@rubyversion.to_s+output_type
      if @is_file
        Cache.read_for_file(input,@name,@input_id,output_type,@output_id)
      else
        Cache.read_for_str(input,@input_id,output_type,@output_id)
      end
    end

    def put input,result,output_type="parsed"
      output_type=@rubyversion.to_s+output_type
      if @is_file
        Cache.write_for_file(input,@name,@input_id,output_type,@output_id,result)
      else
        Cache.write_for_str(input,@input_id,output_type,@output_id,result)
      end
    end
  end

  CACHEDIRNAME="obj"
  GLOBALCACHEDIRNAME=".ruby-"+CACHEDIRNAME

  def Cache.get input,name,input_id,output_type,outputter_id
    if String===input
      read_for_str(input,input_id,output_type,outputter_id)
    else
      read_for_file(input,name,input_id,output_type,outputter_id)
    end
  end

  def Cache.put input,name,input_id,output_type,outputter_id,result
    if String===input
      write_for_str(input,input_id,output_type,outputter_id,result)
    else
      write_for_file(input,name,input_id,output_type,outputter_id,result)
    end
  end

  def Cache.read input,name,input_id,output_type,outputter_id
    result=get input,name,input_id,output_type,outputter_id
    return result if result

    result=yield
    put input,name,input_id,output_type,outputter_id,result
  end

  def Cache.cachename_for_file(name,output_type)
    cache_fn=File.join(File.dirname(name),CACHEDIRNAME,File.basename(name))+"."+output_type
    dir=File.dirname(cache_fn)
    begin
      Dir.mkdir dir unless File.exists?(dir)
      raise unless File.writable?(dir)
    rescue Exception #chosen dir is not writeable
      cache_fn=File.join(find_home,GLOBALCACHEDIRNAME,name)+"."+output_type
      FileUtils.mkdir_p( File.dirname( cache_fn ) )
    end
    return cache_fn
  end

  def Cache.cachename_for_str(str,options,output_type)
    options=options.to_a.sort_by{|k,v| k}.map{|k,v| k+'='+v} if Hash===options
    options=options.join(',') unless String===options
    #options.gsub!('%n','%%n'); options.gsub!("\n",'%n')
    #fail if /\n/===options
    #str=options+"\n"+str
    options=Digest::SHA2.hexdigest(options) if options.size>100
    digest=Digest::SHA2.hexdigest(str)
    cache_fn=File.join(find_home, GLOBALCACHEDIRNAME,"-e",options,digest)+"."+output_type
    FileUtils.mkdir_p( File.dirname( cache_fn ) )
    return cache_fn
  end

  HEADER_REX=/\A\n\#encoded\ with\ (ascii|Ron|Marshal)\n
                  \#(.*)\n
                  \#([0-9A-Fa-f]+)\n
                  \#([0-9A-Fa-f]+)\n
                  \#\s{0,9}([0-9]{1,9})\s{0,9}\n\z
  /x

  #encoder, options, inputhash, outputterhash, size
  def Cache.read_trailer(f)
    f.seek(-(9*3+2+1),IO::SEEK_END)
    size=f.read[/\n\# *([0-9]+) *\n\z/,1].to_i
    return if size.zero?
    f.seek(-size,IO::SEEK_END)
    buf=f.read
    return unless result=buf.match(HEADER_REX)
    result=result.to_a
    result.shift
    return result
  end

  def Cache.attempt_read(cache_fn,inputdigest,options,output_identity,want_file=false)
    return if !File.exist? cache_fn
    
    warn "...reading from cache file #{cache_fn}"
    warn "...options=#{options.inspect}"

      outid=Digest::SHA2.hexdigest output_identity.join(',')

      cache_f=File.open(cache_fn,"rb")
      pos=cache_f.pos
      options=options.to_s

      encoder,saved_options,saved_inputdigest,saved_outid=read_trailer cache_f
      error=case
      when !encoder; "trailer not found"
      when saved_inputdigest!=inputdigest; "input changed"
      when saved_outid!=outid; "outputter changed"
      when saved_options!=options; "options changed from #{saved_options.inspect} to #{options.inspect}"
      end
      if error
        warn "...cache read failed because #{error}"
        cache_f.close
        cache_f=nil
        return
      end
     
      case encoder
      when 'ascii'
        return cache_f if want_file
        return cache_f.read
      when 'Ron'
        begin
          require 'ron'
          return Ron.load( cache_f.read )
        rescue Exception=>e
          warn "#{e.class}: #{e}"
          warn "cache ron read failed for:\n#{input}"
          return nil
        end
      when 'Marshal'
        cache_f.pos=pos
        begin
          return Marshal.load( cache_f )
        rescue Exception=>e              
          warn "#{e.class}: #{e}"
          warn "cache read failed for:\n#{input}"
          return nil
        end
      else huh
      end
  end

  def Cache.attempt_write(cache_fn,inputdigest,options,output_identity,result)
    STDERR.write "...writing to cache file #{cache_fn}... "

    if result.respond_to? :sysread
      path=result.path
      begin
        FileUtils.move(path,cache_fn)
      rescue Exception
        FileUtils.copy(path,cache_fn)
      end
      encoder= "ascii"
      STDERR.puts 'file'
      cache_f=File.open(cache_fn,"a")
    else
      cache_f=File.open(cache_fn,"wb")

      if String===result
        cache_f.write result
        encoder= "ascii"
        STDERR.puts 'ascii'
      else begin
        Thread.current["Marshal.ignore_sclass"]=true
        Marshal.dump(result,cache_f)
        encoder= "Marshal"
        STDERR.puts 'Marshal'
      rescue TypeError=>e #dump failed
        STDERR.write "Marshal failed => "
        cache_f.close
        cache_f=File.open(cache_fn,"wb")
        begin
          require 'ron'
          cache_f.write Ron.dump(result)
          encoder='Ron'
          STDERR.puts "Ron"
        rescue Exception
          STDERR.puts "Ron failed"
          File.unlink(cache_fn)
          return
        end
      ensure
        Thread.current["Marshal.ignore_sclass"]=nil
      end end
    end

    outid=Digest::SHA2.hexdigest output_identity.join(',')
    trailer= "\n#encoded with #{encoder}\n##{options}\n##{inputdigest}\n##{outid}\n"
    sz=trailer.size+2
    szsz=sz.to_s.size
    sz+=szsz
    sz+=1 unless szsz==sz.to_s.size
    trailer<< "##{sz}\n"
    fail 'bad trailer size' unless trailer[/^\#([0-9]+)\n\Z/,1].to_i==trailer.size
    cache_f.write trailer

    return result
  ensure 
    cache_f.close if cache_f
    STDERR.puts("...options=#{options.inspect}")
  end

  def Cache.read_for_file(input,name,options,output_type,output_identity,&if_missing)
    name=File.expand_path(name)
    inputdigest=Digest::SHA2.file(name).hexdigest
    cache_fn=cachename_for_file name,output_type
    result=attempt_read(cache_fn,inputdigest,options,output_identity,:want_file)
    return result if result

    if if_missing and result=if_missing.call
      write_for_file(input,name,options,output_type,output_identity,result)
    end
  end

  def Cache.write_for_file(input,name,options,output_type,output_identity,result)
    name=File.expand_path(name)
    inputdigest=Digest::SHA2.file(name).hexdigest
    cache_fn=cachename_for_file name,output_type
    attempt_write(cache_fn,inputdigest,options,output_identity,result)
  end

  def Cache.read_for_str(input,options,output_type,output_identity,&if_missing)
    inputdigest=Digest::SHA2.hexdigest(input)
    cache_fn= cachename_for_str input,options,output_type
    result=attempt_read(cache_fn,inputdigest,options,output_identity)
    return result if result

    if if_missing and result=if_missing.call
      write_for_str(input,options,output_type,output_identity,result)
    end
  end

  def Cache.write_for_str(input,options,output_type,output_identity,result)
    inputdigest=Digest::SHA2.hexdigest(input)
    cache_fn= cachename_for_str input,options,output_type
    attempt_write(cache_fn,inputdigest,options,output_identity,result)
  end
end
