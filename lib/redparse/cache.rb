require 'digest/sha2'
class RedParse
  class Cache
    def initialize *params
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
    private :find_home, :entry_files, :redparse_rb_hexdigest, :retire_old_entries, :max_size, :hexdigest_of_file

    def hash_of_input input
      if IO===input
        hexdigest_of_file input
      else
        Digest::SHA2.hexdigest input
      end
    end

    def get input
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
    rescue EOFError
      return nil
    end

    def put input,result
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
  end
end
