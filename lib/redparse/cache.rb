class RedParse
  class Cache
    def initialize
      @homedir=find_home+"/.redparse"
      saved_digest= File.open(@homedir+"/parserdigest"){|fd| fd.read.chomp } rescue nil
      actual_digest= redparse_rb_hexdigest
      if saved_digest!=actual_digest
        File.unlink(*entry_files)        #flush cache
        File.open(@homedir+"/parserdigest","w"){|fd| fd.puts actual_digest } #update saved digest
      end
      retire_old_entries
    end
  
    def entry_files
      Dir[@homedir+"/*"].grep(%r{/[0-9a-f]+\Z}i)
    end

    def retire_old_entries
      size=max_size||10_000_000
      files=entry_files
      total=files.inject(0){|fn,sum| sum+File.size(fn) }
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
      huh
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
    private :find_home, :entry_files, :redparse_rb_hexdigest, :retire_old_entries

    def get input
      huh
      huh touch
    end

    def put input,result
      huh
    end

  end
end
