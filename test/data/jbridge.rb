catch (:f){}
__END__
	if !RUBY_PLATFORM["cyg"].nil? then
	  ret = `cygpath -w #{ret}`.gsub(/\\/,"/").chomp
	end

  ##########################################################################
  # Low level API area
  ##########################################################################

  def __build_bridge
	return if defined?(@@object_table)

	@@driver = nil
	if defined?(JBRIDGE_OPTIONS) then
	  @@options = JBRIDGE_OPTIONS
	else
	  @@options = Hash.new
	end

	@@object_table = Hash.new          # proxy_id -> object
	@@gc_manager = JGCManager.instance

	@@debug_out = __jbopt(:bridge_log)

	print_debug("JavaBridge: Startup communication bridge...")

	case __jbopt(:bridge_driver)
	when :xmlrpc
	  @@driver = XMLRPCBridge.instance
	when :bstream
	  @@driver = BinStreamBridge.instance
	else
	  raise "Driver #{__jbopt(:bridge_driver).to_s} not found."
	end
	@@driver.set_debugout( lambda{|a| print_debug(a)} )
	
	__startup_JVM
	@@driver.startup_server(@@opt_proc, lambda { |*args| __called_by_java(*args)})
	@@class_repository = JClassRepository.new

	print_debug("JavaBridge: Finished initialized.")
  end

  def __startup_JVM
	#JVM CLASSPATH
	cp = `echo "#{__jbopt(:classpath)}"`.chomp
	cp = "#{cp}#{__cpseparator}#{@@driver.get_bridge_classpath(__search_libpath)}"
	print_debug("CLASSPATH: #{cp}")
	#JVM PATH
	path = __jbopt(:jvm_path)
	return unless path
	#JVM VM ARGS
	vmarg = __jbopt(:jvm_vm_args)
	
	#JVM LOG
	if __jbopt(:jvm_log_file).nil? then
	  logfile = ""
	else
	  logfile = " -logfile:#{__jbopt(:jvm_log_file)} "
	end
	loglevel = __jbopt(:jvm_log_level)

	#DRIVER ARGS
	driver_args = @@driver.get_bridge_args(@@opt_proc)
	return if driver_args.nil?

	cmd = "#{path} #{vmarg} -classpath \"#{cp}\" #{driver_args} -logLevel:#{loglevel} #{logfile}"
	print_debug(cmd)

	#EXEC COMMAND AND WAIT FOR INIT
	io = IO.popen(cmd,"r")
	while true
	  line = io.gets
	  puts "JVM: #{line}" if @@debug_out
	  abort "Can not start JVM: \n#{cmd}" if line.nil?
	  break if line =~ /^OK/
	end
	
	#START STDOUT PUTTER
	if __jbopt(:jvm_stdout) != :never then
	  Thread.start(io) {|java_io|
		loop {
		  if __jbopt(:jvm_stdout) then
			puts java_io.gets
		  else
			java_io.gets
		  end
		}
	  }
	end

	#REGISTER JVM KILLER
	at_exit {
	  print_debug "  EXIT JavaBridge..."
	  begin
		break_bridge
	  rescue => e
		puts e.message
		puts e.backtrace.join("\n")
	  ensure
		begin
		  Process.kill(:QUIT, io.pid)
		rescue
		  puts e.message
		  puts e.backtrace.join("\n")
		end
	  end
	  print_debug "  EXIT JavaBridge..."
	}
	print_debug("JVM: Initialized.")
  end

  def __register(obj)
	@@object_table[obj.__object_id] = obj
	obj
  end
  
  def print_debug(out)
	puts out if @@debug_out
  end

  # Called by java
  def __called_by_java(sid,obj_id,method_name,args)
	print_debug "%% Received: #{obj_id}.#{method_name} : #{args.join(", ")}"
	obj = @@object_table[obj_id]
	if obj.nil? then
	  # not found extended object
	  raise "Object: #{obj_id} not found."
	else
	  # OK
	  Thread.current[:__jb_session] = sid
	  begin
		ret = obj.__jsend__(method_name,*args)
		return ret
	  rescue => ev
		print_debug "EXCEPTION (for debug): #{ev} -------------"
		print_debug ev.backtrace
		sa = args.join(",")
		print_debug "sid:#{sid}  object:#{obj.__classname}  method:#{method_name}  args:#{sa}:#{args.size}"
		print_debug "------------------------------------------"
		raise ev
	  ensure
		Thread.current[:__jb_session] = nil
	  end
	end
  end

  # calling the JavaBridge API
  def __send_message_to_java(method,*args)
	sid = Thread.current[:__jb_session]
	if sid then
	  # session thread logic
	  print_debug "## SESSION[#{sid}]  SendMessage: #{method}( #{args.join(", ")} )"
	  args.insert(0,sid,method)
	  return @@driver.send_message_to_java("sessionCall",*args)
	else
	  # normal calling
	  print_debug "## SendMessage: #{method}( #{args.join(", ")} )"
	  return @@driver.send_message_to_java(method,*args)
	end
  end

  ##########################################################################
  # Public API area
  ##########################################################################

  public

  # Sending a shutdown message to the JVM.
  def break_bridge()
	return unless defined?(@@object_table)
	@@gc_manager.cancel_all_finalizer
	@@driver.shutdown_server if @@driver.connected?
	nil
  end

  # Resuming the main thread.
  def wakeup_thread()
	@@main_thread.wakeup
	nil
  end

  def stop_thread()
	return unless defined?(@@object_table)
	@@main_thread = Thread.current
	Thread.stop
	nil
  end

  def jnew(classname,*args)
	__build_bridge
	obj = JCreatedObject.new(__to_s(classname),*args)
	@@gc_manager.register_finalizer(obj)
	return obj
  end

  def jextend(classnames,*args,&impl)
	__build_bridge
	ret = __register(JExtendedClass.new(__to_s(classnames),*args))
	ret.set_default_dispatcher(impl) if impl
	return ret
  end

  def jstatic(classname)
	__build_bridge
	obj = JClass.new(__to_s(classname))
	return obj
  end

  def jimport(lines)
	__build_bridge
	__send_message_to_java("import",lines)
	nil
  end

  def junlink(proxy)
	__build_bridge
	if proxy.kind_of?(JExtendedClass) then
	  key = proxy.__object_id
	  __send_message_to_java("unlink",key)
	else 
	  print_debug("The object #{proxy.__object_id} will be garbage_collected automatically.")
	end
  end

  #return all proxy objects in the object repositry.
  def jdump_object_list
	__build_bridge
	return __send_message_to_java("allObjects")
  end

  ##########################################################################
  # private API
  ##########################################################################

  private

  def __jclassname(key)
	__send_message_to_java("classname",key)
  end

  # creating a proxy object.
  def jproxy(classname,key)
	__build_bridge
	obj = JProxyObject.new(__to_s(classname),key)
	@@gc_manager.register_finalizer(obj)
	return obj
  end

  def __to_s(classname)
	return classname if classname.instance_of? String
	classname.to_s.gsub(/_/,".")
  end

  def __classinfo(classname)
	__send_message_to_java("classinfo",__to_s(classname)).split(",")
  end

  ##########################################################################
  # JClass area
  ##########################################################################

  #
  # Abstract proxy class: a subclass should initialize @__classname, @__object_id
  #  and @__classinfo.
  #
  class JObject 

	def method_missing(name,*args)
	  print_debug "#### method_missing : #{name}( #{args.join(", ")} )"
	  name = name.to_s
	  if args.size == 0 then
		#property get?
		return __ref__(name) if __define_jfield?(name)
	  else
		args = __obj2ids__(args)
		#property set?
		if !(name =~ /^.*=$/).nil? then
		  fname = name[0,name.length-1]
		  return __set__(fname,args[0]) if __define_jfield?(fname)
		end
	  end
	  #method call
	  return __id2obj__(__call__(name,*args)) if __define_jmethod?(name)
	  #not found
	  #super(name,*args)
	  as = args.join(",")
	  raise NoMethodError.new("Not found method: #{name}(#{as}) in #{__classname}",name, args)
	end

	def __define_jfield?(name)
	  return false if __define_jmethod?(name)
	  @__classinfo.define_jfield?(name)
	end

	def __define_jmethod?(name)
	  @__classinfo.define_jmethod?(name)
	end

	# called by java
	def __jsend__(method,*args)
	  args = __id2objs__(args)
	  return __send__(method,*args).to_trans_obj
	end

	def to_trans_obj
	  @__object_id
	end

	attr_reader :__object_id,:__classname,:__classinfo

	private

	def __obj2ids__(args)
	  args.map{|i| i.to_trans_obj }
	end

	def __id2obj__(arg)
	  return nil if (arg.nil?)
	  return __id2objs__(arg) if arg.instance_of?(Array)
	  return arg unless arg.instance_of?(String)
	  return arg if arg["\n"]
	  cn = __jclassname(arg)
	  if !cn.nil? then
		return @@object_table[arg] if @@object_table.key?(arg)
		jproxy(cn,arg)
	  else 
		arg
	  end
	end

	def __id2objs__(args)
	  args.map {|i| __id2obj__(i)}
	end

	def __call__(method,*args)
	  args = __obj2ids__(args)
	  __id2obj__(__send_message_to_java("call",@__object_id,method,*args))
	end

	def __ref__(fieldname)
	  __id2obj__(__send_message_to_java("ref",@__object_id,fieldname))
	end

	def __set__(fieldname,value)
	  __send_message_to_java("set",@__object_id,fieldname,value)
	  nil
	end

	public

	def to_s
	  "JB:ProxyObject  class:#{@__classname}, value:#{__call__('toString')}"
	end

	def inspect
	  to_s
	end

	def methods
	  return @accessible_methods if @accessible_methods
	  @accessible_methods = super
	  @accessible_methods |= @__classinfo.get_accessible_methods
	  return @accessible_methods
	end

  end # class JObject

  # the object created by ruby
  class JCreatedObject < JObject
	def initialize(classname,*args)
	  if args.size > 0 then
		args = __obj2ids__(args)
	  end
	  @__object_id = __send_message_to_java("new",classname,*args)
	  @__classname = __jclassname(@__object_id)
	  @__classinfo = @@class_repository.get_classinfo(@__classname)
	end
  end

  # the extended object created by ruby
  class JExtendedClass < JObject

	def initialize(_classnames,*args)
	  if args.size > 0 then
		args = __obj2ids__(args)
	  end
	  @__object_id = __send_message_to_java("extend",_classnames,*args)
	  @__classname = __jclassname(@__object_id)
	  @__classinfo = @@class_repository.get_classinfo(@__classname)
	  @table_impl_proc = Hash.new
	  @default_proc = nil
	end
	
	def __jsend__(method,*args)
	  if respond_to?(method) then
		return super(method,*args)
	  elsif @default_proc then 
		return @default_proc.call(method,args)
	  else
		raise "BUG: called not implemented method."
	  end
	end

	def __super__(method,*args)
	  args = __obj2ids__(args)
	  __id2obj__(__send_message_to_java("superCall",@__object_id,method,*args))
	end

	def __call__(method,*args)
	  __super__(method,*args)
	end

	def singleton_method_added(name)
	  __send_message_to_java("impl",@__object_id,name.to_s,true)
	end

	def singleton_method_removed(name)
	  __send_message_to_java("impl",@__object_id,name.to_s,false)
	end

	def singleton_method_undefined(name)
	  __send_message_to_java("impl",@__object_id,name.to_s,false)
	end

	#the easy method implementation by the block notation.
	#users can write the event handler with the java-like variable scope.
	def jdef(name,&aproc)
	  @table_impl_proc[name] = aproc
	  instance_eval %Q{
		def #{name.to_s}(*args)
		  return @table_impl_proc[:#{name.to_s}].call(*args)
		  end
		}
	end

	def set_default_dispatcher(aproc)
	  @default_proc = aproc
	end

  end

  # the object created in the JVM
  class JProxyObject <  JObject
	def initialize(classname,key)
	  @__object_id = key
	  @__classname = classname
	  @__classinfo = @@class_repository.get_classinfo(@__classname)
	end
  end
  
  # the static class object
  class JClass < JObject

	attr_reader :__metainfo
	
	def initialize(classname)
	  @__object_id = __send_message_to_java("static",classname)
	  @__classname = __jclassname(@__object_id)
	  @__classinfo = @@class_repository.get_classinfo(@__classname)
	  @__metainfo = @@class_repository.get_classinfo("java.lang.Class")
	end

	def __define_jmethod?(name)
	  @__classinfo.define_jmethod?(name) || @__metainfo.define_jmethod?(name)
	end

	def methods
	  return @accessible_methods if @accessible_methods
	  @accessible_methods = super
	  @accessible_methods |= @__classinfo.get_accessible_methods
	  @accessible_methods |= @__metainfo.get_accessible_methods
	  return @accessible_methods
	end

  end

  ##########################################################################
  # JClassRepository and JClassInfo area
  ##########################################################################

  # 
  # JClassRepository manages the class information represented by JClassInfo.
  # 
  class JClassRepository
	def initialize
	  @classtable = Hash.new
	end

	def get_classinfo(classname)
	  return nil if classname.nil?
	  info = @classtable[classname]
	  return info if info
	  info = JClassInfo.new(classname)
	  @classtable[classname] = info
	  info
	end
  end

  # 
  # JClassInfo treats superclass, public fields, public and protected methods.
  # The instances of JClassInfo make a tree form in which the subclass 
  # makes a branche.
  #
  class JClassInfo 

	attr_reader :jsuperclass,:jclassname,:jfields,:jmethods,:protected_jmethods,:jinterfaces

	def initialize(_classname)
	  info = __classinfo(_classname)
	  @jclassname = _classname
	  @jinterfaces = []
	  @jfields = Hash.new
	  @jmethods = Hash.new
	  @protected_jmethods = Hash.new
	  _superclass = nil
	  mode = nil
	  info.each{|i|
		case i
		when "====Superclass"
		  mode = :superclass
		when "====Interfaces"
		  mode = :interfaces
		when "====Field"
		  mode = :fields
		when "====PublicMethod"
		  mode = :methods
		when "====ProtectedMethod"
		  mode = :protected_methods
		else
		  case mode
		  when :superclass
			_superclass = i
		  when :interfaces
			@jinterfaces << @@class_repository.get_classinfo(i)
		  when :fields
			@jfields[i] = :t
		  when :methods
			@jmethods[i] = :t
		  when :protected_methods
			@protected_jmethods[i] = :t
		  end
		end
	  }
	  if _superclass then
		@jsuperclass = @@class_repository.get_classinfo(_superclass) 
	  else
		@jsuperclass = nil
	  end

	  @public_jmethods = @jmethods.keys
	  @public_jmethods |= @jsuperclass.get_accessible_methods if @jsuperclass
	  @jinterfaces.each {|i| @public_jmethods |= i.get_accessible_methods }
	end

	def define_jfield?(name)
	  return true if @jfields.key?(name)
	  if (!@jsuperclass.nil?) then
		return true if @jsuperclass.define_jfield?(name)
	  end
	  @jinterfaces.each {|i|
		return true if i.define_jfield?(name)
	  }
	  false
	end

	def define_jmethod?(name)
	  return true if @jmethods.key?(name)
	  if @jsuperclass then
		return true if @jsuperclass.define_jmethod?(name)
	  end
	  @jinterfaces.each {|i|
		return true if i.define_jmethod?(name) 
	  }
	  false
	end

	def define_protected_jmethod?(name)
	  return true if @protected_jmethods.key?(name)
	  return @jsuperclass.define_protected_jmethod?(name) if @jsuperclass
	  @jinterfaces.each {|i|
		return true if i.define_jmethod?(name)
	  }
	  false
	end

	#return public java methods
	def get_accessible_methods
	  return @public_jmethods
	end

	def dump
	  puts "========: #{@jclassname}"
	  puts "  == Superclass: #{@jsuperclass.jclassname}" if @jsuperclass
	  putter = lambda {|i| puts "   #{i}" }
	  puts "  == Interface"
	  @jinterfaces.each {|i| puts "   #{i.jclassname}"}
	  puts "  == Field"
	  @jfields.each_key(putter)
	  puts "  == Public Method"
	  @jmethods.each_key(putter)
	  puts "  == ProtectedMethod Method"
	  @protected_jmethods.each_key(putter)
	end
  end

  ##########################################################################
  # Exception holder for the exceptions occurred in Java 
  ##########################################################################

  class JException < RuntimeError

	attr_reader :klass,:message,:detail

	def initialize(klass,message,detail)
	  @klass = klass
	  @message = message
	  @detail = detail
	end

	def message
	  ret = ""
	  ret += @klass if @klass
	  ret += "\n"+@message if @message
	  ret += "\n"+@detail if @detail
	  return ret
	end

	alias :to_s :message

  end

  private
  
  ##########################################################################
  # GC manager: send unlink message to Java for removing the object reference
  ##########################################################################

  class JGCManager 
	include Singleton

	def initialize
	  @@object_id_table = Hash.new    # __id__ -> proxy_id
	  @@object_ref_counter = Hash.new # proxy_id -> refcounter
	  @@object_lock = Monitor.new     # lock for finalizer registration and deregistration

	  # I'm not sure that using lists without locking...
	  @@using_list1 = false
	  @@finalizable_id_list1 = []
	  @@using_list2 = false
	  @@finalizable_id_list2 = []

	  @@later_registration_list = []
	end

	public
	def exec_finalizable_objects
	  @@object_lock.synchronize do 
		@@using_list1 = true
		@@finalizable_id_list1.each {|i| finalize_jobject(i)}
		@@finalizable_id_list1 = []
		@@using_list1 = false
		@@using_list2 = true
		@@finalizable_id_list2.each {|i| finalize_jobject(i)}
		@@finalizable_id_list2 = []
		@@using_list2 = false
	  end
	end

	private
	def finalize_jobject(proxy_id)
	  begin
		counter = @@object_ref_counter[proxy_id]
		if counter > 1 then
		  counter = counter - 1
		  @@object_ref_counter[proxy_id] = counter
		  print_debug "     ----GC: decrease ref count [#{counter}] : #{proxy_id}"
		  return nil
		end
		@@object_ref_counter.delete(proxy_id)
		print_debug "     ----GC: sending unlink... : #{proxy_id}"
		__send_message_to_java("unlink",proxy_id)
		print_debug "     ----GC: sent ok."
	  rescue Exception => e
		p e
		raise RuntimeError.new("Failed to finalize object: [#{proxy_id}] => #{e.class.to_s} : #{e.message}")
	  ensure
		print_debug "     ----GC: exiting finalizer."
	  end
	end

	def self.register_gc_object_id(pid)
	  if !@@using_list1 then
		@@finalizable_id_list1 << pid
		if @@later_registration_list.size > 0 then
		  @@finalizable_id_list1.concat @later_registration_list
		  @@later_registration_list = []
		end
	  elsif !@@using_list2 then
		@@finalizable_id_list2 << pid
		if @@later_registration_list.size > 0 then
		  @@finalizable_id_list2.concat @later_registration_list
		  @@later_registration_list = []
		end
	  else
		print_debug "     ----GC: finalize later: #{pid}"
		@@later_registration_list << pid
	  end
	end
	
	def self.get_finalizer_proc
	  return Proc.new {|id|
		proxy_id = @@object_id_table[id]
		begin
		  print_debug "     ----GC: register unlink: #{proxy_id}"
		  JGCManager.register_gc_object_id( proxy_id )
		rescue Exception => e
		  p e
		ensure
		  @@object_id_table.delete(id)
		end
	  }
	end

	public
	def register_finalizer(proxy)
	  @@object_lock.synchronize {
		@@object_id_table[proxy.__id__] = proxy.__object_id
		counter = @@object_ref_counter[proxy.__object_id]
		if counter then
		  counter += 1
		else 
		  counter = 1
		end
		print_debug "     ----GC: #{counter} : #{proxy.__object_id}"
		@@object_ref_counter[proxy.__object_id] = counter
		if (!proxy.kind_of?(JObject)) then
		  raise RuntimeError.new("GC: different object: #{proxy.to_s}")
		end
		ObjectSpace.define_finalizer(proxy, JGCManager.get_finalizer_proc)
	  }
	  exec_finalizable_objects
	end

	public
	def cancel_all_finalizer
	  @@object_lock.synchronize {
		print_debug "      ----GC: begin cancelling finalizer: #{@@object_id_table.size}"
		@@object_id_table.reject! {|key,value|
		  begin
			obj = ObjectSpace._id2ref(key)
			ObjectSpace.undefine_finalizer(obj)
			print_debug "      ----GC:   cancel: #{obj.__object_id}"
		  rescue RangeError => e
		  end
		  true
		}
	  }
	end
  end

end #module

# Ruby like new methods
class Symbol
  def jext(*args,&impl)
	JavaBridge::jextend(self, *args, &impl)
  end
  
  def jnew(*args)
	JavaBridge::jnew(self, *args)
  end
  
  def jclass
	JavaBridge::jstatic(self)
  end
end

