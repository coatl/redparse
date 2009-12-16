class Float
  SIZE=[1.1].pack("d").size
  BITSIZE=SIZE*8
  BASE10_DIGITS=(2**BITSIZE-1).to_s.size
  def accurate_to_s
    return "#{'-' if self<0}Infinity" if infinite?
    return "NaN" if nan?
    return "0.0e0" if zero?

    as_str=sprintf("%.#{BASE10_DIGITS+2}e",self)

    #decompose self into sign, mantissa, and exponent (in string form)
    all,sign,first,digits,exp=*as_str.match(/^([+-]?)(\d)\.(\d+)e(.*)$/)
    digits=first<<digits
    exp=exp.to_i+1
    lead=sign<<"0."
    return digits=digits if as_str.to_f.zero? #hopeless

    #recompose back to a float
    result=[lead,digits,"e",exp].join
    result_f=result.to_f
    delta=result_f - self
    return digits=digits if delta.zero? #if representation is exact, return here

    #figure out which direction to go to get toward the right answer
    if delta<0
      incr=1
    else #delta>0
      incr=-1
    end

    #keep adding increasing increments to mantissa
    #until we get to a value on the other side of the correct answer
    while true
      while true
        try_digits=digits.to_i.+(incr).to_s
        if try_digits.size>digits.size
          exp+=1
          digits="0"+digits
        end
        fail if try_digits[0]==?- #can't happen... I think?
        trying=[lead,try_digits,"e",exp].join
        trying_f=trying.to_f
        break unless trying_f.zero?
        digits[-1,1]='' #workaround 1.8 bug
      end
      return digits=try_digits if trying_f==self
      break if self.between?(*[trying_f,result_f].sort) #(trying_f-self)*delta<0
      incr*=2
    end

    #we now have lower and upper bounds on the correct answer
    lower,upper=*[digits.to_i, digits.to_i.+(incr)].sort

    #maybe one of the bounds is already the correct answer?
    result=[lead,lower,"e",exp].join
    return digits=lower if result.to_f==self
    result=[lead,upper,"e",exp].join
    return digits=upper if result.to_f==self

    #binary search between lower and upper bounds til we find a correct answer
    digits=nil
    while true
      return as_str if upper-lower <= 1 #hopeless
      mid=(lower+upper)/2
      mid_s=[lead,mid,"e",exp].join
      mid_f=mid_s.to_f
      return digits=mid if mid_f==self
      if mid_f<self
        lower=mid
      else #mid_f>self
        upper=mid
      end
    end
  ensure

    #try to drop unneeded trailing digits
    if digits
      digits=digits.to_s
      begin
        last=digits.slice!( -1 )
        result=[lead,digits,"e",exp].join.to_f
      end while result==self or result.zero? && digits.size.nonzero?
      roundup=(digits.to_i+1).to_s
      if roundup.size>digits.size
        exp+=1
        digits="0"+digits
      end
      roundup.slice!( /0+\Z/ )
      roundup=[lead,roundup,"e",exp].join
      return roundup if roundup.to_f==self
      return [lead,digits<<last,"e",exp].join
    end
  end
end

=begin not quite accurate, tho
class String
  def accurate_to_f
    all,sign,int,frac,exp=*self.match(/\A([+-])?([0-9_]+)(?:\.([0-9_]+))?(?:[eE]([+-]?[0-9_]+))?/)
    exp=exp.to_i||0
    exp-=frac.size
    mantissa=sign<<int<<frac
    mantissa=mantissa.to_f
    scale=10.0**exp
    return mantissa*scale
  end
end
=end

eval DATA.read if __FILE__==$0
__END__

require 'test/unit'

class FloatRoundTripTest<Test::Unit::TestCase
def float_round_trip f
  str=f.accurate_to_s
  if str=="Infinity"
    return 1.0/0
  elsif str=="-Infinity"
    return -1.0/0
  elsif str=="NaN"
    return 0.0/0.0
  else
    str.to_f
  end
end
alias frt float_round_trip

def rand_float
  base=rand
  range=Float::MIN_10_EXP..Float::MAX_10_EXP
  extent=range.last-range.first
  offset=rand(extent+1)
  exp=range.first+offset
  base*=10**exp
end

def test_frt
  data=[0.399891415240566, 0.198188037200931, 0.90302699802093,
        7.48121345153454520016e-01, 4.11408313999083175005e-02,
        2.68070684698467065488e-02, 3.85029229764812574999e-01,
        "\327\237vY\343n\342?".unpack("d")[0], 
        1.0/0, -1.0/0, 0.0/0.0,
        "\333\211>4\253Atu".unpack("d")[0],
        "\2461\220\253\214\267e\021".unpack("d")[0],
        "\r\032N\f'\336\243\003".unpack("d")[0],
        "B[\226\001\2175f\021".unpack("d")[0],
        "%\217\336\362\326\272\026\001".unpack("d")[0],
        "\312\342\344\206\237\024\"\003".unpack("d")[0],
  ]
  1_000_000.times{|i|
    f=data.shift||rand_float
    p sprintf("%.#{Float::BASE10_DIGITS}e",f)
    p [f].pack"d"
    p f.accurate_to_s
    f2=frt f
    warn "failed with i=#{i} f=#{f}, #{[f].pack("d").inspect}; f2=#{f2}, #{[f2].pack("d").inspect}" unless [f].pack("d")==[f2].pack("d")
  }
end
end
