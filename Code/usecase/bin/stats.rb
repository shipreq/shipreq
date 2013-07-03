#!/usr/bin/env ruby

def countTodoFileContents(content)
  keys = %w[M S C]
  vals = Hash[keys.map{|t| [t,content.select{|c| c =~ /^\* \[#{t}/}.size] }]
  keys.map{|k| "%3d" % [vals[k]] }.join(" ") + " = %3d" % [vals.values.inject(0,:+).to_s]
end

def searchMain(dp, &cmdFn)
  v = `#{cmdFn.call}`.chomp.to_i
  "%#{dp}d" % [v]
end

def searchSrc(dp, ratio=true, &cmdFn)
  v = %w[src/main src/test].map{|dir| `#{cmdFn.call(dir)}`.chomp.to_i }
  r = "%#{dp}d + %#{dp}d = %#{dp}d" % [v[0], v[1], v[0]+v[1]]
  r += "  (%.2f|%.2f)" % [v[0].to_f/v[1].to_f, v[1].to_f/v[0].to_f] if ratio
  r
end

def wcStdout(arg)          arg + "| wc -l" end
def wcFiles(arg)           arg + "| xargs wc -l | tail -1 | perl -pe 's/\D+//g'" end
def wcFilesInDir(dir)      wcFiles("find #{dir} -type f") end
def wcFilesByType(dir,pat) wcFiles("find #{dir} -name '#{pat}'") end

puts "main.html       - #{searchMain(5){ wcFilesByType 'src/main', '*.html' }}"
puts "main.scaml      - #{searchMain(5){ wcFilesByType 'src/main', '*.scaml' }}"
puts "main.javascript - #{searchMain(5){ wcFiles "find src/main/javascript -type f | fgrep -v vendor" }}"
puts "main.sass       - #{searchMain(5){ wcFilesInDir 'src/main/sass' }}"
puts
puts "Scala - #{searchSrc(5){|dir| wcFilesInDir("#{dir}/scala") }}"
puts
puts "TODO.Func - #{countTodoFileContents `cat TODO.md | sed '0,/FUNC TODO/d; /TECH TODO/,$d'`.split(/[\r\n]+/)}"
puts "TODO.Tech - #{countTodoFileContents `cat TODO.md | sed '0,/TECH TODO/d'`.split(/[\r\n]+/)}"
puts "TODO.src  -   #{searchSrc(3){|dir| wcStdout("find #{dir} -type f | egrep -v '/_scalate/|vendor' | xargs fgrep TODO") }}"


