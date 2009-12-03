# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library => ["lib/Redis.cmxa"]

desc "Create the binary used for testing"
task :test_binary => "build/test"

desc "Run unit tests"
task :test => :test_binary do
    sh "./build/test"
end

desc "Run smoke test"
task :smoke_test => ["build/smoke_test"] do
    sh "./build/smoke_test"
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
    rm_rf "lib"
end

desc "Start a Redis Server"
task :start_redis do
    sh "redis-server var/run/redis.conf"
end

directory "build"
directory "lib"

# Utility code

class OcamlFileList < FileList
    def to_cmx prefix="build"
        self.map do |filename|
            filename.pathmap("#{prefix}/%f").ext("cmx")
        end
    end
    def to_dest prefix="build"
        self.map do |filename|
            filename.pathmap("#{prefix}/%n") # remove ext
        end
    end
    def target_dest_source
        self.to_cmx.zip(self.to_dest, self)
    end
end

def compile source, include_directory="build", dest=nil
    if dest == nil then
        dest = source.pathmap("build/%n")
    end
    sh "ocamlopt -c -I #{include_directory} -o #{dest} #{source}"
end

# Library

external_cmxas = "Unix.cmxa Str.cmxa"
lib_objs = OcamlFileList.new("src/redis*.ml")
lib_objs.target_dest_source.each do |target, dest, src|
    file target => ["build", src] do
        compile src
    end
    if not src.match(/redis_util.ml$/) then
        file target => ["build/redis_util.cmx"]
    end
    multitask :lib_obj_files => target
end

file "lib/Redis.cmxa" => ["lib", :lib_obj_files] do
    objs = lib_objs.to_cmx.reject{|obj| obj.match(/redis_util.cmx$/)}
    sh "ocamlopt -a -o lib/Redis.cmxa build/redis_util.cmx #{objs}"
end

# Test files

test_files = OcamlFileList.new("tests/test*.ml")
file "build/test" => [:library, "build/all_test.ml", "build/script.cmx", :all_test_binaries] do
    sh "ocamlopt -I build -o build/test #{external_cmxas} lib/Redis.cmxa build/script.cmx #{test_files.to_cmx} build/all_test.ml"
end

file "build/all_test.ml" => (["tests/create_test.rb"] + test_files.to_cmx) do
    sh "ruby tests/create_test.rb #{test_files} > build/all_test.ml"
end

file "build/smoke_test" => [:library, "tests/smoke_test.ml"] do
    compile "tests/smoke_test.ml"
    sh "ocamlopt -I build -o build/smoke_test #{external_cmxas} lib/Redis.cmxa build/smoke_test.cmx"
end

test_files.target_dest_source.each do |target, dest, src|
    file target => ["build", "build/script.cmx", :library, src] do
        compile src
    end
    multitask :all_test_binaries => target
end

file "build/script.cmx" => [:library, "build", "tests/script.ml"] do
    sh "ocamlopt -I build -c -o build/script tests/script.ml"
end
