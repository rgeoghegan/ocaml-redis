# Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
# Released under the BSD license. See the LICENSE.txt file for more info.

desc "Compile the library"
task :library

desc "Create the binary used for testing"
task :test_binaries

desc "Run unit tests"
task :test => [:test_binaries] do
    sh "./build/tests/run_test_redis_util"
    sh "./build/tests/all_other_tests"
end

desc "Run smoke test"
task :smoke_test => ["build/tests/smoke_test"] do
    sh "./build/tests/smoke_test"
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
end

desc "Start a Redis Server"
task :start_redis => "/tmp/redis" do
    sh "redis-server redis.conf"
end

desc "Build html documentation"
task :docs => "doc/index.html"

directory "build"
directory "/tmp/redis"

# Utility code

def compile source, dest
    sh "ocamlopt -w X -c -I build -o #{dest} #{source}"
end

# Library

external_libs = ["unix.cmxa", "str.cmxa"].join(" ")

file "build/redis_common.cmx" => ["src/redis_common.ml", "build"] do
    compile "src/redis_common.ml", "build/redis_common"
end

file "build/redis.cmi" => ["src/redis.mli", "build"] do
    compile "src/redis.mli", "build/redis.cmi"
end

file "build/redis.cmx" => ["src/redis.ml", "build/redis_common.cmx", "build/redis.cmi", "build"] do
    compile "src/redis.ml", "build/redis"
end

file "build/Redis.cmxa" => ["build/redis.cmx", "build/redis_common.cmx", "build"] do
    sh %W{
        ocamlopt
        -a
        -o build/Redis.cmxa
        build/redis_common.cmx
        build/redis.cmx
    }.join(" ")
end

task :library => "build/Redis.cmxa"

# Test files

directory "build/tests"

# Redis_utils is a bit funny because we need to test more functions than the default mli files gives us access to
test_objects = "build/tests/test_objects"
directory test_objects

file "build/tests/run_test_redis_util" => ["build/tests/script.cmx", test_objects, "build/tests/test_redis_util.cmx", "#{test_objects}/redis_common.cmx", "build/tests/run_test_redis_util.ml"] do
    sh %W{
        ocamlopt
        -o build/tests/run_test_redis_util
        -I build/tests
        #{external_libs}
        build/tests/script.cmx
        #{test_objects}/redis_common.cmx
        build/tests/test_redis_util.cmx
        build/tests/run_test_redis_util.ml
    }.join(" ")
end
file "build/tests/test_redis_util.cmx" => ["tests/test_redis_util.ml", "build/tests/script.cmx", "#{test_objects}/redis_common.cmx"] do
    sh %W{
        ocamlopt -c
        -o build/tests/test_redis_util
        -I build/tests
        -I build/tests/test_objects
        tests/test_redis_util.ml
    }.join(" ")
end
file "#{test_objects}/redis_common.cmx" => "#{test_objects}/redis_common.ml" do
    compile "#{test_objects}/redis_common.ml", "#{test_objects}/redis_common"
end
file "#{test_objects}/redis_common.ml" => "src/redis_common.ml" do
    cp "src/redis_common.ml", "#{test_objects}/redis_common.ml"
end
file "build/tests/run_test_redis_util.ml" => "tests/test_redis_util.ml" do
    sh "ruby tests/create_test.rb tests/test_redis_util.ml > build/tests/run_test_redis_util.ml"
end

task :test_binaries => "build/tests/run_test_redis_util"

## Now for all other tests
test_files = FileList["tests/test_*.ml"].exclude(/test_redis_util.ml$/)

test_files.each do |test_file|
    file test_file.pathmap("build/tests/%n.cmx") => [test_file, :library, "build/tests/script.cmx"] do
        sh %W{
            ocamlopt
            -w X -c
            -I build
            -I build/tests
            -o #{test_file.pathmap("build/tests/%n")}
            #{test_file}
        }.join(" ")
    end
    file "build/tests/all_other_tests" => test_file.pathmap("build/tests/%n.cmx")
    file "build/tests/all_other_tests.ml" => test_file
end

file "build/tests/all_other_tests.ml" do
    sh %W{
        ruby
        tests/create_test.rb
        #{test_files.join(" ")}
        > build/tests/all_other_tests.ml
    }.join(" ")
end

file "build/tests/all_other_tests" => ["build/tests/all_other_tests.ml", :library] do
    sh %W{
        ocamlopt
        -w X
        -I build
        -I build/tests
        -o build/tests/all_other_tests
        #{external_libs}
        build/Redis.cmxa
        build/tests/script.cmx
        #{test_files.map{|f| f.pathmap("build/tests/%n.cmx")}.join(" ")}
        build/tests/all_other_tests.ml
    }.join(" ")
end

task :test_binaries => "build/tests/all_other_tests"

## Smoke test
file "build/tests/smoke_test" => [:library, "tests/smoke_test.ml", "build/tests"] do
    sh "ocamlopt -I build -o build/tests/smoke_test #{external_libs} build/Redis.cmxa tests/smoke_test.ml"
end

file "build/tests/script.cmx" => ["build/tests", "tests/script.ml"] do
    sh %W{
        ocamlopt -c
            -o build/tests/script
            tests/script.ml
    }.join(" ")
end

# Documentation
directory "doc"
file "doc/index.html" => ["doc", "src/redis.mli"] do
    sh "ocamldoc -html -d doc src/redis.mli"
end
