desc "Compile the library"
task :lib

desc "Run unit tests"
task :test => :test_binary do
    sh "./build/test"
end

desc "Run smoke test"
task :smoke_test => ["build/smoke_test"] do
    sh "./build/smoke_test"
end

# Test files

desc "Create the binary used for testing"
task :test_binary => "build/test"

file "build/test" => [:lib, "build/all_test.cmx"] do
    libs = %w{redis script test_redis all_test}.collect { |f|
        "#{f}.cmx"
    }.join " "
    sh "ocamlopt -I build -o build/test unix.cmxa #{libs}"
end

file "build/all_test.cmx" => [:all_test_binaries, "build/all_test.ml"] do
    sh "ocamlopt -c -I build -o build/all_test build/all_test.ml"
end

file "build/all_test.ml" => ["tests/create_test.rb"] do
    test_files = FileList["tests/test*.ml"].join(" ")
    sh "ruby tests/create_test.rb #{test_files} > build/all_test.ml"
end

file "build/smoke_test" => [:lib, "tests/smoke_test.ml"] do
    sh "ocamlopt -I build -c -o build/smoke_test tests/smoke_test.ml"
    sh "ocamlopt -I build -o build/smoke_test Unix.cmxa build/redis.cmx build/smoke_test.cmx"
end


# Build binaries
directory "build"

FileList.new("src/*.ml").each do |filename|
    build_obj = filename.pathmap("build/%f").ext("cmx")
    file build_obj => ["build", filename] do
        sh "ocamlopt -c -o #{build_obj.pathmap("build/%n")} #{filename}"
    end
    multitask :lib => build_obj
end

file "build/script.cmx" => ["build", "tests/script.ml"] do
    sh "ocamlopt -c -o build/script tests/script.ml"
end
    
FileList.new("tests/test_*.ml").each do |filename|
    build_obj = filename.pathmap("build/%f").ext("cmx")
    original_obj = filename.pathmap("build/%{test_,}f").ext("cmx")
    file build_obj => ["build", "build/script.cmx", original_obj, filename] do
        sh "ocamlopt -c -I build -o #{build_obj.pathmap("build/%n")} #{filename}"
    end
    multitask :all_test_binaries => build_obj
end

desc "Delete droppings"
task :clean do
    rm_rf "build"
end
