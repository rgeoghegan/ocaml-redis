file "redis.cmx" => ["redis.ml"] do
    sh "ocamlopt -c redis.ml"
end

desc "Compile the library"
task :redis_lib => ["redis.cmx"]
desc "Run unit tests"
task :test => [:clean, :test_binary] do
    sh "./test"
end

task :test_binary => "all_test.cmx" do
    sh "ocamlopt -o test unix.cmxa redis.cmx test_redis.cmx all_test.cmx"
end

desc "Interpolate master test code file"
task :all_test_file => "all_test.ml"

file "all_test.cmx" => ["all_test.ml", "test_redis.cmx"] do
    sh "ocamlopt -c all_test.ml"
end

file "test_redis.cmx" => "redis.cmx"

file "all_test.ml" => ["create_test.rb"] do
    test_files = FileList["test*.ml"].join(" ")
    sh "ruby create_test.rb #{test_files} > all_test.ml"
end

FileList["*.ml"].each do |filename|
    cmx = "#{filename[0..-4]}.cmx"
    file cmx => filename do
        sh "ocamlopt -c #{filename}"
    end
    task :test_binary => cmx
    if filename[0..4] == 'test_' then
        file cmx => "#{filename[5..-4]}.cmx"
    end
end

desc "Delete droppings"
task :clean do
    ["cmx", "cmi", "o"].each do |file_ending|
        FileList["*.#{file_ending}"].each do |src|
            rm src
        end
    end
    ["all_test.ml", "test"].each do |extra_file|
        if File.exists? extra_file then
            rm extra_file
        end
    end
end
