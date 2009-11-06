#!/usr/bin/env ruby
require "erb"

MODULE_NAMER = /^(.*)\.ml$/
tests = {}
ARGV.each do |filename|
    module_name = MODULE_NAMER.match(filename)[1].capitalize
    File.new(filename, "r").each_line do |line|
        if line =~ /^let (test_\S+) \(\) =/
        then
            if not tests.key? module_name then
                tests[module_name] = []
            end
            tests[module_name] << $1
        end
    end
end

template_text = %q{let main () =
    begin
% a = 0
% tests.each_pair do |mod_name, test_names|
% a += 1
        print_endline "In module <%=mod_name%>:";
% test_names.each do |name|
            print_string "    <%=name%>... ";
            <%=mod_name%>.<%=name%>();
% if a == tests.length then
            print_endline "passed"
% else
            print_newline "passed";

% end
% end
% end
    end;;

main();;
}
template = ERB.new(template_text, 0, "%")
puts template.result
