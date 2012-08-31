module Rabbit
  module Parser
    module Ext
      module TeX
        include GetText

        module_function
        def make_image_by_LaTeX(path, prop, logger)
          image_file = Tempfile.new("rabbit-image")
          latex_file = Tempfile.new("rabbit-image-latex")
          dir = File.dirname(latex_file.path)
          base = latex_file.path.sub(/\.[^.]+$/, '')
          dvi_path = "#{base}.dvi"
          eps_path = "#{base}.eps"
          log_path = "#{base}.log"
          aux_path = "#{base}.aux"
          File.open(path) do |f|
            src = []
            f.each_line do |line|
              src << line.chomp
            end
            latex_file.open
            latex_file.puts(make_latex_source(src.join("\n"), prop))
            latex_file.close
            begin
              latex_command = ["latex", "-halt-on-error",
                               "-output-directory=#{dir}", latex_file.path]
              dvips_command = ["dvips", "-q", "-E", dvi_path, "-o", eps_path]
              unless SystemRunner.run(*latex_command)
                raise TeXCanNotHandleError.new(latex_command.join(" "))
              end
              unless SystemRunner.run(*dvips_command)
                raise TeXCanNotHandleError.new(dvips_command.join(" "))
              end
              FileUtils.mv(eps_path, image_file.path)
              image_file
            ensure
              FileUtils.rm_f(dvi_path)
              FileUtils.rm_f(eps_path)
              FileUtils.rm_f(log_path)
              FileUtils.rm_f(aux_path)
            end
          end
        end

        def make_image_by_mimeTeX(path, prop, logger)
          image_file = Tempfile.new("rabbit-image-mimetex")
          args = ["-e", image_file.path, "-f", path]
          commands = ["mimetex", "mimetex.cgi"]
          if commands.any? {|command| SystemRunner.run(command, *args)}
            image_file
          else
            format = _("tried mimeTeX commands: %s")
            additional_info = format % commands.inspect
            raise TeXCanNotHandleError.new("mimetex #{args.join(' ')}",
                                           additional_info)
          end
        end

        def make_latex_source(src, prop)
          latex = "\\documentclass[fleqn]{article}\n"
          latex << "\\usepackage[latin1]{inputenc}\n"
          (prop["style"] || "").split.each do |style|
            latex << "\\usepackage[#{style}]\n"
          end
          latex << <<-PREAMBLE
\\begin{document}
\\thispagestyle{empty}
\\mathindent0cm
\\parindent0cm
PREAMBLE
          latex << src
          latex << "\n\\end{document}\n"
          latex
        end
      end
    end
  end
end
