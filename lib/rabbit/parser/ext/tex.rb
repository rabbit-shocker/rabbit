module Rabbit
  module Parser
    module Ext
      module TeX
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
          command = ["mimetex.cgi", "-e", image_file.path, "-f", path]
          if SystemRunner.run(*command)
            image_file
          else
            raise TeXCanNotHandleError.new(command.join(" "))
          end
        end

        def make_image_by_Tgif(path, prop, logger)
          Tgif.init
          tgif_file = Tempfile.new("rabbit-image-tgif")
          File.open(path) do |f|
            src = []
            f.each_line do |line|
              src << line.chomp
            end
            exp = parse_expression_for_Tgif(src.join(" "), prop, logger)
            exp.set_pos(prop['x'] || 100, prop['y'] || 100)
            tgif_file.open
            tgif_file.print(Tgif::TgifObject.preamble)
            tgif_file.print(exp.tgif_format)
            tgif_file.close
            tgif_file
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

        def parse_expression_for_Tgif(src, prop, logger)
          begin
            token = Tgif::TokenList.new(src)
            exp = Tgif::Expression.from_token(token, nil, prop['color'], logger)
            if exp.is_a?(Tgif::TgifObject)
              exp
            else
              raise Tgif::Error
            end
          rescue Tgif::Error
            raise TeXCanNotHandleError.new(_("invalid source: %s") % src)
          end
        end
      end
    end
  end
end
