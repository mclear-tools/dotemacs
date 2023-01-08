;;;;; Compilation
;; Multi-compile settings
(customize-set-variable 'multi-compile-alist '((org-mode .
                                                         (("pandoc-docx & Open" . "pandoc -s -C -f org+smart --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx   --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua -o %file-sans.docx %file-name && open %file-sans.docx")
                                                          ("pandoc-pdf & Open" . "pandoc -s -C -f org+smart --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                          ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                          ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                          ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")))

                                               ;; commands for pandoc
                                               (markdown-mode .
                                                              (("pandoc-normalize" . "pandoc -f markdown -t markdown -s --id-prefix=%file-sans: --atx-headers --columns=85 --wrap=auto --reference-location=block -o %file-name %file-name")
                                                               ("pandoc-sep-html & Open" . "pandoc -f markdown -t html4 -s --base-header-level=1 --number-sections --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/sep.html4 --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                               ("pandoc-pdf & Open" . "pandoc -s -C --pdf-engine=/Library/TeX/texbin/xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                               ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                               ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")
                                                               ("pandoc-handout & Open" . "pandoc -s -C --pdf-engine=/Library/TeX/texbin/xelatex  --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-docx & Open" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name && open %file-sans.docx")
                                                               ("pandoc-html & Open" . "pandoc -C -f markdown -t html5 -s --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                               ("pandoc-pdf" . "pandoc -s -C --pdf-engine=/Library/TeX/texbin/xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                               ("pandoc-docx" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name")
                                                               ("pandoc-html" . "pandoc -f markdown -t html5 -s -C --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name")
                                                               ("pandoc-handout" . "pandoc -s --pdf-engine=/Library/TeX/texbin/xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                               ("test pdf" . "pandoc -s --pdf-engine=/Library/TeX/texbin/xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-letter-pdf & Open" . "pandoc -s --pdf-engine=/Library/TeX/texbin/xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/Roambot/dotfiles/pandoc/pandoc-templates/letter.tex -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-book-chapter-pdf & Open" . "pandoc -s -N --pdf-engine=/Library/TeX/texbin/xelatex --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --citeproc --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/promote-headers.lua --filter pandoc-latex-color --metadata=reference-section-title:'References' --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ((string/starts-with buffer-file-name "/Users/roambot/Dropbox/Work/projects/Book-Projects/rationality-book/") . (("compile rationality book" . "cd %make-dir && make -k && open %make-dirbuild/pdf/kant-rationality-book.pdf")))))))

(provide 'cpm-setup-multi-compile)
