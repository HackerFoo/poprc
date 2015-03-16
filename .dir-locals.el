((nil . ((eval . (setq default-directory (locate-dominating-file buffer-file-name ".dir-locals.el")))
         (flycheck-clang-args . ("-D USE_LLVM"
                                 "-D USE_LINENOISE"
                                 "-I/opt/local/libexec/llvm-3.7/include"
                                 "-D_DEBUG"
                                 "-D_GNU_SOURCE"
                                 "-D__STDC_CONSTANT_MACROS"
                                 "-D__STDC_FORMAT_MACROS"
                                 "-D__STDC_LIMIT_MACROS"
                                 "-O3"
                                 "-g"
                                 "-std=c++11"
                                 "-fvisibility-inlines-hidden"
                                 "-fno-exceptions"
                                 "-fno-common"
                                 "-Wcast-qual"))))
 (c-mode . ((flycheck-checker . c/c++-clang)))
 (c++-mode . ((flycheck-checker . c/c++-clang))))
     
