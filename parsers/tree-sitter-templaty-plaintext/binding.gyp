{
  "targets": [
    {
      "target_name": "tree_sitter_templaty_binding",
      "include_dirs": [
        "<!(node -e \"require('nan')\")",
        "src"
      ],
      "sources": [
        "bindings/node/binding.cc",
        "src/scanner.c",
        "src/parser.c",
        #"src/scanner.c"
        # If your language uses an external scanner, add it here.
      ],
      "cflags_c": [
        "-std=c99",
      ]
    }
  ]
}
