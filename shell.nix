with import <nixpkgs> { };
let

  # define packages to install with special handling for OSX
  basePackages = [
    # Core runtime libraries
    clojure
    jdk
    leiningen
  ];

  inputs = basePackages ++ lib.optional stdenv.isLinux inotify-tools
    ++ lib.optionals stdenv.isDarwin
    (with darwin.apple_sdk.frameworks; [ CoreFoundation CoreServices ]);

  # define shell startup command
  hooks = "";

in mkShell {
  buildInputs = inputs;
  shellHook = hooks;
}
