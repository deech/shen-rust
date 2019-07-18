{ lib, buildRustCrate, buildRustCrateHelpers }:
with buildRustCrateHelpers;
let inherit (lib.lists) fold;
    inherit (lib.attrsets) recursiveUpdate;
in
rec {

# aster-0.25.0

  crates.aster."0.25.0" = deps: { features?(features_.aster."0.25.0" deps {}) }: buildRustCrate {
    crateName = "aster";
    version = "0.25.0";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" ];
    sha256 = "05fsgi7ib2bd77yhcx0s801ab98a84gc08yiqql0xbylp7fcvskw";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.aster."0.25.0".syntex_syntax or false then [ (crates.syntex_syntax."${deps."aster"."0.25.0".syntex_syntax}" deps) ] else []));
    features = mkFeatures (features."aster"."0.25.0" or {});
  };
  features_.aster."0.25.0" = deps: f: updateFeatures f (rec {
    aster = fold recursiveUpdate {} [
      { "0.25.0".clippy =
        (f.aster."0.25.0".clippy or false) ||
        (f.aster."0.25.0".unstable-testing or false) ||
        (aster."0.25.0"."unstable-testing" or false); }
      { "0.25.0".compiletest_rs =
        (f.aster."0.25.0".compiletest_rs or false) ||
        (f.aster."0.25.0".unstable-testing or false) ||
        (aster."0.25.0"."unstable-testing" or false); }
      { "0.25.0".default = (f.aster."0.25.0".default or true); }
      { "0.25.0".syntex_syntax =
        (f.aster."0.25.0".syntex_syntax or false) ||
        (f.aster."0.25.0".with-syntex or false) ||
        (aster."0.25.0"."with-syntex" or false); }
    ];
    syntex_syntax."${deps.aster."0.25.0".syntex_syntax}".default = true;
  }) [
    (features_.syntex_syntax."${deps."aster"."0.25.0"."syntex_syntax"}" deps)
  ];


# end
# bitflags-0.5.0

  crates.bitflags."0.5.0" = deps: { features?(features_.bitflags."0.5.0" deps {}) }: buildRustCrate {
    crateName = "bitflags";
    version = "0.5.0";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0bgw1kiy121kikjrwj6zsd7l8n1gg1jirivzkc7zpjsvqa3p0hla";
    features = mkFeatures (features."bitflags"."0.5.0" or {});
  };
  features_.bitflags."0.5.0" = deps: f: updateFeatures f (rec {
    bitflags."0.5.0".default = (f.bitflags."0.5.0".default or true);
  }) [];


# end
# cfg-if-0.1.9

  crates.cfg_if."0.1.9" = deps: { features?(features_.cfg_if."0.1.9" deps {}) }: buildRustCrate {
    crateName = "cfg-if";
    version = "0.1.9";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "13g9p2mc5b2b5wn716fwvilzib376ycpkgk868yxfp16jzix57p7";
  };
  features_.cfg_if."0.1.9" = deps: f: updateFeatures f (rec {
    cfg_if."0.1.9".default = (f.cfg_if."0.1.9".default or true);
  }) [];


# end
# fuchsia-cprng-0.1.1

  crates.fuchsia_cprng."0.1.1" = deps: { features?(features_.fuchsia_cprng."0.1.1" deps {}) }: buildRustCrate {
    crateName = "fuchsia-cprng";
    version = "0.1.1";
    authors = [ "Erick Tryzelaar <etryzelaar@google.com>" ];
    edition = "2018";
    sha256 = "07apwv9dj716yjlcj29p94vkqn5zmfh7hlrqvrjx3wzshphc95h9";
  };
  features_.fuchsia_cprng."0.1.1" = deps: f: updateFeatures f (rec {
    fuchsia_cprng."0.1.1".default = (f.fuchsia_cprng."0.1.1".default or true);
  }) [];


# end
# kernel32-sys-0.2.2

  crates.kernel32_sys."0.2.2" = deps: { features?(features_.kernel32_sys."0.2.2" deps {}) }: buildRustCrate {
    crateName = "kernel32-sys";
    version = "0.2.2";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "1lrw1hbinyvr6cp28g60z97w32w8vsk6pahk64pmrv2fmby8srfj";
    libName = "kernel32";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."winapi"."${deps."kernel32_sys"."0.2.2"."winapi"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."winapi_build"."${deps."kernel32_sys"."0.2.2"."winapi_build"}" deps)
    ]);
  };
  features_.kernel32_sys."0.2.2" = deps: f: updateFeatures f (rec {
    kernel32_sys."0.2.2".default = (f.kernel32_sys."0.2.2".default or true);
    winapi."${deps.kernel32_sys."0.2.2".winapi}".default = true;
    winapi_build."${deps.kernel32_sys."0.2.2".winapi_build}".default = true;
  }) [
    (features_.winapi."${deps."kernel32_sys"."0.2.2"."winapi"}" deps)
    (features_.winapi_build."${deps."kernel32_sys"."0.2.2"."winapi_build"}" deps)
  ];


# end
# libc-0.2.60

  crates.libc."0.2.60" = deps: { features?(features_.libc."0.2.60" deps {}) }: buildRustCrate {
    crateName = "libc";
    version = "0.2.60";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1zklw10b6lwz6ldamxvdxr8gsxbqhphxhn8n5n5dndl7avafx49b";
    build = "build.rs";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."libc"."0.2.60" or {});
  };
  features_.libc."0.2.60" = deps: f: updateFeatures f (rec {
    libc = fold recursiveUpdate {} [
      { "0.2.60".align =
        (f.libc."0.2.60".align or false) ||
        (f.libc."0.2.60".rustc-dep-of-std or false) ||
        (libc."0.2.60"."rustc-dep-of-std" or false); }
      { "0.2.60".default = (f.libc."0.2.60".default or true); }
      { "0.2.60".rustc-std-workspace-core =
        (f.libc."0.2.60".rustc-std-workspace-core or false) ||
        (f.libc."0.2.60".rustc-dep-of-std or false) ||
        (libc."0.2.60"."rustc-dep-of-std" or false); }
      { "0.2.60".std =
        (f.libc."0.2.60".std or false) ||
        (f.libc."0.2.60".default or false) ||
        (libc."0.2.60"."default" or false) ||
        (f.libc."0.2.60".use_std or false) ||
        (libc."0.2.60"."use_std" or false); }
    ];
  }) [];


# end
# log-0.3.9

  crates.log."0.3.9" = deps: { features?(features_.log."0.3.9" deps {}) }: buildRustCrate {
    crateName = "log";
    version = "0.3.9";
    authors = [ "The Rust Project Developers" ];
    sha256 = "19i9pwp7lhaqgzangcpw00kc3zsgcqcx84crv07xgz3v7d3kvfa2";
    dependencies = mapFeatures features ([
      (crates."log"."${deps."log"."0.3.9"."log"}" deps)
    ]);
    features = mkFeatures (features."log"."0.3.9" or {});
  };
  features_.log."0.3.9" = deps: f: updateFeatures f (rec {
    log = fold recursiveUpdate {} [
      { "${deps.log."0.3.9".log}"."max_level_debug" =
        (f.log."${deps.log."0.3.9".log}"."max_level_debug" or false) ||
        (log."0.3.9"."max_level_debug" or false) ||
        (f."log"."0.3.9"."max_level_debug" or false); }
      { "${deps.log."0.3.9".log}"."max_level_error" =
        (f.log."${deps.log."0.3.9".log}"."max_level_error" or false) ||
        (log."0.3.9"."max_level_error" or false) ||
        (f."log"."0.3.9"."max_level_error" or false); }
      { "${deps.log."0.3.9".log}"."max_level_info" =
        (f.log."${deps.log."0.3.9".log}"."max_level_info" or false) ||
        (log."0.3.9"."max_level_info" or false) ||
        (f."log"."0.3.9"."max_level_info" or false); }
      { "${deps.log."0.3.9".log}"."max_level_off" =
        (f.log."${deps.log."0.3.9".log}"."max_level_off" or false) ||
        (log."0.3.9"."max_level_off" or false) ||
        (f."log"."0.3.9"."max_level_off" or false); }
      { "${deps.log."0.3.9".log}"."max_level_trace" =
        (f.log."${deps.log."0.3.9".log}"."max_level_trace" or false) ||
        (log."0.3.9"."max_level_trace" or false) ||
        (f."log"."0.3.9"."max_level_trace" or false); }
      { "${deps.log."0.3.9".log}"."max_level_warn" =
        (f.log."${deps.log."0.3.9".log}"."max_level_warn" or false) ||
        (log."0.3.9"."max_level_warn" or false) ||
        (f."log"."0.3.9"."max_level_warn" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_debug" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_debug" or false) ||
        (log."0.3.9"."release_max_level_debug" or false) ||
        (f."log"."0.3.9"."release_max_level_debug" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_error" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_error" or false) ||
        (log."0.3.9"."release_max_level_error" or false) ||
        (f."log"."0.3.9"."release_max_level_error" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_info" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_info" or false) ||
        (log."0.3.9"."release_max_level_info" or false) ||
        (f."log"."0.3.9"."release_max_level_info" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_off" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_off" or false) ||
        (log."0.3.9"."release_max_level_off" or false) ||
        (f."log"."0.3.9"."release_max_level_off" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_trace" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_trace" or false) ||
        (log."0.3.9"."release_max_level_trace" or false) ||
        (f."log"."0.3.9"."release_max_level_trace" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_warn" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_warn" or false) ||
        (log."0.3.9"."release_max_level_warn" or false) ||
        (f."log"."0.3.9"."release_max_level_warn" or false); }
      { "${deps.log."0.3.9".log}"."std" =
        (f.log."${deps.log."0.3.9".log}"."std" or false) ||
        (log."0.3.9"."use_std" or false) ||
        (f."log"."0.3.9"."use_std" or false); }
      { "${deps.log."0.3.9".log}".default = true; }
      { "0.3.9".default = (f.log."0.3.9".default or true); }
      { "0.3.9".use_std =
        (f.log."0.3.9".use_std or false) ||
        (f.log."0.3.9".default or false) ||
        (log."0.3.9"."default" or false); }
    ];
  }) [
    (features_.log."${deps."log"."0.3.9"."log"}" deps)
  ];


# end
# log-0.4.7

  crates.log."0.4.7" = deps: { features?(features_.log."0.4.7" deps {}) }: buildRustCrate {
    crateName = "log";
    version = "0.4.7";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0l5y0kd63l6mpw68r74asgk59rwqxmcjz8azjk9fax04r3gyzh05";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."log"."0.4.7"."cfg_if"}" deps)
    ]);
    features = mkFeatures (features."log"."0.4.7" or {});
  };
  features_.log."0.4.7" = deps: f: updateFeatures f (rec {
    cfg_if."${deps.log."0.4.7".cfg_if}".default = true;
    log."0.4.7".default = (f.log."0.4.7".default or true);
  }) [
    (features_.cfg_if."${deps."log"."0.4.7"."cfg_if"}" deps)
  ];


# end
# nom-1.2.4

  crates.nom."1.2.4" = deps: { features?(features_.nom."1.2.4" deps {}) }: buildRustCrate {
    crateName = "nom";
    version = "1.2.4";
    authors = [ "contact@geoffroycouprie.com" ];
    sha256 = "0c0cid817dml3pyhlzsyy47xdc86lhp3dcpa4zim0swp5xq67r50";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."nom"."1.2.4" or {});
  };
  features_.nom."1.2.4" = deps: f: updateFeatures f (rec {
    nom = fold recursiveUpdate {} [
      { "1.2.4".default = (f.nom."1.2.4".default or true); }
      { "1.2.4".lazy_static =
        (f.nom."1.2.4".lazy_static or false) ||
        (f.nom."1.2.4".regexp_macros or false) ||
        (nom."1.2.4"."regexp_macros" or false); }
      { "1.2.4".regex =
        (f.nom."1.2.4".regex or false) ||
        (f.nom."1.2.4".regexp or false) ||
        (nom."1.2.4"."regexp" or false); }
      { "1.2.4".regexp =
        (f.nom."1.2.4".regexp or false) ||
        (f.nom."1.2.4".regexp_macros or false) ||
        (nom."1.2.4"."regexp_macros" or false); }
      { "1.2.4".stream =
        (f.nom."1.2.4".stream or false) ||
        (f.nom."1.2.4".default or false) ||
        (nom."1.2.4"."default" or false); }
    ];
  }) [];


# end
# quasi-0.18.0

  crates.quasi."0.18.0" = deps: { features?(features_.quasi."0.18.0" deps {}) }: buildRustCrate {
    crateName = "quasi";
    version = "0.18.0";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" ];
    sha256 = "02lzkpbak6cixk6zp0a96wi8p2m82q25s55k4vcfy38f58ap8gb8";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.quasi."0.18.0".syntex_errors or false then [ (crates.syntex_errors."${deps."quasi"."0.18.0".syntex_errors}" deps) ] else [])
      ++ (if features.quasi."0.18.0".syntex_syntax or false then [ (crates.syntex_syntax."${deps."quasi"."0.18.0".syntex_syntax}" deps) ] else []));
    features = mkFeatures (features."quasi"."0.18.0" or {});
  };
  features_.quasi."0.18.0" = deps: f: updateFeatures f (rec {
    quasi = fold recursiveUpdate {} [
      { "0.18.0".clippy =
        (f.quasi."0.18.0".clippy or false) ||
        (f.quasi."0.18.0".unstable-testing or false) ||
        (quasi."0.18.0"."unstable-testing" or false); }
      { "0.18.0".default = (f.quasi."0.18.0".default or true); }
      { "0.18.0".syntex_errors =
        (f.quasi."0.18.0".syntex_errors or false) ||
        (f.quasi."0.18.0".with-syntex or false) ||
        (quasi."0.18.0"."with-syntex" or false); }
      { "0.18.0".syntex_syntax =
        (f.quasi."0.18.0".syntex_syntax or false) ||
        (f.quasi."0.18.0".with-syntex or false) ||
        (quasi."0.18.0"."with-syntex" or false); }
    ];
    syntex_errors."${deps.quasi."0.18.0".syntex_errors}".default = true;
    syntex_syntax."${deps.quasi."0.18.0".syntex_syntax}".default = true;
  }) [
    (features_.syntex_errors."${deps."quasi"."0.18.0"."syntex_errors"}" deps)
    (features_.syntex_syntax."${deps."quasi"."0.18.0"."syntex_syntax"}" deps)
  ];


# end
# rand-0.3.23

  crates.rand."0.3.23" = deps: { features?(features_.rand."0.3.23" deps {}) }: buildRustCrate {
    crateName = "rand";
    version = "0.3.23";
    authors = [ "The Rust Project Developers" ];
    sha256 = "118rairvv46npqqx7hmkf97kkimjrry9z31z4inxcv2vn0nj1s2g";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."rand"."0.3.23"."libc"}" deps)
      (crates."rand"."${deps."rand"."0.3.23"."rand"}" deps)
    ]);
    features = mkFeatures (features."rand"."0.3.23" or {});
  };
  features_.rand."0.3.23" = deps: f: updateFeatures f (rec {
    libc."${deps.rand."0.3.23".libc}".default = true;
    rand = fold recursiveUpdate {} [
      { "${deps.rand."0.3.23".rand}".default = true; }
      { "0.3.23".default = (f.rand."0.3.23".default or true); }
      { "0.3.23".i128_support =
        (f.rand."0.3.23".i128_support or false) ||
        (f.rand."0.3.23".nightly or false) ||
        (rand."0.3.23"."nightly" or false); }
    ];
  }) [
    (features_.libc."${deps."rand"."0.3.23"."libc"}" deps)
    (features_.rand."${deps."rand"."0.3.23"."rand"}" deps)
  ];


# end
# rand-0.4.6

  crates.rand."0.4.6" = deps: { features?(features_.rand."0.4.6" deps {}) }: buildRustCrate {
    crateName = "rand";
    version = "0.4.6";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0c3rmg5q7d6qdi7cbmg5py9alm70wd3xsg0mmcawrnl35qv37zfs";
    dependencies = (if abi == "sgx" then mapFeatures features ([
      (crates."rand_core"."${deps."rand"."0.4.6"."rand_core"}" deps)
      (crates."rdrand"."${deps."rand"."0.4.6"."rdrand"}" deps)
    ]) else [])
      ++ (if kernel == "fuchsia" then mapFeatures features ([
      (crates."fuchsia_cprng"."${deps."rand"."0.4.6"."fuchsia_cprng"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
    ]
      ++ (if features.rand."0.4.6".libc or false then [ (crates.libc."${deps."rand"."0.4.6".libc}" deps) ] else [])) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."rand"."0.4.6"."winapi"}" deps)
    ]) else []);
    features = mkFeatures (features."rand"."0.4.6" or {});
  };
  features_.rand."0.4.6" = deps: f: updateFeatures f (rec {
    fuchsia_cprng."${deps.rand."0.4.6".fuchsia_cprng}".default = true;
    libc."${deps.rand."0.4.6".libc}".default = true;
    rand = fold recursiveUpdate {} [
      { "0.4.6".default = (f.rand."0.4.6".default or true); }
      { "0.4.6".i128_support =
        (f.rand."0.4.6".i128_support or false) ||
        (f.rand."0.4.6".nightly or false) ||
        (rand."0.4.6"."nightly" or false); }
      { "0.4.6".libc =
        (f.rand."0.4.6".libc or false) ||
        (f.rand."0.4.6".std or false) ||
        (rand."0.4.6"."std" or false); }
      { "0.4.6".std =
        (f.rand."0.4.6".std or false) ||
        (f.rand."0.4.6".default or false) ||
        (rand."0.4.6"."default" or false); }
    ];
    rand_core."${deps.rand."0.4.6".rand_core}".default = (f.rand_core."${deps.rand."0.4.6".rand_core}".default or false);
    rdrand."${deps.rand."0.4.6".rdrand}".default = true;
    winapi = fold recursiveUpdate {} [
      { "${deps.rand."0.4.6".winapi}"."minwindef" = true; }
      { "${deps.rand."0.4.6".winapi}"."ntsecapi" = true; }
      { "${deps.rand."0.4.6".winapi}"."profileapi" = true; }
      { "${deps.rand."0.4.6".winapi}"."winnt" = true; }
      { "${deps.rand."0.4.6".winapi}".default = true; }
    ];
  }) [
    (features_.rand_core."${deps."rand"."0.4.6"."rand_core"}" deps)
    (features_.rdrand."${deps."rand"."0.4.6"."rdrand"}" deps)
    (features_.fuchsia_cprng."${deps."rand"."0.4.6"."fuchsia_cprng"}" deps)
    (features_.libc."${deps."rand"."0.4.6"."libc"}" deps)
    (features_.winapi."${deps."rand"."0.4.6"."winapi"}" deps)
  ];


# end
# rand_core-0.3.1

  crates.rand_core."0.3.1" = deps: { features?(features_.rand_core."0.3.1" deps {}) }: buildRustCrate {
    crateName = "rand_core";
    version = "0.3.1";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0q0ssgpj9x5a6fda83nhmfydy7a6c0wvxm0jhncsmjx8qp8gw91m";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_core"."0.3.1"."rand_core"}" deps)
    ]);
    features = mkFeatures (features."rand_core"."0.3.1" or {});
  };
  features_.rand_core."0.3.1" = deps: f: updateFeatures f (rec {
    rand_core = fold recursiveUpdate {} [
      { "${deps.rand_core."0.3.1".rand_core}"."alloc" =
        (f.rand_core."${deps.rand_core."0.3.1".rand_core}"."alloc" or false) ||
        (rand_core."0.3.1"."alloc" or false) ||
        (f."rand_core"."0.3.1"."alloc" or false); }
      { "${deps.rand_core."0.3.1".rand_core}"."serde1" =
        (f.rand_core."${deps.rand_core."0.3.1".rand_core}"."serde1" or false) ||
        (rand_core."0.3.1"."serde1" or false) ||
        (f."rand_core"."0.3.1"."serde1" or false); }
      { "${deps.rand_core."0.3.1".rand_core}"."std" =
        (f.rand_core."${deps.rand_core."0.3.1".rand_core}"."std" or false) ||
        (rand_core."0.3.1"."std" or false) ||
        (f."rand_core"."0.3.1"."std" or false); }
      { "${deps.rand_core."0.3.1".rand_core}".default = true; }
      { "0.3.1".default = (f.rand_core."0.3.1".default or true); }
      { "0.3.1".std =
        (f.rand_core."0.3.1".std or false) ||
        (f.rand_core."0.3.1".default or false) ||
        (rand_core."0.3.1"."default" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rand_core"."0.3.1"."rand_core"}" deps)
  ];


# end
# rand_core-0.4.0

  crates.rand_core."0.4.0" = deps: { features?(features_.rand_core."0.4.0" deps {}) }: buildRustCrate {
    crateName = "rand_core";
    version = "0.4.0";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0wb5iwhffibj0pnpznhv1g3i7h1fnhz64s3nz74fz6vsm3q6q3br";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."rand_core"."0.4.0" or {});
  };
  features_.rand_core."0.4.0" = deps: f: updateFeatures f (rec {
    rand_core = fold recursiveUpdate {} [
      { "0.4.0".alloc =
        (f.rand_core."0.4.0".alloc or false) ||
        (f.rand_core."0.4.0".std or false) ||
        (rand_core."0.4.0"."std" or false); }
      { "0.4.0".default = (f.rand_core."0.4.0".default or true); }
      { "0.4.0".serde =
        (f.rand_core."0.4.0".serde or false) ||
        (f.rand_core."0.4.0".serde1 or false) ||
        (rand_core."0.4.0"."serde1" or false); }
      { "0.4.0".serde_derive =
        (f.rand_core."0.4.0".serde_derive or false) ||
        (f.rand_core."0.4.0".serde1 or false) ||
        (rand_core."0.4.0"."serde1" or false); }
    ];
  }) [];


# end
# rdrand-0.4.0

  crates.rdrand."0.4.0" = deps: { features?(features_.rdrand."0.4.0" deps {}) }: buildRustCrate {
    crateName = "rdrand";
    version = "0.4.0";
    authors = [ "Simonas Kazlauskas <rdrand@kazlauskas.me>" ];
    sha256 = "15hrcasn0v876wpkwab1dwbk9kvqwrb3iv4y4dibb6yxnfvzwajk";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rdrand"."0.4.0"."rand_core"}" deps)
    ]);
    features = mkFeatures (features."rdrand"."0.4.0" or {});
  };
  features_.rdrand."0.4.0" = deps: f: updateFeatures f (rec {
    rand_core."${deps.rdrand."0.4.0".rand_core}".default = (f.rand_core."${deps.rdrand."0.4.0".rand_core}".default or false);
    rdrand = fold recursiveUpdate {} [
      { "0.4.0".default = (f.rdrand."0.4.0".default or true); }
      { "0.4.0".std =
        (f.rdrand."0.4.0".std or false) ||
        (f.rdrand."0.4.0".default or false) ||
        (rdrand."0.4.0"."default" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rdrand"."0.4.0"."rand_core"}" deps)
  ];


# end
# redox_syscall-0.1.56

  crates.redox_syscall."0.1.56" = deps: { features?(features_.redox_syscall."0.1.56" deps {}) }: buildRustCrate {
    crateName = "redox_syscall";
    version = "0.1.56";
    authors = [ "Jeremy Soller <jackpot51@gmail.com>" ];
    sha256 = "0jcp8nd947zcy938bz09pzlmi3vyxfdzg92pjxdvvk0699vwcc26";
    libName = "syscall";
  };
  features_.redox_syscall."0.1.56" = deps: f: updateFeatures f (rec {
    redox_syscall."0.1.56".default = (f.redox_syscall."0.1.56".default or true);
  }) [];


# end
# ref_eq-1.0.0

  crates.ref_eq."1.0.0" = deps: { features?(features_.ref_eq."1.0.0" deps {}) }: buildRustCrate {
    crateName = "ref_eq";
    version = "1.0.0";
    authors = [ "Eitan Mosenkis <eitan@mosenkis.net>" ];
    sha256 = "07a28g6p5n8k1n3gsrcjp35xhj4gd6089y8d8w7fm73vbsjkf8hg";
  };
  features_.ref_eq."1.0.0" = deps: f: updateFeatures f (rec {
    ref_eq."1.0.0".default = (f.ref_eq."1.0.0".default or true);
  }) [];


# end
# rustc-serialize-0.3.24

  crates.rustc_serialize."0.3.24" = deps: { features?(features_.rustc_serialize."0.3.24" deps {}) }: buildRustCrate {
    crateName = "rustc-serialize";
    version = "0.3.24";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0rfk6p66mqkd3g36l0ddlv2rvnp1mp3lrq5frq9zz5cbnz5pmmxn";
  };
  features_.rustc_serialize."0.3.24" = deps: f: updateFeatures f (rec {
    rustc_serialize."0.3.24".default = (f.rustc_serialize."0.3.24".default or true);
  }) [];


# end
# syntex-0.42.2

  crates.syntex."0.42.2" = deps: { features?(features_.syntex."0.42.2" deps {}) }: buildRustCrate {
    crateName = "syntex";
    version = "0.42.2";
    authors = [ "erick.tryzelaar@gmail.com" ];
    sha256 = "086xswq4yshh6mpglrmb5y92g206a139xwy1mci8pqffqwxq1r1x";
    dependencies = mapFeatures features ([
      (crates."syntex_errors"."${deps."syntex"."0.42.2"."syntex_errors"}" deps)
      (crates."syntex_syntax"."${deps."syntex"."0.42.2"."syntex_syntax"}" deps)
    ]);
  };
  features_.syntex."0.42.2" = deps: f: updateFeatures f (rec {
    syntex."0.42.2".default = (f.syntex."0.42.2".default or true);
    syntex_errors."${deps.syntex."0.42.2".syntex_errors}".default = true;
    syntex_syntax."${deps.syntex."0.42.2".syntex_syntax}".default = true;
  }) [
    (features_.syntex_errors."${deps."syntex"."0.42.2"."syntex_errors"}" deps)
    (features_.syntex_syntax."${deps."syntex"."0.42.2"."syntex_syntax"}" deps)
  ];


# end
# syntex_errors-0.42.0

  crates.syntex_errors."0.42.0" = deps: { features?(features_.syntex_errors."0.42.0" deps {}) }: buildRustCrate {
    crateName = "syntex_errors";
    version = "0.42.0";
    authors = [ "erick.tryzelaar@gmail.com" ];
    sha256 = "15j7mxa52kxdbwmc6vh0kn96d0jdbirfavy9yyv2jy60qpq53p4b";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."syntex_errors"."0.42.0"."libc"}" deps)
      (crates."log"."${deps."syntex_errors"."0.42.0"."log"}" deps)
      (crates."rustc_serialize"."${deps."syntex_errors"."0.42.0"."rustc_serialize"}" deps)
      (crates."syntex_pos"."${deps."syntex_errors"."0.42.0"."syntex_pos"}" deps)
      (crates."term"."${deps."syntex_errors"."0.42.0"."term"}" deps)
      (crates."unicode_xid"."${deps."syntex_errors"."0.42.0"."unicode_xid"}" deps)
    ]);
  };
  features_.syntex_errors."0.42.0" = deps: f: updateFeatures f (rec {
    libc."${deps.syntex_errors."0.42.0".libc}".default = true;
    log."${deps.syntex_errors."0.42.0".log}".default = true;
    rustc_serialize."${deps.syntex_errors."0.42.0".rustc_serialize}".default = true;
    syntex_errors."0.42.0".default = (f.syntex_errors."0.42.0".default or true);
    syntex_pos."${deps.syntex_errors."0.42.0".syntex_pos}".default = true;
    term."${deps.syntex_errors."0.42.0".term}".default = true;
    unicode_xid."${deps.syntex_errors."0.42.0".unicode_xid}".default = true;
  }) [
    (features_.libc."${deps."syntex_errors"."0.42.0"."libc"}" deps)
    (features_.log."${deps."syntex_errors"."0.42.0"."log"}" deps)
    (features_.rustc_serialize."${deps."syntex_errors"."0.42.0"."rustc_serialize"}" deps)
    (features_.syntex_pos."${deps."syntex_errors"."0.42.0"."syntex_pos"}" deps)
    (features_.term."${deps."syntex_errors"."0.42.0"."term"}" deps)
    (features_.unicode_xid."${deps."syntex_errors"."0.42.0"."unicode_xid"}" deps)
  ];


# end
# syntex_pos-0.42.0

  crates.syntex_pos."0.42.0" = deps: { features?(features_.syntex_pos."0.42.0" deps {}) }: buildRustCrate {
    crateName = "syntex_pos";
    version = "0.42.0";
    authors = [ "erick.tryzelaar@gmail.com" ];
    sha256 = "0g3k4l1b6vagzr5zqmb3wf8m62rnsz1njb9i5f8zifnbiqs21kqx";
    dependencies = mapFeatures features ([
      (crates."rustc_serialize"."${deps."syntex_pos"."0.42.0"."rustc_serialize"}" deps)
    ]);
  };
  features_.syntex_pos."0.42.0" = deps: f: updateFeatures f (rec {
    rustc_serialize."${deps.syntex_pos."0.42.0".rustc_serialize}".default = true;
    syntex_pos."0.42.0".default = (f.syntex_pos."0.42.0".default or true);
  }) [
    (features_.rustc_serialize."${deps."syntex_pos"."0.42.0"."rustc_serialize"}" deps)
  ];


# end
# syntex_syntax-0.42.0

  crates.syntex_syntax."0.42.0" = deps: { features?(features_.syntex_syntax."0.42.0" deps {}) }: buildRustCrate {
    crateName = "syntex_syntax";
    version = "0.42.0";
    authors = [ "erick.tryzelaar@gmail.com" ];
    sha256 = "1kyw91dgbznflqmmsdbi1sfs2yb9wnfn1cpix0867vcb92bbk8mp";
    dependencies = mapFeatures features ([
      (crates."bitflags"."${deps."syntex_syntax"."0.42.0"."bitflags"}" deps)
      (crates."libc"."${deps."syntex_syntax"."0.42.0"."libc"}" deps)
      (crates."log"."${deps."syntex_syntax"."0.42.0"."log"}" deps)
      (crates."rustc_serialize"."${deps."syntex_syntax"."0.42.0"."rustc_serialize"}" deps)
      (crates."syntex_errors"."${deps."syntex_syntax"."0.42.0"."syntex_errors"}" deps)
      (crates."syntex_pos"."${deps."syntex_syntax"."0.42.0"."syntex_pos"}" deps)
      (crates."term"."${deps."syntex_syntax"."0.42.0"."term"}" deps)
      (crates."unicode_xid"."${deps."syntex_syntax"."0.42.0"."unicode_xid"}" deps)
    ]);
  };
  features_.syntex_syntax."0.42.0" = deps: f: updateFeatures f (rec {
    bitflags."${deps.syntex_syntax."0.42.0".bitflags}".default = true;
    libc."${deps.syntex_syntax."0.42.0".libc}".default = true;
    log."${deps.syntex_syntax."0.42.0".log}".default = true;
    rustc_serialize."${deps.syntex_syntax."0.42.0".rustc_serialize}".default = true;
    syntex_errors."${deps.syntex_syntax."0.42.0".syntex_errors}".default = true;
    syntex_pos."${deps.syntex_syntax."0.42.0".syntex_pos}".default = true;
    syntex_syntax."0.42.0".default = (f.syntex_syntax."0.42.0".default or true);
    term."${deps.syntex_syntax."0.42.0".term}".default = true;
    unicode_xid."${deps.syntex_syntax."0.42.0".unicode_xid}".default = true;
  }) [
    (features_.bitflags."${deps."syntex_syntax"."0.42.0"."bitflags"}" deps)
    (features_.libc."${deps."syntex_syntax"."0.42.0"."libc"}" deps)
    (features_.log."${deps."syntex_syntax"."0.42.0"."log"}" deps)
    (features_.rustc_serialize."${deps."syntex_syntax"."0.42.0"."rustc_serialize"}" deps)
    (features_.syntex_errors."${deps."syntex_syntax"."0.42.0"."syntex_errors"}" deps)
    (features_.syntex_pos."${deps."syntex_syntax"."0.42.0"."syntex_pos"}" deps)
    (features_.term."${deps."syntex_syntax"."0.42.0"."term"}" deps)
    (features_.unicode_xid."${deps."syntex_syntax"."0.42.0"."unicode_xid"}" deps)
  ];


# end
# term-0.4.6

  crates.term."0.4.6" = deps: { features?(features_.term."0.4.6" deps {}) }: buildRustCrate {
    crateName = "term";
    version = "0.4.6";
    authors = [ "The Rust Project Developers" "Steven Allen" ];
    sha256 = "14fll0l6247b4iyxnj52lpvxhd10yxbkmnpyxrn84iafzqmp56kv";
    dependencies = (if kernel == "windows" then mapFeatures features ([
      (crates."kernel32_sys"."${deps."term"."0.4.6"."kernel32_sys"}" deps)
      (crates."winapi"."${deps."term"."0.4.6"."winapi"}" deps)
    ]) else []);
  };
  features_.term."0.4.6" = deps: f: updateFeatures f (rec {
    kernel32_sys."${deps.term."0.4.6".kernel32_sys}".default = true;
    term."0.4.6".default = (f.term."0.4.6".default or true);
    winapi."${deps.term."0.4.6".winapi}".default = true;
  }) [
    (features_.kernel32_sys."${deps."term"."0.4.6"."kernel32_sys"}" deps)
    (features_.winapi."${deps."term"."0.4.6"."winapi"}" deps)
  ];


# end
# time-0.1.42

  crates.time."0.1.42" = deps: { features?(features_.time."0.1.42" deps {}) }: buildRustCrate {
    crateName = "time";
    version = "0.1.42";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1ny809kmdjwd4b478ipc33dz7q6nq7rxk766x8cnrg6zygcksmmx";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."time"."0.1.42"."libc"}" deps)
    ])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."time"."0.1.42"."redox_syscall"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."time"."0.1.42"."winapi"}" deps)
    ]) else []);
  };
  features_.time."0.1.42" = deps: f: updateFeatures f (rec {
    libc."${deps.time."0.1.42".libc}".default = true;
    redox_syscall."${deps.time."0.1.42".redox_syscall}".default = true;
    time."0.1.42".default = (f.time."0.1.42".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.time."0.1.42".winapi}"."minwinbase" = true; }
      { "${deps.time."0.1.42".winapi}"."minwindef" = true; }
      { "${deps.time."0.1.42".winapi}"."ntdef" = true; }
      { "${deps.time."0.1.42".winapi}"."profileapi" = true; }
      { "${deps.time."0.1.42".winapi}"."std" = true; }
      { "${deps.time."0.1.42".winapi}"."sysinfoapi" = true; }
      { "${deps.time."0.1.42".winapi}"."timezoneapi" = true; }
      { "${deps.time."0.1.42".winapi}".default = true; }
    ];
  }) [
    (features_.libc."${deps."time"."0.1.42"."libc"}" deps)
    (features_.redox_syscall."${deps."time"."0.1.42"."redox_syscall"}" deps)
    (features_.winapi."${deps."time"."0.1.42"."winapi"}" deps)
  ];


# end
# unicode-xid-0.0.3

  crates.unicode_xid."0.0.3" = deps: { features?(features_.unicode_xid."0.0.3" deps {}) }: buildRustCrate {
    crateName = "unicode-xid";
    version = "0.0.3";
    authors = [ "erick.tryzelaar <erick.tryzelaar@gmail.com>" "kwantam <kwantam@gmail.com>" ];
    sha256 = "1azc1dh9lz7x8ywzsiclnd9j78pgjyh67z9irkgjprqj4s8dgx9s";
    features = mkFeatures (features."unicode_xid"."0.0.3" or {});
  };
  features_.unicode_xid."0.0.3" = deps: f: updateFeatures f (rec {
    unicode_xid."0.0.3".default = (f.unicode_xid."0.0.3".default or true);
  }) [];


# end
# uuid-0.2.3

  crates.uuid."0.2.3" = deps: { features?(features_.uuid."0.2.3" deps {}) }: buildRustCrate {
    crateName = "uuid";
    version = "0.2.3";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0xw4cy74rwkfqrzbqf85349q36iz20fagdnj8d82h0rdjlmb9na3";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.uuid."0.2.3".rand or false then [ (crates.rand."${deps."uuid"."0.2.3".rand}" deps) ] else []));
    features = mkFeatures (features."uuid"."0.2.3" or {});
  };
  features_.uuid."0.2.3" = deps: f: updateFeatures f (rec {
    rand."${deps.uuid."0.2.3".rand}".default = true;
    uuid = fold recursiveUpdate {} [
      { "0.2.3".default = (f.uuid."0.2.3".default or true); }
      { "0.2.3".rand =
        (f.uuid."0.2.3".rand or false) ||
        (f.uuid."0.2.3".v4 or false) ||
        (uuid."0.2.3"."v4" or false); }
      { "0.2.3".sha1 =
        (f.uuid."0.2.3".sha1 or false) ||
        (f.uuid."0.2.3".v5 or false) ||
        (uuid."0.2.3"."v5" or false); }
    ];
  }) [
    (features_.rand."${deps."uuid"."0.2.3"."rand"}" deps)
  ];


# end
# winapi-0.2.8

  crates.winapi."0.2.8" = deps: { features?(features_.winapi."0.2.8" deps {}) }: buildRustCrate {
    crateName = "winapi";
    version = "0.2.8";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "0a45b58ywf12vb7gvj6h3j264nydynmzyqz8d8rqxsj6icqv82as";
  };
  features_.winapi."0.2.8" = deps: f: updateFeatures f (rec {
    winapi."0.2.8".default = (f.winapi."0.2.8".default or true);
  }) [];


# end
# winapi-0.3.7

  crates.winapi."0.3.7" = deps: { features?(features_.winapi."0.3.7" deps {}) }: buildRustCrate {
    crateName = "winapi";
    version = "0.3.7";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "1k51gfkp0zqw7nj07y443mscs46icmdhld442s2073niap0kkdr8";
    build = "build.rs";
    dependencies = (if kernel == "i686-pc-windows-gnu" then mapFeatures features ([
      (crates."winapi_i686_pc_windows_gnu"."${deps."winapi"."0.3.7"."winapi_i686_pc_windows_gnu"}" deps)
    ]) else [])
      ++ (if kernel == "x86_64-pc-windows-gnu" then mapFeatures features ([
      (crates."winapi_x86_64_pc_windows_gnu"."${deps."winapi"."0.3.7"."winapi_x86_64_pc_windows_gnu"}" deps)
    ]) else []);
    features = mkFeatures (features."winapi"."0.3.7" or {});
  };
  features_.winapi."0.3.7" = deps: f: updateFeatures f (rec {
    winapi = fold recursiveUpdate {} [
      { "0.3.7".default = (f.winapi."0.3.7".default or true); }
      { "0.3.7".impl-debug =
        (f.winapi."0.3.7".impl-debug or false) ||
        (f.winapi."0.3.7".debug or false) ||
        (winapi."0.3.7"."debug" or false); }
    ];
    winapi_i686_pc_windows_gnu."${deps.winapi."0.3.7".winapi_i686_pc_windows_gnu}".default = true;
    winapi_x86_64_pc_windows_gnu."${deps.winapi."0.3.7".winapi_x86_64_pc_windows_gnu}".default = true;
  }) [
    (features_.winapi_i686_pc_windows_gnu."${deps."winapi"."0.3.7"."winapi_i686_pc_windows_gnu"}" deps)
    (features_.winapi_x86_64_pc_windows_gnu."${deps."winapi"."0.3.7"."winapi_x86_64_pc_windows_gnu"}" deps)
  ];


# end
# winapi-build-0.1.1

  crates.winapi_build."0.1.1" = deps: { features?(features_.winapi_build."0.1.1" deps {}) }: buildRustCrate {
    crateName = "winapi-build";
    version = "0.1.1";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "1lxlpi87rkhxcwp2ykf1ldw3p108hwm24nywf3jfrvmff4rjhqga";
    libName = "build";
  };
  features_.winapi_build."0.1.1" = deps: f: updateFeatures f (rec {
    winapi_build."0.1.1".default = (f.winapi_build."0.1.1".default or true);
  }) [];


# end
# winapi-i686-pc-windows-gnu-0.4.0

  crates.winapi_i686_pc_windows_gnu."0.4.0" = deps: { features?(features_.winapi_i686_pc_windows_gnu."0.4.0" deps {}) }: buildRustCrate {
    crateName = "winapi-i686-pc-windows-gnu";
    version = "0.4.0";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "05ihkij18r4gamjpxj4gra24514can762imjzlmak5wlzidplzrp";
    build = "build.rs";
  };
  features_.winapi_i686_pc_windows_gnu."0.4.0" = deps: f: updateFeatures f (rec {
    winapi_i686_pc_windows_gnu."0.4.0".default = (f.winapi_i686_pc_windows_gnu."0.4.0".default or true);
  }) [];


# end
# winapi-x86_64-pc-windows-gnu-0.4.0

  crates.winapi_x86_64_pc_windows_gnu."0.4.0" = deps: { features?(features_.winapi_x86_64_pc_windows_gnu."0.4.0" deps {}) }: buildRustCrate {
    crateName = "winapi-x86_64-pc-windows-gnu";
    version = "0.4.0";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "0n1ylmlsb8yg1v583i4xy0qmqg42275flvbc51hdqjjfjcl9vlbj";
    build = "build.rs";
  };
  features_.winapi_x86_64_pc_windows_gnu."0.4.0" = deps: f: updateFeatures f (rec {
    winapi_x86_64_pc_windows_gnu."0.4.0".default = (f.winapi_x86_64_pc_windows_gnu."0.4.0".default or true);
  }) [];


# end
}
