let
  build = channel: date:
    let pkgs = import ./. {
          rustRelease = { inherit channel date; };
        }; in pkgs.shen_rust;

in rec {

  nightly = nightly_2019_07_15;
  nightly_2019_07_15 = build "nightly" "2019-07-15";
  nightly_2019_07_01 = build "nightly" "2019-07-01";
  nightly_2019_06_15 = build "nightly" "2019-06-15";
  nightly_2019_06_01 = build "nightly" "2019-06-01";

}
